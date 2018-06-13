{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Familia where

import Import
import Database.Persist.Postgresql

getFamCemiIdR :: CemiterioId -> Handler Value
getFamCemiIdR cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    familiasCemiterio <- runDB $ selectList [FamiliaCemiterioId ==. cid] []
    sendStatusJSON ok200 (object ["resp" .= familiasCemiterio])
    
putFamIdR :: FamiliaId -> Handler Value
putFamIdR fid = do
    addHeader "Access-Control-Allow-Origin" "*"
    familia <- requireJsonBody :: Handler Familia
    runDB $ replace fid familia
    sendStatusJSON ok200 (object ["resp" .= fromSqlKey fid])    

deleteFamIdR :: FamiliaId -> Handler Value
deleteFamIdR cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    _ <- runDB $ get404 cid
    runDB $ delete cid
    sendStatusJSON noContent204 (object [])

getFamIdR :: FamiliaId -> Handler Value
getFamIdR cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    familia <- runDB $ get404 cid
    sendStatusJSON ok200 (object ["resp" .= familia])    

getFamiliaR :: Handler Value
getFamiliaR = do
    addHeader "Access-Control-Allow-Origin" "*"
    todasFamilias <- runDB $ selectList [] [Asc FamiliaNome]
    sendStatusJSON ok200 (object ["resp" .= todasFamilias])
