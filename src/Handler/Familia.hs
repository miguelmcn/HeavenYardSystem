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
