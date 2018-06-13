{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Cemiterio where

import Import
import Database.Persist.Postgresql

deleteCemIdR :: CemiterioId -> Handler Value
deleteCemIdR cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    _ <- runDB $ get404 cid
    runDB $ delete cid
    sendStatusJSON noContent204 (object [])
    
getCemIdR :: CemiterioId -> Handler Value
getCemIdR cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    cemiterio <- runDB $ get404 cid
    sendStatusJSON ok200 (object ["resp" .= cemiterio])    
    
putCemIdR :: CemiterioId -> Handler Value
putCemIdR cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    cemiterio <- requireJsonBody :: Handler Cemiterio
    runDB $ replace cid cemiterio
    sendStatusJSON ok200 (object ["resp" .= fromSqlKey cid])    
