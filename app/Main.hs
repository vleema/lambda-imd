{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Web.Scotty

{-
 TODO: implement / route as html documentation informing on how to use the system
 TODO: implement /subscribe as the root that will receive a email to be subscribed. Then route should return the respective email uuid for further deletion.
 TODO: implement /notices route as a root that will respond the notices that the system has stored. (Maybe add filter options)
-}

routes :: IO ()
routes = scotty 8080 $ do
  post "/subscribe" undefined
  get "/list" undefined

main :: IO ()
main = routes
