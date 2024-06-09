{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- A demonstration of the hooks system that arrived in Amazonka 2.0.
--
-- This example uses hooks to:
--
-- 1. Log the type name of any AWS request that it issues.
-- 2. Log any GetObject requests (of which there are none).
-- 3. Log the owner name returned by any ListBuckets calls.
module Hooks where

import Amazonka (ClientResponse, Request, discover, newEnv, runResourceT, send)
import Amazonka.Env.Hooks (addHook, addHookFor, addHookFor_, requestHook, responseHook)
import Amazonka.S3
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.Generics.Labels ()
import Data.Proxy (Proxy (..))
import Data.Text.IO (putStrLn)
import Data.Typeable (typeRep)
import Network.HTTP.Client (responseBody)
import Prelude hiding (putStrLn)

main :: IO ()
main = do
  env <-
    newEnv discover
      <&> #hooks
        %~ requestHook
          ( -- A blanket request hook, which fires for any request. We
            -- have a Typeable constraint in scope, so we can capture
            -- the TypeRep (and here print its name), as well as doing
            -- other tricks using Data.Typeable.
            addHook $ \_env (req :: req) -> do
              putStr "Requesting: "
              print (typeRep (Proxy @req))
              pure req
          )
          . requestHook
            ( -- A request hook for a specific request type.
              addHookFor @GetObject $ \_env req -> do
                putStrLn "This hook fires when you call GetObject"
                putStrLn "(This code doesn't, so you won't see this printed.)"
                pure req
            )
          . responseHook
            ( -- A response hook. Note that you have to match the type
              -- of the hook argument exactly, or it won't get called.
              addHookFor_ @(Request ListBuckets, ClientResponse ListBucketsResponse) $
                \_env (_, resp) -> do
                  putStr "Received ListBucketsResponse with "
                  case responseBody resp ^? #owner . folded . #displayName . folded of
                    Nothing -> putStrLn "no named owner"
                    Just name -> putStrLn $ "owner: " <> name
            )

  -- List S3 buckets.
  runResourceT $ do
    bucketNames <-
      send env newListBuckets
        <&> toListOf (#buckets . folded . folded . #name . _BucketName)
    liftIO $ putStrLn "Bucket names are:"
    liftIO $ traverse_ putStrLn bucketNames
