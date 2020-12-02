{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Example.EC2
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Example.EC2 where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.AWS
import           Data.ByteString.Builder (hPutBuilder)
import           Data.Conduit
import qualified Data.Conduit.List       as CL
import           Data.Monoid
import           Network.AWS.Data
import           Network.AWS.EC2
import           System.IO

instanceOverview :: Region -> IO ()
instanceOverview r = do
    lgr <- newLogger Info stdout
    env <- newEnv Discover <&> set envLogger lgr

    let pp x = mconcat
          [ "[instance:" <> build (x ^. insInstanceId) <> "] {"
          , "\n  public-dns = " <> build (x ^. insPublicDNSName)
          , "\n  tags       = " <> build (x ^. insTags . to show)
          , "\n  state      = " <> build (x ^. insState . isName . to toBS)
          , "\n}\n"
          ]

    runResourceT . runAWST env . within r $
        paginate describeInstances
            =$= CL.concatMap (view dirsReservations)
            =$= CL.concatMap (view rInstances)
             $$ CL.mapM_ (liftIO . hPutBuilder stdout . pp)
