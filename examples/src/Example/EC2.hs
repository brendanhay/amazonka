{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Example.EC2
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Example.EC2 where

import Control.Lens
import Control.Monad.IO.Class
import Data.ByteString.Builder (hPutBuilder)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Generics.Product
import Network.AWS
import Network.AWS.EC2
import System.IO

instanceOverview :: Region -> IO ()
instanceOverview r = do
  lgr <- newLogger Info stdout
  env <- newEnv Discover <&> set (field @"envLogger") lgr . within r

  let pp x =
        mconcat
          [ "[instance:" <> build (x ^. field @"instanceId") <> "] {",
            "\n  public-dns = " <> build (x ^. field @"publicDnsName"),
            "\n  tags       = " <> build (x ^. field @"tags" . to show),
            "\n  state      = " <> build (x ^. field @"state" . field @"name" . to toBS),
            "\n}\n"
          ]

  runResourceT . runConduit $
    paginate env newDescribeInstances
    .| CL.concatMap (view (field @"reservations" . _Just))
    .| CL.concatMap (view (field @"instances" . _Just))
    .| CL.mapM_ (liftIO . hPutBuilder stdout . pp)
