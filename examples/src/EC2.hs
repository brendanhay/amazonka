{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module EC2 where

import Control.Lens
import Control.Monad.IO.Class
import Data.ByteString.Builder (hPutBuilder)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Generics.Product
import Amazonka
import Amazonka.EC2
import System.IO

instanceOverview :: Region -> IO ()
instanceOverview r = do
  lgr <- newLogger Info stdout
  env <- newEnv Discover <&> set (field @"_envLogger") lgr . within r

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
