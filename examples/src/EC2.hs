{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module EC2 where

import Amazonka
import Amazonka.EC2
import Control.Lens
import Control.Monad.IO.Class
import Data.ByteString.Builder (hPutBuilder)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Generics.Labels ()
import System.IO

instanceOverview :: Region -> IO ()
instanceOverview reg = do
  lgr <- newLogger Info stdout
  env <- newEnv discover <&> set #logger lgr . set #region reg

  let pp x =
        mconcat
          [ "[instance:" <> build (x ^. #instanceId) <> "] {",
            "\n  public-dns = " <> build (x ^. #publicDnsName),
            "\n  tags       = " <> build (x ^. #tags . to show),
            "\n  state      = " <> build (x ^. #state . #name . to fromInstanceStateName),
            "\n}\n"
          ]

  runResourceT . runConduit $
    paginate env newDescribeInstances
      .| CL.concatMap (view $ #reservations . _Just)
      .| CL.concatMap (view $ #instances . _Just)
      .| CL.mapM_ (liftIO . hPutBuilder stdout . pp)
