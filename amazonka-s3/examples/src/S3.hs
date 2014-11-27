{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- Module      : S3
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module S3 where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.AWS
import           Data.ByteString.Builder (Builder)
import           Data.Conduit
import qualified Data.Conduit.List       as Conduit
import           Data.Monoid
import           Network.AWS.S3
import           System.IO

default (Builder)

listAllObjects :: IO (Either Error ())
listAllObjects = do
    lgr <- newLogger Info stdout
    env <- getEnv Ireland Discover <&> envLogger .~ lgr
    runAWST env $ do
        logInfo "Listing Buckets ..."
        bs <- view lbrBuckets <$> send listBuckets
        forM_ bs $ \(view bName -> b) -> do
            logInfo $ "Listing Keys in " <> build b
            paginate (listObjects b)
                =$ Conduit.concatMap (view lorContents)
                $$ Conduit.mapM_ (logInfo . view oKey)
        logInfo "Completed."
