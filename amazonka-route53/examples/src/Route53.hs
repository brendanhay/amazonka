{-# LANGUAGE OverloadedStrings #-}

-- Module      : Route53
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Route53 where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.AWS
import Network.AWS.Route53
import System.IO

example :: IO (Either Error ListHostedZonesResponse)
example = do
    lgr <- newLogger Trace stdout
    env <- getEnv Ireland Discover <&> envLogger .~ lgr
    runAWST (env & envRetryCheck .~ (\_ _ -> return False))
        . once
        $ send listHostedZones
