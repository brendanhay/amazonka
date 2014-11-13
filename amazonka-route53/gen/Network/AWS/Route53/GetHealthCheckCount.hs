{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.Route53.GetHealthCheckCount
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | To retrieve a count of all your health checks, send a GET request to the
-- 2013-04-01/healthcheckcount resource.
module Network.AWS.Route53.GetHealthCheckCount
    (
    -- * Request
      GetHealthCheckCount
    -- ** Request constructor
    , getHealthCheckCount

    -- * Response
    , GetHealthCheckCountResponse
    -- ** Response constructor
    , getHealthCheckCountResponse
    -- ** Response lenses
    , ghccrHealthCheckCount
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Route53.Types

data GetHealthCheckCount = GetHealthCheckCount
    deriving (Eq, Ord, Show, Generic)

-- | 'GetHealthCheckCount' constructor.
getHealthCheckCount :: GetHealthCheckCount
getHealthCheckCount = GetHealthCheckCount

instance ToPath GetHealthCheckCount where
    toPath = const "/2013-04-01/healthcheckcount"

instance ToQuery GetHealthCheckCount where
    toQuery = const mempty

instance ToHeaders GetHealthCheckCount

newtype GetHealthCheckCountResponse = GetHealthCheckCountResponse
    { _ghccrHealthCheckCount :: Integer
    } deriving (Eq, Ord, Show, Generic, Enum, Num, Integral, Real)

-- | 'GetHealthCheckCountResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ghccrHealthCheckCount' @::@ 'Integer'
--
getHealthCheckCountResponse :: Integer -- ^ 'ghccrHealthCheckCount'
                            -> GetHealthCheckCountResponse
getHealthCheckCountResponse p1 = GetHealthCheckCountResponse
    { _ghccrHealthCheckCount = p1
    }

-- | The number of health checks associated with the current AWS account.
ghccrHealthCheckCount :: Lens' GetHealthCheckCountResponse Integer
ghccrHealthCheckCount =
    lens _ghccrHealthCheckCount (\s a -> s { _ghccrHealthCheckCount = a })

instance AWSRequest GetHealthCheckCount where
    type Sv GetHealthCheckCount = Route53
    type Rs GetHealthCheckCount = GetHealthCheckCountResponse

    request  = get
    response = xmlResponse $ \h x -> GetHealthCheckCountResponse
        <$> x %| "HealthCheckCount"
