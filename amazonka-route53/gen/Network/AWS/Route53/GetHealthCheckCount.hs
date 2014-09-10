{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53
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
module Network.AWS.Route53
    (
    -- * Request
      GetHealthCheckCount
    -- ** Request constructor
    , mkGetHealthCheckCount
    -- * Response
    , GetHealthCheckCountResponse
    -- ** Response constructor
    , mkGetHealthCheckCountResponse
    -- ** Response lenses
    , ghccrHealthCheckCount
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | To retrieve a count of all your health checks, send a GET request to the
-- 2013-04-01/healthcheckcount resource.
data GetHealthCheckCount = GetHealthCheckCount
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetHealthCheckCount' request.
mkGetHealthCheckCount :: GetHealthCheckCount
mkGetHealthCheckCount = GetHealthCheckCount

instance ToPath GetHealthCheckCount

instance ToQuery GetHealthCheckCount

instance ToHeaders GetHealthCheckCount

instance ToXML GetHealthCheckCount where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GetHealthCheckCountRequest"

-- | A complex type that contains the count of health checks associated with the
-- current AWS account.
newtype GetHealthCheckCountResponse = GetHealthCheckCountResponse
    { _ghccrHealthCheckCount :: !Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetHealthCheckCountResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @HealthCheckCount ::@ @Integer@
--
mkGetHealthCheckCountResponse :: Integer -- ^ 'ghccrHealthCheckCount'
                              -> GetHealthCheckCountResponse
mkGetHealthCheckCountResponse p1 = GetHealthCheckCountResponse
    { _ghccrHealthCheckCount = p1
    }

-- | The number of health checks associated with the current AWS account.
ghccrHealthCheckCount :: Lens' GetHealthCheckCountResponse Integer
ghccrHealthCheckCount =
    lens _ghccrHealthCheckCount (\s a -> s { _ghccrHealthCheckCount = a })

instance FromXML GetHealthCheckCountResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetHealthCheckCount where
    type Sv GetHealthCheckCount = Route53
    type Rs GetHealthCheckCount = GetHealthCheckCountResponse

    request = get
    response _ = xmlResponse
