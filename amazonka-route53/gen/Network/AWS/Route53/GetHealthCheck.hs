{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.GetHealthCheck
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | To retrieve the health check, send a GET request to the
-- 2013-04-01/healthcheck/health check ID resource.
module Network.AWS.Route53
    (
    -- * Request
      GetHealthCheck
    -- ** Request constructor
    , mkGetHealthCheck
    -- ** Request lenses
    , ghcHealthCheckId

    -- * Response
    , GetHealthCheckResponse
    -- ** Response constructor
    , mkGetHealthCheckResponse
    -- ** Response lenses
    , ghcrHealthCheck
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | A complex type that contains information about the request to get a health
-- check.
newtype GetHealthCheck = GetHealthCheck
    { _ghcHealthCheckId :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetHealthCheck' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @HealthCheckId ::@ @Text@
--
mkGetHealthCheck :: Text -- ^ 'ghcHealthCheckId'
                 -> GetHealthCheck
mkGetHealthCheck p1 = GetHealthCheck
    { _ghcHealthCheckId = p1
    }

-- | The ID of the health check to retrieve.
ghcHealthCheckId :: Lens' GetHealthCheck Text
ghcHealthCheckId =
    lens _ghcHealthCheckId (\s a -> s { _ghcHealthCheckId = a })

instance ToPath GetHealthCheck

instance ToQuery GetHealthCheck

instance ToHeaders GetHealthCheck

instance ToXML GetHealthCheck where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GetHealthCheckRequest"

-- | A complex type containing information about the specified health check.
newtype GetHealthCheckResponse = GetHealthCheckResponse
    { _ghcrHealthCheck :: HealthCheck
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetHealthCheckResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @HealthCheck ::@ @HealthCheck@
--
mkGetHealthCheckResponse :: HealthCheck -- ^ 'ghcrHealthCheck'
                         -> GetHealthCheckResponse
mkGetHealthCheckResponse p1 = GetHealthCheckResponse
    { _ghcrHealthCheck = p1
    }

-- | A complex type that contains the information about the specified health
-- check.
ghcrHealthCheck :: Lens' GetHealthCheckResponse HealthCheck
ghcrHealthCheck = lens _ghcrHealthCheck (\s a -> s { _ghcrHealthCheck = a })

instance FromXML GetHealthCheckResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetHealthCheck where
    type Sv GetHealthCheck = Route53
    type Rs GetHealthCheck = GetHealthCheckResponse

    request = get
    response _ = xmlResponse
