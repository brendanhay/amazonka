{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.V2013_04_01.GetHealthCheck
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
module Network.AWS.Route53.V2013_04_01.GetHealthCheck
    (
    -- * Request
      GetHealthCheck
    -- ** Request constructor
    , mkGetHealthCheckRequest
    -- ** Request lenses
    , ghcrHealthCheckId

    -- * Response
    , GetHealthCheckResponse
    -- ** Response lenses
    , ghcsHealthCheck
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.V2013_04_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetHealthCheck' request.
mkGetHealthCheckRequest :: Text -- ^ 'ghcrHealthCheckId'
                        -> GetHealthCheck
mkGetHealthCheckRequest p1 = GetHealthCheck
    { _ghcrHealthCheckId = p1
    }
{-# INLINE mkGetHealthCheckRequest #-}

newtype GetHealthCheck = GetHealthCheck
    { _ghcrHealthCheckId :: Text
      -- ^ The ID of the health check to retrieve.
    } deriving (Show, Generic)

-- | The ID of the health check to retrieve.
ghcrHealthCheckId :: Lens' GetHealthCheck (Text)
ghcrHealthCheckId = lens _ghcrHealthCheckId (\s a -> s { _ghcrHealthCheckId = a })
{-# INLINE ghcrHealthCheckId #-}

instance ToPath GetHealthCheck where
    toPath GetHealthCheck{..} = mconcat
        [ "/2013-04-01/healthcheck/"
        , toBS _ghcrHealthCheckId
        ]

instance ToQuery GetHealthCheck

instance ToHeaders GetHealthCheck

instance ToXML GetHealthCheck where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GetHealthCheckRequest"

newtype GetHealthCheckResponse = GetHealthCheckResponse
    { _ghcsHealthCheck :: HealthCheck
      -- ^ A complex type that contains the information about the specified
      -- health check.
    } deriving (Show, Generic)

-- | A complex type that contains the information about the specified health
-- check.
ghcsHealthCheck :: Lens' GetHealthCheckResponse (HealthCheck)
ghcsHealthCheck = lens _ghcsHealthCheck (\s a -> s { _ghcsHealthCheck = a })
{-# INLINE ghcsHealthCheck #-}

instance FromXML GetHealthCheckResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetHealthCheck where
    type Sv GetHealthCheck = Route53
    type Rs GetHealthCheck = GetHealthCheckResponse

    request = get
    response _ = xmlResponse
