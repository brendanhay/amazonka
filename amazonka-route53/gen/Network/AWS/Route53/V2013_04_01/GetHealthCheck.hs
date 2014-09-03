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
    , getHealthCheck
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

-- | Minimum specification for a 'GetHealthCheck' request.
getHealthCheck :: Text -- ^ 'ghcrHealthCheckId'
               -> GetHealthCheck
getHealthCheck p1 = GetHealthCheck
    { _ghcrHealthCheckId = p1
    }

data GetHealthCheck = GetHealthCheck
    { _ghcrHealthCheckId :: Text
      -- ^ The ID of the health check to retrieve.
    } deriving (Show, Generic)

-- | The ID of the health check to retrieve.
ghcrHealthCheckId
    :: Functor f
    => (Text
    -> f (Text))
    -> GetHealthCheck
    -> f GetHealthCheck
ghcrHealthCheckId f x =
    (\y -> x { _ghcrHealthCheckId = y })
       <$> f (_ghcrHealthCheckId x)
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

data GetHealthCheckResponse = GetHealthCheckResponse
    { _ghcsHealthCheck :: HealthCheck
      -- ^ A complex type that contains the information about the specified
      -- health check.
    } deriving (Show, Generic)

-- | A complex type that contains the information about the specified health
-- check.
ghcsHealthCheck
    :: Functor f
    => (HealthCheck
    -> f (HealthCheck))
    -> GetHealthCheckResponse
    -> f GetHealthCheckResponse
ghcsHealthCheck f x =
    (\y -> x { _ghcsHealthCheck = y })
       <$> f (_ghcsHealthCheck x)
{-# INLINE ghcsHealthCheck #-}

instance FromXML GetHealthCheckResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetHealthCheck where
    type Sv GetHealthCheck = Route53
    type Rs GetHealthCheck = GetHealthCheckResponse

    request = get
    response _ = xmlResponse
