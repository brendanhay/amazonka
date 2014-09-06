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
    , mkGetHealthCheck
    -- ** Request lenses
    , ghcHealthCheckId

    -- * Response
    , GetHealthCheckResponse
    -- ** Response lenses
    , ghcrsHealthCheck
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.V2013_04_01.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | A complex type that contains information about the request to get a health
-- check.
newtype GetHealthCheck = GetHealthCheck
    { _ghcHealthCheckId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetHealthCheck' request.
mkGetHealthCheck :: Text -- ^ 'ghcHealthCheckId'
                 -> GetHealthCheck
mkGetHealthCheck p1 = GetHealthCheck
    { _ghcHealthCheckId = p1
    }
{-# INLINE mkGetHealthCheck #-}

-- | The ID of the health check to retrieve.
ghcHealthCheckId :: Lens' GetHealthCheck Text
ghcHealthCheckId =
    lens _ghcHealthCheckId (\s a -> s { _ghcHealthCheckId = a })
{-# INLINE ghcHealthCheckId #-}

instance ToPath GetHealthCheck where
    toPath GetHealthCheck{..} = mconcat
        [ "/2013-04-01/healthcheck/"
        , toBS _ghcHealthCheckId
        ]

instance ToQuery GetHealthCheck

instance ToHeaders GetHealthCheck

instance ToXML GetHealthCheck where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GetHealthCheckRequest"

-- | A complex type containing information about the specified health check.
newtype GetHealthCheckResponse = GetHealthCheckResponse
    { _ghcrsHealthCheck :: HealthCheck
    } deriving (Show, Generic)

-- | A complex type that contains the information about the specified health
-- check.
ghcrsHealthCheck :: Lens' GetHealthCheckResponse HealthCheck
ghcrsHealthCheck =
    lens _ghcrsHealthCheck (\s a -> s { _ghcrsHealthCheck = a })
{-# INLINE ghcrsHealthCheck #-}

instance FromXML GetHealthCheckResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetHealthCheck where
    type Sv GetHealthCheck = Route53
    type Rs GetHealthCheck = GetHealthCheckResponse

    request = get
    response _ = xmlResponse
