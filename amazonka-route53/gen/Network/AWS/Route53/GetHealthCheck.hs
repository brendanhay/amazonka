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
module Network.AWS.Route53.GetHealthCheck
    (
    -- * Request
      GetHealthCheck
    -- ** Request constructor
    , getHealthCheck
    -- ** Request lenses
    , ghcHealthCheckId

    -- * Response
    , GetHealthCheckResponse
    -- ** Response constructor
    , getHealthCheckResponse
    -- ** Response lenses
    , ghcrHealthCheck
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Route53.Types

newtype GetHealthCheck = GetHealthCheck
    { _ghcHealthCheckId :: Text
    } (Eq, Ord, Show, Generic, Monoid)

-- | 'GetHealthCheck' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ghcHealthCheckId' @::@ 'Text'
--
getHealthCheck :: Text -- ^ 'ghcHealthCheckId'
               -> GetHealthCheck
getHealthCheck p1 = GetHealthCheck
    { _ghcHealthCheckId = p1
    }

-- | The ID of the health check to retrieve.
ghcHealthCheckId :: Lens' GetHealthCheck Text
ghcHealthCheckId = lens _ghcHealthCheckId (\s a -> s { _ghcHealthCheckId = a })

instance ToPath GetHealthCheck where
    toPath GetHealthCheck{..} = mconcat
        [ "/2013-04-01/healthcheck/"
        , toText _ghcHealthCheckId
        ]

instance ToQuery GetHealthCheck where
    toQuery = const mempty

instance ToHeaders GetHealthCheck

newtype GetHealthCheckResponse = GetHealthCheckResponse
    { _ghcrHealthCheck :: HealthCheck
    } (Eq, Show, Generic)

-- | 'GetHealthCheckResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ghcrHealthCheck' @::@ 'HealthCheck'
--
getHealthCheckResponse :: HealthCheck -- ^ 'ghcrHealthCheck'
                       -> GetHealthCheckResponse
getHealthCheckResponse p1 = GetHealthCheckResponse
    { _ghcrHealthCheck = p1
    }

-- | A complex type that contains the information about the specified health
-- check.
ghcrHealthCheck :: Lens' GetHealthCheckResponse HealthCheck
ghcrHealthCheck = lens _ghcrHealthCheck (\s a -> s { _ghcrHealthCheck = a })

instance FromXML GetHealthCheckResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GetHealthCheckResponse"
instance AWSRequest GetHealthCheck where
    type Sv GetHealthCheck = Route53
    type Rs GetHealthCheck = GetHealthCheckResponse

    request  = get
    response = xmlResponse $ \h x -> GetHealthCheckResponse
        <$> x %| "HealthCheck"
