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

-- Module      : Network.AWS.Route53.GetHealthCheckLastFailureReason
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | If you want to learn why a health check is currently failing or why it
-- failed most recently (if at all), you can get the failure reason for the
-- most recent failure. Send a GET request to the
-- 2013-04-01/healthcheck/health check ID/lastfailurereason resource.
module Network.AWS.Route53.GetHealthCheckLastFailureReason
    (
    -- * Request
      GetHealthCheckLastFailureReason
    -- ** Request constructor
    , getHealthCheckLastFailureReason
    -- ** Request lenses
    , ghclfrHealthCheckId

    -- * Response
    , GetHealthCheckLastFailureReasonResponse
    -- ** Response constructor
    , getHealthCheckLastFailureReasonResponse
    -- ** Response lenses
    , ghclfrrHealthCheckObservations
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Route53.Types

newtype GetHealthCheckLastFailureReason = GetHealthCheckLastFailureReason
    { _ghclfrHealthCheckId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'GetHealthCheckLastFailureReason' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ghclfrHealthCheckId' @::@ 'Text'
--
getHealthCheckLastFailureReason :: Text -- ^ 'ghclfrHealthCheckId'
                                -> GetHealthCheckLastFailureReason
getHealthCheckLastFailureReason p1 = GetHealthCheckLastFailureReason
    { _ghclfrHealthCheckId = p1
    }

-- | The ID of the health check for which you want to retrieve the reason for
-- the most recent failure.
ghclfrHealthCheckId :: Lens' GetHealthCheckLastFailureReason Text
ghclfrHealthCheckId =
    lens _ghclfrHealthCheckId (\s a -> s { _ghclfrHealthCheckId = a })

instance ToPath GetHealthCheckLastFailureReason where
    toPath GetHealthCheckLastFailureReason{..} = mconcat
        [ "/2013-04-01/healthcheck/"
        , toText _ghclfrHealthCheckId
        , "/lastfailurereason"
        ]

instance ToQuery GetHealthCheckLastFailureReason where
    toQuery = const mempty

instance ToHeaders GetHealthCheckLastFailureReason

newtype GetHealthCheckLastFailureReasonResponse = GetHealthCheckLastFailureReasonResponse
    { _ghclfrrHealthCheckObservations :: [HealthCheckObservation]
    } deriving (Eq, Show, Generic, Monoid)

-- | 'GetHealthCheckLastFailureReasonResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ghclfrrHealthCheckObservations' @::@ ['HealthCheckObservation']
--
getHealthCheckLastFailureReasonResponse :: GetHealthCheckLastFailureReasonResponse
getHealthCheckLastFailureReasonResponse = GetHealthCheckLastFailureReasonResponse
    { _ghclfrrHealthCheckObservations = mempty
    }

-- | A list that contains one HealthCheckObservation element for each Route 53
-- health checker.
ghclfrrHealthCheckObservations :: Lens' GetHealthCheckLastFailureReasonResponse [HealthCheckObservation]
ghclfrrHealthCheckObservations =
    lens _ghclfrrHealthCheckObservations
        (\s a -> s { _ghclfrrHealthCheckObservations = a })

instance AWSRequest GetHealthCheckLastFailureReason where
    type Sv GetHealthCheckLastFailureReason = Route53
    type Rs GetHealthCheckLastFailureReason = GetHealthCheckLastFailureReasonResponse

    request  = get
    response = const . xmlResponse $ \h x -> GetHealthCheckLastFailureReasonResponse
newtype
