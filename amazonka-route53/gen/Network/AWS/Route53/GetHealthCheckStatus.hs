{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.GetHealthCheckStatus
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | To retrieve the health check status, send a @GET@ request to the
-- @2013-04-01/healthcheck//health check ID//status resource. You can use this
-- call to get a health check's current status.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_GetHealthCheckStatus.html>
module Network.AWS.Route53.GetHealthCheckStatus
    (
    -- * Request
      GetHealthCheckStatus
    -- ** Request constructor
    , getHealthCheckStatus
    -- ** Request lenses
    , ghcsHealthCheckId

    -- * Response
    , GetHealthCheckStatusResponse
    -- ** Response constructor
    , getHealthCheckStatusResponse
    -- ** Response lenses
    , ghcsrHealthCheckObservations
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.Route53.Types
import qualified GHC.Exts

newtype GetHealthCheckStatus = GetHealthCheckStatus
    { _ghcsHealthCheckId :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'GetHealthCheckStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ghcsHealthCheckId' @::@ 'Text'
--
getHealthCheckStatus :: Text -- ^ 'ghcsHealthCheckId'
                     -> GetHealthCheckStatus
getHealthCheckStatus p1 = GetHealthCheckStatus
    { _ghcsHealthCheckId = p1
    }

-- | The ID of the health check for which you want to retrieve the most recent
-- status.
ghcsHealthCheckId :: Lens' GetHealthCheckStatus Text
ghcsHealthCheckId =
    lens _ghcsHealthCheckId (\s a -> s { _ghcsHealthCheckId = a })

newtype GetHealthCheckStatusResponse = GetHealthCheckStatusResponse
    { _ghcsrHealthCheckObservations :: List "HealthCheckObservation" HealthCheckObservation
    } deriving (Eq, Show, Monoid, Semigroup)

instance GHC.Exts.IsList GetHealthCheckStatusResponse where
    type Item GetHealthCheckStatusResponse = HealthCheckObservation

    fromList = GetHealthCheckStatusResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _ghcsrHealthCheckObservations

-- | 'GetHealthCheckStatusResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ghcsrHealthCheckObservations' @::@ ['HealthCheckObservation']
--
getHealthCheckStatusResponse :: GetHealthCheckStatusResponse
getHealthCheckStatusResponse = GetHealthCheckStatusResponse
    { _ghcsrHealthCheckObservations = mempty
    }

-- | A list that contains one @HealthCheckObservation@ element for each Route
-- 53 health checker.
ghcsrHealthCheckObservations :: Lens' GetHealthCheckStatusResponse [HealthCheckObservation]
ghcsrHealthCheckObservations =
    lens _ghcsrHealthCheckObservations
        (\s a -> s { _ghcsrHealthCheckObservations = a })
            . _List

instance ToPath GetHealthCheckStatus where
    toPath GetHealthCheckStatus{..} = mconcat
        [ "/2013-04-01/healthcheck/"
        , toText _ghcsHealthCheckId
        , "/status"
        ]

instance ToQuery GetHealthCheckStatus where
    toQuery = const mempty

instance ToHeaders GetHealthCheckStatus

instance ToXMLRoot GetHealthCheckStatus where
    toXMLRoot = const (namespaced ns "GetHealthCheckStatus" [])

instance ToXML GetHealthCheckStatus

instance AWSRequest GetHealthCheckStatus where
    type Sv GetHealthCheckStatus = Route53
    type Rs GetHealthCheckStatus = GetHealthCheckStatusResponse

    request  = get
    response = xmlResponse

instance FromXML GetHealthCheckStatusResponse where
    parseXML x = GetHealthCheckStatusResponse
        <$> x .@  "HealthCheckObservations"
