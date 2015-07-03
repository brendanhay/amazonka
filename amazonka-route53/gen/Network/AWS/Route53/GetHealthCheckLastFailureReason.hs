{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Route53.GetHealthCheckLastFailureReason
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | If you want to learn why a health check is currently failing or why it
-- failed most recently (if at all), you can get the failure reason for the
-- most recent failure. Send a @GET@ request to the
-- @2013-04-01\/healthcheck\/health check ID\/lastfailurereason@ resource.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_GetHealthCheckLastFailureReason.html>
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
    , ghclfrrStatus
    , ghclfrrHealthCheckObservations
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types

-- | A complex type that contains information about the request to get the
-- most recent failure reason for a health check.
--
-- /See:/ 'getHealthCheckLastFailureReason' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ghclfrHealthCheckId'
newtype GetHealthCheckLastFailureReason = GetHealthCheckLastFailureReason'
    { _ghclfrHealthCheckId :: Text
    } deriving (Eq,Read,Show)

-- | 'GetHealthCheckLastFailureReason' smart constructor.
getHealthCheckLastFailureReason :: Text -> GetHealthCheckLastFailureReason
getHealthCheckLastFailureReason pHealthCheckId =
    GetHealthCheckLastFailureReason'
    { _ghclfrHealthCheckId = pHealthCheckId
    }

-- | The ID of the health check for which you want to retrieve the reason for
-- the most recent failure.
ghclfrHealthCheckId :: Lens' GetHealthCheckLastFailureReason Text
ghclfrHealthCheckId = lens _ghclfrHealthCheckId (\ s a -> s{_ghclfrHealthCheckId = a});

instance AWSRequest GetHealthCheckLastFailureReason
         where
        type Sv GetHealthCheckLastFailureReason = Route53
        type Rs GetHealthCheckLastFailureReason =
             GetHealthCheckLastFailureReasonResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetHealthCheckLastFailureReasonResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .@? "HealthCheckObservations" .!@ mempty >>=
                        parseXMLList "HealthCheckObservation"))

instance ToHeaders GetHealthCheckLastFailureReason
         where
        toHeaders = const mempty

instance ToPath GetHealthCheckLastFailureReason where
        toPath GetHealthCheckLastFailureReason'{..}
          = mconcat
              ["/2013-04-01/healthcheck/",
               toText _ghclfrHealthCheckId, "/lastfailurereason"]

instance ToQuery GetHealthCheckLastFailureReason
         where
        toQuery = const mempty

-- | A complex type that contains information about the most recent failure
-- for the specified health check.
--
-- /See:/ 'getHealthCheckLastFailureReasonResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ghclfrrStatus'
--
-- * 'ghclfrrHealthCheckObservations'
data GetHealthCheckLastFailureReasonResponse = GetHealthCheckLastFailureReasonResponse'
    { _ghclfrrStatus                  :: !Int
    , _ghclfrrHealthCheckObservations :: ![HealthCheckObservation]
    } deriving (Eq,Read,Show)

-- | 'GetHealthCheckLastFailureReasonResponse' smart constructor.
getHealthCheckLastFailureReasonResponse :: Int -> GetHealthCheckLastFailureReasonResponse
getHealthCheckLastFailureReasonResponse pStatus =
    GetHealthCheckLastFailureReasonResponse'
    { _ghclfrrStatus = pStatus
    , _ghclfrrHealthCheckObservations = mempty
    }

-- | FIXME: Undocumented member.
ghclfrrStatus :: Lens' GetHealthCheckLastFailureReasonResponse Int
ghclfrrStatus = lens _ghclfrrStatus (\ s a -> s{_ghclfrrStatus = a});

-- | A list that contains one @HealthCheckObservation@ element for each Route
-- 53 health checker.
ghclfrrHealthCheckObservations :: Lens' GetHealthCheckLastFailureReasonResponse [HealthCheckObservation]
ghclfrrHealthCheckObservations = lens _ghclfrrHealthCheckObservations (\ s a -> s{_ghclfrrHealthCheckObservations = a});
