{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Route53.GetHealthCheckStatus
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | To retrieve the health check status, send a @GET@ request to the
-- @2013-04-01\/healthcheck\/health check ID\/status@ resource. You can use
-- this call to get a health check\'s current status.
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
    , ghcsrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types

-- | A complex type that contains information about the request to get health
-- check status for a health check.
--
-- /See:/ 'getHealthCheckStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ghcsHealthCheckId'
newtype GetHealthCheckStatus = GetHealthCheckStatus'
    { _ghcsHealthCheckId :: Text
    } deriving (Eq,Read,Show)

-- | 'GetHealthCheckStatus' smart constructor.
getHealthCheckStatus :: Text -> GetHealthCheckStatus
getHealthCheckStatus pHealthCheckId =
    GetHealthCheckStatus'
    { _ghcsHealthCheckId = pHealthCheckId
    }

-- | The ID of the health check for which you want to retrieve the most
-- recent status.
ghcsHealthCheckId :: Lens' GetHealthCheckStatus Text
ghcsHealthCheckId = lens _ghcsHealthCheckId (\ s a -> s{_ghcsHealthCheckId = a});

instance AWSRequest GetHealthCheckStatus where
        type Sv GetHealthCheckStatus = Route53
        type Rs GetHealthCheckStatus =
             GetHealthCheckStatusResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetHealthCheckStatusResponse' <$>
                   (x .@? "HealthCheckObservations" .!@ mempty >>=
                      parseXMLList "HealthCheckObservation")
                     <*> (pure s))

instance ToHeaders GetHealthCheckStatus where
        toHeaders = const mempty

instance ToPath GetHealthCheckStatus where
        toPath GetHealthCheckStatus'{..}
          = mconcat
              ["/2013-04-01/healthcheck/",
               toText _ghcsHealthCheckId, "/status"]

instance ToQuery GetHealthCheckStatus where
        toQuery = const mempty

-- | A complex type that contains information about the status of the
-- specified health check.
--
-- /See:/ 'getHealthCheckStatusResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ghcsrHealthCheckObservations'
--
-- * 'ghcsrStatus'
data GetHealthCheckStatusResponse = GetHealthCheckStatusResponse'
    { _ghcsrHealthCheckObservations :: ![HealthCheckObservation]
    , _ghcsrStatus                  :: !Status
    } deriving (Eq,Show)

-- | 'GetHealthCheckStatusResponse' smart constructor.
getHealthCheckStatusResponse :: Status -> GetHealthCheckStatusResponse
getHealthCheckStatusResponse pStatus =
    GetHealthCheckStatusResponse'
    { _ghcsrHealthCheckObservations = mempty
    , _ghcsrStatus = pStatus
    }

-- | A list that contains one @HealthCheckObservation@ element for each Route
-- 53 health checker.
ghcsrHealthCheckObservations :: Lens' GetHealthCheckStatusResponse [HealthCheckObservation]
ghcsrHealthCheckObservations = lens _ghcsrHealthCheckObservations (\ s a -> s{_ghcsrHealthCheckObservations = a});

-- | FIXME: Undocumented member.
ghcsrStatus :: Lens' GetHealthCheckStatusResponse Status
ghcsrStatus = lens _ghcsrStatus (\ s a -> s{_ghcsrStatus = a});
