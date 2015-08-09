{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetHealthCheckStatus
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- To retrieve the health check status, send a @GET@ request to the
-- @2013-04-01\/healthcheck\/health check ID\/status@ resource. You can use
-- this call to get a health check\'s current status.
--
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/API_GetHealthCheckStatus.html AWS API Reference> for GetHealthCheckStatus.
module Network.AWS.Route53.GetHealthCheckStatus
    (
    -- * Creating a Request
      GetHealthCheckStatus
    , getHealthCheckStatus
    -- * Request Lenses
    , ghcsHealthCheckId

    -- * Destructuring the Response
    , GetHealthCheckStatusResponse
    , getHealthCheckStatusResponse
    -- * Response Lenses
    , ghcsrsStatus
    , ghcsrsHealthCheckObservations
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types
import           Network.AWS.Route53.Types.Product

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetHealthCheckStatus' smart constructor.
getHealthCheckStatus :: Text -> GetHealthCheckStatus
getHealthCheckStatus pHealthCheckId_ =
    GetHealthCheckStatus'
    { _ghcsHealthCheckId = pHealthCheckId_
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
                   (pure (fromEnum s)) <*>
                     (x .@? "HealthCheckObservations" .!@ mempty >>=
                        parseXMLList "HealthCheckObservation"))

instance ToHeaders GetHealthCheckStatus where
        toHeaders = const mempty

instance ToPath GetHealthCheckStatus where
        toPath GetHealthCheckStatus'{..}
          = mconcat
              ["/2013-04-01/healthcheck/", toBS _ghcsHealthCheckId,
               "/status"]

instance ToQuery GetHealthCheckStatus where
        toQuery = const mempty

-- | A complex type that contains information about the status of the
-- specified health check.
--
-- /See:/ 'getHealthCheckStatusResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ghcsrsStatus'
--
-- * 'ghcsrsHealthCheckObservations'
data GetHealthCheckStatusResponse = GetHealthCheckStatusResponse'
    { _ghcsrsStatus                  :: !Int
    , _ghcsrsHealthCheckObservations :: ![HealthCheckObservation]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetHealthCheckStatusResponse' smart constructor.
getHealthCheckStatusResponse :: Int -> GetHealthCheckStatusResponse
getHealthCheckStatusResponse pStatus_ =
    GetHealthCheckStatusResponse'
    { _ghcsrsStatus = pStatus_
    , _ghcsrsHealthCheckObservations = mempty
    }

-- | Undocumented member.
ghcsrsStatus :: Lens' GetHealthCheckStatusResponse Int
ghcsrsStatus = lens _ghcsrsStatus (\ s a -> s{_ghcsrsStatus = a});

-- | A list that contains one @HealthCheckObservation@ element for each Route
-- 53 health checker.
ghcsrsHealthCheckObservations :: Lens' GetHealthCheckStatusResponse [HealthCheckObservation]
ghcsrsHealthCheckObservations = lens _ghcsrsHealthCheckObservations (\ s a -> s{_ghcsrsHealthCheckObservations = a}) . _Coerce;
