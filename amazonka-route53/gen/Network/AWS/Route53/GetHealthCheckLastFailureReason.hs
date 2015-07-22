{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetHealthCheckLastFailureReason
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- If you want to learn why a health check is currently failing or why it
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
    , ghclfrrqHealthCheckId

    -- * Response
    , GetHealthCheckLastFailureReasonResponse
    -- ** Response constructor
    , getHealthCheckLastFailureReasonResponse
    -- ** Response lenses
    , ghclfrrsStatus
    , ghclfrrsHealthCheckObservations
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
-- * 'ghclfrrqHealthCheckId'
newtype GetHealthCheckLastFailureReason = GetHealthCheckLastFailureReason'
    { _ghclfrrqHealthCheckId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetHealthCheckLastFailureReason' smart constructor.
getHealthCheckLastFailureReason :: Text -> GetHealthCheckLastFailureReason
getHealthCheckLastFailureReason pHealthCheckId_ =
    GetHealthCheckLastFailureReason'
    { _ghclfrrqHealthCheckId = pHealthCheckId_
    }

-- | The ID of the health check for which you want to retrieve the reason for
-- the most recent failure.
ghclfrrqHealthCheckId :: Lens' GetHealthCheckLastFailureReason Text
ghclfrrqHealthCheckId = lens _ghclfrrqHealthCheckId (\ s a -> s{_ghclfrrqHealthCheckId = a});

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
               toText _ghclfrrqHealthCheckId, "/lastfailurereason"]

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
-- * 'ghclfrrsStatus'
--
-- * 'ghclfrrsHealthCheckObservations'
data GetHealthCheckLastFailureReasonResponse = GetHealthCheckLastFailureReasonResponse'
    { _ghclfrrsStatus                  :: !Int
    , _ghclfrrsHealthCheckObservations :: ![HealthCheckObservation]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetHealthCheckLastFailureReasonResponse' smart constructor.
getHealthCheckLastFailureReasonResponse :: Int -> GetHealthCheckLastFailureReasonResponse
getHealthCheckLastFailureReasonResponse pStatus_ =
    GetHealthCheckLastFailureReasonResponse'
    { _ghclfrrsStatus = pStatus_
    , _ghclfrrsHealthCheckObservations = mempty
    }

-- | FIXME: Undocumented member.
ghclfrrsStatus :: Lens' GetHealthCheckLastFailureReasonResponse Int
ghclfrrsStatus = lens _ghclfrrsStatus (\ s a -> s{_ghclfrrsStatus = a});

-- | A list that contains one @HealthCheckObservation@ element for each Route
-- 53 health checker.
ghclfrrsHealthCheckObservations :: Lens' GetHealthCheckLastFailureReasonResponse [HealthCheckObservation]
ghclfrrsHealthCheckObservations = lens _ghclfrrsHealthCheckObservations (\ s a -> s{_ghclfrrsHealthCheckObservations = a});
