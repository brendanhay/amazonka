{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetHealthCheckLastFailureReason
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the reason that a specified health check failed most recently.
--
--
module Network.AWS.Route53.GetHealthCheckLastFailureReason
    (
    -- * Creating a Request
      getHealthCheckLastFailureReason
    , GetHealthCheckLastFailureReason
    -- * Request Lenses
    , ghclfrHealthCheckId

    -- * Destructuring the Response
    , getHealthCheckLastFailureReasonResponse
    , GetHealthCheckLastFailureReasonResponse
    -- * Response Lenses
    , ghclfrrsResponseStatus
    , ghclfrrsHealthCheckObservations
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types
import Network.AWS.Route53.Types.Product

-- | A request for the reason that a health check failed most recently.
--
--
--
-- /See:/ 'getHealthCheckLastFailureReason' smart constructor.
newtype GetHealthCheckLastFailureReason = GetHealthCheckLastFailureReason'
  { _ghclfrHealthCheckId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetHealthCheckLastFailureReason' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ghclfrHealthCheckId' - The ID for the health check for which you want the last failure reason. When you created the health check, @CreateHealthCheck@ returned the ID in the response, in the @HealthCheckId@ element.
getHealthCheckLastFailureReason
    :: Text -- ^ 'ghclfrHealthCheckId'
    -> GetHealthCheckLastFailureReason
getHealthCheckLastFailureReason pHealthCheckId_ =
  GetHealthCheckLastFailureReason' {_ghclfrHealthCheckId = pHealthCheckId_}


-- | The ID for the health check for which you want the last failure reason. When you created the health check, @CreateHealthCheck@ returned the ID in the response, in the @HealthCheckId@ element.
ghclfrHealthCheckId :: Lens' GetHealthCheckLastFailureReason Text
ghclfrHealthCheckId = lens _ghclfrHealthCheckId (\ s a -> s{_ghclfrHealthCheckId = a})

instance AWSRequest GetHealthCheckLastFailureReason
         where
        type Rs GetHealthCheckLastFailureReason =
             GetHealthCheckLastFailureReasonResponse
        request = get route53
        response
          = receiveXML
              (\ s h x ->
                 GetHealthCheckLastFailureReasonResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .@? "HealthCheckObservations" .!@ mempty >>=
                        parseXMLList "HealthCheckObservation"))

instance Hashable GetHealthCheckLastFailureReason
         where

instance NFData GetHealthCheckLastFailureReason where

instance ToHeaders GetHealthCheckLastFailureReason
         where
        toHeaders = const mempty

instance ToPath GetHealthCheckLastFailureReason where
        toPath GetHealthCheckLastFailureReason'{..}
          = mconcat
              ["/2013-04-01/healthcheck/",
               toBS _ghclfrHealthCheckId, "/lastfailurereason"]

instance ToQuery GetHealthCheckLastFailureReason
         where
        toQuery = const mempty

-- | A complex type that contains the response to a @GetHealthCheckLastFailureReason@ request.
--
--
--
-- /See:/ 'getHealthCheckLastFailureReasonResponse' smart constructor.
data GetHealthCheckLastFailureReasonResponse = GetHealthCheckLastFailureReasonResponse'
  { _ghclfrrsResponseStatus          :: !Int
  , _ghclfrrsHealthCheckObservations :: ![HealthCheckObservation]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetHealthCheckLastFailureReasonResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ghclfrrsResponseStatus' - -- | The response status code.
--
-- * 'ghclfrrsHealthCheckObservations' - A list that contains one @Observation@ element for each Amazon Route 53 health checker that is reporting a last failure reason.
getHealthCheckLastFailureReasonResponse
    :: Int -- ^ 'ghclfrrsResponseStatus'
    -> GetHealthCheckLastFailureReasonResponse
getHealthCheckLastFailureReasonResponse pResponseStatus_ =
  GetHealthCheckLastFailureReasonResponse'
    { _ghclfrrsResponseStatus = pResponseStatus_
    , _ghclfrrsHealthCheckObservations = mempty
    }


-- | -- | The response status code.
ghclfrrsResponseStatus :: Lens' GetHealthCheckLastFailureReasonResponse Int
ghclfrrsResponseStatus = lens _ghclfrrsResponseStatus (\ s a -> s{_ghclfrrsResponseStatus = a})

-- | A list that contains one @Observation@ element for each Amazon Route 53 health checker that is reporting a last failure reason.
ghclfrrsHealthCheckObservations :: Lens' GetHealthCheckLastFailureReasonResponse [HealthCheckObservation]
ghclfrrsHealthCheckObservations = lens _ghclfrrsHealthCheckObservations (\ s a -> s{_ghclfrrsHealthCheckObservations = a}) . _Coerce

instance NFData
           GetHealthCheckLastFailureReasonResponse
         where
