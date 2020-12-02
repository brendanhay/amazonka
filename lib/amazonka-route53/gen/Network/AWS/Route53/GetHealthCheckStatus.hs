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
-- Module      : Network.AWS.Route53.GetHealthCheckStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets status of a specified health check.
--
--
module Network.AWS.Route53.GetHealthCheckStatus
    (
    -- * Creating a Request
      getHealthCheckStatus
    , GetHealthCheckStatus
    -- * Request Lenses
    , ghcsHealthCheckId

    -- * Destructuring the Response
    , getHealthCheckStatusResponse
    , GetHealthCheckStatusResponse
    -- * Response Lenses
    , ghcsrsResponseStatus
    , ghcsrsHealthCheckObservations
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types
import Network.AWS.Route53.Types.Product

-- | A request to get the status for a health check.
--
--
--
-- /See:/ 'getHealthCheckStatus' smart constructor.
newtype GetHealthCheckStatus = GetHealthCheckStatus'
  { _ghcsHealthCheckId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetHealthCheckStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ghcsHealthCheckId' - The ID for the health check that you want the current status for. When you created the health check, @CreateHealthCheck@ returned the ID in the response, in the @HealthCheckId@ element.
getHealthCheckStatus
    :: Text -- ^ 'ghcsHealthCheckId'
    -> GetHealthCheckStatus
getHealthCheckStatus pHealthCheckId_ =
  GetHealthCheckStatus' {_ghcsHealthCheckId = pHealthCheckId_}


-- | The ID for the health check that you want the current status for. When you created the health check, @CreateHealthCheck@ returned the ID in the response, in the @HealthCheckId@ element.
ghcsHealthCheckId :: Lens' GetHealthCheckStatus Text
ghcsHealthCheckId = lens _ghcsHealthCheckId (\ s a -> s{_ghcsHealthCheckId = a})

instance AWSRequest GetHealthCheckStatus where
        type Rs GetHealthCheckStatus =
             GetHealthCheckStatusResponse
        request = get route53
        response
          = receiveXML
              (\ s h x ->
                 GetHealthCheckStatusResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .@? "HealthCheckObservations" .!@ mempty >>=
                        parseXMLList "HealthCheckObservation"))

instance Hashable GetHealthCheckStatus where

instance NFData GetHealthCheckStatus where

instance ToHeaders GetHealthCheckStatus where
        toHeaders = const mempty

instance ToPath GetHealthCheckStatus where
        toPath GetHealthCheckStatus'{..}
          = mconcat
              ["/2013-04-01/healthcheck/", toBS _ghcsHealthCheckId,
               "/status"]

instance ToQuery GetHealthCheckStatus where
        toQuery = const mempty

-- | A complex type that contains the response to a @GetHealthCheck@ request.
--
--
--
-- /See:/ 'getHealthCheckStatusResponse' smart constructor.
data GetHealthCheckStatusResponse = GetHealthCheckStatusResponse'
  { _ghcsrsResponseStatus          :: !Int
  , _ghcsrsHealthCheckObservations :: ![HealthCheckObservation]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetHealthCheckStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ghcsrsResponseStatus' - -- | The response status code.
--
-- * 'ghcsrsHealthCheckObservations' - A list that contains one @HealthCheckObservation@ element for each Amazon Route 53 health checker that is reporting a status about the health check endpoint.
getHealthCheckStatusResponse
    :: Int -- ^ 'ghcsrsResponseStatus'
    -> GetHealthCheckStatusResponse
getHealthCheckStatusResponse pResponseStatus_ =
  GetHealthCheckStatusResponse'
    { _ghcsrsResponseStatus = pResponseStatus_
    , _ghcsrsHealthCheckObservations = mempty
    }


-- | -- | The response status code.
ghcsrsResponseStatus :: Lens' GetHealthCheckStatusResponse Int
ghcsrsResponseStatus = lens _ghcsrsResponseStatus (\ s a -> s{_ghcsrsResponseStatus = a})

-- | A list that contains one @HealthCheckObservation@ element for each Amazon Route 53 health checker that is reporting a status about the health check endpoint.
ghcsrsHealthCheckObservations :: Lens' GetHealthCheckStatusResponse [HealthCheckObservation]
ghcsrsHealthCheckObservations = lens _ghcsrsHealthCheckObservations (\ s a -> s{_ghcsrsHealthCheckObservations = a}) . _Coerce

instance NFData GetHealthCheckStatusResponse where
