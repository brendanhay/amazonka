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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets status of a specified health check. Send a @GET@ request to the @/2013-04-01/healthcheck//health check ID/ /status@ resource. You can use this call to get a health check's current status.
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

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types
import           Network.AWS.Route53.Types.Product

-- | A complex type that contains information about the request to get health check status for a health check.
--
--
--
-- /See:/ 'getHealthCheckStatus' smart constructor.
newtype GetHealthCheckStatus = GetHealthCheckStatus'
    { _ghcsHealthCheckId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetHealthCheckStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ghcsHealthCheckId' - If you want Amazon Route 53 to return this resource record set in response to a DNS query only when a health check is passing, include the @HealthCheckId@ element and specify the ID of the applicable health check. Amazon Route 53 determines whether a resource record set is healthy by periodically sending a request to the endpoint that is specified in the health check. If that endpoint returns an HTTP status code of 2xx or 3xx, the endpoint is healthy. If the endpoint returns an HTTP status code of 400 or greater, or if the endpoint doesn't respond for a certain amount of time, Amazon Route 53 considers the endpoint unhealthy and also considers the resource record set unhealthy. The @HealthCheckId@ element is only useful when Amazon Route 53 is choosing between two or more resource record sets to respond to a DNS query, and you want Amazon Route 53 to base the choice in part on the status of a health check. Configuring health checks only makes sense in the following configurations:     * You're checking the health of the resource record sets in a weighted, latency, geolocation, or failover resource record set, and you specify health check IDs for all of the resource record sets. If the health check for one resource record set specifies an endpoint that is not healthy, Amazon Route 53 stops responding to queries using the value for that resource record set.     * You set @EvaluateTargetHealth@ to @true@ for the resource record sets in an alias, weighted alias, latency alias, geolocation alias, or failover alias resource record set, and you specify health check IDs for all of the resource record sets that are referenced by the alias resource record sets. For more information about this configuration, see @EvaluateTargetHealth@ . Amazon Route 53 doesn't check the health of the endpoint specified in the resource record set, for example, the endpoint specified by the IP address in the @Value@ element. When you add a @HealthCheckId@ element to a resource record set, Amazon Route 53 checks the health of the endpoint that you specified in the health check. For geolocation resource record sets, if an endpoint is unhealthy, Amazon Route 53 looks for a resource record set for the larger, associated geographic region. For example, suppose you have resource record sets for a state in the United States, for the United States, for North America, and for all locations. If the endpoint for the state resource record set is unhealthy, Amazon Route 53 checks the resource record sets for the United States, for North America, and for all locations (a resource record set for which the value of CountryCode is @*@ ), in that order, until it finds a resource record set for which the endpoint is healthy. If your health checks specify the endpoint only by domain name, we recommend that you create a separate health check for each endpoint. For example, create a health check for each HTTP server that is serving content for www.example.com. For the value of @FullyQualifiedDomainName@ , specify the domain name of the server (such as @us-east-1-www.example.com@ ), not the name of the resource record sets (example.com). /Important:/ In this configuration, if you create a health check for which the value of @FullyQualifiedDomainName@ matches the name of the resource record sets and then associate the health check with those resource record sets, health check results will be unpredictable.
getHealthCheckStatus
    :: Text -- ^ 'ghcsHealthCheckId'
    -> GetHealthCheckStatus
getHealthCheckStatus pHealthCheckId_ =
    GetHealthCheckStatus'
    { _ghcsHealthCheckId = pHealthCheckId_
    }

-- | If you want Amazon Route 53 to return this resource record set in response to a DNS query only when a health check is passing, include the @HealthCheckId@ element and specify the ID of the applicable health check. Amazon Route 53 determines whether a resource record set is healthy by periodically sending a request to the endpoint that is specified in the health check. If that endpoint returns an HTTP status code of 2xx or 3xx, the endpoint is healthy. If the endpoint returns an HTTP status code of 400 or greater, or if the endpoint doesn't respond for a certain amount of time, Amazon Route 53 considers the endpoint unhealthy and also considers the resource record set unhealthy. The @HealthCheckId@ element is only useful when Amazon Route 53 is choosing between two or more resource record sets to respond to a DNS query, and you want Amazon Route 53 to base the choice in part on the status of a health check. Configuring health checks only makes sense in the following configurations:     * You're checking the health of the resource record sets in a weighted, latency, geolocation, or failover resource record set, and you specify health check IDs for all of the resource record sets. If the health check for one resource record set specifies an endpoint that is not healthy, Amazon Route 53 stops responding to queries using the value for that resource record set.     * You set @EvaluateTargetHealth@ to @true@ for the resource record sets in an alias, weighted alias, latency alias, geolocation alias, or failover alias resource record set, and you specify health check IDs for all of the resource record sets that are referenced by the alias resource record sets. For more information about this configuration, see @EvaluateTargetHealth@ . Amazon Route 53 doesn't check the health of the endpoint specified in the resource record set, for example, the endpoint specified by the IP address in the @Value@ element. When you add a @HealthCheckId@ element to a resource record set, Amazon Route 53 checks the health of the endpoint that you specified in the health check. For geolocation resource record sets, if an endpoint is unhealthy, Amazon Route 53 looks for a resource record set for the larger, associated geographic region. For example, suppose you have resource record sets for a state in the United States, for the United States, for North America, and for all locations. If the endpoint for the state resource record set is unhealthy, Amazon Route 53 checks the resource record sets for the United States, for North America, and for all locations (a resource record set for which the value of CountryCode is @*@ ), in that order, until it finds a resource record set for which the endpoint is healthy. If your health checks specify the endpoint only by domain name, we recommend that you create a separate health check for each endpoint. For example, create a health check for each HTTP server that is serving content for www.example.com. For the value of @FullyQualifiedDomainName@ , specify the domain name of the server (such as @us-east-1-www.example.com@ ), not the name of the resource record sets (example.com). /Important:/ In this configuration, if you create a health check for which the value of @FullyQualifiedDomainName@ matches the name of the resource record sets and then associate the health check with those resource record sets, health check results will be unpredictable.
ghcsHealthCheckId :: Lens' GetHealthCheckStatus Text
ghcsHealthCheckId = lens _ghcsHealthCheckId (\ s a -> s{_ghcsHealthCheckId = a});

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

instance Hashable GetHealthCheckStatus

instance NFData GetHealthCheckStatus

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
ghcsrsResponseStatus = lens _ghcsrsResponseStatus (\ s a -> s{_ghcsrsResponseStatus = a});

-- | A list that contains one @HealthCheckObservation@ element for each Amazon Route 53 health checker that is reporting a status about the health check endpoint.
ghcsrsHealthCheckObservations :: Lens' GetHealthCheckStatusResponse [HealthCheckObservation]
ghcsrsHealthCheckObservations = lens _ghcsrsHealthCheckObservations (\ s a -> s{_ghcsrsHealthCheckObservations = a}) . _Coerce;

instance NFData GetHealthCheckStatusResponse
