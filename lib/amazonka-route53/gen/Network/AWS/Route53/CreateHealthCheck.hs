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
-- Module      : Network.AWS.Route53.CreateHealthCheck
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new health check.
--
--
-- For information about adding health checks to resource record sets, see 'ResourceRecordSet$HealthCheckId' in 'ChangeResourceRecordSets' .
--
-- __ELB Load Balancers__
--
-- If you're registering EC2 instances with an Elastic Load Balancing (ELB) load balancer, do not create Amazon Route 53 health checks for the EC2 instances. When you register an EC2 instance with a load balancer, you configure settings for an ELB health check, which performs a similar function to an Amazon Route 53 health check.
--
-- __Private Hosted Zones__
--
-- You can associate health checks with failover resource record sets in a private hosted zone. Note the following:
--
--     * Amazon Route 53 health checkers are outside the VPC. To check the health of an endpoint within a VPC by IP address, you must assign a public IP address to the instance in the VPC.
--
--     * You can configure a health checker to check the health of an external resource that the instance relies on, such as a database server.
--
--     * You can create a CloudWatch metric, associate an alarm with the metric, and then create a health check that is based on the state of the alarm. For example, you might create a CloudWatch metric that checks the status of the Amazon EC2 @StatusCheckFailed@ metric, add an alarm to the metric, and then create a health check that is based on the state of the alarm. For information about creating CloudWatch metrics and alarms by using the CloudWatch console, see the <http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/WhatIsCloudWatch.html Amazon CloudWatch User Guide> .
--
--
--
module Network.AWS.Route53.CreateHealthCheck
    (
    -- * Creating a Request
      createHealthCheck
    , CreateHealthCheck
    -- * Request Lenses
    , chcCallerReference
    , chcHealthCheckConfig

    -- * Destructuring the Response
    , createHealthCheckResponse
    , CreateHealthCheckResponse
    -- * Response Lenses
    , chcrsResponseStatus
    , chcrsHealthCheck
    , chcrsLocation
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types
import Network.AWS.Route53.Types.Product

-- | A complex type that contains the health check request information.
--
--
--
-- /See:/ 'createHealthCheck' smart constructor.
data CreateHealthCheck = CreateHealthCheck'
  { _chcCallerReference   :: !Text
  , _chcHealthCheckConfig :: !HealthCheckConfig
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateHealthCheck' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chcCallerReference' - A unique string that identifies the request and that allows you to retry a failed @CreateHealthCheck@ request without the risk of creating two identical health checks:     * If you send a @CreateHealthCheck@ request with the same @CallerReference@ and settings as a previous request, and if the health check doesn't exist, Amazon Route 53 creates the health check. If the health check does exist, Amazon Route 53 returns the settings for the existing health check.     * If you send a @CreateHealthCheck@ request with the same @CallerReference@ as a deleted health check, regardless of the settings, Amazon Route 53 returns a @HealthCheckAlreadyExists@ error.     * If you send a @CreateHealthCheck@ request with the same @CallerReference@ as an existing health check but with different settings, Amazon Route 53 returns a @HealthCheckAlreadyExists@ error.     * If you send a @CreateHealthCheck@ request with a unique @CallerReference@ but settings identical to an existing health check, Amazon Route 53 creates the health check.
--
-- * 'chcHealthCheckConfig' - A complex type that contains the response to a @CreateHealthCheck@ request.
createHealthCheck
    :: Text -- ^ 'chcCallerReference'
    -> HealthCheckConfig -- ^ 'chcHealthCheckConfig'
    -> CreateHealthCheck
createHealthCheck pCallerReference_ pHealthCheckConfig_ =
  CreateHealthCheck'
    { _chcCallerReference = pCallerReference_
    , _chcHealthCheckConfig = pHealthCheckConfig_
    }


-- | A unique string that identifies the request and that allows you to retry a failed @CreateHealthCheck@ request without the risk of creating two identical health checks:     * If you send a @CreateHealthCheck@ request with the same @CallerReference@ and settings as a previous request, and if the health check doesn't exist, Amazon Route 53 creates the health check. If the health check does exist, Amazon Route 53 returns the settings for the existing health check.     * If you send a @CreateHealthCheck@ request with the same @CallerReference@ as a deleted health check, regardless of the settings, Amazon Route 53 returns a @HealthCheckAlreadyExists@ error.     * If you send a @CreateHealthCheck@ request with the same @CallerReference@ as an existing health check but with different settings, Amazon Route 53 returns a @HealthCheckAlreadyExists@ error.     * If you send a @CreateHealthCheck@ request with a unique @CallerReference@ but settings identical to an existing health check, Amazon Route 53 creates the health check.
chcCallerReference :: Lens' CreateHealthCheck Text
chcCallerReference = lens _chcCallerReference (\ s a -> s{_chcCallerReference = a})

-- | A complex type that contains the response to a @CreateHealthCheck@ request.
chcHealthCheckConfig :: Lens' CreateHealthCheck HealthCheckConfig
chcHealthCheckConfig = lens _chcHealthCheckConfig (\ s a -> s{_chcHealthCheckConfig = a})

instance AWSRequest CreateHealthCheck where
        type Rs CreateHealthCheck = CreateHealthCheckResponse
        request = postXML route53
        response
          = receiveXML
              (\ s h x ->
                 CreateHealthCheckResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "HealthCheck") <*>
                     (h .# "Location"))

instance Hashable CreateHealthCheck where

instance NFData CreateHealthCheck where

instance ToElement CreateHealthCheck where
        toElement
          = mkElement
              "{https://route53.amazonaws.com/doc/2013-04-01/}CreateHealthCheckRequest"

instance ToHeaders CreateHealthCheck where
        toHeaders = const mempty

instance ToPath CreateHealthCheck where
        toPath = const "/2013-04-01/healthcheck"

instance ToQuery CreateHealthCheck where
        toQuery = const mempty

instance ToXML CreateHealthCheck where
        toXML CreateHealthCheck'{..}
          = mconcat
              ["CallerReference" @= _chcCallerReference,
               "HealthCheckConfig" @= _chcHealthCheckConfig]

-- | A complex type containing the response information for the new health check.
--
--
--
-- /See:/ 'createHealthCheckResponse' smart constructor.
data CreateHealthCheckResponse = CreateHealthCheckResponse'
  { _chcrsResponseStatus :: !Int
  , _chcrsHealthCheck    :: !HealthCheck
  , _chcrsLocation       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateHealthCheckResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chcrsResponseStatus' - -- | The response status code.
--
-- * 'chcrsHealthCheck' - A complex type that contains identifying information about the health check.
--
-- * 'chcrsLocation' - The unique URL representing the new health check.
createHealthCheckResponse
    :: Int -- ^ 'chcrsResponseStatus'
    -> HealthCheck -- ^ 'chcrsHealthCheck'
    -> Text -- ^ 'chcrsLocation'
    -> CreateHealthCheckResponse
createHealthCheckResponse pResponseStatus_ pHealthCheck_ pLocation_ =
  CreateHealthCheckResponse'
    { _chcrsResponseStatus = pResponseStatus_
    , _chcrsHealthCheck = pHealthCheck_
    , _chcrsLocation = pLocation_
    }


-- | -- | The response status code.
chcrsResponseStatus :: Lens' CreateHealthCheckResponse Int
chcrsResponseStatus = lens _chcrsResponseStatus (\ s a -> s{_chcrsResponseStatus = a})

-- | A complex type that contains identifying information about the health check.
chcrsHealthCheck :: Lens' CreateHealthCheckResponse HealthCheck
chcrsHealthCheck = lens _chcrsHealthCheck (\ s a -> s{_chcrsHealthCheck = a})

-- | The unique URL representing the new health check.
chcrsLocation :: Lens' CreateHealthCheckResponse Text
chcrsLocation = lens _chcrsLocation (\ s a -> s{_chcrsLocation = a})

instance NFData CreateHealthCheckResponse where
