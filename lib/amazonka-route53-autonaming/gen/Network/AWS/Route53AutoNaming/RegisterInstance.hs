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
-- Module      : Network.AWS.Route53AutoNaming.RegisterInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates one or more records and optionally a health check based on the settings in a specified service. When you submit a @RegisterInstance@ request, Amazon Route 53 does the following:
--
--
--     * For each DNS record that you define in the service specified by @ServiceId@ , creates or updates a record in the hosted zone that is associated with the corresponding namespace
--
--     * If the service includes @HealthCheckConfig@ , creates or updates a health check based on the settings in the health check configuration
--
--     * Associates the health check, if any, with each of the records
--
--
--
-- /Important:/ One @RegisterInstance@ request must complete before you can submit another request and specify the same service ID and instance ID.
--
-- For more information, see 'CreateService' .
--
-- When Route 53 receives a DNS query for the specified DNS name, it returns the applicable value:
--
--     * __If the health check is healthy__ : returns all the records
--
--     * __If the health check is unhealthy__ : returns the applicable value for the last healthy instance
--
--     * __If you didn't specify a health check configuration__ : returns all the records
--
--
--
-- For the current limit on the number of instances that you can register using the same namespace and using the same service, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html#limits-api-entities-autonaming Limits on Auto Naming> in the /Route 53 Developer Guide/ .
--
module Network.AWS.Route53AutoNaming.RegisterInstance
    (
    -- * Creating a Request
      registerInstance
    , RegisterInstance
    -- * Request Lenses
    , riCreatorRequestId
    , riServiceId
    , riInstanceId
    , riAttributes

    -- * Destructuring the Response
    , registerInstanceResponse
    , RegisterInstanceResponse
    -- * Response Lenses
    , rirsOperationId
    , rirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53AutoNaming.Types
import Network.AWS.Route53AutoNaming.Types.Product

-- | /See:/ 'registerInstance' smart constructor.
data RegisterInstance = RegisterInstance'
  { _riCreatorRequestId :: !(Maybe Text)
  , _riServiceId        :: !Text
  , _riInstanceId       :: !Text
  , _riAttributes       :: !(Map Text Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riCreatorRequestId' - A unique string that identifies the request and that allows failed @RegisterInstance@ requests to be retried without the risk of executing the operation twice. You must use a unique @CreatorRequestId@ string every time you submit a @RegisterInstance@ request if you're registering additional instances for the same namespace and service. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
--
-- * 'riServiceId' - The ID of the service that you want to use for settings for the records and health check that Route 53 will create.
--
-- * 'riInstanceId' - An identifier that you want to associate with the instance. Note the following:     * If the service that is specified by @ServiceId@ includes settings for an SRV record, the value of @InstanceId@ is automatically included as part of the value for the SRV record. For more information, see 'DnsRecord$Type' .     * You can use this value to update an existing instance.     * To register a new instance, you must specify a value that is unique among instances that you register by using the same service.      * If you specify an existing @InstanceId@ and @ServiceId@ , Route 53 updates the existing records. If there's also an existing health check, Route 53 deletes the old health check and creates a new one.
--
-- * 'riAttributes' - A string map that contains the following information for the service that you specify in @ServiceId@ :     * The attributes that apply to the records that are defined in the service.      * For each attribute, the applicable value. Supported attribute keys include the following: __AWS_ALIAS_DNS_NAME__  ____  If you want Route 53 to create an alias record that routes traffic to an Elastic Load Balancing load balancer, specify the DNS name that is associated with the load balancer. For information about how to get the DNS name, see "DNSName" in the topic <http://docs.aws.amazon.com/http:/docs.aws.amazon.com/Route53/latest/APIReference/API_AliasTarget.html AliasTarget> . Note the following:     * The configuration for the service that is specified by @ServiceId@ must include settings for an A record, an AAAA record, or both.     * In the service that is specified by @ServiceId@ , the value of @RoutingPolicy@ must be @WEIGHTED@ .     * If the service that is specified by @ServiceId@ includes @HealthCheckConfig@ settings, Route 53 will create the health check, but it won't associate the health check with the alias record.     * Auto naming currently doesn't support creating alias records that route traffic to AWS resources other than ELB load balancers.     * If you specify a value for @AWS_ALIAS_DNS_NAME@ , don't specify values for any of the @AWS_INSTANCE@ attributes. __AWS_INSTANCE_CNAME__  If the service configuration includes a CNAME record, the domain name that you want Route 53 to return in response to DNS queries, for example, @example.com@ . This value is required if the service specified by @ServiceId@ includes settings for an CNAME record. __AWS_INSTANCE_IPV4__  If the service configuration includes an A record, the IPv4 address that you want Route 53 to return in response to DNS queries, for example, @192.0.2.44@ . This value is required if the service specified by @ServiceId@ includes settings for an A record. If the service includes settings for an SRV record, you must specify a value for @AWS_INSTANCE_IPV4@ , @AWS_INSTANCE_IPV6@ , or both. __AWS_INSTANCE_IPV6__  If the service configuration includes an AAAA record, the IPv6 address that you want Route 53 to return in response to DNS queries, for example, @2001:0db8:85a3:0000:0000:abcd:0001:2345@ . This value is required if the service specified by @ServiceId@ includes settings for an AAAA record. If the service includes settings for an SRV record, you must specify a value for @AWS_INSTANCE_IPV4@ , @AWS_INSTANCE_IPV6@ , or both. __AWS_INSTANCE_PORT__  If the service includes an SRV record, the value that you want Route 53 to return for the port. If the service includes @HealthCheckConfig@ , the port on the endpoint that you want Route 53 to send requests to.  This value is required if you specified settings for an SRV record when you created the service.
registerInstance
    :: Text -- ^ 'riServiceId'
    -> Text -- ^ 'riInstanceId'
    -> RegisterInstance
registerInstance pServiceId_ pInstanceId_ =
  RegisterInstance'
    { _riCreatorRequestId = Nothing
    , _riServiceId = pServiceId_
    , _riInstanceId = pInstanceId_
    , _riAttributes = mempty
    }


-- | A unique string that identifies the request and that allows failed @RegisterInstance@ requests to be retried without the risk of executing the operation twice. You must use a unique @CreatorRequestId@ string every time you submit a @RegisterInstance@ request if you're registering additional instances for the same namespace and service. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
riCreatorRequestId :: Lens' RegisterInstance (Maybe Text)
riCreatorRequestId = lens _riCreatorRequestId (\ s a -> s{_riCreatorRequestId = a})

-- | The ID of the service that you want to use for settings for the records and health check that Route 53 will create.
riServiceId :: Lens' RegisterInstance Text
riServiceId = lens _riServiceId (\ s a -> s{_riServiceId = a})

-- | An identifier that you want to associate with the instance. Note the following:     * If the service that is specified by @ServiceId@ includes settings for an SRV record, the value of @InstanceId@ is automatically included as part of the value for the SRV record. For more information, see 'DnsRecord$Type' .     * You can use this value to update an existing instance.     * To register a new instance, you must specify a value that is unique among instances that you register by using the same service.      * If you specify an existing @InstanceId@ and @ServiceId@ , Route 53 updates the existing records. If there's also an existing health check, Route 53 deletes the old health check and creates a new one.
riInstanceId :: Lens' RegisterInstance Text
riInstanceId = lens _riInstanceId (\ s a -> s{_riInstanceId = a})

-- | A string map that contains the following information for the service that you specify in @ServiceId@ :     * The attributes that apply to the records that are defined in the service.      * For each attribute, the applicable value. Supported attribute keys include the following: __AWS_ALIAS_DNS_NAME__  ____  If you want Route 53 to create an alias record that routes traffic to an Elastic Load Balancing load balancer, specify the DNS name that is associated with the load balancer. For information about how to get the DNS name, see "DNSName" in the topic <http://docs.aws.amazon.com/http:/docs.aws.amazon.com/Route53/latest/APIReference/API_AliasTarget.html AliasTarget> . Note the following:     * The configuration for the service that is specified by @ServiceId@ must include settings for an A record, an AAAA record, or both.     * In the service that is specified by @ServiceId@ , the value of @RoutingPolicy@ must be @WEIGHTED@ .     * If the service that is specified by @ServiceId@ includes @HealthCheckConfig@ settings, Route 53 will create the health check, but it won't associate the health check with the alias record.     * Auto naming currently doesn't support creating alias records that route traffic to AWS resources other than ELB load balancers.     * If you specify a value for @AWS_ALIAS_DNS_NAME@ , don't specify values for any of the @AWS_INSTANCE@ attributes. __AWS_INSTANCE_CNAME__  If the service configuration includes a CNAME record, the domain name that you want Route 53 to return in response to DNS queries, for example, @example.com@ . This value is required if the service specified by @ServiceId@ includes settings for an CNAME record. __AWS_INSTANCE_IPV4__  If the service configuration includes an A record, the IPv4 address that you want Route 53 to return in response to DNS queries, for example, @192.0.2.44@ . This value is required if the service specified by @ServiceId@ includes settings for an A record. If the service includes settings for an SRV record, you must specify a value for @AWS_INSTANCE_IPV4@ , @AWS_INSTANCE_IPV6@ , or both. __AWS_INSTANCE_IPV6__  If the service configuration includes an AAAA record, the IPv6 address that you want Route 53 to return in response to DNS queries, for example, @2001:0db8:85a3:0000:0000:abcd:0001:2345@ . This value is required if the service specified by @ServiceId@ includes settings for an AAAA record. If the service includes settings for an SRV record, you must specify a value for @AWS_INSTANCE_IPV4@ , @AWS_INSTANCE_IPV6@ , or both. __AWS_INSTANCE_PORT__  If the service includes an SRV record, the value that you want Route 53 to return for the port. If the service includes @HealthCheckConfig@ , the port on the endpoint that you want Route 53 to send requests to.  This value is required if you specified settings for an SRV record when you created the service.
riAttributes :: Lens' RegisterInstance (HashMap Text Text)
riAttributes = lens _riAttributes (\ s a -> s{_riAttributes = a}) . _Map

instance AWSRequest RegisterInstance where
        type Rs RegisterInstance = RegisterInstanceResponse
        request = postJSON route53AutoNaming
        response
          = receiveJSON
              (\ s h x ->
                 RegisterInstanceResponse' <$>
                   (x .?> "OperationId") <*> (pure (fromEnum s)))

instance Hashable RegisterInstance where

instance NFData RegisterInstance where

instance ToHeaders RegisterInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53AutoNaming_v20170314.RegisterInstance" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RegisterInstance where
        toJSON RegisterInstance'{..}
          = object
              (catMaybes
                 [("CreatorRequestId" .=) <$> _riCreatorRequestId,
                  Just ("ServiceId" .= _riServiceId),
                  Just ("InstanceId" .= _riInstanceId),
                  Just ("Attributes" .= _riAttributes)])

instance ToPath RegisterInstance where
        toPath = const "/"

instance ToQuery RegisterInstance where
        toQuery = const mempty

-- | /See:/ 'registerInstanceResponse' smart constructor.
data RegisterInstanceResponse = RegisterInstanceResponse'
  { _rirsOperationId    :: !(Maybe Text)
  , _rirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rirsOperationId' - A value that you can use to determine whether the request completed successfully. To get the status of the operation, see 'GetOperation' .
--
-- * 'rirsResponseStatus' - -- | The response status code.
registerInstanceResponse
    :: Int -- ^ 'rirsResponseStatus'
    -> RegisterInstanceResponse
registerInstanceResponse pResponseStatus_ =
  RegisterInstanceResponse'
    {_rirsOperationId = Nothing, _rirsResponseStatus = pResponseStatus_}


-- | A value that you can use to determine whether the request completed successfully. To get the status of the operation, see 'GetOperation' .
rirsOperationId :: Lens' RegisterInstanceResponse (Maybe Text)
rirsOperationId = lens _rirsOperationId (\ s a -> s{_rirsOperationId = a})

-- | -- | The response status code.
rirsResponseStatus :: Lens' RegisterInstanceResponse Int
rirsResponseStatus = lens _rirsResponseStatus (\ s a -> s{_rirsResponseStatus = a})

instance NFData RegisterInstanceResponse where
