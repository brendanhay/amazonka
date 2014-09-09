{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Module      : Network.AWS.ELB.V2012_06_01.Trans
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Elastic Load Balancing automatically distributes incoming application
-- traffic across multiple Amazon EC2 instances. It enables you to achieve
-- greater levels of fault tolerance in your applications, seamlessly
-- providing the required amount of load balancing capacity needed to
-- distribute application traffic.
--
-- The 'State' operator variants from 'Control.Lens' such as '.=' can be
-- used to modify any additional request parameters before sending.
module Network.AWS.ELB.V2012_06_01.Trans
    (
    -- * AddTags
      addTags
    -- * ApplySecurityGroupsToLoadBalancer
    , applySecurityGroupsToLoadBalancer
    -- * AttachLoadBalancerToSubnets
    , attachLoadBalancerToSubnets
    -- * ConfigureHealthCheck
    , configureHealthCheck
    -- * CreateAppCookieStickinessPolicy
    , createAppCookieStickinessPolicy
    -- * CreateLBCookieStickinessPolicy
    , createLBCookieStickinessPolicy
    -- * CreateLoadBalancer
    , createLoadBalancer
    -- * CreateLoadBalancerListeners
    , createLoadBalancerListeners
    -- * CreateLoadBalancerPolicy
    , createLoadBalancerPolicy
    -- * DeleteLoadBalancer
    , deleteLoadBalancer
    -- * DeleteLoadBalancerListeners
    , deleteLoadBalancerListeners
    -- * DeleteLoadBalancerPolicy
    , deleteLoadBalancerPolicy
    -- * DeregisterInstancesFromLoadBalancer
    , deregisterInstancesFromLoadBalancer
    -- * DescribeInstanceHealth
    , describeInstanceHealth
    -- * DescribeLoadBalancerAttributes
    , describeLoadBalancerAttributes
    -- * DescribeLoadBalancerPolicies
    , describeLoadBalancerPolicies
    -- * DescribeLoadBalancerPolicyTypes
    , describeLoadBalancerPolicyTypes
    -- * DescribeLoadBalancers
    , describeLoadBalancers
    -- * DescribeTags
    , describeTags
    -- * DetachLoadBalancerFromSubnets
    , detachLoadBalancerFromSubnets
    -- * DisableAvailabilityZonesForLoadBalancer
    , disableAvailabilityZonesForLoadBalancer
    -- * EnableAvailabilityZonesForLoadBalancer
    , enableAvailabilityZonesForLoadBalancer
    -- * ModifyLoadBalancerAttributes
    , modifyLoadBalancerAttributes
    -- * RegisterInstancesWithLoadBalancer
    , registerInstancesWithLoadBalancer
    -- * RemoveTags
    , removeTags
    -- * SetLoadBalancerListenerSSLCertificate
    , setLoadBalancerListenerSSLCertificate
    -- * SetLoadBalancerPoliciesForBackendServer
    , setLoadBalancerPoliciesForBackendServer
    -- * SetLoadBalancerPoliciesOfListener
    , setLoadBalancerPoliciesOfListener

    -- * Re-exported
    , module Control.Monad.Trans.AWS
    , module Network.AWS.ELB.V2012_06_01
    -- ** Lenses
    , (.=)
    , (?=)
    , (<>=)
    , (%=)
    ) where

import Control.Monad.Trans.AWS as AWS
import Network.AWS.Prelude
import Network.AWS.ELB.V2012_06_01

-- | Adds one or more tags for the specified load balancer. Each load balancer
-- can have a maximum of 10 tags. Each tag consists of a key and an optional
-- value. Tag keys must be unique for each load balancer. If a tag with the
-- same key is already associated with the load balancer, this action will
-- update the value of the key. For more information, see Tagging in the
-- Elastic Load Balancing Developer Guide.
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerNames.member.1=my-test-loadbalancer
-- &Action=AddTags &Tags.member.1.Key=project
-- &Tags.member.1.Value=my-test-project &Version=2012-06-01 &AUTHPARAMS
-- 360e81f7-1100-11e4-b6ed-0f30EXAMPLE.
--
-- See: 'Network.AWS.ELB.V2012_06_01.AddTags'
addTags :: ( MonadCatch m
           , MonadResource m
           , MonadError AWS.Error m
           , MonadReader Env m
           , AWSRequest a
           )
        => [Text] -- ^ 'atLoadBalancerNames'
        -> List1 Tag -- ^ 'atTags'
        -> State AddTags a
        -> m AddTagsResponse
addTags p1 p2 s =
    send $ (mkAddTags p1 p2) &~ s

-- | Associates one or more security groups with your load balancer in Amazon
-- Virtual Private Cloud (Amazon VPC). The provided security group IDs will
-- override any currently applied security groups. For more information, see
-- Manage Security Groups in Amazon VPC in the Elastic Load Balancing
-- Developer Guide.
-- https://elasticloadbalancing.amazonaws.com/?SecurityGroups.member.1=sg-123456789
-- &LoadBalancerName=my-test-vpc-loadbalancer &Version=2012-06-01
-- &Action=ApplySecurityGroupsToLoadBalancer &AUTHPARAMS sg-123456789
-- 06b5decc-102a-11e3-9ad6-bf3e4EXAMPLE.
--
-- See: 'Network.AWS.ELB.V2012_06_01.ApplySecurityGroupsToLoadBalancer'
applySecurityGroupsToLoadBalancer :: ( MonadCatch m
                                     , MonadResource m
                                     , MonadError AWS.Error m
                                     , MonadReader Env m
                                     , AWSRequest a
                                     )
                                  => Text -- ^ 'asgtlbLoadBalancerName'
                                  -> [Text] -- ^ 'asgtlbSecurityGroups'
                                  -> State ApplySecurityGroupsToLoadBalancer a
                                  -> m ApplySecurityGroupsToLoadBalancerResponse
applySecurityGroupsToLoadBalancer p1 p2 s =
    send $ (mkApplySecurityGroupsToLoadBalancer p1 p2) &~ s

-- | Adds one or more subnets to the set of configured subnets in the Amazon
-- Virtual Private Cloud (Amazon VPC) for the load balancer. The load
-- balancers evenly distribute requests across all of the registered subnets.
-- For more information, see Deploy Elastic Load Balancing in Amazon VPC in
-- the Elastic Load Balancing Developer Guide.
-- https://elasticloadbalancing.amazonaws.com/?Subnets.member.1=subnet-3561b05e
-- &LoadBalancerName=my-test-vpc-loadbalancer &Version=2012-06-01
-- &Action=AttachLoadBalancerToSubnets &AUTHPARAMS subnet-119f0078
-- subnet-3561b05e 07b1ecbc-1100-11e3-acaf-dd7edEXAMPLE.
--
-- See: 'Network.AWS.ELB.V2012_06_01.AttachLoadBalancerToSubnets'
attachLoadBalancerToSubnets :: ( MonadCatch m
                               , MonadResource m
                               , MonadError AWS.Error m
                               , MonadReader Env m
                               , AWSRequest a
                               )
                            => Text -- ^ 'albtsLoadBalancerName'
                            -> [Text] -- ^ 'albtsSubnets'
                            -> State AttachLoadBalancerToSubnets a
                            -> m AttachLoadBalancerToSubnetsResponse
attachLoadBalancerToSubnets p1 p2 s =
    send $ (mkAttachLoadBalancerToSubnets p1 p2) &~ s

-- | Specifies the health check settings to use for evaluating the health state
-- of your back-end instances. For more information, see Health Check in the
-- Elastic Load Balancing Developer Guide.
-- https://elasticloadbalancing.amazonaws.com/?HealthCheck.HealthyThreshold=2
-- &HealthCheck.UnhealthyThreshold=2 &LoadBalancerName=MyLoadBalancer
-- &HealthCheck.Target=HTTP:80/ping &HealthCheck.Interval=30
-- &HealthCheck.Timeout=3 &Version=2012-06-01 &Action=ConfigureHealthCheck
-- &AUTHPARAMS 30 HTTP:80/ping 2 3 2 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE.
--
-- See: 'Network.AWS.ELB.V2012_06_01.ConfigureHealthCheck'
configureHealthCheck :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env m
                        , AWSRequest a
                        )
                     => Text -- ^ 'chcLoadBalancerName'
                     -> HealthCheck -- ^ 'chcHealthCheck'
                     -> State ConfigureHealthCheck a
                     -> m ConfigureHealthCheckResponse
configureHealthCheck p1 p2 s =
    send $ (mkConfigureHealthCheck p1 p2) &~ s

-- | Generates a stickiness policy with sticky session lifetimes that follow
-- that of an application-generated cookie. This policy can be associated only
-- with HTTP/HTTPS listeners. This policy is similar to the policy created by
-- CreateLBCookieStickinessPolicy, except that the lifetime of the special
-- Elastic Load Balancing cookie follows the lifetime of the
-- application-generated cookie specified in the policy configuration. The
-- load balancer only inserts a new stickiness cookie when the application
-- response includes a new application cookie. If the application cookie is
-- explicitly removed or expires, the session stops being sticky until a new
-- application cookie is issued. An application client must receive and send
-- two cookies: the application-generated cookie and the special Elastic Load
-- Balancing cookie named AWSELB. This is the default behavior for many common
-- web browsers. For more information, see Enabling Application-Controlled
-- Session Stickiness in the Elastic Load Balancing Developer Guide.
-- https://elasticloadbalancing.amazonaws.com/?CookieName=MyAppCookie
-- &LoadBalancerName=MyLoadBalancer &PolicyName=MyAppStickyPolicy
-- &Version=2012-06-01 &Action=CreateAppCookieStickinessPolicy &AUTHPARAMS
-- 99a693e9-12b8-11e3-9ad6-bf3e4EXAMPLE.
--
-- See: 'Network.AWS.ELB.V2012_06_01.CreateAppCookieStickinessPolicy'
createAppCookieStickinessPolicy :: ( MonadCatch m
                                   , MonadResource m
                                   , MonadError AWS.Error m
                                   , MonadReader Env m
                                   , AWSRequest a
                                   )
                                => Text -- ^ 'cacspLoadBalancerName'
                                -> Text -- ^ 'cacspPolicyName'
                                -> Text -- ^ 'cacspCookieName'
                                -> State CreateAppCookieStickinessPolicy a
                                -> m CreateAppCookieStickinessPolicyResponse
createAppCookieStickinessPolicy p1 p2 p3 s =
    send $ (mkCreateAppCookieStickinessPolicy p1 p2 p3) &~ s

-- | Generates a stickiness policy with sticky session lifetimes controlled by
-- the lifetime of the browser (user-agent) or a specified expiration period.
-- This policy can be associated only with HTTP/HTTPS listeners. When a load
-- balancer implements this policy, the load balancer uses a special cookie to
-- track the backend server instance for each request. When the load balancer
-- receives a request, it first checks to see if this cookie is present in the
-- request. If so, the load balancer sends the request to the application
-- server specified in the cookie. If not, the load balancer sends the request
-- to a server that is chosen based on the existing load balancing algorithm.
-- A cookie is inserted into the response for binding subsequent requests from
-- the same user to that server. The validity of the cookie is based on the
-- cookie expiration time, which is specified in the policy configuration. For
-- more information, see Enabling Duration-Based Session Stickiness in the
-- Elastic Load Balancing Developer Guide.
-- https://elasticloadbalancing.amazonaws.com/?CookieExpirationPeriod=60
-- &LoadBalancerName=MyLoadBalancer&PolicyName=MyDurationStickyPolicy
-- &Version=2012-06-01 &Action=CreateLBCookieStickinessPolicy &AUTHPARAMS
-- 99a693e9-12b8-11e3-9ad6-bf3e4EXAMPLE.
--
-- See: 'Network.AWS.ELB.V2012_06_01.CreateLBCookieStickinessPolicy'
createLBCookieStickinessPolicy :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadError AWS.Error m
                                  , MonadReader Env m
                                  , AWSRequest a
                                  )
                               => Text -- ^ 'clbcspLoadBalancerName'
                               -> Text -- ^ 'clbcspPolicyName'
                               -> State CreateLBCookieStickinessPolicy a
                               -> m CreateLBCookieStickinessPolicyResponse
createLBCookieStickinessPolicy p1 p2 s =
    send $ (mkCreateLBCookieStickinessPolicy p1 p2) &~ s

-- | Creates a new load balancer. After the call has completed successfully, a
-- new load balancer is created with a unique Domain Name Service (DNS) name.
-- The DNS name includes the name of the AWS region in which the load balance
-- was created. For example, if your load balancer was created in the United
-- States, the DNS name might end with either of the following:
-- us-east-1.elb.amazonaws.com (for the Northern Virginia region)
-- us-west-1.elb.amazonaws.com (for the Northern California region) For
-- information about the AWS regions supported by Elastic Load Balancing, see
-- Regions and Endpoints. You can create up to 20 load balancers per region
-- per account. Elastic Load Balancing supports load balancing your Amazon EC2
-- instances launched within any one of the following platforms: EC2-Classic
-- For information on creating and managing your load balancers in
-- EC2-Classic, see Deploy Elastic Load Balancing in Amazon EC2-Classic.
-- EC2-VPC For information on creating and managing your load balancers in
-- EC2-VPC, see Deploy Elastic Load Balancing in Amazon VPC. Create a TCP Load
-- Balancer in EC2-Classic
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=my-test-loadbalancer
-- &AvailabilityZones.member.1=us-east-1c
-- &Listeners.member.1.LoadBalancerPort=80 &Listeners.member.1.InstancePort=80
-- &Listeners.member.1.Protocol=http &Listeners.member.1.InstanceProtocol=http
-- &Version=2012-06-01 &Action=CreateLoadBalancer &AUTHPARAMS
-- my-test-loadbalancer-1234567890.us-east-1.elb.amazonaws.com
-- 1549581b-12b7-11e3-895e-1334aEXAMPLE Create an HTTPS Load Balancer in
-- EC2-Classic
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=MyHTTPSLoadBalancer
-- &AvailabilityZones.member.1=us-east-1c
-- &Listeners.member.1.LoadBalancerPort=443
-- &Listeners.member.1.InstancePort=443 &Listeners.member.1.Protocol=https
-- &Listeners.member.1.InstanceProtocol=https
-- &Listeners.member.1.SSLCertificateId=arn:aws:iam::123456789012:server-certificate/servercert
-- &Version=2012-06-01 &Action=CreateLoadBalancer &AUTHPARAMS
-- MyHTTPSLoadBalancer-1234567890.us-east-1.elb.amazonaws.com
-- 1549581b-12b7-11e3-895e-1334aEXAMPLE Create a TCP Load Balancer in EC2-VPC
-- https://elasticloadbalancing.amazonaws.com/?SecurityGroups.member.1=sg-6801da07
-- &LoadBalancerName=MyVPCLoadBalancer &Listeners.member.1.LoadBalancerPort=80
-- &Listeners.member.1.InstancePort=80 &Listeners.member.1.Protocol=http
-- &Listeners.member.1.InstanceProtocol=http &Subnets.member.1=subnet-6dec9f03
-- &Version=2012-06-01 &Action=CreateLoadBalancer &AUTHPARAMS
-- MyVPCLoadBalancer-1234567890.us-east-1.elb.amazonaws.com
-- 1549581b-12b7-11e3-895e-1334aEXAMPLE Create an Internal TCP Load Balancer
-- in EC2-VPC https://elasticloadbalancing.amazonaws.com/?Scheme=internal
-- &SecurityGroups.member.1=sg-706cb61f
-- &LoadBalancerName=MyInternalLoadBalancer
-- &Listeners.member.1.LoadBalancerPort=80 &Listeners.member.1.InstancePort=80
-- &Listeners.member.1.Protocol=http &Listeners.member.1.InstanceProtocol=http
-- &Subnets.member.1=subnet-9edc97f0 &Version=2012-06-01
-- &Action=CreateLoadBalancer &AUTHPARAMS
-- internal-MyInternalLoadBalancer-1234567890.us-east-1.elb.amazonaws.com
-- 1549581b-12b7-11e3-895e-1334aEXAMPLE Create a TCP Load Balancer in a
-- Default VPC
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=MyDefaultVPCLoadBalancer
-- &AvailabilityZones.member.1=sa-east-1b
-- &Listeners.member.1.LoadBalancerPort=80 &Listeners.member.1.InstancePort=80
-- &Listeners.member.1.Protocol=http &Listeners.member.1.InstanceProtocol=http
-- &Version=2012-06-01 &Action=CreateLoadBalancer &AUTHPARAMS
-- MyDefaultVPCLoadBalancer-1234567890.sa.east-1.elb.amazonaws.com
-- 1549581b-12b7-11e3-895e-1334aEXAMPLE Create a TCP Load Balancer in
-- EC2-Classic and Assign a Tag
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=my-test-loadbalancer
-- &AvailabilityZones.member.1=us-east-1c
-- &Listeners.member.1.LoadBalancerPort=80 &Listeners.member.1.InstancePort=80
-- &Listeners.member.1.Protocol=http &Listeners.member.1.InstanceProtocol=http
-- &Tags.member.1.Value=test &Tags.member.1.Key=environment
-- &Version=2012-06-01 &Action=CreateLoadBalancer &AUTHPARAMS
-- my-test-loadbalancer-1234567890.us-east-1.elb.amazonaws.com
-- 1549581b-12b7-11e3-895e-1334aEXAMPLE.
--
-- See: 'Network.AWS.ELB.V2012_06_01.CreateLoadBalancer'
createLoadBalancer :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      , AWSRequest a
                      )
                   => Text -- ^ 'clbLoadBalancerName'
                   -> [Listener] -- ^ 'clbListeners'
                   -> State CreateLoadBalancer a
                   -> m CreateLoadBalancerResponse
createLoadBalancer p1 p2 s =
    send $ (mkCreateLoadBalancer p1 p2) &~ s

-- | Creates one or more listeners on a load balancer for the specified port. If
-- a listener with the given port does not already exist, it will be created;
-- otherwise, the properties of the new listener must match the properties of
-- the existing listener. For more information, see Add a Listener to Your
-- Load Balancer in the Elastic Load Balancing Developer Guide. Create an
-- HTTPS Load Balancer listener in EC2-Classic
-- https://elasticloadbalancing.amazonaws.com/?Listeners.member.1.Protocol=https
-- &Listeners.member.1.LoadBalancerPort=443
-- &Listeners.member.1.InstancePort=443
-- &Listeners.member.1.InstanceProtocol=https
-- &Listeners.member.1.SSLCertificateId=arn:aws:iam::123456789012:server-certificate/servercert
-- &LoadBalancerName=MyHTTPSLoadBalancer &Version=2012-06-01
-- &Action=CreateLoadBalancerListeners &AUTHPARAMS
-- 1549581b-12b7-11e3-895e-1334aEXAMPLE.
--
-- See: 'Network.AWS.ELB.V2012_06_01.CreateLoadBalancerListeners'
createLoadBalancerListeners :: ( MonadCatch m
                               , MonadResource m
                               , MonadError AWS.Error m
                               , MonadReader Env m
                               , AWSRequest a
                               )
                            => Text -- ^ 'clblLoadBalancerName'
                            -> [Listener] -- ^ 'clblListeners'
                            -> State CreateLoadBalancerListeners a
                            -> m CreateLoadBalancerListenersResponse
createLoadBalancerListeners p1 p2 s =
    send $ (mkCreateLoadBalancerListeners p1 p2) &~ s

-- | Creates a new policy that contains the necessary attributes depending on
-- the policy type. Policies are settings that are saved for your load
-- balancer and that can be applied to the front-end listener, or the back-end
-- application server, depending on your policy type.
-- https://elasticloadbalancing.amazonaws.com/?PolicyAttributes.member.1.AttributeName=ProxyProtocol
-- &PolicyAttributes.member.1.AttributeValue=true
-- &PolicyTypeName=ProxyProtocolPolicyType
-- &LoadBalancerName=my-test-loadbalancer &PolicyName=EnableProxyProtocol
-- &Version=2012-06-01 &Action=CreateLoadBalancerPolicy &AUTHPARAMS
-- 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE.
--
-- See: 'Network.AWS.ELB.V2012_06_01.CreateLoadBalancerPolicy'
createLoadBalancerPolicy :: ( MonadCatch m
                            , MonadResource m
                            , MonadError AWS.Error m
                            , MonadReader Env m
                            , AWSRequest a
                            )
                         => Text -- ^ 'clbpLoadBalancerName'
                         -> Text -- ^ 'clbpPolicyName'
                         -> Text -- ^ 'clbpPolicyTypeName'
                         -> State CreateLoadBalancerPolicy a
                         -> m CreateLoadBalancerPolicyResponse
createLoadBalancerPolicy p1 p2 p3 s =
    send $ (mkCreateLoadBalancerPolicy p1 p2 p3) &~ s

-- | Deletes the specified load balancer. If attempting to recreate the load
-- balancer, you must reconfigure all the settings. The DNS name associated
-- with a deleted load balancer will no longer be usable. Once deleted, the
-- name and associated DNS record of the load balancer no longer exist and
-- traffic sent to any of its IP addresses will no longer be delivered to
-- back-end instances. To successfully call this API, you must provide the
-- same account credentials as were used to create the load balancer. By
-- design, if the load balancer does not exist or has already been deleted, a
-- call to DeleteLoadBalancer action still succeeds.
--
-- See: 'Network.AWS.ELB.V2012_06_01.DeleteLoadBalancer'
deleteLoadBalancer :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      , AWSRequest a
                      )
                   => Text -- ^ 'dlbLoadBalancerName'
                   -> State DeleteLoadBalancer a
                   -> m DeleteLoadBalancerResponse
deleteLoadBalancer p1 s =
    send $ (mkDeleteLoadBalancer p1) &~ s

-- | Deletes listeners from the load balancer for the specified port.
--
-- See: 'Network.AWS.ELB.V2012_06_01.DeleteLoadBalancerListeners'
deleteLoadBalancerListeners :: ( MonadCatch m
                               , MonadResource m
                               , MonadError AWS.Error m
                               , MonadReader Env m
                               , AWSRequest a
                               )
                            => Text -- ^ 'dlblLoadBalancerName'
                            -> [Integer] -- ^ 'dlblLoadBalancerPorts'
                            -> State DeleteLoadBalancerListeners a
                            -> m DeleteLoadBalancerListenersResponse
deleteLoadBalancerListeners p1 p2 s =
    send $ (mkDeleteLoadBalancerListeners p1 p2) &~ s

-- | Deletes a policy from the load balancer. The specified policy must not be
-- enabled for any listeners.
--
-- See: 'Network.AWS.ELB.V2012_06_01.DeleteLoadBalancerPolicy'
deleteLoadBalancerPolicy :: ( MonadCatch m
                            , MonadResource m
                            , MonadError AWS.Error m
                            , MonadReader Env m
                            , AWSRequest a
                            )
                         => Text -- ^ 'dlbpLoadBalancerName'
                         -> Text -- ^ 'dlbpPolicyName'
                         -> State DeleteLoadBalancerPolicy a
                         -> m DeleteLoadBalancerPolicyResponse
deleteLoadBalancerPolicy p1 p2 s =
    send $ (mkDeleteLoadBalancerPolicy p1 p2) &~ s

-- | Deregisters instances from the load balancer. Once the instance is
-- deregistered, it will stop receiving traffic from the load balancer. In
-- order to successfully call this API, the same account credentials as those
-- used to create the load balancer must be provided. For more information,
-- see De-register and Register Amazon EC2 Instances in the Elastic Load
-- Balancing Developer Guide. You can use DescribeLoadBalancers to verify if
-- the instance is deregistered from the load balancer. Deregister instance
-- i-e3677ad7 from MyHTTPSLoadBalancer load balancer.
-- https://elasticloadbalancing.amazonaws.com/?Instances.member.1.InstanceId=i-e3677ad7
-- &LoadBalancerName=MyHTTPSLoadBalancer &Version=2012-06-01
-- &Action=DeregisterInstancesFromLoadBalancer &AUTHPARAMS i-6ec63d59
-- 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE.
--
-- See: 'Network.AWS.ELB.V2012_06_01.DeregisterInstancesFromLoadBalancer'
deregisterInstancesFromLoadBalancer :: ( MonadCatch m
                                       , MonadResource m
                                       , MonadError AWS.Error m
                                       , MonadReader Env m
                                       , AWSRequest a
                                       )
                                    => Text -- ^ 'diflbLoadBalancerName'
                                    -> [Instance] -- ^ 'diflbInstances'
                                    -> State DeregisterInstancesFromLoadBalancer a
                                    -> m DeregisterInstancesFromLoadBalancerResponse
deregisterInstancesFromLoadBalancer p1 p2 s =
    send $ (mkDeregisterInstancesFromLoadBalancer p1 p2) &~ s

-- | Returns the current state of the specified instances registered with the
-- specified load balancer. If no instances are specified, the state of all
-- the instances registered with the load balancer is returned. You must
-- provide the same account credentials as those that were used to create the
-- load balancer. Description of a healthy (InService) instance
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=my-test-loadbalancer
-- &Version=2012-06-01 &Action=DescribeInstanceHealth &AUTHPARAMS N/A
-- i-90d8c2a5 InService N/A 1549581b-12b7-11e3-895e-1334aEXAMPLE Description
-- of an instance with registration in progress
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=my-test-loadbalancer
-- &Version=2012-06-01 &Action=DescribeInstanceHealth &AUTHPARAMS Instance
-- registration is still in progress. i-315b7e51 OutOfService ELB
-- 1549581b-12b7-11e3-895e-1334aEXAMPLE Description of an unhealthy
-- (OutOfService) instance
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=my-test-loadbalancer
-- &Version=2012-06-01 &Action=DescribeInstanceHealth &AUTHPARAMS Instance has
-- failed at least the UnhealthyThreshold number of health checks
-- consecutively. i-fda142c9 OutOfService Instance
-- 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE Description of an instance in an
-- unknown state
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=my-test-loadbalancer
-- &Version=2012-06-01 &Action=DescribeInstanceHealth &AUTHPARAMS A transient
-- error occurred. Please try again later. i-7f12e649 Unknown ELB
-- 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE.
--
-- See: 'Network.AWS.ELB.V2012_06_01.DescribeInstanceHealth'
describeInstanceHealth :: ( MonadCatch m
                          , MonadResource m
                          , MonadError AWS.Error m
                          , MonadReader Env m
                          , AWSRequest a
                          )
                       => Text -- ^ 'dihLoadBalancerName'
                       -> State DescribeInstanceHealth a
                       -> m DescribeInstanceHealthResponse
describeInstanceHealth p1 s =
    send $ (mkDescribeInstanceHealth p1) &~ s

-- | Returns detailed information about all of the attributes associated with
-- the specified load balancer.
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=my-test-loadbalancer
-- &Version=2012-06-01 &Action=DescribeLoadBalancerAttributes &AUTHPARAMS true
-- my-loadbalancer-logs testprefix 5 30 true true 60
-- 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE.
--
-- See: 'Network.AWS.ELB.V2012_06_01.DescribeLoadBalancerAttributes'
describeLoadBalancerAttributes :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadError AWS.Error m
                                  , MonadReader Env m
                                  , AWSRequest a
                                  )
                               => Text -- ^ 'dlbaLoadBalancerName'
                               -> State DescribeLoadBalancerAttributes a
                               -> m DescribeLoadBalancerAttributesResponse
describeLoadBalancerAttributes p1 s =
    send $ (mkDescribeLoadBalancerAttributes p1) &~ s

-- | Returns detailed descriptions of the policies. If you specify a load
-- balancer name, the action returns the descriptions of all the policies
-- created for the load balancer. If you specify a policy name associated with
-- your load balancer, the action returns the description of that policy. If
-- you don't specify a load balancer name, the action returns descriptions of
-- the specified sample policies, or descriptions of all the sample policies.
-- The names of the sample policies have the ELBSample- prefix. Description of
-- all the policies associated with a load balancer
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=MyLoadBalancer
-- &Version=2012-06-01 &Action=DescribeLoadBalancerPolicies &AUTHPARAMS
-- MyDurationStickyPolicy LBCookieStickinessPolicyType CookieExpirationPeriod
-- 60 MyAppStickyPolicy AppCookieStickinessPolicyType CookieName MyAppCookie
-- 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE Description of a specified policy
-- associated with the load balancer
-- https://elasticloadbalancing.amazonaws.com/?PolicyNames.member.1=EnableProxyProtocol
-- &LoadBalancerName=my-test-loadbalancer &Version=2012-06-01
-- &Action=DescribeLoadBalancerPolicies &AUTHPARAMS EnableProxyProtocol
-- ProxyProtocolPolicyType ProxyProtocol true
-- 1549581b-12b7-11e3-895e-1334aEXAMPLE.
--
-- See: 'Network.AWS.ELB.V2012_06_01.DescribeLoadBalancerPolicies'
describeLoadBalancerPolicies :: ( MonadCatch m
                                , MonadResource m
                                , MonadError AWS.Error m
                                , MonadReader Env m
                                , AWSRequest a
                                )
                             => State DescribeLoadBalancerPolicies a
                             -> m DescribeLoadBalancerPoliciesResponse
describeLoadBalancerPolicies s =
    send (mkDescribeLoadBalancerPolicies &~ s)

-- | Returns meta-information on the specified load balancer policies defined by
-- the Elastic Load Balancing service. The policy types that are returned from
-- this action can be used in a CreateLoadBalancerPolicy action to instantiate
-- specific policy configurations that will be applied to a load balancer.
-- Partial description of all the policy types defined by Elastic Load
-- Balancing for your account
-- https://elasticloadbalancing.amazonaws.com/?Version=2012-06-01
-- &Action=DescribeLoadBalancerPolicyTypes &AUTHPARAMS
-- SSLNegotiationPolicyType BackendServerAuthenticationPolicyType
-- PublicKeyPolicyType AppCookieStickinessPolicyType
-- LBCookieStickinessPolicyType ProxyProtocolPolicyType
-- 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE Description of ProxyProtocolPolicyType
-- https://elasticloadbalancing.amazonaws.com/?PolicyTypeNames.member.1=ProxyProtocolPolicyType
-- &Version=2012-06-01 &Action=DescribeLoadBalancerPolicyTypes &AUTHPARAMS
-- ProxyProtocol Boolean ONE ProxyProtocolPolicyType Policy that controls
-- whether to include the IP address and port of the originating request for
-- TCP messages. This policy operates on TCP/SSL listeners only
-- 1549581b-12b7-11e3-895e-1334aEXAMPLE.
--
-- See: 'Network.AWS.ELB.V2012_06_01.DescribeLoadBalancerPolicyTypes'
describeLoadBalancerPolicyTypes :: ( MonadCatch m
                                   , MonadResource m
                                   , MonadError AWS.Error m
                                   , MonadReader Env m
                                   , AWSRequest a
                                   )
                                => State DescribeLoadBalancerPolicyTypes a
                                -> m DescribeLoadBalancerPolicyTypesResponse
describeLoadBalancerPolicyTypes s =
    send (mkDescribeLoadBalancerPolicyTypes &~ s)

-- | Returns detailed configuration information for all the load balancers
-- created for the account. If you specify load balancer names, the action
-- returns configuration information of the specified load balancers. In order
-- to retrieve this information, you must provide the same account credentials
-- that was used to create the load balancer. Description of a specified load
-- balancer
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerNames.member.1=MyLoadBalancer
-- &Version=2012-06-01 &Action=DescribeLoadBalancers &AUTHPARAMS
-- MyLoadBalancer 2013-05-24T21:15:31.280Z 90 HTTP:80/ 2 60 10 HTTP 80 HTTP 80
-- i-e4cbe38d us-east-1a ZZZZZZZZZZZ123X
-- MyLoadBalancer-123456789.us-east-1.elb.amazonaws.com internet-facing
-- amazon-elb amazon-elb-sg
-- MyLoadBalancer-123456789.us-east-1.elb.amazonaws.com
-- 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE.
--
-- See: 'Network.AWS.ELB.V2012_06_01.DescribeLoadBalancers'
describeLoadBalancers :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env (ResumableSource m)
                         , AWSPager a
                         )
                      => State DescribeLoadBalancers a
                      -> ResumableSource m DescribeLoadBalancersResponse
describeLoadBalancers s =
    paginate (mkDescribeLoadBalancers &~ s)

-- | Describes the tags associated with one or more load balancers.
-- https://elasticloadbalancing.amazonaws.com//?Action=DescribeTags
-- &LoadBalancerNames.member.1=my-test-loadbalancer &Version=2012-06-01
-- &AUTHPARAMS my-test-project project test environment my-test-loadbalancer
-- 07b1ecbc-1100-11e3-acaf-dd7edEXAMPLE.
--
-- See: 'Network.AWS.ELB.V2012_06_01.DescribeTags'
describeTags :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                , AWSRequest a
                )
             => List1 Text -- ^ 'dtLoadBalancerNames'
             -> State DescribeTags a
             -> m DescribeTagsResponse
describeTags p1 s =
    send $ (mkDescribeTags p1) &~ s

-- | Removes subnets from the set of configured subnets in the Amazon Virtual
-- Private Cloud (Amazon VPC) for the load balancer. After a subnet is removed
-- all of the EC2 instances registered with the load balancer that are in the
-- removed subnet will go into the OutOfService state. When a subnet is
-- removed, the load balancer will balance the traffic among the remaining
-- routable subnets for the load balancer.
-- https://elasticloadbalancing.amazonaws.com/?Subnets.member.1=subnet-119f0078
-- &LoadBalancerName=my-test-vpc-loadbalancer &Version=2012-06-01
-- &Action=DetachLoadBalancerFromSubnets &AUTHPARAMS subnet-159f007c
-- subnet-3561b05e 07b1ecbc-1100-11e3-acaf-dd7edEXAMPLE.
--
-- See: 'Network.AWS.ELB.V2012_06_01.DetachLoadBalancerFromSubnets'
detachLoadBalancerFromSubnets :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadError AWS.Error m
                                 , MonadReader Env m
                                 , AWSRequest a
                                 )
                              => Text -- ^ 'dlbfsLoadBalancerName'
                              -> [Text] -- ^ 'dlbfsSubnets'
                              -> State DetachLoadBalancerFromSubnets a
                              -> m DetachLoadBalancerFromSubnetsResponse
detachLoadBalancerFromSubnets p1 p2 s =
    send $ (mkDetachLoadBalancerFromSubnets p1 p2) &~ s

-- | Removes the specified EC2 Availability Zones from the set of configured
-- Availability Zones for the load balancer. There must be at least one
-- Availability Zone registered with a load balancer at all times. Once an
-- Availability Zone is removed, all the instances registered with the load
-- balancer that are in the removed Availability Zone go into the OutOfService
-- state. Upon Availability Zone removal, the load balancer attempts to
-- equally balance the traffic among its remaining usable Availability Zones.
-- Trying to remove an Availability Zone that was not associated with the load
-- balancer does nothing. For more information, see Disable an Availability
-- Zone from a Load-Balanced Application in the Elastic Load Balancing
-- Developer Guide.
-- https://elasticloadbalancing.amazonaws.com/?AvailabilityZones.member.1=us-east-1a
-- &LoadBalancerName=MyHTTPSLoadBalancer &Version=2012-06-01
-- &Action=DisableAvailabilityZonesForLoadBalancer &AUTHPARAMS us-east-1b
-- ba6267d5-2566-11e3-9c6d-eb728EXAMPLE.
--
-- See: 'Network.AWS.ELB.V2012_06_01.DisableAvailabilityZonesForLoadBalancer'
disableAvailabilityZonesForLoadBalancer :: ( MonadCatch m
                                           , MonadResource m
                                           , MonadError AWS.Error m
                                           , MonadReader Env m
                                           , AWSRequest a
                                           )
                                        => Text -- ^ 'dazflbLoadBalancerName'
                                        -> [Text] -- ^ 'dazflbAvailabilityZones'
                                        -> State DisableAvailabilityZonesForLoadBalancer a
                                        -> m DisableAvailabilityZonesForLoadBalancerResponse
disableAvailabilityZonesForLoadBalancer p1 p2 s =
    send $ (mkDisableAvailabilityZonesForLoadBalancer p1 p2) &~ s

-- | Adds one or more EC2 Availability Zones to the load balancer. The load
-- balancer evenly distributes requests across all its registered Availability
-- Zones that contain instances. The new EC2 Availability Zones to be added
-- must be in the same EC2 Region as the Availability Zones for which the load
-- balancer was created. For more information, see Expand a Load Balanced
-- Application to an Additional Availability Zone in the Elastic Load
-- Balancing Developer Guide. Enable Availability Zone us-east-1c for
-- my-test-loadbalancer.
-- https://elasticloadbalancing.amazonaws.com/?AvailabilityZones.member.1=us-east-1c
-- &LoadBalancerName=my-test-loadbalancer &Version=2012-06-01
-- &Action=EnableAvailabilityZonesForLoadBalancer &AUTHPARAMS us-east-1a
-- us-east-1c 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE.
--
-- See: 'Network.AWS.ELB.V2012_06_01.EnableAvailabilityZonesForLoadBalancer'
enableAvailabilityZonesForLoadBalancer :: ( MonadCatch m
                                          , MonadResource m
                                          , MonadError AWS.Error m
                                          , MonadReader Env m
                                          , AWSRequest a
                                          )
                                       => Text -- ^ 'eazflbLoadBalancerName'
                                       -> [Text] -- ^ 'eazflbAvailabilityZones'
                                       -> State EnableAvailabilityZonesForLoadBalancer a
                                       -> m EnableAvailabilityZonesForLoadBalancerResponse
enableAvailabilityZonesForLoadBalancer p1 p2 s =
    send $ (mkEnableAvailabilityZonesForLoadBalancer p1 p2) &~ s

-- | Modifies the attributes of a specified load balancer. You can modify the
-- load balancer attributes, such as AccessLogs, ConnectionDraining, and
-- CrossZoneLoadBalancing by either enabling or disabling them. Or, you can
-- modify the load balancer attribute ConnectionSettings by specifying an idle
-- connection timeout value for your load balancer. For more information, see
-- the following: Cross-Zone Load Balancing Connection Draining Access Logs
-- Idle Connection Timeout Enable Cross-Zone Load Balancing
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=my-test-loadbalancer
-- &LoadBalancerAttributes.CrossZoneLoadBalancing.Enabled=true
-- &Version=2012-06-01 &Action=ModifyLoadBalancerAttributes &AUTHPARAMS
-- my-test-loadbalancer true 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE Enable
-- Access Log
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=my-test-loadbalancer
-- &LoadBalancerAttributes.AccessLog.Enabled=true
-- &LoadBalancerAttributes.AccessLog.S3BucketName=my-loadbalancer-logs
-- &LoadBalancerAttributes.AccessLog.S3BucketPrefix=my-bucket-prefix/prod
-- &LoadBalancerAttributes.AccessLog.EmitInterval=60 &Version=2012-06-01
-- &Action=ModifyLoadBalancerAttributes &AUTHPARAMS my-test-loadbalancer true
-- my-loadbalancer-logs my-bucket-prefix/prod 60
-- 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE Enable Connection Draining
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=my-test-loadbalancer
-- &LoadBalancerAttributes.ConnectionDraining.Enabled=true
-- &LoadBalancerAttributes.ConnectionDraining.Timeout=60 &Version=2012-06-01
-- &Action=ModifyLoadBalancerAttributes &AUTHPARAMS my-test-loadbalancer true
-- 60 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE Configure Connection Settings
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=my-test-loadbalancer
-- &LoadBalancerAttributes.ConnectionSettings.IdleTimeout=30
-- &Version=2012-06-01 &Action=ModifyLoadBalancerAttributes &AUTHPARAMS
-- my-test-loadbalancer 30 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE.
--
-- See: 'Network.AWS.ELB.V2012_06_01.ModifyLoadBalancerAttributes'
modifyLoadBalancerAttributes :: ( MonadCatch m
                                , MonadResource m
                                , MonadError AWS.Error m
                                , MonadReader Env m
                                , AWSRequest a
                                )
                             => Text -- ^ 'mlbaLoadBalancerName'
                             -> LoadBalancerAttributes -- ^ 'mlbaLoadBalancerAttributes'
                             -> State ModifyLoadBalancerAttributes a
                             -> m ModifyLoadBalancerAttributesResponse
modifyLoadBalancerAttributes p1 p2 s =
    send $ (mkModifyLoadBalancerAttributes p1 p2) &~ s

-- | Adds new instances to the load balancer. Once the instance is registered,
-- it starts receiving traffic and requests from the load balancer. Any
-- instance that is not in any of the Availability Zones registered for the
-- load balancer will be moved to the OutOfService state. It will move to the
-- InService state when the Availability Zone is added to the load balancer.
-- When an instance registered with a load balancer is stopped and then
-- restarted, the IP addresses associated with the instance changes. Elastic
-- Load Balancing cannot recognize the new IP address, which prevents it from
-- routing traffic to the instances. We recommend that you de-register your
-- Amazon EC2 instances from your load balancer after you stop your instance,
-- and then register the load balancer with your instance after you've
-- restarted. To de-register your instances from load balancer, use
-- DeregisterInstancesFromLoadBalancer action. For more information, see
-- De-register and Register Amazon EC2 Instances in the Elastic Load Balancing
-- Developer Guide. In order for this call to be successful, you must provide
-- the same account credentials as those that were used to create the load
-- balancer. Completion of this API does not guarantee that operation has
-- completed. Rather, it means that the request has been registered and the
-- changes will happen shortly. You can use DescribeLoadBalancers or
-- DescribeInstanceHealth action to check the state of the newly registered
-- instances.
-- https://elasticloadbalancing.amazonaws.com/?Instances.member.1.InstanceId=i-315b7e51
-- &LoadBalancerName=my-test-loadbalancer &Version=2012-06-01
-- &Action=RegisterInstancesWithLoadBalancer &AUTHPARAMS i-315b7e51
-- 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE.
--
-- See: 'Network.AWS.ELB.V2012_06_01.RegisterInstancesWithLoadBalancer'
registerInstancesWithLoadBalancer :: ( MonadCatch m
                                     , MonadResource m
                                     , MonadError AWS.Error m
                                     , MonadReader Env m
                                     , AWSRequest a
                                     )
                                  => Text -- ^ 'riwlbLoadBalancerName'
                                  -> [Instance] -- ^ 'riwlbInstances'
                                  -> State RegisterInstancesWithLoadBalancer a
                                  -> m RegisterInstancesWithLoadBalancerResponse
registerInstancesWithLoadBalancer p1 p2 s =
    send $ (mkRegisterInstancesWithLoadBalancer p1 p2) &~ s

-- | Removes one or more tags from the specified load balancer. Remove Two Tag
-- Keys from the Load Balancer
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=my-test-loadbalancer
-- &Tags.member.1.Key=owner &Tags.member.2.Key=project &Action=RemoveTags
-- &Version=2012-06-01 &AUTHPARAMS 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE.
--
-- See: 'Network.AWS.ELB.V2012_06_01.RemoveTags'
removeTags :: ( MonadCatch m
              , MonadResource m
              , MonadError AWS.Error m
              , MonadReader Env m
              , AWSRequest a
              )
           => [Text] -- ^ 'rtLoadBalancerNames'
           -> List1 TagKeyOnly -- ^ 'rtTags'
           -> State RemoveTags a
           -> m RemoveTagsResponse
removeTags p1 p2 s =
    send $ (mkRemoveTags p1 p2) &~ s

-- | Sets the certificate that terminates the specified listener's SSL
-- connections. The specified certificate replaces any prior certificate that
-- was used on the same load balancer and port. For more information on
-- updating your SSL certificate, see Updating an SSL Certificate for a Load
-- Balancer in the Elastic Load Balancing Developer Guide.
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=MyInternalLoadBalancer
-- &SSLCertificateId=arn:aws:iam::123456789012:server-certificate/testcert
-- &LoadBalancerPort=443 &Version=2012-06-01
-- &Action=SetLoadBalancerListenerSSLCertificate &AUTHPARAMS
-- 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE.
--
-- See: 'Network.AWS.ELB.V2012_06_01.SetLoadBalancerListenerSSLCertificate'
setLoadBalancerListenerSSLCertificate :: ( MonadCatch m
                                         , MonadResource m
                                         , MonadError AWS.Error m
                                         , MonadReader Env m
                                         , AWSRequest a
                                         )
                                      => Text -- ^ 'slblsslcLoadBalancerName'
                                      -> Integer -- ^ 'slblsslcLoadBalancerPort'
                                      -> Text -- ^ 'slblsslcSSLCertificateId'
                                      -> State SetLoadBalancerListenerSSLCertificate a
                                      -> m SetLoadBalancerListenerSSLCertificateResponse
setLoadBalancerListenerSSLCertificate p1 p2 p3 s =
    send $ (mkSetLoadBalancerListenerSSLCertificate p1 p2 p3) &~ s

-- | Replaces the current set of policies associated with a port on which the
-- back-end server is listening with a new set of policies. After the policies
-- have been created using CreateLoadBalancerPolicy, they can be applied here
-- as a list. At this time, only the back-end server authentication policy
-- type can be applied to the back-end ports; this policy type is composed of
-- multiple public key policies. The SetLoadBalancerPoliciesForBackendServer
-- replaces the current set of policies associated with the specified instance
-- port. Every time you use this action to enable the policies, use the
-- PolicyNames parameter to list all the policies you want to enable.
-- https://elasticloadbalancing.amazonaws.com/?InstancePort=80
-- &PolicyNames.member.1=EnableProxyProtocol
-- &PolicyNames.member.2=MyPolicyName2 &PolicyNames.member.3=MyPolicyName3
-- &LoadBalancerName=my-test-loadbalancer &Version=2012-06-01
-- &Action=SetLoadBalancerPoliciesForBackendServer &AUTHPARAMS
-- 0eb9b381-dde0-11e2-8d78-6ddbaEXAMPLE You can use DescribeLoadBalancers or
-- DescribeLoadBalancerPolicies action to verify that the policy has been
-- associated with the back-end server.
--
-- See: 'Network.AWS.ELB.V2012_06_01.SetLoadBalancerPoliciesForBackendServer'
setLoadBalancerPoliciesForBackendServer :: ( MonadCatch m
                                           , MonadResource m
                                           , MonadError AWS.Error m
                                           , MonadReader Env m
                                           , AWSRequest a
                                           )
                                        => Text -- ^ 'slbpfbsLoadBalancerName'
                                        -> Integer -- ^ 'slbpfbsInstancePort'
                                        -> [Text] -- ^ 'slbpfbsPolicyNames'
                                        -> State SetLoadBalancerPoliciesForBackendServer a
                                        -> m SetLoadBalancerPoliciesForBackendServerResponse
setLoadBalancerPoliciesForBackendServer p1 p2 p3 s =
    send $ (mkSetLoadBalancerPoliciesForBackendServer p1 p2 p3) &~ s

-- | Associates, updates, or disables a policy with a listener on the load
-- balancer. You can associate multiple policies with a listener. Associate
-- MySSLNegotiationPolicy with the load balancer port 443 on the
-- MyInternalLoadbalancer load balancer.
-- https://elasticloadbalancing.amazonaws.com/?PolicyNames.member.1=MySSLNegotiationPolicy
-- &LoadBalancerName=MyInternalLoadBalancer &LoadBalancerPort=443
-- &Version=2012-06-01 &Action=SetLoadBalancerPoliciesOfListener &AUTHPARAMS
-- azonaws.com/doc/2012-06-01/"> 07b1ecbc-1100-11e3-acaf-dd7edEXAMPLE.
--
-- See: 'Network.AWS.ELB.V2012_06_01.SetLoadBalancerPoliciesOfListener'
setLoadBalancerPoliciesOfListener :: ( MonadCatch m
                                     , MonadResource m
                                     , MonadError AWS.Error m
                                     , MonadReader Env m
                                     , AWSRequest a
                                     )
                                  => Text -- ^ 'slbpolLoadBalancerName'
                                  -> Integer -- ^ 'slbpolLoadBalancerPort'
                                  -> [Text] -- ^ 'slbpolPolicyNames'
                                  -> State SetLoadBalancerPoliciesOfListener a
                                  -> m SetLoadBalancerPoliciesOfListenerResponse
setLoadBalancerPoliciesOfListener p1 p2 p3 s =
    send $ (mkSetLoadBalancerPoliciesOfListener p1 p2 p3) &~ s
