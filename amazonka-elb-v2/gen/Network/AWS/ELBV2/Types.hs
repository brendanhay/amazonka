{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBV2.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBV2.Types
    (
    -- * Service Configuration
      elbv2

    -- * Errors
    , _InvalidConfigurationRequestException
    , _SubnetNotFoundException
    , _TooManyTargetsException
    , _RuleNotFoundException
    , _InvalidSubnetException
    , _TooManyRulesException
    , _TooManyTargetGroupsException
    , _DuplicateLoadBalancerNameException
    , _IncompatibleProtocolsException
    , _TooManyCertificatesException
    , _DuplicateTagKeysException
    , _DuplicateListenerException
    , _TooManyTagsException
    , _DuplicateTargetGroupNameException
    , _HealthUnavailableException
    , _PriorityInUseException
    , _TooManyLoadBalancersException
    , _UnsupportedProtocolException
    , _InvalidTargetException
    , _InvalidSecurityGroupException
    , _TargetGroupNotFoundException
    , _ListenerNotFoundException
    , _TooManyRegistrationsForTargetIdException
    , _TooManyListenersException
    , _TargetGroupAssociationLimitException
    , _OperationNotPermittedException
    , _SSLPolicyNotFoundException
    , _InvalidSchemeException
    , _LoadBalancerNotFoundException
    , _ResourceInUseException
    , _CertificateNotFoundException

    -- * ActionTypeEnum
    , ActionTypeEnum (..)

    -- * LoadBalancerSchemeEnum
    , LoadBalancerSchemeEnum (..)

    -- * LoadBalancerStateEnum
    , LoadBalancerStateEnum (..)

    -- * LoadBalancerTypeEnum
    , LoadBalancerTypeEnum (..)

    -- * ProtocolEnum
    , ProtocolEnum (..)

    -- * TargetHealthReasonEnum
    , TargetHealthReasonEnum (..)

    -- * TargetHealthStateEnum
    , TargetHealthStateEnum (..)

    -- * Action
    , Action
    , action
    , aType
    , aTargetGroupARN

    -- * AvailabilityZone
    , AvailabilityZone
    , availabilityZone
    , azSubnetId
    , azZoneName

    -- * Certificate
    , Certificate
    , certificate
    , cCertificateARN

    -- * Cipher
    , Cipher
    , cipher
    , cPriority
    , cName

    -- * Listener
    , Listener
    , listener
    , lSSLPolicy
    , lListenerARN
    , lProtocol
    , lDefaultActions
    , lCertificates
    , lLoadBalancerARN
    , lPort

    -- * LoadBalancer
    , LoadBalancer
    , loadBalancer
    , lbState
    , lbSecurityGroups
    , lbLoadBalancerName
    , lbCreatedTime
    , lbVPCId
    , lbCanonicalHostedZoneId
    , lbAvailabilityZones
    , lbLoadBalancerARN
    , lbScheme
    , lbType
    , lbDNSName

    -- * LoadBalancerAttribute
    , LoadBalancerAttribute
    , loadBalancerAttribute
    , lbaValue
    , lbaKey

    -- * LoadBalancerState
    , LoadBalancerState
    , loadBalancerState
    , lbsReason
    , lbsCode

    -- * Matcher
    , Matcher
    , matcher
    , mHTTPCode

    -- * Rule
    , Rule
    , rule
    , rPriority
    , rActions
    , rConditions
    , rRuleARN
    , rIsDefault

    -- * RuleCondition
    , RuleCondition
    , ruleCondition
    , rcField
    , rcValues

    -- * RulePriorityPair
    , RulePriorityPair
    , rulePriorityPair
    , rppPriority
    , rppRuleARN

    -- * SSLPolicy
    , SSLPolicy
    , sslPolicy
    , spCiphers
    , spName
    , spSSLProtocols

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- * TagDescription
    , TagDescription
    , tagDescription
    , tdResourceARN
    , tdTags

    -- * TargetDescription
    , TargetDescription
    , targetDescription
    , tdPort
    , tdId

    -- * TargetGroup
    , TargetGroup
    , targetGroup
    , tgMatcher
    , tgHealthCheckPath
    , tgUnhealthyThresholdCount
    , tgVPCId
    , tgTargetGroupARN
    , tgProtocol
    , tgHealthCheckIntervalSeconds
    , tgHealthyThresholdCount
    , tgHealthCheckProtocol
    , tgLoadBalancerARNs
    , tgHealthCheckTimeoutSeconds
    , tgHealthCheckPort
    , tgTargetGroupName
    , tgPort

    -- * TargetGroupAttribute
    , TargetGroupAttribute
    , targetGroupAttribute
    , tgaValue
    , tgaKey

    -- * TargetHealth
    , TargetHealth
    , targetHealth
    , thState
    , thReason
    , thDescription

    -- * TargetHealthDescription
    , TargetHealthDescription
    , targetHealthDescription
    , thdTargetHealth
    , thdHealthCheckPort
    , thdTarget
    ) where

import           Network.AWS.ELBV2.Types.Product
import           Network.AWS.ELBV2.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version '2015-12-01' of the Amazon Elastic Load Balancing SDK configuration.
elbv2 :: Service
elbv2 =
    Service
    { _svcAbbrev = "ELBV2"
    , _svcSigner = v4
    , _svcPrefix = "elasticloadbalancing"
    , _svcVersion = "2015-12-01"
    , _svcEndpoint = defaultEndpoint elbv2
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseXMLError "ELBV2"
    , _svcRetry = retry
    }
  where
    retry =
        Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | The requested configuration is not valid.
_InvalidConfigurationRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidConfigurationRequestException =
    _ServiceError . hasStatus 400 . hasCode "InvalidConfigurationRequest"

-- | The specified subnet does not exist.
_SubnetNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_SubnetNotFoundException =
    _ServiceError . hasStatus 400 . hasCode "SubnetNotFound"

-- | You\'ve reached the limit on the number of targets.
_TooManyTargetsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyTargetsException =
    _ServiceError . hasStatus 400 . hasCode "TooManyTargets"

-- | The specified rule does not exist.
_RuleNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_RuleNotFoundException = _ServiceError . hasStatus 400 . hasCode "RuleNotFound"

-- | The specified subnet is out of available addresses.
_InvalidSubnetException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSubnetException =
    _ServiceError . hasStatus 400 . hasCode "InvalidSubnet"

-- | You\'ve reached the limit on the number of rules per load balancer.
_TooManyRulesException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyRulesException = _ServiceError . hasStatus 400 . hasCode "TooManyRules"

-- | You\'ve reached the limit on the number of target groups for your AWS account.
_TooManyTargetGroupsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyTargetGroupsException =
    _ServiceError . hasStatus 400 . hasCode "TooManyTargetGroups"

-- | A load balancer with the specified name already exists for this account.
_DuplicateLoadBalancerNameException :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateLoadBalancerNameException =
    _ServiceError . hasStatus 400 . hasCode "DuplicateLoadBalancerName"

-- | The specified configuration is not valid with this protocol.
_IncompatibleProtocolsException :: AsError a => Getting (First ServiceError) a ServiceError
_IncompatibleProtocolsException =
    _ServiceError . hasStatus 400 . hasCode "IncompatibleProtocols"

-- | You\'ve reached the limit on the number of certificates per listener.
_TooManyCertificatesException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyCertificatesException =
    _ServiceError . hasStatus 400 . hasCode "TooManyCertificates"

-- | A tag key was specified more than once.
_DuplicateTagKeysException :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateTagKeysException =
    _ServiceError . hasStatus 400 . hasCode "DuplicateTagKeys"

-- | A listener with the specified port already exists.
_DuplicateListenerException :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateListenerException =
    _ServiceError . hasStatus 400 . hasCode "DuplicateListener"

-- | You\'ve reached the limit on the number of tags per load balancer.
_TooManyTagsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyTagsException = _ServiceError . hasStatus 400 . hasCode "TooManyTags"

-- | A target group with the specified name already exists.
_DuplicateTargetGroupNameException :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateTargetGroupNameException =
    _ServiceError . hasStatus 400 . hasCode "DuplicateTargetGroupName"

-- | The health of the specified targets could not be retrieved due to an internal error.
_HealthUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_HealthUnavailableException =
    _ServiceError . hasStatus 500 . hasCode "HealthUnavailable"

-- | The specified priority is in use.
_PriorityInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_PriorityInUseException =
    _ServiceError . hasStatus 400 . hasCode "PriorityInUse"

-- | You\'ve reached the limit on the number of load balancers for your AWS account.
_TooManyLoadBalancersException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyLoadBalancersException =
    _ServiceError . hasStatus 400 . hasCode "TooManyLoadBalancers"

-- | The specified protocol is not supported.
_UnsupportedProtocolException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedProtocolException =
    _ServiceError . hasStatus 400 . hasCode "UnsupportedProtocol"

-- | The specified target does not exist or is not in the same VPC as the target group.
_InvalidTargetException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTargetException =
    _ServiceError . hasStatus 400 . hasCode "InvalidTarget"

-- | The specified security group does not exist.
_InvalidSecurityGroupException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSecurityGroupException =
    _ServiceError . hasStatus 400 . hasCode "InvalidSecurityGroup"

-- | The specified target group does not exist.
_TargetGroupNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_TargetGroupNotFoundException =
    _ServiceError . hasStatus 400 . hasCode "TargetGroupNotFound"

-- | The specified listener does not exist.
_ListenerNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ListenerNotFoundException =
    _ServiceError . hasStatus 400 . hasCode "ListenerNotFound"

-- | You\'ve reached the limit on the number of times a target can be registered with a load balancer.
_TooManyRegistrationsForTargetIdException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyRegistrationsForTargetIdException =
    _ServiceError . hasStatus 400 . hasCode "TooManyRegistrationsForTargetId"

-- | You\'ve reached the limit on the number of listeners per load balancer.
_TooManyListenersException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyListenersException =
    _ServiceError . hasStatus 400 . hasCode "TooManyListeners"

-- | You\'ve reached the limit on the number of load balancers per target group.
_TargetGroupAssociationLimitException :: AsError a => Getting (First ServiceError) a ServiceError
_TargetGroupAssociationLimitException =
    _ServiceError . hasStatus 400 . hasCode "TargetGroupAssociationLimit"

-- | This operation is not allowed.
_OperationNotPermittedException :: AsError a => Getting (First ServiceError) a ServiceError
_OperationNotPermittedException =
    _ServiceError . hasStatus 400 . hasCode "OperationNotPermitted"

-- | The specified SSL policy does not exist.
_SSLPolicyNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_SSLPolicyNotFoundException =
    _ServiceError . hasStatus 400 . hasCode "SSLPolicyNotFound"

-- | The requested scheme is not valid.
_InvalidSchemeException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSchemeException =
    _ServiceError . hasStatus 400 . hasCode "InvalidScheme"

-- | The specified load balancer does not exist.
_LoadBalancerNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_LoadBalancerNotFoundException =
    _ServiceError . hasStatus 400 . hasCode "LoadBalancerNotFound"

-- | A specified resource is in use.
_ResourceInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceInUseException =
    _ServiceError . hasStatus 400 . hasCode "ResourceInUse"

-- | The specified certificate does not exist.
_CertificateNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_CertificateNotFoundException =
    _ServiceError . hasStatus 400 . hasCode "CertificateNotFound"
