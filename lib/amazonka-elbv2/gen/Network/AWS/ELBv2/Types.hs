-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _InvalidConfigurationRequestException
    , _SubnetNotFoundException
    , _TooManyTargetsException
    , _RuleNotFoundException
    , _InvalidSubnetException
    , _TooManyRulesException
    , _TooManyTargetGroupsException
    , _TooManyActionsException
    , _DuplicateLoadBalancerNameException
    , _IncompatibleProtocolsException
    , _TooManyCertificatesException
    , _DuplicateTagKeysException
    , _DuplicateListenerException
    , _TooManyTagsException
    , _DuplicateTargetGroupNameException
    , _HealthUnavailableException
    , _AllocationIdNotFoundException
    , _PriorityInUseException
    , _TooManyLoadBalancersException
    , _UnsupportedProtocolException
    , _ALPNPolicyNotSupportedException
    , _InvalidTargetException
    , _InvalidSecurityGroupException
    , _TargetGroupNotFoundException
    , _ListenerNotFoundException
    , _InvalidLoadBalancerActionException
    , _TooManyRegistrationsForTargetIdException
    , _TooManyListenersException
    , _TargetGroupAssociationLimitException
    , _OperationNotPermittedException
    , _SSLPolicyNotFoundException
    , _InvalidSchemeException
    , _AvailabilityZoneNotSupportedException
    , _TooManyUniqueTargetGroupsPerLoadBalancerException
    , _LoadBalancerNotFoundException
    , _ResourceInUseException
    , _CertificateNotFoundException

    -- * ProtocolVersion
    , ProtocolVersion (..)

    -- * LoadBalancerAddress
    , LoadBalancerAddress (..)
    , mkLoadBalancerAddress
    , lbaAllocationId
    , lbaIPv6Address
    , lbaIpAddress
    , lbaPrivateIPv4Address

    -- * SslPolicy
    , SslPolicy (..)
    , mkSslPolicy
    , spCiphers
    , spName
    , spSslProtocols

    -- * AuthenticateCognitoActionUserPoolArn
    , AuthenticateCognitoActionUserPoolArn (..)

    -- * TargetId
    , TargetId (..)

    -- * Matcher
    , Matcher (..)
    , mkMatcher
    , mGrpcCode
    , mHttpCode

    -- * TagDescription
    , TagDescription (..)
    , mkTagDescription
    , tdResourceArn
    , tdTags

    -- * Max
    , Max (..)

    -- * HttpRequestMethodConditionConfig
    , HttpRequestMethodConditionConfig (..)
    , mkHttpRequestMethodConditionConfig
    , hrmccValues

    -- * AuthenticateOidcActionAuthenticationRequestParamValue
    , AuthenticateOidcActionAuthenticationRequestParamValue (..)

    -- * TargetDescription
    , TargetDescription (..)
    , mkTargetDescription
    , tdId
    , tdAvailabilityZone
    , tdPort

    -- * IPv6Address
    , IPv6Address (..)

    -- * IpAddress
    , IpAddress (..)

    -- * SubnetMapping
    , SubnetMapping (..)
    , mkSubnetMapping
    , smAllocationId
    , smIPv6Address
    , smPrivateIPv4Address
    , smSubnetId

    -- * AuthenticateOidcActionSessionCookieName
    , AuthenticateOidcActionSessionCookieName (..)

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * TargetHealthReasonEnum
    , TargetHealthReasonEnum (..)

    -- * FixedResponseActionStatusCode
    , FixedResponseActionStatusCode (..)

    -- * SslProtocol
    , SslProtocol (..)

    -- * TargetGroupAttribute
    , TargetGroupAttribute (..)
    , mkTargetGroupAttribute
    , tgaKey
    , tgaValue

    -- * AuthenticateOidcActionAuthorizationEndpoint
    , AuthenticateOidcActionAuthorizationEndpoint (..)

    -- * AuthenticateOidcActionTokenEndpoint
    , AuthenticateOidcActionTokenEndpoint (..)

    -- * CipherName
    , CipherName (..)

    -- * LoadBalancerName
    , LoadBalancerName (..)

    -- * TargetHealth
    , TargetHealth (..)
    , mkTargetHealth
    , thDescription
    , thReason
    , thState

    -- * AuthenticateCognitoActionAuthenticationRequestParamName
    , AuthenticateCognitoActionAuthenticationRequestParamName (..)

    -- * AllocationId
    , AllocationId (..)

    -- * HttpCode
    , HttpCode (..)

    -- * ProtocolEnum
    , ProtocolEnum (..)

    -- * Path
    , Path (..)

    -- * RuleCondition
    , RuleCondition (..)
    , mkRuleCondition
    , rcField
    , rcHostHeaderConfig
    , rcHttpHeaderConfig
    , rcHttpRequestMethodConfig
    , rcPathPatternConfig
    , rcQueryStringConfig
    , rcSourceIpConfig
    , rcValues

    -- * AuthenticateCognitoActionConditionalBehaviorEnum
    , AuthenticateCognitoActionConditionalBehaviorEnum (..)

    -- * SourceIpConditionConfig
    , SourceIpConditionConfig (..)
    , mkSourceIpConditionConfig
    , siccValues

    -- * CertificateArn
    , CertificateArn (..)

    -- * VpcId
    , VpcId (..)

    -- * AlpnPolicyValue
    , AlpnPolicyValue (..)

    -- * CanonicalHostedZoneId
    , CanonicalHostedZoneId (..)

    -- * AuthenticateOidcActionScope
    , AuthenticateOidcActionScope (..)

    -- * PrivateIPv4Address
    , PrivateIPv4Address (..)

    -- * TargetGroupArn
    , TargetGroupArn (..)

    -- * GrpcCode
    , GrpcCode (..)

    -- * ListenerArn
    , ListenerArn (..)

    -- * AuthenticateCognitoActionScope
    , AuthenticateCognitoActionScope (..)

    -- * QueryStringConditionConfig
    , QueryStringConditionConfig (..)
    , mkQueryStringConditionConfig
    , qsccValues

    -- * LoadBalancerAttribute
    , LoadBalancerAttribute (..)
    , mkLoadBalancerAttribute
    , lbaKey
    , lbaValue

    -- * Action
    , Action (..)
    , mkAction
    , aType
    , aAuthenticateCognitoConfig
    , aAuthenticateOidcConfig
    , aFixedResponseConfig
    , aForwardConfig
    , aOrder
    , aRedirectConfig
    , aTargetGroupArn

    -- * StringValue
    , StringValue (..)

    -- * RedirectActionQuery
    , RedirectActionQuery (..)

    -- * Rule
    , Rule (..)
    , mkRule
    , rActions
    , rConditions
    , rIsDefault
    , rPriority
    , rRuleArn

    -- * SubnetId
    , SubnetId (..)

    -- * AuthenticateOidcActionClientId
    , AuthenticateOidcActionClientId (..)

    -- * TargetHealthStateEnum
    , TargetHealthStateEnum (..)

    -- * AuthenticateOidcActionIssuer
    , AuthenticateOidcActionIssuer (..)

    -- * RedirectActionPath
    , RedirectActionPath (..)

    -- * ZoneName
    , ZoneName (..)

    -- * QueryStringKeyValuePair
    , QueryStringKeyValuePair (..)
    , mkQueryStringKeyValuePair
    , qskvpKey
    , qskvpValue

    -- * SecurityGroupId
    , SecurityGroupId (..)

    -- * LoadBalancerSchemeEnum
    , LoadBalancerSchemeEnum (..)

    -- * PathPatternConditionConfig
    , PathPatternConditionConfig (..)
    , mkPathPatternConditionConfig
    , ppccValues

    -- * RedirectActionHost
    , RedirectActionHost (..)

    -- * CustomerOwnedIpv4Pool
    , CustomerOwnedIpv4Pool (..)

    -- * ForwardActionConfig
    , ForwardActionConfig (..)
    , mkForwardActionConfig
    , facTargetGroupStickinessConfig
    , facTargetGroups

    -- * LoadBalancerArn
    , LoadBalancerArn (..)

    -- * IpAddressType
    , IpAddressType (..)

    -- * ResourceArn
    , ResourceArn (..)

    -- * AvailabilityZone
    , AvailabilityZone (..)
    , mkAvailabilityZone
    , azLoadBalancerAddresses
    , azOutpostId
    , azSubnetId
    , azZoneName

    -- * Name
    , Name (..)

    -- * RedirectActionProtocol
    , RedirectActionProtocol (..)

    -- * LoadBalancer
    , LoadBalancer (..)
    , mkLoadBalancer
    , lbAvailabilityZones
    , lbCanonicalHostedZoneId
    , lbCreatedTime
    , lbCustomerOwnedIpv4Pool
    , lbDNSName
    , lbIpAddressType
    , lbLoadBalancerArn
    , lbLoadBalancerName
    , lbScheme
    , lbSecurityGroups
    , lbState
    , lbType
    , lbVpcId

    -- * FixedResponseActionContentType
    , FixedResponseActionContentType (..)

    -- * Marker
    , Marker (..)

    -- * TargetTypeEnum
    , TargetTypeEnum (..)

    -- * Limit
    , Limit (..)
    , mkLimit
    , lMax
    , lName

    -- * AuthenticateOidcActionClientSecret
    , AuthenticateOidcActionClientSecret (..)

    -- * Certificate
    , Certificate (..)
    , mkCertificate
    , cCertificateArn
    , cIsDefault

    -- * AuthenticateCognitoActionUserPoolClientId
    , AuthenticateCognitoActionUserPoolClientId (..)

    -- * LoadBalancerState
    , LoadBalancerState (..)
    , mkLoadBalancerState
    , lbsCode
    , lbsReason

    -- * RedirectActionPort
    , RedirectActionPort (..)

    -- * Cipher
    , Cipher (..)
    , mkCipher
    , cName
    , cPriority

    -- * ActionTypeEnum
    , ActionTypeEnum (..)

    -- * TagKey
    , TagKey (..)

    -- * TargetGroupStickinessConfig
    , TargetGroupStickinessConfig (..)
    , mkTargetGroupStickinessConfig
    , tgscDurationSeconds
    , tgscEnabled

    -- * FixedResponseActionMessage
    , FixedResponseActionMessage (..)

    -- * AuthenticateOidcActionConfig
    , AuthenticateOidcActionConfig (..)
    , mkAuthenticateOidcActionConfig
    , aoacIssuer
    , aoacAuthorizationEndpoint
    , aoacTokenEndpoint
    , aoacUserInfoEndpoint
    , aoacClientId
    , aoacAuthenticationRequestExtraParams
    , aoacClientSecret
    , aoacOnUnauthenticatedRequest
    , aoacScope
    , aoacSessionCookieName
    , aoacSessionTimeout
    , aoacUseExistingClientSecret

    -- * AuthenticateOidcActionAuthenticationRequestParamName
    , AuthenticateOidcActionAuthenticationRequestParamName (..)

    -- * HealthCheckPort
    , HealthCheckPort (..)

    -- * SslPolicyName
    , SslPolicyName (..)

    -- * HttpHeaderConditionName
    , HttpHeaderConditionName (..)

    -- * AuthenticateOidcActionConditionalBehaviorEnum
    , AuthenticateOidcActionConditionalBehaviorEnum (..)

    -- * RuleArn
    , RuleArn (..)

    -- * TargetHealthDescription
    , TargetHealthDescription (..)
    , mkTargetHealthDescription
    , thdHealthCheckPort
    , thdTarget
    , thdTargetHealth

    -- * OutpostId
    , OutpostId (..)

    -- * HttpHeaderConditionConfig
    , HttpHeaderConditionConfig (..)
    , mkHttpHeaderConditionConfig
    , hHttpHeaderName
    , hValues

    -- * AuthenticateCognitoActionConfig
    , AuthenticateCognitoActionConfig (..)
    , mkAuthenticateCognitoActionConfig
    , acacUserPoolArn
    , acacUserPoolClientId
    , acacUserPoolDomain
    , acacAuthenticationRequestExtraParams
    , acacOnUnauthenticatedRequest
    , acacScope
    , acacSessionCookieName
    , acacSessionTimeout

    -- * RulePriorityPair
    , RulePriorityPair (..)
    , mkRulePriorityPair
    , rppPriority
    , rppRuleArn

    -- * Description
    , Description (..)

    -- * RedirectActionConfig
    , RedirectActionConfig (..)
    , mkRedirectActionConfig
    , racStatusCode
    , racHost
    , racPath
    , racPort
    , racProtocol
    , racQuery

    -- * DNSName
    , DNSName (..)

    -- * RedirectActionStatusCodeEnum
    , RedirectActionStatusCodeEnum (..)

    -- * Listener
    , Listener (..)
    , mkListener
    , lAlpnPolicy
    , lCertificates
    , lDefaultActions
    , lListenerArn
    , lLoadBalancerArn
    , lPort
    , lProtocol
    , lSslPolicy

    -- * LoadBalancerStateEnum
    , LoadBalancerStateEnum (..)

    -- * HostHeaderConditionConfig
    , HostHeaderConditionConfig (..)
    , mkHostHeaderConditionConfig
    , hhccValues

    -- * TargetGroupTuple
    , TargetGroupTuple (..)
    , mkTargetGroupTuple
    , tgtTargetGroupArn
    , tgtWeight

    -- * TargetGroup
    , TargetGroup (..)
    , mkTargetGroup
    , tgHealthCheckEnabled
    , tgHealthCheckIntervalSeconds
    , tgHealthCheckPath
    , tgHealthCheckPort
    , tgHealthCheckProtocol
    , tgHealthCheckTimeoutSeconds
    , tgHealthyThresholdCount
    , tgLoadBalancerArns
    , tgMatcher
    , tgPort
    , tgProtocol
    , tgProtocolVersion
    , tgTargetGroupArn
    , tgTargetGroupName
    , tgTargetType
    , tgUnhealthyThresholdCount
    , tgVpcId

    -- * TargetGroupName
    , TargetGroupName (..)

    -- * LoadBalancerTypeEnum
    , LoadBalancerTypeEnum (..)

    -- * AuthenticateCognitoActionAuthenticationRequestParamValue
    , AuthenticateCognitoActionAuthenticationRequestParamValue (..)

    -- * FixedResponseActionConfig
    , FixedResponseActionConfig (..)
    , mkFixedResponseActionConfig
    , fracStatusCode
    , fracContentType
    , fracMessageBody

    -- * HealthCheckPath
    , HealthCheckPath (..)

    -- * NextMarker
    , NextMarker (..)

    -- * Key
    , Key (..)

    -- * Value
    , Value (..)

    -- * Field
    , Field (..)

    -- * Reason
    , Reason (..)

    -- * UserInfoEndpoint
    , UserInfoEndpoint (..)

    -- * UserPoolDomain
    , UserPoolDomain (..)

    -- * SessionCookieName
    , SessionCookieName (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.ELBv2.Types.ProtocolVersion
  
import Network.AWS.ELBv2.Types.LoadBalancerAddress
  
import Network.AWS.ELBv2.Types.SslPolicy
  
import Network.AWS.ELBv2.Types.AuthenticateCognitoActionUserPoolArn
  
import Network.AWS.ELBv2.Types.TargetId
  
import Network.AWS.ELBv2.Types.Matcher
  
import Network.AWS.ELBv2.Types.TagDescription
  
import Network.AWS.ELBv2.Types.Max
  
import Network.AWS.ELBv2.Types.HttpRequestMethodConditionConfig
  
import Network.AWS.ELBv2.Types.AuthenticateOidcActionAuthenticationRequestParamValue
  
import Network.AWS.ELBv2.Types.TargetDescription
  
  
  
import Network.AWS.ELBv2.Types.IPv6Address
  
import Network.AWS.ELBv2.Types.IpAddress
  
import Network.AWS.ELBv2.Types.SubnetMapping
  
import Network.AWS.ELBv2.Types.AuthenticateOidcActionSessionCookieName
  
import Network.AWS.ELBv2.Types.Tag
  
  
import Network.AWS.ELBv2.Types.TargetHealthReasonEnum
  
  
import Network.AWS.ELBv2.Types.FixedResponseActionStatusCode
  
import Network.AWS.ELBv2.Types.SslProtocol
  
import Network.AWS.ELBv2.Types.TargetGroupAttribute
  
import Network.AWS.ELBv2.Types.AuthenticateOidcActionAuthorizationEndpoint
  
import Network.AWS.ELBv2.Types.AuthenticateOidcActionTokenEndpoint
  
import Network.AWS.ELBv2.Types.CipherName
  
import Network.AWS.ELBv2.Types.LoadBalancerName
  
import Network.AWS.ELBv2.Types.TargetHealth
  
import Network.AWS.ELBv2.Types.AuthenticateCognitoActionAuthenticationRequestParamName
  
import Network.AWS.ELBv2.Types.AllocationId
  
import Network.AWS.ELBv2.Types.HttpCode
  
  
  
import Network.AWS.ELBv2.Types.ProtocolEnum
  
import Network.AWS.ELBv2.Types.Path
  
  
import Network.AWS.ELBv2.Types.RuleCondition
  
import Network.AWS.ELBv2.Types.AuthenticateCognitoActionConditionalBehaviorEnum
  
  
  
import Network.AWS.ELBv2.Types.SourceIpConditionConfig
  
import Network.AWS.ELBv2.Types.CertificateArn
  
import Network.AWS.ELBv2.Types.VpcId
  
import Network.AWS.ELBv2.Types.AlpnPolicyValue
  
import Network.AWS.ELBv2.Types.CanonicalHostedZoneId
  
import Network.AWS.ELBv2.Types.AuthenticateOidcActionScope
  
import Network.AWS.ELBv2.Types.PrivateIPv4Address
  
  
import Network.AWS.ELBv2.Types.TargetGroupArn
  
import Network.AWS.ELBv2.Types.GrpcCode
  
import Network.AWS.ELBv2.Types.ListenerArn
  
import Network.AWS.ELBv2.Types.AuthenticateCognitoActionScope
  
import Network.AWS.ELBv2.Types.QueryStringConditionConfig
  
import Network.AWS.ELBv2.Types.LoadBalancerAttribute
  
  
import Network.AWS.ELBv2.Types.Action
  
import Network.AWS.ELBv2.Types.StringValue
  
  
  
import Network.AWS.ELBv2.Types.RedirectActionQuery
  
import Network.AWS.ELBv2.Types.Rule
  
import Network.AWS.ELBv2.Types.SubnetId
  
import Network.AWS.ELBv2.Types.AuthenticateOidcActionClientId
  
import Network.AWS.ELBv2.Types.TargetHealthStateEnum
  
import Network.AWS.ELBv2.Types.AuthenticateOidcActionIssuer
  
import Network.AWS.ELBv2.Types.RedirectActionPath
  
import Network.AWS.ELBv2.Types.ZoneName
  
  
  
import Network.AWS.ELBv2.Types.QueryStringKeyValuePair
  
  
  
  
  
import Network.AWS.ELBv2.Types.SecurityGroupId
  
import Network.AWS.ELBv2.Types.LoadBalancerSchemeEnum
  
import Network.AWS.ELBv2.Types.PathPatternConditionConfig
  
  
import Network.AWS.ELBv2.Types.RedirectActionHost
  
import Network.AWS.ELBv2.Types.CustomerOwnedIpv4Pool
  
import Network.AWS.ELBv2.Types.ForwardActionConfig
  
import Network.AWS.ELBv2.Types.LoadBalancerArn
  
import Network.AWS.ELBv2.Types.IpAddressType
  
import Network.AWS.ELBv2.Types.ResourceArn
  
  
import Network.AWS.ELBv2.Types.AvailabilityZone
  
  
  
import Network.AWS.ELBv2.Types.Name
  
import Network.AWS.ELBv2.Types.RedirectActionProtocol
  
import Network.AWS.ELBv2.Types.LoadBalancer
  
import Network.AWS.ELBv2.Types.FixedResponseActionContentType
  
import Network.AWS.ELBv2.Types.Marker
  
  
  
  
import Network.AWS.ELBv2.Types.TargetTypeEnum
  
import Network.AWS.ELBv2.Types.Limit
  
import Network.AWS.ELBv2.Types.AuthenticateOidcActionClientSecret
  
import Network.AWS.ELBv2.Types.Certificate
  
  
import Network.AWS.ELBv2.Types.AuthenticateCognitoActionUserPoolClientId
  
import Network.AWS.ELBv2.Types.LoadBalancerState
  
  
import Network.AWS.ELBv2.Types.RedirectActionPort
  
import Network.AWS.ELBv2.Types.Cipher
  
  
  
import Network.AWS.ELBv2.Types.ActionTypeEnum
  
import Network.AWS.ELBv2.Types.TagKey
  
import Network.AWS.ELBv2.Types.TargetGroupStickinessConfig
  
import Network.AWS.ELBv2.Types.FixedResponseActionMessage
  
  
import Network.AWS.ELBv2.Types.AuthenticateOidcActionConfig
  
import Network.AWS.ELBv2.Types.AuthenticateOidcActionAuthenticationRequestParamName
  
import Network.AWS.ELBv2.Types.HealthCheckPort
  
  
import Network.AWS.ELBv2.Types.SslPolicyName
  
import Network.AWS.ELBv2.Types.HttpHeaderConditionName
  
import Network.AWS.ELBv2.Types.AuthenticateOidcActionConditionalBehaviorEnum
  
import Network.AWS.ELBv2.Types.RuleArn
  
import Network.AWS.ELBv2.Types.TargetHealthDescription
  
  
import Network.AWS.ELBv2.Types.OutpostId
  
  
import Network.AWS.ELBv2.Types.HttpHeaderConditionConfig
  
import Network.AWS.ELBv2.Types.AuthenticateCognitoActionConfig
  
import Network.AWS.ELBv2.Types.RulePriorityPair
  
import Network.AWS.ELBv2.Types.Description
  
import Network.AWS.ELBv2.Types.RedirectActionConfig
  
import Network.AWS.ELBv2.Types.DNSName
  
import Network.AWS.ELBv2.Types.RedirectActionStatusCodeEnum
  
import Network.AWS.ELBv2.Types.Listener
  
import Network.AWS.ELBv2.Types.LoadBalancerStateEnum
  
import Network.AWS.ELBv2.Types.HostHeaderConditionConfig
  
import Network.AWS.ELBv2.Types.TargetGroupTuple
  
  
import Network.AWS.ELBv2.Types.TargetGroup
  
import Network.AWS.ELBv2.Types.TargetGroupName
  
import Network.AWS.ELBv2.Types.LoadBalancerTypeEnum
  
import Network.AWS.ELBv2.Types.AuthenticateCognitoActionAuthenticationRequestParamValue
  
  
import Network.AWS.ELBv2.Types.FixedResponseActionConfig
  
  
import Network.AWS.ELBv2.Types.HealthCheckPath
  
import Network.AWS.ELBv2.Types.NextMarker
  
import Network.AWS.ELBv2.Types.Key
  
import Network.AWS.ELBv2.Types.Value
  
import Network.AWS.ELBv2.Types.Field
  
import Network.AWS.ELBv2.Types.Reason
  
import Network.AWS.ELBv2.Types.UserInfoEndpoint
  
import Network.AWS.ELBv2.Types.UserPoolDomain
  
import Network.AWS.ELBv2.Types.SessionCookieName
  

-- | API version @2015-12-01@ of the Amazon Elastic Load Balancing SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "ELBv2",
                 Core._svcSigner = Sign.v4,
                 Core._svcPrefix = "elasticloadbalancing",
                 Core._svcVersion = "2015-12-01", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseXMLError "ELBv2",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | The requested configuration is not valid.
_InvalidConfigurationRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidConfigurationRequestException
  = Core._MatchServiceError mkServiceConfig
      "InvalidConfigurationRequest"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidConfigurationRequestException #-}
{-# DEPRECATED _InvalidConfigurationRequestException "Use generic-lens or generic-optics instead"  #-}

-- | The specified subnet does not exist.
_SubnetNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SubnetNotFoundException
  = Core._MatchServiceError mkServiceConfig "SubnetNotFound" Core..
      Core.hasStatues 400
{-# INLINEABLE _SubnetNotFoundException #-}
{-# DEPRECATED _SubnetNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | You've reached the limit on the number of targets.
_TooManyTargetsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyTargetsException
  = Core._MatchServiceError mkServiceConfig "TooManyTargets" Core..
      Core.hasStatues 400
{-# INLINEABLE _TooManyTargetsException #-}
{-# DEPRECATED _TooManyTargetsException "Use generic-lens or generic-optics instead"  #-}

-- | The specified rule does not exist.
_RuleNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RuleNotFoundException
  = Core._MatchServiceError mkServiceConfig "RuleNotFound" Core..
      Core.hasStatues 400
{-# INLINEABLE _RuleNotFoundException #-}
{-# DEPRECATED _RuleNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The specified subnet is out of available addresses.
_InvalidSubnetException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSubnetException
  = Core._MatchServiceError mkServiceConfig "InvalidSubnet" Core..
      Core.hasStatues 400
{-# INLINEABLE _InvalidSubnetException #-}
{-# DEPRECATED _InvalidSubnetException "Use generic-lens or generic-optics instead"  #-}

-- | You've reached the limit on the number of rules per load balancer.
_TooManyRulesException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyRulesException
  = Core._MatchServiceError mkServiceConfig "TooManyRules" Core..
      Core.hasStatues 400
{-# INLINEABLE _TooManyRulesException #-}
{-# DEPRECATED _TooManyRulesException "Use generic-lens or generic-optics instead"  #-}

-- | You've reached the limit on the number of target groups for your AWS account.
_TooManyTargetGroupsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyTargetGroupsException
  = Core._MatchServiceError mkServiceConfig "TooManyTargetGroups"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyTargetGroupsException #-}
{-# DEPRECATED _TooManyTargetGroupsException "Use generic-lens or generic-optics instead"  #-}

-- | You've reached the limit on the number of actions per rule.
_TooManyActionsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyActionsException
  = Core._MatchServiceError mkServiceConfig "TooManyActions" Core..
      Core.hasStatues 400
{-# INLINEABLE _TooManyActionsException #-}
{-# DEPRECATED _TooManyActionsException "Use generic-lens or generic-optics instead"  #-}

-- | A load balancer with the specified name already exists.
_DuplicateLoadBalancerNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateLoadBalancerNameException
  = Core._MatchServiceError mkServiceConfig
      "DuplicateLoadBalancerName"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DuplicateLoadBalancerNameException #-}
{-# DEPRECATED _DuplicateLoadBalancerNameException "Use generic-lens or generic-optics instead"  #-}

-- | The specified configuration is not valid with this protocol.
_IncompatibleProtocolsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IncompatibleProtocolsException
  = Core._MatchServiceError mkServiceConfig "IncompatibleProtocols"
      Core.. Core.hasStatues 400
{-# INLINEABLE _IncompatibleProtocolsException #-}
{-# DEPRECATED _IncompatibleProtocolsException "Use generic-lens or generic-optics instead"  #-}

-- | You've reached the limit on the number of certificates per load balancer.
_TooManyCertificatesException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyCertificatesException
  = Core._MatchServiceError mkServiceConfig "TooManyCertificates"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyCertificatesException #-}
{-# DEPRECATED _TooManyCertificatesException "Use generic-lens or generic-optics instead"  #-}

-- | A tag key was specified more than once.
_DuplicateTagKeysException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateTagKeysException
  = Core._MatchServiceError mkServiceConfig "DuplicateTagKeys" Core..
      Core.hasStatues 400
{-# INLINEABLE _DuplicateTagKeysException #-}
{-# DEPRECATED _DuplicateTagKeysException "Use generic-lens or generic-optics instead"  #-}

-- | A listener with the specified port already exists.
_DuplicateListenerException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateListenerException
  = Core._MatchServiceError mkServiceConfig "DuplicateListener"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DuplicateListenerException #-}
{-# DEPRECATED _DuplicateListenerException "Use generic-lens or generic-optics instead"  #-}

-- | You've reached the limit on the number of tags per load balancer.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException
  = Core._MatchServiceError mkServiceConfig "TooManyTags" Core..
      Core.hasStatues 400
{-# INLINEABLE _TooManyTagsException #-}
{-# DEPRECATED _TooManyTagsException "Use generic-lens or generic-optics instead"  #-}

-- | A target group with the specified name already exists.
_DuplicateTargetGroupNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateTargetGroupNameException
  = Core._MatchServiceError mkServiceConfig
      "DuplicateTargetGroupName"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DuplicateTargetGroupNameException #-}
{-# DEPRECATED _DuplicateTargetGroupNameException "Use generic-lens or generic-optics instead"  #-}

-- | The health of the specified targets could not be retrieved due to an internal error.
_HealthUnavailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_HealthUnavailableException
  = Core._MatchServiceError mkServiceConfig "HealthUnavailable"
      Core.. Core.hasStatues 500
{-# INLINEABLE _HealthUnavailableException #-}
{-# DEPRECATED _HealthUnavailableException "Use generic-lens or generic-optics instead"  #-}

-- | The specified allocation ID does not exist.
_AllocationIdNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AllocationIdNotFoundException
  = Core._MatchServiceError mkServiceConfig "AllocationIdNotFound"
      Core.. Core.hasStatues 400
{-# INLINEABLE _AllocationIdNotFoundException #-}
{-# DEPRECATED _AllocationIdNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The specified priority is in use.
_PriorityInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PriorityInUseException
  = Core._MatchServiceError mkServiceConfig "PriorityInUse" Core..
      Core.hasStatues 400
{-# INLINEABLE _PriorityInUseException #-}
{-# DEPRECATED _PriorityInUseException "Use generic-lens or generic-optics instead"  #-}

-- | You've reached the limit on the number of load balancers for your AWS account.
_TooManyLoadBalancersException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyLoadBalancersException
  = Core._MatchServiceError mkServiceConfig "TooManyLoadBalancers"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyLoadBalancersException #-}
{-# DEPRECATED _TooManyLoadBalancersException "Use generic-lens or generic-optics instead"  #-}

-- | The specified protocol is not supported.
_UnsupportedProtocolException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedProtocolException
  = Core._MatchServiceError mkServiceConfig "UnsupportedProtocol"
      Core.. Core.hasStatues 400
{-# INLINEABLE _UnsupportedProtocolException #-}
{-# DEPRECATED _UnsupportedProtocolException "Use generic-lens or generic-optics instead"  #-}

-- | The specified ALPN policy is not supported.
_ALPNPolicyNotSupportedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ALPNPolicyNotSupportedException
  = Core._MatchServiceError mkServiceConfig "ALPNPolicyNotFound"
      Core.. Core.hasStatues 400
{-# INLINEABLE _ALPNPolicyNotSupportedException #-}
{-# DEPRECATED _ALPNPolicyNotSupportedException "Use generic-lens or generic-optics instead"  #-}

-- | The specified target does not exist, is not in the same VPC as the target group, or has an unsupported instance type.
_InvalidTargetException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTargetException
  = Core._MatchServiceError mkServiceConfig "InvalidTarget" Core..
      Core.hasStatues 400
{-# INLINEABLE _InvalidTargetException #-}
{-# DEPRECATED _InvalidTargetException "Use generic-lens or generic-optics instead"  #-}

-- | The specified security group does not exist.
_InvalidSecurityGroupException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSecurityGroupException
  = Core._MatchServiceError mkServiceConfig "InvalidSecurityGroup"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidSecurityGroupException #-}
{-# DEPRECATED _InvalidSecurityGroupException "Use generic-lens or generic-optics instead"  #-}

-- | The specified target group does not exist.
_TargetGroupNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TargetGroupNotFoundException
  = Core._MatchServiceError mkServiceConfig "TargetGroupNotFound"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TargetGroupNotFoundException #-}
{-# DEPRECATED _TargetGroupNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The specified listener does not exist.
_ListenerNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ListenerNotFoundException
  = Core._MatchServiceError mkServiceConfig "ListenerNotFound" Core..
      Core.hasStatues 400
{-# INLINEABLE _ListenerNotFoundException #-}
{-# DEPRECATED _ListenerNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The requested action is not valid.
_InvalidLoadBalancerActionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidLoadBalancerActionException
  = Core._MatchServiceError mkServiceConfig
      "InvalidLoadBalancerAction"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidLoadBalancerActionException #-}
{-# DEPRECATED _InvalidLoadBalancerActionException "Use generic-lens or generic-optics instead"  #-}

-- | You've reached the limit on the number of times a target can be registered with a load balancer.
_TooManyRegistrationsForTargetIdException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyRegistrationsForTargetIdException
  = Core._MatchServiceError mkServiceConfig
      "TooManyRegistrationsForTargetId"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyRegistrationsForTargetIdException #-}
{-# DEPRECATED _TooManyRegistrationsForTargetIdException "Use generic-lens or generic-optics instead"  #-}

-- | You've reached the limit on the number of listeners per load balancer.
_TooManyListenersException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyListenersException
  = Core._MatchServiceError mkServiceConfig "TooManyListeners" Core..
      Core.hasStatues 400
{-# INLINEABLE _TooManyListenersException #-}
{-# DEPRECATED _TooManyListenersException "Use generic-lens or generic-optics instead"  #-}

-- | You've reached the limit on the number of load balancers per target group.
_TargetGroupAssociationLimitException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TargetGroupAssociationLimitException
  = Core._MatchServiceError mkServiceConfig
      "TargetGroupAssociationLimit"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TargetGroupAssociationLimitException #-}
{-# DEPRECATED _TargetGroupAssociationLimitException "Use generic-lens or generic-optics instead"  #-}

-- | This operation is not allowed.
_OperationNotPermittedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OperationNotPermittedException
  = Core._MatchServiceError mkServiceConfig "OperationNotPermitted"
      Core.. Core.hasStatues 400
{-# INLINEABLE _OperationNotPermittedException #-}
{-# DEPRECATED _OperationNotPermittedException "Use generic-lens or generic-optics instead"  #-}

-- | The specified SSL policy does not exist.
_SSLPolicyNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SSLPolicyNotFoundException
  = Core._MatchServiceError mkServiceConfig "SSLPolicyNotFound"
      Core.. Core.hasStatues 400
{-# INLINEABLE _SSLPolicyNotFoundException #-}
{-# DEPRECATED _SSLPolicyNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The requested scheme is not valid.
_InvalidSchemeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSchemeException
  = Core._MatchServiceError mkServiceConfig "InvalidScheme" Core..
      Core.hasStatues 400
{-# INLINEABLE _InvalidSchemeException #-}
{-# DEPRECATED _InvalidSchemeException "Use generic-lens or generic-optics instead"  #-}

-- | The specified Availability Zone is not supported.
_AvailabilityZoneNotSupportedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AvailabilityZoneNotSupportedException
  = Core._MatchServiceError mkServiceConfig
      "AvailabilityZoneNotSupported"
      Core.. Core.hasStatues 400
{-# INLINEABLE _AvailabilityZoneNotSupportedException #-}
{-# DEPRECATED _AvailabilityZoneNotSupportedException "Use generic-lens or generic-optics instead"  #-}

-- | You've reached the limit on the number of unique target groups per load balancer across all listeners. If a target group is used by multiple actions for a load balancer, it is counted as only one use.
_TooManyUniqueTargetGroupsPerLoadBalancerException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyUniqueTargetGroupsPerLoadBalancerException
  = Core._MatchServiceError mkServiceConfig
      "TooManyUniqueTargetGroupsPerLoadBalancer"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyUniqueTargetGroupsPerLoadBalancerException #-}
{-# DEPRECATED _TooManyUniqueTargetGroupsPerLoadBalancerException "Use generic-lens or generic-optics instead"  #-}

-- | The specified load balancer does not exist.
_LoadBalancerNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LoadBalancerNotFoundException
  = Core._MatchServiceError mkServiceConfig "LoadBalancerNotFound"
      Core.. Core.hasStatues 400
{-# INLINEABLE _LoadBalancerNotFoundException #-}
{-# DEPRECATED _LoadBalancerNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | A specified resource is in use.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException
  = Core._MatchServiceError mkServiceConfig "ResourceInUse" Core..
      Core.hasStatues 400
{-# INLINEABLE _ResourceInUseException #-}
{-# DEPRECATED _ResourceInUseException "Use generic-lens or generic-optics instead"  #-}

-- | The specified certificate does not exist.
_CertificateNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CertificateNotFoundException
  = Core._MatchServiceError mkServiceConfig "CertificateNotFound"
      Core.. Core.hasStatues 400
{-# INLINEABLE _CertificateNotFoundException #-}
{-# DEPRECATED _CertificateNotFoundException "Use generic-lens or generic-optics instead"  #-}
