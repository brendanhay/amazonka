-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _WAFInvalidAccountException,
    _WAFSubscriptionNotFoundException,
    _WAFReferencedItemException,
    _WAFTagOperationException,
    _WAFEntityMigrationException,
    _WAFInvalidRegexPatternException,
    _WAFInvalidOperationException,
    _WAFBadRequestException,
    _WAFNonexistentItemException,
    _WAFInvalidParameterException,
    _WAFTagOperationInternalErrorException,
    _WAFServiceLinkedRoleErrorException,
    _WAFLimitsExceededException,
    _WAFInvalidPermissionPolicyException,
    _WAFStaleDataException,
    _WAFInternalErrorException,
    _WAFNonexistentContainerException,
    _WAFDisallowedNameException,
    _WAFNonEmptyEntityException,

    -- * IPString
    IPString (..),

    -- * RegexMatchSet
    RegexMatchSet (..),
    mkRegexMatchSet,
    rmsName,
    rmsRegexMatchSetId,
    rmsRegexMatchTuples,

    -- * ExcludedRule
    ExcludedRule (..),
    mkExcludedRule,
    erRuleId,

    -- * SqlInjectionMatchSetSummary
    SqlInjectionMatchSetSummary (..),
    mkSqlInjectionMatchSetSummary,
    simssSqlInjectionMatchSetId,
    simssName,

    -- * HeaderValue
    HeaderValue (..),

    -- * RuleUpdate
    RuleUpdate (..),
    mkRuleUpdate,
    ruAction,
    ruPredicate,

    -- * WebACLUpdate
    WebACLUpdate (..),
    mkWebACLUpdate,
    wacluAction,
    wacluActivatedRule,

    -- * RegexPatternSet
    RegexPatternSet (..),
    mkRegexPatternSet,
    rpsRegexPatternSetId,
    rpsRegexPatternStrings,
    rpsName,

    -- * HTTPHeader
    HTTPHeader (..),
    mkHTTPHeader,
    httphName,
    httphValue,

    -- * IPSetSummary
    IPSetSummary (..),
    mkIPSetSummary,
    ipssIPSetId,
    ipssName,

    -- * ResourceId
    ResourceId (..),

    -- * HTTPMethod
    HTTPMethod (..),

    -- * HTTPVersion
    HTTPVersion (..),

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * WafOverrideActionType
    WafOverrideActionType (..),

    -- * ManagedKey
    ManagedKey (..),

    -- * SqlInjectionMatchTuple
    SqlInjectionMatchTuple (..),
    mkSqlInjectionMatchTuple,
    simtFieldToMatch,
    simtTextTransformation,

    -- * IPSetUpdate
    IPSetUpdate (..),
    mkIPSetUpdate,
    ipsuAction,
    ipsuIPSetDescriptor,

    -- * FieldToMatch
    FieldToMatch (..),
    mkFieldToMatch,
    ftmType,
    ftmData,

    -- * GeoMatchSetUpdate
    GeoMatchSetUpdate (..),
    mkGeoMatchSetUpdate,
    gmsuAction,
    gmsuGeoMatchConstraint,

    -- * RateKey
    RateKey (..),

    -- * TagInfoForResource
    TagInfoForResource (..),
    mkTagInfoForResource,
    tifrResourceARN,
    tifrTagList,

    -- * ResourceName
    ResourceName (..),

    -- * RateBasedRule
    RateBasedRule (..),
    mkRateBasedRule,
    rbrRuleId,
    rbrMatchPredicates,
    rbrRateKey,
    rbrRateLimit,
    rbrMetricName,
    rbrName,

    -- * RuleGroupSummary
    RuleGroupSummary (..),
    mkRuleGroupSummary,
    rgsRuleGroupId,
    rgsName,

    -- * ChangeAction
    ChangeAction (..),

    -- * RegexMatchSetSummary
    RegexMatchSetSummary (..),
    mkRegexMatchSetSummary,
    rmssRegexMatchSetId,
    rmssName,

    -- * SqlInjectionMatchSet
    SqlInjectionMatchSet (..),
    mkSqlInjectionMatchSet,
    simsSqlInjectionMatchSetId,
    simsSqlInjectionMatchTuples,
    simsName,

    -- * Country
    Country (..),

    -- * IPSetDescriptor
    IPSetDescriptor (..),
    mkIPSetDescriptor,
    ipsdType,
    ipsdValue,

    -- * RuleGroupUpdate
    RuleGroupUpdate (..),
    mkRuleGroupUpdate,
    rguAction,
    rguActivatedRule,

    -- * MetricName
    MetricName (..),

    -- * ByteMatchSet
    ByteMatchSet (..),
    mkByteMatchSet,
    bmsByteMatchSetId,
    bmsByteMatchTuples,
    bmsName,

    -- * SizeConstraintSetUpdate
    SizeConstraintSetUpdate (..),
    mkSizeConstraintSetUpdate,
    scsuAction,
    scsuSizeConstraint,

    -- * XssMatchTuple
    XssMatchTuple (..),
    mkXssMatchTuple,
    xmtFieldToMatch,
    xmtTextTransformation,

    -- * GeoMatchConstraint
    GeoMatchConstraint (..),
    mkGeoMatchConstraint,
    gmcType,
    gmcValue,

    -- * Action
    Action (..),

    -- * WafActionType
    WafActionType (..),

    -- * S3ObjectUrl
    S3ObjectUrl (..),

    -- * SizeConstraintSetSummary
    SizeConstraintSetSummary (..),
    mkSizeConstraintSetSummary,
    scssSizeConstraintSetId,
    scssName,

    -- * Rule
    Rule (..),
    mkRule,
    rRuleId,
    rPredicates,
    rMetricName,
    rName,

    -- * RegexPatternSetUpdate
    RegexPatternSetUpdate (..),
    mkRegexPatternSetUpdate,
    rpsuAction,
    rpsuRegexPatternString,

    -- * WafRuleType
    WafRuleType (..),

    -- * WebACL
    WebACL (..),
    mkWebACL,
    waclWebACLId,
    waclDefaultAction,
    waclRules,
    waclMetricName,
    waclName,
    waclWebACLArn,

    -- * HTTPRequest
    HTTPRequest (..),
    mkHTTPRequest,
    httprClientIP,
    httprCountry,
    httprHTTPVersion,
    httprHeaders,
    httprMethod,
    httprURI,

    -- * ComparisonOperator
    ComparisonOperator (..),

    -- * SubscribedRuleGroupSummary
    SubscribedRuleGroupSummary (..),
    mkSubscribedRuleGroupSummary,
    srgsRuleGroupId,
    srgsName,
    srgsMetricName,

    -- * Predicate
    Predicate (..),
    mkPredicate,
    pNegated,
    pType,
    pDataId,

    -- * XssMatchSet
    XssMatchSet (..),
    mkXssMatchSet,
    xmsXssMatchSetId,
    xmsXssMatchTuples,
    xmsName,

    -- * ByteMatchTuple
    ByteMatchTuple (..),
    mkByteMatchTuple,
    bmtFieldToMatch,
    bmtTargetString,
    bmtTextTransformation,
    bmtPositionalConstraint,

    -- * TimeWindow
    TimeWindow (..),
    mkTimeWindow,
    twStartTime,
    twEndTime,

    -- * TextTransformation
    TextTransformation (..),

    -- * GeoMatchSet
    GeoMatchSet (..),
    mkGeoMatchSet,
    gmsGeoMatchSetId,
    gmsGeoMatchConstraints,
    gmsName,

    -- * ByteMatchSetSummary
    ByteMatchSetSummary (..),
    mkByteMatchSetSummary,
    bmssByteMatchSetId,
    bmssName,

    -- * ResourceArn
    ResourceArn (..),

    -- * SizeConstraintSet
    SizeConstraintSet (..),
    mkSizeConstraintSet,
    scsSizeConstraintSetId,
    scsSizeConstraints,
    scsName,

    -- * WebACLSummary
    WebACLSummary (..),
    mkWebACLSummary,
    waclsWebACLId,
    waclsName,

    -- * ByteMatchSetUpdate
    ByteMatchSetUpdate (..),
    mkByteMatchSetUpdate,
    bmsuAction,
    bmsuByteMatchTuple,

    -- * WafOverrideAction
    WafOverrideAction (..),
    mkWafOverrideAction,
    woaType,

    -- * RuleSummary
    RuleSummary (..),
    mkRuleSummary,
    rsRuleId,
    rsName,

    -- * NextMarker
    NextMarker (..),

    -- * RegexMatchSetUpdate
    RegexMatchSetUpdate (..),
    mkRegexMatchSetUpdate,
    rmsuAction,
    rmsuRegexMatchTuple,

    -- * XssMatchSetSummary
    XssMatchSetSummary (..),
    mkXssMatchSetSummary,
    xmssXssMatchSetId,
    xmssName,

    -- * ChangeToken
    ChangeToken (..),

    -- * TagKey
    TagKey (..),

    -- * XssMatchSetUpdate
    XssMatchSetUpdate (..),
    mkXssMatchSetUpdate,
    xmsuAction,
    xmsuXssMatchTuple,

    -- * SizeConstraint
    SizeConstraint (..),
    mkSizeConstraint,
    scFieldToMatch,
    scTextTransformation,
    scComparisonOperator,
    scSize,

    -- * RegexPatternString
    RegexPatternString (..),

    -- * IPSetDescriptorType
    IPSetDescriptorType (..),

    -- * GeoMatchConstraintValue
    GeoMatchConstraintValue (..),

    -- * GeoMatchSetSummary
    GeoMatchSetSummary (..),
    mkGeoMatchSetSummary,
    gmssGeoMatchSetId,
    gmssName,

    -- * ActivatedRule
    ActivatedRule (..),
    mkActivatedRule,
    arPriority,
    arRuleId,
    arAction,
    arExcludedRules,
    arOverrideAction,
    arType,

    -- * LoggingConfiguration
    LoggingConfiguration (..),
    mkLoggingConfiguration,
    lcResourceArn,
    lcLogDestinationConfigs,
    lcRedactedFields,

    -- * SampledHTTPRequest
    SampledHTTPRequest (..),
    mkSampledHTTPRequest,
    shttprRequest,
    shttprWeight,
    shttprAction,
    shttprRuleWithinRuleGroup,
    shttprTimestamp,

    -- * RegexPatternSetSummary
    RegexPatternSetSummary (..),
    mkRegexPatternSetSummary,
    rpssRegexPatternSetId,
    rpssName,

    -- * GeoMatchConstraintType
    GeoMatchConstraintType (..),

    -- * IPSet
    IPSet (..),
    mkIPSet,
    ipsIPSetId,
    ipsIPSetDescriptors,
    ipsName,

    -- * RegexMatchTuple
    RegexMatchTuple (..),
    mkRegexMatchTuple,
    rmtFieldToMatch,
    rmtTextTransformation,
    rmtRegexPatternSetId,

    -- * WafAction
    WafAction (..),
    mkWafAction,
    waType,

    -- * S3BucketName
    S3BucketName (..),

    -- * PositionalConstraint
    PositionalConstraint (..),

    -- * SqlInjectionMatchSetUpdate
    SqlInjectionMatchSetUpdate (..),
    mkSqlInjectionMatchSetUpdate,
    simsuAction,
    simsuSqlInjectionMatchTuple,

    -- * PredicateType
    PredicateType (..),

    -- * ChangeTokenStatus
    ChangeTokenStatus (..),

    -- * RuleGroup
    RuleGroup (..),
    mkRuleGroup,
    rgRuleGroupId,
    rgMetricName,
    rgName,

    -- * MatchFieldType
    MatchFieldType (..),

    -- * Name
    Name (..),

    -- * RegexMatchSetId
    RegexMatchSetId (..),

    -- * RuleId
    RuleId (..),

    -- * IPSetId
    IPSetId (..),

    -- * SqlInjectionMatchSetId
    SqlInjectionMatchSetId (..),

    -- * WebAclId
    WebAclId (..),

    -- * RegexPatternSetId
    RegexPatternSetId (..),

    -- * Key
    Key (..),

    -- * Value
    Value (..),

    -- * Data
    Data (..),

    -- * ResourceARN
    ResourceARN (..),

    -- * Policy
    Policy (..),

    -- * WebACLArn
    WebACLArn (..),

    -- * URI
    URI (..),
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.WAF.Types.Action
import Network.AWS.WAF.Types.ActivatedRule
import Network.AWS.WAF.Types.ByteMatchSet
import Network.AWS.WAF.Types.ByteMatchSetSummary
import Network.AWS.WAF.Types.ByteMatchSetUpdate
import Network.AWS.WAF.Types.ByteMatchTuple
import Network.AWS.WAF.Types.ChangeAction
import Network.AWS.WAF.Types.ChangeToken
import Network.AWS.WAF.Types.ChangeTokenStatus
import Network.AWS.WAF.Types.ComparisonOperator
import Network.AWS.WAF.Types.Country
import Network.AWS.WAF.Types.Data
import Network.AWS.WAF.Types.ExcludedRule
import Network.AWS.WAF.Types.FieldToMatch
import Network.AWS.WAF.Types.GeoMatchConstraint
import Network.AWS.WAF.Types.GeoMatchConstraintType
import Network.AWS.WAF.Types.GeoMatchConstraintValue
import Network.AWS.WAF.Types.GeoMatchSet
import Network.AWS.WAF.Types.GeoMatchSetSummary
import Network.AWS.WAF.Types.GeoMatchSetUpdate
import Network.AWS.WAF.Types.HTTPHeader
import Network.AWS.WAF.Types.HTTPMethod
import Network.AWS.WAF.Types.HTTPRequest
import Network.AWS.WAF.Types.HTTPVersion
import Network.AWS.WAF.Types.HeaderValue
import Network.AWS.WAF.Types.IPSet
import Network.AWS.WAF.Types.IPSetDescriptor
import Network.AWS.WAF.Types.IPSetDescriptorType
import Network.AWS.WAF.Types.IPSetId
import Network.AWS.WAF.Types.IPSetSummary
import Network.AWS.WAF.Types.IPSetUpdate
import Network.AWS.WAF.Types.IPString
import Network.AWS.WAF.Types.Key
import Network.AWS.WAF.Types.LoggingConfiguration
import Network.AWS.WAF.Types.ManagedKey
import Network.AWS.WAF.Types.MatchFieldType
import Network.AWS.WAF.Types.MetricName
import Network.AWS.WAF.Types.Name
import Network.AWS.WAF.Types.NextMarker
import Network.AWS.WAF.Types.Policy
import Network.AWS.WAF.Types.PositionalConstraint
import Network.AWS.WAF.Types.Predicate
import Network.AWS.WAF.Types.PredicateType
import Network.AWS.WAF.Types.RateBasedRule
import Network.AWS.WAF.Types.RateKey
import Network.AWS.WAF.Types.RegexMatchSet
import Network.AWS.WAF.Types.RegexMatchSetId
import Network.AWS.WAF.Types.RegexMatchSetSummary
import Network.AWS.WAF.Types.RegexMatchSetUpdate
import Network.AWS.WAF.Types.RegexMatchTuple
import Network.AWS.WAF.Types.RegexPatternSet
import Network.AWS.WAF.Types.RegexPatternSetId
import Network.AWS.WAF.Types.RegexPatternSetSummary
import Network.AWS.WAF.Types.RegexPatternSetUpdate
import Network.AWS.WAF.Types.RegexPatternString
import Network.AWS.WAF.Types.ResourceARN
import Network.AWS.WAF.Types.ResourceArn
import Network.AWS.WAF.Types.ResourceId
import Network.AWS.WAF.Types.ResourceName
import Network.AWS.WAF.Types.Rule
import Network.AWS.WAF.Types.RuleGroup
import Network.AWS.WAF.Types.RuleGroupSummary
import Network.AWS.WAF.Types.RuleGroupUpdate
import Network.AWS.WAF.Types.RuleId
import Network.AWS.WAF.Types.RuleSummary
import Network.AWS.WAF.Types.RuleUpdate
import Network.AWS.WAF.Types.S3BucketName
import Network.AWS.WAF.Types.S3ObjectUrl
import Network.AWS.WAF.Types.SampledHTTPRequest
import Network.AWS.WAF.Types.SizeConstraint
import Network.AWS.WAF.Types.SizeConstraintSet
import Network.AWS.WAF.Types.SizeConstraintSetSummary
import Network.AWS.WAF.Types.SizeConstraintSetUpdate
import Network.AWS.WAF.Types.SqlInjectionMatchSet
import Network.AWS.WAF.Types.SqlInjectionMatchSetId
import Network.AWS.WAF.Types.SqlInjectionMatchSetSummary
import Network.AWS.WAF.Types.SqlInjectionMatchSetUpdate
import Network.AWS.WAF.Types.SqlInjectionMatchTuple
import Network.AWS.WAF.Types.SubscribedRuleGroupSummary
import Network.AWS.WAF.Types.Tag
import Network.AWS.WAF.Types.TagInfoForResource
import Network.AWS.WAF.Types.TagKey
import Network.AWS.WAF.Types.TextTransformation
import Network.AWS.WAF.Types.TimeWindow
import Network.AWS.WAF.Types.URI
import Network.AWS.WAF.Types.Value
import Network.AWS.WAF.Types.WafAction
import Network.AWS.WAF.Types.WafActionType
import Network.AWS.WAF.Types.WafOverrideAction
import Network.AWS.WAF.Types.WafOverrideActionType
import Network.AWS.WAF.Types.WafRuleType
import Network.AWS.WAF.Types.WebACL
import Network.AWS.WAF.Types.WebACLArn
import Network.AWS.WAF.Types.WebACLSummary
import Network.AWS.WAF.Types.WebACLUpdate
import Network.AWS.WAF.Types.WebAclId
import Network.AWS.WAF.Types.XssMatchSet
import Network.AWS.WAF.Types.XssMatchSetSummary
import Network.AWS.WAF.Types.XssMatchSetUpdate
import Network.AWS.WAF.Types.XssMatchTuple

-- | API version @2015-08-24@ of the Amazon WAF SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "WAF",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "waf",
      Core._svcVersion = "2015-08-24",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "WAF",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | The operation failed because you tried to create, update, or delete an object by using an invalid account identifier.
_WAFInvalidAccountException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFInvalidAccountException =
  Core._MatchServiceError
    mkServiceConfig
    "WAFInvalidAccountException"
{-# DEPRECATED _WAFInvalidAccountException "Use generic-lens or generic-optics instead." #-}

-- | The specified subscription does not exist.
_WAFSubscriptionNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFSubscriptionNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "WAFSubscriptionNotFoundException"
{-# DEPRECATED _WAFSubscriptionNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The operation failed because you tried to delete an object that is still in use. For example:
--
--
--     * You tried to delete a @ByteMatchSet@ that is still referenced by a @Rule@ .
--
--
--     * You tried to delete a @Rule@ that is still referenced by a @WebACL@ .
_WAFReferencedItemException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFReferencedItemException =
  Core._MatchServiceError
    mkServiceConfig
    "WAFReferencedItemException"
{-# DEPRECATED _WAFReferencedItemException "Use generic-lens or generic-optics instead." #-}

-- |
_WAFTagOperationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFTagOperationException =
  Core._MatchServiceError
    mkServiceConfig
    "WAFTagOperationException"
{-# DEPRECATED _WAFTagOperationException "Use generic-lens or generic-optics instead." #-}

-- | The operation failed due to a problem with the migration. The failure cause is provided in the exception, in the @MigrationErrorType@ :
--
--
--     * @ENTITY_NOT_SUPPORTED@ - The web ACL has an unsupported entity but the @IgnoreUnsupportedType@ is not set to true.
--
--
--     * @ENTITY_NOT_FOUND@ - The web ACL doesn't exist.
--
--
--     * @S3_BUCKET_NO_PERMISSION@ - You don't have permission to perform the @PutObject@ action to the specified Amazon S3 bucket.
--
--
--     * @S3_BUCKET_NOT_ACCESSIBLE@ - The bucket policy doesn't allow AWS WAF to perform the @PutObject@ action in the bucket.
--
--
--     * @S3_BUCKET_NOT_FOUND@ - The S3 bucket doesn't exist.
--
--
--     * @S3_BUCKET_INVALID_REGION@ - The S3 bucket is not in the same Region as the web ACL.
--
--
--     * @S3_INTERNAL_ERROR@ - AWS WAF failed to create the template in the S3 bucket for another reason.
_WAFEntityMigrationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFEntityMigrationException =
  Core._MatchServiceError
    mkServiceConfig
    "WAFEntityMigrationException"
{-# DEPRECATED _WAFEntityMigrationException "Use generic-lens or generic-optics instead." #-}

-- | The regular expression (regex) you specified in @RegexPatternString@ is invalid.
_WAFInvalidRegexPatternException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFInvalidRegexPatternException =
  Core._MatchServiceError
    mkServiceConfig
    "WAFInvalidRegexPatternException"
{-# DEPRECATED _WAFInvalidRegexPatternException "Use generic-lens or generic-optics instead." #-}

-- | The operation failed because there was nothing to do. For example:
--
--
--     * You tried to remove a @Rule@ from a @WebACL@ , but the @Rule@ isn't in the specified @WebACL@ .
--
--
--     * You tried to remove an IP address from an @IPSet@ , but the IP address isn't in the specified @IPSet@ .
--
--
--     * You tried to remove a @ByteMatchTuple@ from a @ByteMatchSet@ , but the @ByteMatchTuple@ isn't in the specified @WebACL@ .
--
--
--     * You tried to add a @Rule@ to a @WebACL@ , but the @Rule@ already exists in the specified @WebACL@ .
--
--
--     * You tried to add a @ByteMatchTuple@ to a @ByteMatchSet@ , but the @ByteMatchTuple@ already exists in the specified @WebACL@ .
_WAFInvalidOperationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFInvalidOperationException =
  Core._MatchServiceError
    mkServiceConfig
    "WAFInvalidOperationException"
{-# DEPRECATED _WAFInvalidOperationException "Use generic-lens or generic-optics instead." #-}

-- |
_WAFBadRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFBadRequestException =
  Core._MatchServiceError mkServiceConfig "WAFBadRequestException"
{-# DEPRECATED _WAFBadRequestException "Use generic-lens or generic-optics instead." #-}

-- | The operation failed because the referenced object doesn't exist.
_WAFNonexistentItemException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFNonexistentItemException =
  Core._MatchServiceError
    mkServiceConfig
    "WAFNonexistentItemException"
{-# DEPRECATED _WAFNonexistentItemException "Use generic-lens or generic-optics instead." #-}

-- | The operation failed because AWS WAF didn't recognize a parameter in the request. For example:
--
--
--     * You specified an invalid parameter name.
--
--
--     * You specified an invalid value.
--
--
--     * You tried to update an object (@ByteMatchSet@ , @IPSet@ , @Rule@ , or @WebACL@ ) using an action other than @INSERT@ or @DELETE@ .
--
--
--     * You tried to create a @WebACL@ with a @DefaultAction@ @Type@ other than @ALLOW@ , @BLOCK@ , or @COUNT@ .
--
--
--     * You tried to create a @RateBasedRule@ with a @RateKey@ value other than @IP@ .
--
--
--     * You tried to update a @WebACL@ with a @WafAction@ @Type@ other than @ALLOW@ , @BLOCK@ , or @COUNT@ .
--
--
--     * You tried to update a @ByteMatchSet@ with a @FieldToMatch@ @Type@ other than HEADER, METHOD, QUERY_STRING, URI, or BODY.
--
--
--     * You tried to update a @ByteMatchSet@ with a @Field@ of @HEADER@ but no value for @Data@ .
--
--
--     * Your request references an ARN that is malformed, or corresponds to a resource with which a web ACL cannot be associated.
_WAFInvalidParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFInvalidParameterException =
  Core._MatchServiceError
    mkServiceConfig
    "WAFInvalidParameterException"
{-# DEPRECATED _WAFInvalidParameterException "Use generic-lens or generic-optics instead." #-}

-- |
_WAFTagOperationInternalErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFTagOperationInternalErrorException =
  Core._MatchServiceError
    mkServiceConfig
    "WAFTagOperationInternalErrorException"
{-# DEPRECATED _WAFTagOperationInternalErrorException "Use generic-lens or generic-optics instead." #-}

-- | AWS WAF is not able to access the service linked role. This can be caused by a previous @PutLoggingConfiguration@ request, which can lock the service linked role for about 20 seconds. Please try your request again. The service linked role can also be locked by a previous @DeleteServiceLinkedRole@ request, which can lock the role for 15 minutes or more. If you recently made a @DeleteServiceLinkedRole@ , wait at least 15 minutes and try the request again. If you receive this same exception again, you will have to wait additional time until the role is unlocked.
_WAFServiceLinkedRoleErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFServiceLinkedRoleErrorException =
  Core._MatchServiceError
    mkServiceConfig
    "WAFServiceLinkedRoleErrorException"
{-# DEPRECATED _WAFServiceLinkedRoleErrorException "Use generic-lens or generic-optics instead." #-}

-- | The operation exceeds a resource limit, for example, the maximum number of @WebACL@ objects that you can create for an AWS account. For more information, see <https://docs.aws.amazon.com/waf/latest/developerguide/limits.html Limits> in the /AWS WAF Developer Guide/ .
_WAFLimitsExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFLimitsExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "WAFLimitsExceededException"
{-# DEPRECATED _WAFLimitsExceededException "Use generic-lens or generic-optics instead." #-}

-- | The operation failed because the specified policy is not in the proper format.
--
-- The policy is subject to the following restrictions:
--
--     * You can attach only one policy with each @PutPermissionPolicy@ request.
--
--
--     * The policy must include an @Effect@ , @Action@ and @Principal@ .
--
--
--     * @Effect@ must specify @Allow@ .
--
--
--     * The @Action@ in the policy must be @waf:UpdateWebACL@ , @waf-regional:UpdateWebACL@ , @waf:GetRuleGroup@ and @waf-regional:GetRuleGroup@ . Any extra or wildcard actions in the policy will be rejected.
--
--
--     * The policy cannot include a @Resource@ parameter.
--
--
--     * The ARN in the request must be a valid WAF RuleGroup ARN and the RuleGroup must exist in the same region.
--
--
--     * The user making the request must be the owner of the RuleGroup.
--
--
--     * Your policy must be composed using IAM Policy version 2012-10-17.
_WAFInvalidPermissionPolicyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFInvalidPermissionPolicyException =
  Core._MatchServiceError
    mkServiceConfig
    "WAFInvalidPermissionPolicyException"
{-# DEPRECATED _WAFInvalidPermissionPolicyException "Use generic-lens or generic-optics instead." #-}

-- | The operation failed because you tried to create, update, or delete an object by using a change token that has already been used.
_WAFStaleDataException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFStaleDataException =
  Core._MatchServiceError mkServiceConfig "WAFStaleDataException"
{-# DEPRECATED _WAFStaleDataException "Use generic-lens or generic-optics instead." #-}

-- | The operation failed because of a system problem, even though the request was valid. Retry your request.
_WAFInternalErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFInternalErrorException =
  Core._MatchServiceError
    mkServiceConfig
    "WAFInternalErrorException"
{-# DEPRECATED _WAFInternalErrorException "Use generic-lens or generic-optics instead." #-}

-- | The operation failed because you tried to add an object to or delete an object from another object that doesn't exist. For example:
--
--
--     * You tried to add a @Rule@ to or delete a @Rule@ from a @WebACL@ that doesn't exist.
--
--
--     * You tried to add a @ByteMatchSet@ to or delete a @ByteMatchSet@ from a @Rule@ that doesn't exist.
--
--
--     * You tried to add an IP address to or delete an IP address from an @IPSet@ that doesn't exist.
--
--
--     * You tried to add a @ByteMatchTuple@ to or delete a @ByteMatchTuple@ from a @ByteMatchSet@ that doesn't exist.
_WAFNonexistentContainerException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFNonexistentContainerException =
  Core._MatchServiceError
    mkServiceConfig
    "WAFNonexistentContainerException"
{-# DEPRECATED _WAFNonexistentContainerException "Use generic-lens or generic-optics instead." #-}

-- | The name specified is invalid.
_WAFDisallowedNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFDisallowedNameException =
  Core._MatchServiceError
    mkServiceConfig
    "WAFDisallowedNameException"
{-# DEPRECATED _WAFDisallowedNameException "Use generic-lens or generic-optics instead." #-}

-- | The operation failed because you tried to delete an object that isn't empty. For example:
--
--
--     * You tried to delete a @WebACL@ that still contains one or more @Rule@ objects.
--
--
--     * You tried to delete a @Rule@ that still contains one or more @ByteMatchSet@ objects or other predicates.
--
--
--     * You tried to delete a @ByteMatchSet@ that contains one or more @ByteMatchTuple@ objects.
--
--
--     * You tried to delete an @IPSet@ that references one or more IP addresses.
_WAFNonEmptyEntityException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFNonEmptyEntityException =
  Core._MatchServiceError
    mkServiceConfig
    "WAFNonEmptyEntityException"
{-# DEPRECATED _WAFNonEmptyEntityException "Use generic-lens or generic-optics instead." #-}
