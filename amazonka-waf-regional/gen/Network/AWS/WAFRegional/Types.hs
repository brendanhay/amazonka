{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _WAFTagOperationInternalErrorException,
    _WAFInvalidAccountException,
    _WAFSubscriptionNotFoundException,
    _WAFBadRequestException,
    _WAFDisallowedNameException,
    _WAFNonEmptyEntityException,
    _WAFInvalidOperationException,
    _WAFStaleDataException,
    _WAFTagOperationException,
    _WAFInternalErrorException,
    _WAFServiceLinkedRoleErrorException,
    _WAFInvalidParameterException,
    _WAFNonexistentItemException,
    _WAFUnavailableEntityException,
    _WAFInvalidRegexPatternException,
    _WAFNonexistentContainerException,
    _WAFEntityMigrationException,
    _WAFReferencedItemException,
    _WAFInvalidPermissionPolicyException,
    _WAFLimitsExceededException,

    -- * ChangeAction
    ChangeAction (..),

    -- * ChangeTokenStatus
    ChangeTokenStatus (..),

    -- * ComparisonOperator
    ComparisonOperator (..),

    -- * GeoMatchConstraintType
    GeoMatchConstraintType (..),

    -- * GeoMatchConstraintValue
    GeoMatchConstraintValue (..),

    -- * IPSetDescriptorType
    IPSetDescriptorType (..),

    -- * MatchFieldType
    MatchFieldType (..),

    -- * PositionalConstraint
    PositionalConstraint (..),

    -- * PredicateType
    PredicateType (..),

    -- * RateKey
    RateKey (..),

    -- * ResourceType
    ResourceType (..),

    -- * TextTransformation
    TextTransformation (..),

    -- * WafActionType
    WafActionType (..),

    -- * WafOverrideActionType
    WafOverrideActionType (..),

    -- * WafRuleType
    WafRuleType (..),

    -- * ActivatedRule
    ActivatedRule (..),
    newActivatedRule,
    activatedRule_excludedRules,
    activatedRule_overrideAction,
    activatedRule_action,
    activatedRule_type,
    activatedRule_priority,
    activatedRule_ruleId,

    -- * ByteMatchSet
    ByteMatchSet (..),
    newByteMatchSet,
    byteMatchSet_name,
    byteMatchSet_byteMatchSetId,
    byteMatchSet_byteMatchTuples,

    -- * ByteMatchSetSummary
    ByteMatchSetSummary (..),
    newByteMatchSetSummary,
    byteMatchSetSummary_byteMatchSetId,
    byteMatchSetSummary_name,

    -- * ByteMatchSetUpdate
    ByteMatchSetUpdate (..),
    newByteMatchSetUpdate,
    byteMatchSetUpdate_action,
    byteMatchSetUpdate_byteMatchTuple,

    -- * ByteMatchTuple
    ByteMatchTuple (..),
    newByteMatchTuple,
    byteMatchTuple_fieldToMatch,
    byteMatchTuple_targetString,
    byteMatchTuple_textTransformation,
    byteMatchTuple_positionalConstraint,

    -- * ExcludedRule
    ExcludedRule (..),
    newExcludedRule,
    excludedRule_ruleId,

    -- * FieldToMatch
    FieldToMatch (..),
    newFieldToMatch,
    fieldToMatch_data,
    fieldToMatch_type,

    -- * GeoMatchConstraint
    GeoMatchConstraint (..),
    newGeoMatchConstraint,
    geoMatchConstraint_type,
    geoMatchConstraint_value,

    -- * GeoMatchSet
    GeoMatchSet (..),
    newGeoMatchSet,
    geoMatchSet_name,
    geoMatchSet_geoMatchSetId,
    geoMatchSet_geoMatchConstraints,

    -- * GeoMatchSetSummary
    GeoMatchSetSummary (..),
    newGeoMatchSetSummary,
    geoMatchSetSummary_geoMatchSetId,
    geoMatchSetSummary_name,

    -- * GeoMatchSetUpdate
    GeoMatchSetUpdate (..),
    newGeoMatchSetUpdate,
    geoMatchSetUpdate_action,
    geoMatchSetUpdate_geoMatchConstraint,

    -- * HTTPHeader
    HTTPHeader (..),
    newHTTPHeader,
    hTTPHeader_name,
    hTTPHeader_value,

    -- * HTTPRequest
    HTTPRequest (..),
    newHTTPRequest,
    hTTPRequest_headers,
    hTTPRequest_uri,
    hTTPRequest_method,
    hTTPRequest_clientIP,
    hTTPRequest_country,
    hTTPRequest_hTTPVersion,

    -- * IPSet
    IPSet (..),
    newIPSet,
    iPSet_name,
    iPSet_iPSetId,
    iPSet_iPSetDescriptors,

    -- * IPSetDescriptor
    IPSetDescriptor (..),
    newIPSetDescriptor,
    iPSetDescriptor_type,
    iPSetDescriptor_value,

    -- * IPSetSummary
    IPSetSummary (..),
    newIPSetSummary,
    iPSetSummary_iPSetId,
    iPSetSummary_name,

    -- * IPSetUpdate
    IPSetUpdate (..),
    newIPSetUpdate,
    iPSetUpdate_action,
    iPSetUpdate_iPSetDescriptor,

    -- * LoggingConfiguration
    LoggingConfiguration (..),
    newLoggingConfiguration,
    loggingConfiguration_redactedFields,
    loggingConfiguration_resourceArn,
    loggingConfiguration_logDestinationConfigs,

    -- * Predicate
    Predicate (..),
    newPredicate,
    predicate_negated,
    predicate_type,
    predicate_dataId,

    -- * RateBasedRule
    RateBasedRule (..),
    newRateBasedRule,
    rateBasedRule_metricName,
    rateBasedRule_name,
    rateBasedRule_ruleId,
    rateBasedRule_matchPredicates,
    rateBasedRule_rateKey,
    rateBasedRule_rateLimit,

    -- * RegexMatchSet
    RegexMatchSet (..),
    newRegexMatchSet,
    regexMatchSet_regexMatchSetId,
    regexMatchSet_regexMatchTuples,
    regexMatchSet_name,

    -- * RegexMatchSetSummary
    RegexMatchSetSummary (..),
    newRegexMatchSetSummary,
    regexMatchSetSummary_regexMatchSetId,
    regexMatchSetSummary_name,

    -- * RegexMatchSetUpdate
    RegexMatchSetUpdate (..),
    newRegexMatchSetUpdate,
    regexMatchSetUpdate_action,
    regexMatchSetUpdate_regexMatchTuple,

    -- * RegexMatchTuple
    RegexMatchTuple (..),
    newRegexMatchTuple,
    regexMatchTuple_fieldToMatch,
    regexMatchTuple_textTransformation,
    regexMatchTuple_regexPatternSetId,

    -- * RegexPatternSet
    RegexPatternSet (..),
    newRegexPatternSet,
    regexPatternSet_name,
    regexPatternSet_regexPatternSetId,
    regexPatternSet_regexPatternStrings,

    -- * RegexPatternSetSummary
    RegexPatternSetSummary (..),
    newRegexPatternSetSummary,
    regexPatternSetSummary_regexPatternSetId,
    regexPatternSetSummary_name,

    -- * RegexPatternSetUpdate
    RegexPatternSetUpdate (..),
    newRegexPatternSetUpdate,
    regexPatternSetUpdate_action,
    regexPatternSetUpdate_regexPatternString,

    -- * Rule
    Rule (..),
    newRule,
    rule_metricName,
    rule_name,
    rule_ruleId,
    rule_predicates,

    -- * RuleGroup
    RuleGroup (..),
    newRuleGroup,
    ruleGroup_metricName,
    ruleGroup_name,
    ruleGroup_ruleGroupId,

    -- * RuleGroupSummary
    RuleGroupSummary (..),
    newRuleGroupSummary,
    ruleGroupSummary_ruleGroupId,
    ruleGroupSummary_name,

    -- * RuleGroupUpdate
    RuleGroupUpdate (..),
    newRuleGroupUpdate,
    ruleGroupUpdate_action,
    ruleGroupUpdate_activatedRule,

    -- * RuleSummary
    RuleSummary (..),
    newRuleSummary,
    ruleSummary_ruleId,
    ruleSummary_name,

    -- * RuleUpdate
    RuleUpdate (..),
    newRuleUpdate,
    ruleUpdate_action,
    ruleUpdate_predicate,

    -- * SampledHTTPRequest
    SampledHTTPRequest (..),
    newSampledHTTPRequest,
    sampledHTTPRequest_timestamp,
    sampledHTTPRequest_action,
    sampledHTTPRequest_ruleWithinRuleGroup,
    sampledHTTPRequest_request,
    sampledHTTPRequest_weight,

    -- * SizeConstraint
    SizeConstraint (..),
    newSizeConstraint,
    sizeConstraint_fieldToMatch,
    sizeConstraint_textTransformation,
    sizeConstraint_comparisonOperator,
    sizeConstraint_size,

    -- * SizeConstraintSet
    SizeConstraintSet (..),
    newSizeConstraintSet,
    sizeConstraintSet_name,
    sizeConstraintSet_sizeConstraintSetId,
    sizeConstraintSet_sizeConstraints,

    -- * SizeConstraintSetSummary
    SizeConstraintSetSummary (..),
    newSizeConstraintSetSummary,
    sizeConstraintSetSummary_sizeConstraintSetId,
    sizeConstraintSetSummary_name,

    -- * SizeConstraintSetUpdate
    SizeConstraintSetUpdate (..),
    newSizeConstraintSetUpdate,
    sizeConstraintSetUpdate_action,
    sizeConstraintSetUpdate_sizeConstraint,

    -- * SqlInjectionMatchSet
    SqlInjectionMatchSet (..),
    newSqlInjectionMatchSet,
    sqlInjectionMatchSet_name,
    sqlInjectionMatchSet_sqlInjectionMatchSetId,
    sqlInjectionMatchSet_sqlInjectionMatchTuples,

    -- * SqlInjectionMatchSetSummary
    SqlInjectionMatchSetSummary (..),
    newSqlInjectionMatchSetSummary,
    sqlInjectionMatchSetSummary_sqlInjectionMatchSetId,
    sqlInjectionMatchSetSummary_name,

    -- * SqlInjectionMatchSetUpdate
    SqlInjectionMatchSetUpdate (..),
    newSqlInjectionMatchSetUpdate,
    sqlInjectionMatchSetUpdate_action,
    sqlInjectionMatchSetUpdate_sqlInjectionMatchTuple,

    -- * SqlInjectionMatchTuple
    SqlInjectionMatchTuple (..),
    newSqlInjectionMatchTuple,
    sqlInjectionMatchTuple_fieldToMatch,
    sqlInjectionMatchTuple_textTransformation,

    -- * SubscribedRuleGroupSummary
    SubscribedRuleGroupSummary (..),
    newSubscribedRuleGroupSummary,
    subscribedRuleGroupSummary_ruleGroupId,
    subscribedRuleGroupSummary_name,
    subscribedRuleGroupSummary_metricName,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TagInfoForResource
    TagInfoForResource (..),
    newTagInfoForResource,
    tagInfoForResource_resourceARN,
    tagInfoForResource_tagList,

    -- * TimeWindow
    TimeWindow (..),
    newTimeWindow,
    timeWindow_startTime,
    timeWindow_endTime,

    -- * WafAction
    WafAction (..),
    newWafAction,
    wafAction_type,

    -- * WafOverrideAction
    WafOverrideAction (..),
    newWafOverrideAction,
    wafOverrideAction_type,

    -- * WebACL
    WebACL (..),
    newWebACL,
    webACL_metricName,
    webACL_webACLArn,
    webACL_name,
    webACL_webACLId,
    webACL_defaultAction,
    webACL_rules,

    -- * WebACLSummary
    WebACLSummary (..),
    newWebACLSummary,
    webACLSummary_webACLId,
    webACLSummary_name,

    -- * WebACLUpdate
    WebACLUpdate (..),
    newWebACLUpdate,
    webACLUpdate_action,
    webACLUpdate_activatedRule,

    -- * XssMatchSet
    XssMatchSet (..),
    newXssMatchSet,
    xssMatchSet_name,
    xssMatchSet_xssMatchSetId,
    xssMatchSet_xssMatchTuples,

    -- * XssMatchSetSummary
    XssMatchSetSummary (..),
    newXssMatchSetSummary,
    xssMatchSetSummary_xssMatchSetId,
    xssMatchSetSummary_name,

    -- * XssMatchSetUpdate
    XssMatchSetUpdate (..),
    newXssMatchSetUpdate,
    xssMatchSetUpdate_action,
    xssMatchSetUpdate_xssMatchTuple,

    -- * XssMatchTuple
    XssMatchTuple (..),
    newXssMatchTuple,
    xssMatchTuple_fieldToMatch,
    xssMatchTuple_textTransformation,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.WAFRegional.Types.ActivatedRule
import Network.AWS.WAFRegional.Types.ByteMatchSet
import Network.AWS.WAFRegional.Types.ByteMatchSetSummary
import Network.AWS.WAFRegional.Types.ByteMatchSetUpdate
import Network.AWS.WAFRegional.Types.ByteMatchTuple
import Network.AWS.WAFRegional.Types.ChangeAction
import Network.AWS.WAFRegional.Types.ChangeTokenStatus
import Network.AWS.WAFRegional.Types.ComparisonOperator
import Network.AWS.WAFRegional.Types.ExcludedRule
import Network.AWS.WAFRegional.Types.FieldToMatch
import Network.AWS.WAFRegional.Types.GeoMatchConstraint
import Network.AWS.WAFRegional.Types.GeoMatchConstraintType
import Network.AWS.WAFRegional.Types.GeoMatchConstraintValue
import Network.AWS.WAFRegional.Types.GeoMatchSet
import Network.AWS.WAFRegional.Types.GeoMatchSetSummary
import Network.AWS.WAFRegional.Types.GeoMatchSetUpdate
import Network.AWS.WAFRegional.Types.HTTPHeader
import Network.AWS.WAFRegional.Types.HTTPRequest
import Network.AWS.WAFRegional.Types.IPSet
import Network.AWS.WAFRegional.Types.IPSetDescriptor
import Network.AWS.WAFRegional.Types.IPSetDescriptorType
import Network.AWS.WAFRegional.Types.IPSetSummary
import Network.AWS.WAFRegional.Types.IPSetUpdate
import Network.AWS.WAFRegional.Types.LoggingConfiguration
import Network.AWS.WAFRegional.Types.MatchFieldType
import Network.AWS.WAFRegional.Types.PositionalConstraint
import Network.AWS.WAFRegional.Types.Predicate
import Network.AWS.WAFRegional.Types.PredicateType
import Network.AWS.WAFRegional.Types.RateBasedRule
import Network.AWS.WAFRegional.Types.RateKey
import Network.AWS.WAFRegional.Types.RegexMatchSet
import Network.AWS.WAFRegional.Types.RegexMatchSetSummary
import Network.AWS.WAFRegional.Types.RegexMatchSetUpdate
import Network.AWS.WAFRegional.Types.RegexMatchTuple
import Network.AWS.WAFRegional.Types.RegexPatternSet
import Network.AWS.WAFRegional.Types.RegexPatternSetSummary
import Network.AWS.WAFRegional.Types.RegexPatternSetUpdate
import Network.AWS.WAFRegional.Types.ResourceType
import Network.AWS.WAFRegional.Types.Rule
import Network.AWS.WAFRegional.Types.RuleGroup
import Network.AWS.WAFRegional.Types.RuleGroupSummary
import Network.AWS.WAFRegional.Types.RuleGroupUpdate
import Network.AWS.WAFRegional.Types.RuleSummary
import Network.AWS.WAFRegional.Types.RuleUpdate
import Network.AWS.WAFRegional.Types.SampledHTTPRequest
import Network.AWS.WAFRegional.Types.SizeConstraint
import Network.AWS.WAFRegional.Types.SizeConstraintSet
import Network.AWS.WAFRegional.Types.SizeConstraintSetSummary
import Network.AWS.WAFRegional.Types.SizeConstraintSetUpdate
import Network.AWS.WAFRegional.Types.SqlInjectionMatchSet
import Network.AWS.WAFRegional.Types.SqlInjectionMatchSetSummary
import Network.AWS.WAFRegional.Types.SqlInjectionMatchSetUpdate
import Network.AWS.WAFRegional.Types.SqlInjectionMatchTuple
import Network.AWS.WAFRegional.Types.SubscribedRuleGroupSummary
import Network.AWS.WAFRegional.Types.Tag
import Network.AWS.WAFRegional.Types.TagInfoForResource
import Network.AWS.WAFRegional.Types.TextTransformation
import Network.AWS.WAFRegional.Types.TimeWindow
import Network.AWS.WAFRegional.Types.WafAction
import Network.AWS.WAFRegional.Types.WafActionType
import Network.AWS.WAFRegional.Types.WafOverrideAction
import Network.AWS.WAFRegional.Types.WafOverrideActionType
import Network.AWS.WAFRegional.Types.WafRuleType
import Network.AWS.WAFRegional.Types.WebACL
import Network.AWS.WAFRegional.Types.WebACLSummary
import Network.AWS.WAFRegional.Types.WebACLUpdate
import Network.AWS.WAFRegional.Types.XssMatchSet
import Network.AWS.WAFRegional.Types.XssMatchSetSummary
import Network.AWS.WAFRegional.Types.XssMatchSetUpdate
import Network.AWS.WAFRegional.Types.XssMatchTuple

-- | API version @2016-11-28@ of the Amazon WAF Regional SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "WAFRegional",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "waf-regional",
      Core._serviceSigningName = "waf-regional",
      Core._serviceVersion = "2016-11-28",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Core.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "WAFRegional",
      Core._serviceRetry = retry
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
      | Lens.has (Core.hasStatus 504) e =
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- |
_WAFTagOperationInternalErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFTagOperationInternalErrorException =
  Core._MatchServiceError
    defaultService
    "WAFTagOperationInternalErrorException"

-- | The operation failed because you tried to create, update, or delete an
-- object by using an invalid account identifier.
_WAFInvalidAccountException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFInvalidAccountException =
  Core._MatchServiceError
    defaultService
    "WAFInvalidAccountException"

-- | The specified subscription does not exist.
_WAFSubscriptionNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFSubscriptionNotFoundException =
  Core._MatchServiceError
    defaultService
    "WAFSubscriptionNotFoundException"

-- |
_WAFBadRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFBadRequestException =
  Core._MatchServiceError
    defaultService
    "WAFBadRequestException"

-- | The name specified is invalid.
_WAFDisallowedNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFDisallowedNameException =
  Core._MatchServiceError
    defaultService
    "WAFDisallowedNameException"

-- | The operation failed because you tried to delete an object that isn\'t
-- empty. For example:
--
-- -   You tried to delete a @WebACL@ that still contains one or more
--     @Rule@ objects.
--
-- -   You tried to delete a @Rule@ that still contains one or more
--     @ByteMatchSet@ objects or other predicates.
--
-- -   You tried to delete a @ByteMatchSet@ that contains one or more
--     @ByteMatchTuple@ objects.
--
-- -   You tried to delete an @IPSet@ that references one or more IP
--     addresses.
_WAFNonEmptyEntityException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFNonEmptyEntityException =
  Core._MatchServiceError
    defaultService
    "WAFNonEmptyEntityException"

-- | The operation failed because there was nothing to do. For example:
--
-- -   You tried to remove a @Rule@ from a @WebACL@, but the @Rule@ isn\'t
--     in the specified @WebACL@.
--
-- -   You tried to remove an IP address from an @IPSet@, but the IP
--     address isn\'t in the specified @IPSet@.
--
-- -   You tried to remove a @ByteMatchTuple@ from a @ByteMatchSet@, but
--     the @ByteMatchTuple@ isn\'t in the specified @WebACL@.
--
-- -   You tried to add a @Rule@ to a @WebACL@, but the @Rule@ already
--     exists in the specified @WebACL@.
--
-- -   You tried to add a @ByteMatchTuple@ to a @ByteMatchSet@, but the
--     @ByteMatchTuple@ already exists in the specified @WebACL@.
_WAFInvalidOperationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFInvalidOperationException =
  Core._MatchServiceError
    defaultService
    "WAFInvalidOperationException"

-- | The operation failed because you tried to create, update, or delete an
-- object by using a change token that has already been used.
_WAFStaleDataException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFStaleDataException =
  Core._MatchServiceError
    defaultService
    "WAFStaleDataException"

-- |
_WAFTagOperationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFTagOperationException =
  Core._MatchServiceError
    defaultService
    "WAFTagOperationException"

-- | The operation failed because of a system problem, even though the
-- request was valid. Retry your request.
_WAFInternalErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFInternalErrorException =
  Core._MatchServiceError
    defaultService
    "WAFInternalErrorException"

-- | AWS WAF is not able to access the service linked role. This can be
-- caused by a previous @PutLoggingConfiguration@ request, which can lock
-- the service linked role for about 20 seconds. Please try your request
-- again. The service linked role can also be locked by a previous
-- @DeleteServiceLinkedRole@ request, which can lock the role for 15
-- minutes or more. If you recently made a @DeleteServiceLinkedRole@, wait
-- at least 15 minutes and try the request again. If you receive this same
-- exception again, you will have to wait additional time until the role is
-- unlocked.
_WAFServiceLinkedRoleErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFServiceLinkedRoleErrorException =
  Core._MatchServiceError
    defaultService
    "WAFServiceLinkedRoleErrorException"

-- | The operation failed because AWS WAF didn\'t recognize a parameter in
-- the request. For example:
--
-- -   You specified an invalid parameter name.
--
-- -   You specified an invalid value.
--
-- -   You tried to update an object (@ByteMatchSet@, @IPSet@, @Rule@, or
--     @WebACL@) using an action other than @INSERT@ or @DELETE@.
--
-- -   You tried to create a @WebACL@ with a @DefaultAction@ @Type@ other
--     than @ALLOW@, @BLOCK@, or @COUNT@.
--
-- -   You tried to create a @RateBasedRule@ with a @RateKey@ value other
--     than @IP@.
--
-- -   You tried to update a @WebACL@ with a @WafAction@ @Type@ other than
--     @ALLOW@, @BLOCK@, or @COUNT@.
--
-- -   You tried to update a @ByteMatchSet@ with a @FieldToMatch@ @Type@
--     other than HEADER, METHOD, QUERY_STRING, URI, or BODY.
--
-- -   You tried to update a @ByteMatchSet@ with a @Field@ of @HEADER@ but
--     no value for @Data@.
--
-- -   Your request references an ARN that is malformed, or corresponds to
--     a resource with which a web ACL cannot be associated.
_WAFInvalidParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFInvalidParameterException =
  Core._MatchServiceError
    defaultService
    "WAFInvalidParameterException"

-- | The operation failed because the referenced object doesn\'t exist.
_WAFNonexistentItemException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFNonexistentItemException =
  Core._MatchServiceError
    defaultService
    "WAFNonexistentItemException"

-- | The operation failed because the entity referenced is temporarily
-- unavailable. Retry your request.
_WAFUnavailableEntityException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFUnavailableEntityException =
  Core._MatchServiceError
    defaultService
    "WAFUnavailableEntityException"

-- | The regular expression (regex) you specified in @RegexPatternString@ is
-- invalid.
_WAFInvalidRegexPatternException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFInvalidRegexPatternException =
  Core._MatchServiceError
    defaultService
    "WAFInvalidRegexPatternException"

-- | The operation failed because you tried to add an object to or delete an
-- object from another object that doesn\'t exist. For example:
--
-- -   You tried to add a @Rule@ to or delete a @Rule@ from a @WebACL@ that
--     doesn\'t exist.
--
-- -   You tried to add a @ByteMatchSet@ to or delete a @ByteMatchSet@ from
--     a @Rule@ that doesn\'t exist.
--
-- -   You tried to add an IP address to or delete an IP address from an
--     @IPSet@ that doesn\'t exist.
--
-- -   You tried to add a @ByteMatchTuple@ to or delete a @ByteMatchTuple@
--     from a @ByteMatchSet@ that doesn\'t exist.
_WAFNonexistentContainerException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFNonexistentContainerException =
  Core._MatchServiceError
    defaultService
    "WAFNonexistentContainerException"

-- | The operation failed due to a problem with the migration. The failure
-- cause is provided in the exception, in the @MigrationErrorType@:
--
-- -   @ENTITY_NOT_SUPPORTED@ - The web ACL has an unsupported entity but
--     the @IgnoreUnsupportedType@ is not set to true.
--
-- -   @ENTITY_NOT_FOUND@ - The web ACL doesn\'t exist.
--
-- -   @S3_BUCKET_NO_PERMISSION@ - You don\'t have permission to perform
--     the @PutObject@ action to the specified Amazon S3 bucket.
--
-- -   @S3_BUCKET_NOT_ACCESSIBLE@ - The bucket policy doesn\'t allow AWS
--     WAF to perform the @PutObject@ action in the bucket.
--
-- -   @S3_BUCKET_NOT_FOUND@ - The S3 bucket doesn\'t exist.
--
-- -   @S3_BUCKET_INVALID_REGION@ - The S3 bucket is not in the same Region
--     as the web ACL.
--
-- -   @S3_INTERNAL_ERROR@ - AWS WAF failed to create the template in the
--     S3 bucket for another reason.
_WAFEntityMigrationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFEntityMigrationException =
  Core._MatchServiceError
    defaultService
    "WAFEntityMigrationException"

-- | The operation failed because you tried to delete an object that is still
-- in use. For example:
--
-- -   You tried to delete a @ByteMatchSet@ that is still referenced by a
--     @Rule@.
--
-- -   You tried to delete a @Rule@ that is still referenced by a @WebACL@.
_WAFReferencedItemException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFReferencedItemException =
  Core._MatchServiceError
    defaultService
    "WAFReferencedItemException"

-- | The operation failed because the specified policy is not in the proper
-- format.
--
-- The policy is subject to the following restrictions:
--
-- -   You can attach only one policy with each @PutPermissionPolicy@
--     request.
--
-- -   The policy must include an @Effect@, @Action@ and @Principal@.
--
-- -   @Effect@ must specify @Allow@.
--
-- -   The @Action@ in the policy must be @waf:UpdateWebACL@,
--     @waf-regional:UpdateWebACL@, @waf:GetRuleGroup@ and
--     @waf-regional:GetRuleGroup@ . Any extra or wildcard actions in the
--     policy will be rejected.
--
-- -   The policy cannot include a @Resource@ parameter.
--
-- -   The ARN in the request must be a valid WAF RuleGroup ARN and the
--     RuleGroup must exist in the same region.
--
-- -   The user making the request must be the owner of the RuleGroup.
--
-- -   Your policy must be composed using IAM Policy version 2012-10-17.
_WAFInvalidPermissionPolicyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFInvalidPermissionPolicyException =
  Core._MatchServiceError
    defaultService
    "WAFInvalidPermissionPolicyException"

-- | The operation exceeds a resource limit, for example, the maximum number
-- of @WebACL@ objects that you can create for an AWS account. For more
-- information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/limits.html Limits>
-- in the /AWS WAF Developer Guide/.
_WAFLimitsExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WAFLimitsExceededException =
  Core._MatchServiceError
    defaultService
    "WAFLimitsExceededException"
