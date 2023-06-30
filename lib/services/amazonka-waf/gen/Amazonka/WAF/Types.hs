{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WAF.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAF.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _WAFBadRequestException,
    _WAFDisallowedNameException,
    _WAFEntityMigrationException,
    _WAFInternalErrorException,
    _WAFInvalidAccountException,
    _WAFInvalidOperationException,
    _WAFInvalidParameterException,
    _WAFInvalidPermissionPolicyException,
    _WAFInvalidRegexPatternException,
    _WAFLimitsExceededException,
    _WAFNonEmptyEntityException,
    _WAFNonexistentContainerException,
    _WAFNonexistentItemException,
    _WAFReferencedItemException,
    _WAFServiceLinkedRoleErrorException,
    _WAFStaleDataException,
    _WAFSubscriptionNotFoundException,
    _WAFTagOperationException,
    _WAFTagOperationInternalErrorException,

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
    activatedRule_action,
    activatedRule_excludedRules,
    activatedRule_overrideAction,
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
    hTTPRequest_clientIP,
    hTTPRequest_country,
    hTTPRequest_hTTPVersion,
    hTTPRequest_headers,
    hTTPRequest_method,
    hTTPRequest_uri,

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
    regexMatchSet_name,
    regexMatchSet_regexMatchSetId,
    regexMatchSet_regexMatchTuples,

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
    sampledHTTPRequest_action,
    sampledHTTPRequest_ruleWithinRuleGroup,
    sampledHTTPRequest_timestamp,
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
    webACL_name,
    webACL_webACLArn,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign
import Amazonka.WAF.Types.ActivatedRule
import Amazonka.WAF.Types.ByteMatchSet
import Amazonka.WAF.Types.ByteMatchSetSummary
import Amazonka.WAF.Types.ByteMatchSetUpdate
import Amazonka.WAF.Types.ByteMatchTuple
import Amazonka.WAF.Types.ChangeAction
import Amazonka.WAF.Types.ChangeTokenStatus
import Amazonka.WAF.Types.ComparisonOperator
import Amazonka.WAF.Types.ExcludedRule
import Amazonka.WAF.Types.FieldToMatch
import Amazonka.WAF.Types.GeoMatchConstraint
import Amazonka.WAF.Types.GeoMatchConstraintType
import Amazonka.WAF.Types.GeoMatchConstraintValue
import Amazonka.WAF.Types.GeoMatchSet
import Amazonka.WAF.Types.GeoMatchSetSummary
import Amazonka.WAF.Types.GeoMatchSetUpdate
import Amazonka.WAF.Types.HTTPHeader
import Amazonka.WAF.Types.HTTPRequest
import Amazonka.WAF.Types.IPSet
import Amazonka.WAF.Types.IPSetDescriptor
import Amazonka.WAF.Types.IPSetDescriptorType
import Amazonka.WAF.Types.IPSetSummary
import Amazonka.WAF.Types.IPSetUpdate
import Amazonka.WAF.Types.LoggingConfiguration
import Amazonka.WAF.Types.MatchFieldType
import Amazonka.WAF.Types.PositionalConstraint
import Amazonka.WAF.Types.Predicate
import Amazonka.WAF.Types.PredicateType
import Amazonka.WAF.Types.RateBasedRule
import Amazonka.WAF.Types.RateKey
import Amazonka.WAF.Types.RegexMatchSet
import Amazonka.WAF.Types.RegexMatchSetSummary
import Amazonka.WAF.Types.RegexMatchSetUpdate
import Amazonka.WAF.Types.RegexMatchTuple
import Amazonka.WAF.Types.RegexPatternSet
import Amazonka.WAF.Types.RegexPatternSetSummary
import Amazonka.WAF.Types.RegexPatternSetUpdate
import Amazonka.WAF.Types.Rule
import Amazonka.WAF.Types.RuleGroup
import Amazonka.WAF.Types.RuleGroupSummary
import Amazonka.WAF.Types.RuleGroupUpdate
import Amazonka.WAF.Types.RuleSummary
import Amazonka.WAF.Types.RuleUpdate
import Amazonka.WAF.Types.SampledHTTPRequest
import Amazonka.WAF.Types.SizeConstraint
import Amazonka.WAF.Types.SizeConstraintSet
import Amazonka.WAF.Types.SizeConstraintSetSummary
import Amazonka.WAF.Types.SizeConstraintSetUpdate
import Amazonka.WAF.Types.SqlInjectionMatchSet
import Amazonka.WAF.Types.SqlInjectionMatchSetSummary
import Amazonka.WAF.Types.SqlInjectionMatchSetUpdate
import Amazonka.WAF.Types.SqlInjectionMatchTuple
import Amazonka.WAF.Types.SubscribedRuleGroupSummary
import Amazonka.WAF.Types.Tag
import Amazonka.WAF.Types.TagInfoForResource
import Amazonka.WAF.Types.TextTransformation
import Amazonka.WAF.Types.TimeWindow
import Amazonka.WAF.Types.WafAction
import Amazonka.WAF.Types.WafActionType
import Amazonka.WAF.Types.WafOverrideAction
import Amazonka.WAF.Types.WafOverrideActionType
import Amazonka.WAF.Types.WafRuleType
import Amazonka.WAF.Types.WebACL
import Amazonka.WAF.Types.WebACLSummary
import Amazonka.WAF.Types.WebACLUpdate
import Amazonka.WAF.Types.XssMatchSet
import Amazonka.WAF.Types.XssMatchSetSummary
import Amazonka.WAF.Types.XssMatchSetUpdate
import Amazonka.WAF.Types.XssMatchTuple

-- | API version @2015-08-24@ of the Amazon WAF SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "WAF",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "waf",
      Core.signingName = "waf",
      Core.version = "2015-08-24",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "WAF",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

_WAFBadRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_WAFBadRequestException =
  Core._MatchServiceError
    defaultService
    "WAFBadRequestException"

-- | The name specified is invalid.
_WAFDisallowedNameException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_WAFDisallowedNameException =
  Core._MatchServiceError
    defaultService
    "WAFDisallowedNameException"

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
_WAFEntityMigrationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_WAFEntityMigrationException =
  Core._MatchServiceError
    defaultService
    "WAFEntityMigrationException"

-- | The operation failed because of a system problem, even though the
-- request was valid. Retry your request.
_WAFInternalErrorException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_WAFInternalErrorException =
  Core._MatchServiceError
    defaultService
    "WAFInternalErrorException"

-- | The operation failed because you tried to create, update, or delete an
-- object by using an invalid account identifier.
_WAFInvalidAccountException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_WAFInvalidAccountException =
  Core._MatchServiceError
    defaultService
    "WAFInvalidAccountException"

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
_WAFInvalidOperationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_WAFInvalidOperationException =
  Core._MatchServiceError
    defaultService
    "WAFInvalidOperationException"

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
_WAFInvalidParameterException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_WAFInvalidParameterException =
  Core._MatchServiceError
    defaultService
    "WAFInvalidParameterException"

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
_WAFInvalidPermissionPolicyException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_WAFInvalidPermissionPolicyException =
  Core._MatchServiceError
    defaultService
    "WAFInvalidPermissionPolicyException"

-- | The regular expression (regex) you specified in @RegexPatternString@ is
-- invalid.
_WAFInvalidRegexPatternException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_WAFInvalidRegexPatternException =
  Core._MatchServiceError
    defaultService
    "WAFInvalidRegexPatternException"

-- | The operation exceeds a resource limit, for example, the maximum number
-- of @WebACL@ objects that you can create for an AWS account. For more
-- information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/limits.html Limits>
-- in the /AWS WAF Developer Guide/.
_WAFLimitsExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_WAFLimitsExceededException =
  Core._MatchServiceError
    defaultService
    "WAFLimitsExceededException"

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
_WAFNonEmptyEntityException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_WAFNonEmptyEntityException =
  Core._MatchServiceError
    defaultService
    "WAFNonEmptyEntityException"

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
_WAFNonexistentContainerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_WAFNonexistentContainerException =
  Core._MatchServiceError
    defaultService
    "WAFNonexistentContainerException"

-- | The operation failed because the referenced object doesn\'t exist.
_WAFNonexistentItemException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_WAFNonexistentItemException =
  Core._MatchServiceError
    defaultService
    "WAFNonexistentItemException"

-- | The operation failed because you tried to delete an object that is still
-- in use. For example:
--
-- -   You tried to delete a @ByteMatchSet@ that is still referenced by a
--     @Rule@.
--
-- -   You tried to delete a @Rule@ that is still referenced by a @WebACL@.
_WAFReferencedItemException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_WAFReferencedItemException =
  Core._MatchServiceError
    defaultService
    "WAFReferencedItemException"

-- | AWS WAF is not able to access the service linked role. This can be
-- caused by a previous @PutLoggingConfiguration@ request, which can lock
-- the service linked role for about 20 seconds. Please try your request
-- again. The service linked role can also be locked by a previous
-- @DeleteServiceLinkedRole@ request, which can lock the role for 15
-- minutes or more. If you recently made a @DeleteServiceLinkedRole@, wait
-- at least 15 minutes and try the request again. If you receive this same
-- exception again, you will have to wait additional time until the role is
-- unlocked.
_WAFServiceLinkedRoleErrorException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_WAFServiceLinkedRoleErrorException =
  Core._MatchServiceError
    defaultService
    "WAFServiceLinkedRoleErrorException"

-- | The operation failed because you tried to create, update, or delete an
-- object by using a change token that has already been used.
_WAFStaleDataException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_WAFStaleDataException =
  Core._MatchServiceError
    defaultService
    "WAFStaleDataException"

-- | The specified subscription does not exist.
_WAFSubscriptionNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_WAFSubscriptionNotFoundException =
  Core._MatchServiceError
    defaultService
    "WAFSubscriptionNotFoundException"

_WAFTagOperationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_WAFTagOperationException =
  Core._MatchServiceError
    defaultService
    "WAFTagOperationException"

_WAFTagOperationInternalErrorException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_WAFTagOperationInternalErrorException =
  Core._MatchServiceError
    defaultService
    "WAFTagOperationInternalErrorException"
