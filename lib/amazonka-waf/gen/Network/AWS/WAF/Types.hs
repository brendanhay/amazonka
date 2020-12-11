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
    wafService,

    -- * Errors

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
    mkActivatedRule,
    arOverrideAction,
    arAction,
    arExcludedRules,
    arType,
    arPriority,
    arRuleId,

    -- * ByteMatchSet
    ByteMatchSet (..),
    mkByteMatchSet,
    bmsName,
    bmsByteMatchSetId,
    bmsByteMatchTuples,

    -- * ByteMatchSetSummary
    ByteMatchSetSummary (..),
    mkByteMatchSetSummary,
    bmssByteMatchSetId,
    bmssName,

    -- * ByteMatchSetUpdate
    ByteMatchSetUpdate (..),
    mkByteMatchSetUpdate,
    bmsuAction,
    bmsuByteMatchTuple,

    -- * ByteMatchTuple
    ByteMatchTuple (..),
    mkByteMatchTuple,
    bmtFieldToMatch,
    bmtTargetString,
    bmtTextTransformation,
    bmtPositionalConstraint,

    -- * ExcludedRule
    ExcludedRule (..),
    mkExcludedRule,
    erRuleId,

    -- * FieldToMatch
    FieldToMatch (..),
    mkFieldToMatch,
    ftmData,
    ftmType,

    -- * GeoMatchConstraint
    GeoMatchConstraint (..),
    mkGeoMatchConstraint,
    gmcType,
    gmcValue,

    -- * GeoMatchSet
    GeoMatchSet (..),
    mkGeoMatchSet,
    gmsName,
    gmsGeoMatchSetId,
    gmsGeoMatchConstraints,

    -- * GeoMatchSetSummary
    GeoMatchSetSummary (..),
    mkGeoMatchSetSummary,
    gmssGeoMatchSetId,
    gmssName,

    -- * GeoMatchSetUpdate
    GeoMatchSetUpdate (..),
    mkGeoMatchSetUpdate,
    gmsuAction,
    gmsuGeoMatchConstraint,

    -- * HTTPHeader
    HTTPHeader (..),
    mkHTTPHeader,
    httphValue,
    httphName,

    -- * HTTPRequest
    HTTPRequest (..),
    mkHTTPRequest,
    httprHTTPVersion,
    httprCountry,
    httprURI,
    httprHeaders,
    httprMethod,
    httprClientIP,

    -- * IPSet
    IPSet (..),
    mkIPSet,
    isName,
    isIPSetId,
    isIPSetDescriptors,

    -- * IPSetDescriptor
    IPSetDescriptor (..),
    mkIPSetDescriptor,
    isdType,
    isdValue,

    -- * IPSetSummary
    IPSetSummary (..),
    mkIPSetSummary,
    issIPSetId,
    issName,

    -- * IPSetUpdate
    IPSetUpdate (..),
    mkIPSetUpdate,
    isuAction,
    isuIPSetDescriptor,

    -- * LoggingConfiguration
    LoggingConfiguration (..),
    mkLoggingConfiguration,
    lcRedactedFields,
    lcResourceARN,
    lcLogDestinationConfigs,

    -- * Predicate
    Predicate (..),
    mkPredicate,
    pNegated,
    pType,
    pDataId,

    -- * RateBasedRule
    RateBasedRule (..),
    mkRateBasedRule,
    rbrMetricName,
    rbrName,
    rbrRuleId,
    rbrMatchPredicates,
    rbrRateKey,
    rbrRateLimit,

    -- * RegexMatchSet
    RegexMatchSet (..),
    mkRegexMatchSet,
    rmsName,
    rmsRegexMatchTuples,
    rmsRegexMatchSetId,

    -- * RegexMatchSetSummary
    RegexMatchSetSummary (..),
    mkRegexMatchSetSummary,
    rmssRegexMatchSetId,
    rmssName,

    -- * RegexMatchSetUpdate
    RegexMatchSetUpdate (..),
    mkRegexMatchSetUpdate,
    rmsuAction,
    rmsuRegexMatchTuple,

    -- * RegexMatchTuple
    RegexMatchTuple (..),
    mkRegexMatchTuple,
    rmtFieldToMatch,
    rmtTextTransformation,
    rmtRegexPatternSetId,

    -- * RegexPatternSet
    RegexPatternSet (..),
    mkRegexPatternSet,
    rpsName,
    rpsRegexPatternSetId,
    rpsRegexPatternStrings,

    -- * RegexPatternSetSummary
    RegexPatternSetSummary (..),
    mkRegexPatternSetSummary,
    rpssRegexPatternSetId,
    rpssName,

    -- * RegexPatternSetUpdate
    RegexPatternSetUpdate (..),
    mkRegexPatternSetUpdate,
    rpsuAction,
    rpsuRegexPatternString,

    -- * Rule
    Rule (..),
    mkRule,
    rMetricName,
    rName,
    rRuleId,
    rPredicates,

    -- * RuleGroup
    RuleGroup (..),
    mkRuleGroup,
    rgMetricName,
    rgName,
    rgRuleGroupId,

    -- * RuleGroupSummary
    RuleGroupSummary (..),
    mkRuleGroupSummary,
    rgsRuleGroupId,
    rgsName,

    -- * RuleGroupUpdate
    RuleGroupUpdate (..),
    mkRuleGroupUpdate,
    rguAction,
    rguActivatedRule,

    -- * RuleSummary
    RuleSummary (..),
    mkRuleSummary,
    rsRuleId,
    rsName,

    -- * RuleUpdate
    RuleUpdate (..),
    mkRuleUpdate,
    ruAction,
    ruPredicate,

    -- * SampledHTTPRequest
    SampledHTTPRequest (..),
    mkSampledHTTPRequest,
    shttprRuleWithinRuleGroup,
    shttprAction,
    shttprTimestamp,
    shttprRequest,
    shttprWeight,

    -- * SizeConstraint
    SizeConstraint (..),
    mkSizeConstraint,
    scFieldToMatch,
    scTextTransformation,
    scComparisonOperator,
    scSize,

    -- * SizeConstraintSet
    SizeConstraintSet (..),
    mkSizeConstraintSet,
    scsName,
    scsSizeConstraintSetId,
    scsSizeConstraints,

    -- * SizeConstraintSetSummary
    SizeConstraintSetSummary (..),
    mkSizeConstraintSetSummary,
    scssSizeConstraintSetId,
    scssName,

    -- * SizeConstraintSetUpdate
    SizeConstraintSetUpdate (..),
    mkSizeConstraintSetUpdate,
    scsuAction,
    scsuSizeConstraint,

    -- * SqlInjectionMatchSet
    SqlInjectionMatchSet (..),
    mkSqlInjectionMatchSet,
    simsName,
    simsSqlInjectionMatchSetId,
    simsSqlInjectionMatchTuples,

    -- * SqlInjectionMatchSetSummary
    SqlInjectionMatchSetSummary (..),
    mkSqlInjectionMatchSetSummary,
    simssSqlInjectionMatchSetId,
    simssName,

    -- * SqlInjectionMatchSetUpdate
    SqlInjectionMatchSetUpdate (..),
    mkSqlInjectionMatchSetUpdate,
    simsuAction,
    simsuSqlInjectionMatchTuple,

    -- * SqlInjectionMatchTuple
    SqlInjectionMatchTuple (..),
    mkSqlInjectionMatchTuple,
    simtFieldToMatch,
    simtTextTransformation,

    -- * SubscribedRuleGroupSummary
    SubscribedRuleGroupSummary (..),
    mkSubscribedRuleGroupSummary,
    srgsRuleGroupId,
    srgsName,
    srgsMetricName,

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * TagInfoForResource
    TagInfoForResource (..),
    mkTagInfoForResource,
    tifrTagList,
    tifrResourceARN,

    -- * TimeWindow
    TimeWindow (..),
    mkTimeWindow,
    twStartTime,
    twEndTime,

    -- * WafAction
    WafAction (..),
    mkWafAction,
    waType,

    -- * WafOverrideAction
    WafOverrideAction (..),
    mkWafOverrideAction,
    woaType,

    -- * WebACL
    WebACL (..),
    mkWebACL,
    waMetricName,
    waName,
    waWebACLARN,
    waWebACLId,
    waDefaultAction,
    waRules,

    -- * WebACLSummary
    WebACLSummary (..),
    mkWebACLSummary,
    wasWebACLId,
    wasName,

    -- * WebACLUpdate
    WebACLUpdate (..),
    mkWebACLUpdate,
    wauAction,
    wauActivatedRule,

    -- * XSSMatchSet
    XSSMatchSet (..),
    mkXSSMatchSet,
    xmsName,
    xmsXSSMatchSetId,
    xmsXSSMatchTuples,

    -- * XSSMatchSetSummary
    XSSMatchSetSummary (..),
    mkXSSMatchSetSummary,
    xmssXSSMatchSetId,
    xmssName,

    -- * XSSMatchSetUpdate
    XSSMatchSetUpdate (..),
    mkXSSMatchSetUpdate,
    xmsuAction,
    xmsuXSSMatchTuple,

    -- * XSSMatchTuple
    XSSMatchTuple (..),
    mkXSSMatchTuple,
    xmtFieldToMatch,
    xmtTextTransformation,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.WAF.Types.ActivatedRule
import Network.AWS.WAF.Types.ByteMatchSet
import Network.AWS.WAF.Types.ByteMatchSetSummary
import Network.AWS.WAF.Types.ByteMatchSetUpdate
import Network.AWS.WAF.Types.ByteMatchTuple
import Network.AWS.WAF.Types.ChangeAction
import Network.AWS.WAF.Types.ChangeTokenStatus
import Network.AWS.WAF.Types.ComparisonOperator
import Network.AWS.WAF.Types.ExcludedRule
import Network.AWS.WAF.Types.FieldToMatch
import Network.AWS.WAF.Types.GeoMatchConstraint
import Network.AWS.WAF.Types.GeoMatchConstraintType
import Network.AWS.WAF.Types.GeoMatchConstraintValue
import Network.AWS.WAF.Types.GeoMatchSet
import Network.AWS.WAF.Types.GeoMatchSetSummary
import Network.AWS.WAF.Types.GeoMatchSetUpdate
import Network.AWS.WAF.Types.HTTPHeader
import Network.AWS.WAF.Types.HTTPRequest
import Network.AWS.WAF.Types.IPSet
import Network.AWS.WAF.Types.IPSetDescriptor
import Network.AWS.WAF.Types.IPSetDescriptorType
import Network.AWS.WAF.Types.IPSetSummary
import Network.AWS.WAF.Types.IPSetUpdate
import Network.AWS.WAF.Types.LoggingConfiguration
import Network.AWS.WAF.Types.MatchFieldType
import Network.AWS.WAF.Types.PositionalConstraint
import Network.AWS.WAF.Types.Predicate
import Network.AWS.WAF.Types.PredicateType
import Network.AWS.WAF.Types.RateBasedRule
import Network.AWS.WAF.Types.RateKey
import Network.AWS.WAF.Types.RegexMatchSet
import Network.AWS.WAF.Types.RegexMatchSetSummary
import Network.AWS.WAF.Types.RegexMatchSetUpdate
import Network.AWS.WAF.Types.RegexMatchTuple
import Network.AWS.WAF.Types.RegexPatternSet
import Network.AWS.WAF.Types.RegexPatternSetSummary
import Network.AWS.WAF.Types.RegexPatternSetUpdate
import Network.AWS.WAF.Types.Rule
import Network.AWS.WAF.Types.RuleGroup
import Network.AWS.WAF.Types.RuleGroupSummary
import Network.AWS.WAF.Types.RuleGroupUpdate
import Network.AWS.WAF.Types.RuleSummary
import Network.AWS.WAF.Types.RuleUpdate
import Network.AWS.WAF.Types.SampledHTTPRequest
import Network.AWS.WAF.Types.SizeConstraint
import Network.AWS.WAF.Types.SizeConstraintSet
import Network.AWS.WAF.Types.SizeConstraintSetSummary
import Network.AWS.WAF.Types.SizeConstraintSetUpdate
import Network.AWS.WAF.Types.SqlInjectionMatchSet
import Network.AWS.WAF.Types.SqlInjectionMatchSetSummary
import Network.AWS.WAF.Types.SqlInjectionMatchSetUpdate
import Network.AWS.WAF.Types.SqlInjectionMatchTuple
import Network.AWS.WAF.Types.SubscribedRuleGroupSummary
import Network.AWS.WAF.Types.Tag
import Network.AWS.WAF.Types.TagInfoForResource
import Network.AWS.WAF.Types.TextTransformation
import Network.AWS.WAF.Types.TimeWindow
import Network.AWS.WAF.Types.WafAction
import Network.AWS.WAF.Types.WafActionType
import Network.AWS.WAF.Types.WafOverrideAction
import Network.AWS.WAF.Types.WafOverrideActionType
import Network.AWS.WAF.Types.WafRuleType
import Network.AWS.WAF.Types.WebACL
import Network.AWS.WAF.Types.WebACLSummary
import Network.AWS.WAF.Types.WebACLUpdate
import Network.AWS.WAF.Types.XSSMatchSet
import Network.AWS.WAF.Types.XSSMatchSetSummary
import Network.AWS.WAF.Types.XSSMatchSetUpdate
import Network.AWS.WAF.Types.XSSMatchTuple

-- | API version @2015-08-24@ of the Amazon WAF SDK configuration.
wafService :: Lude.Service
wafService =
  Lude.Service
    { Lude._svcAbbrev = "WAF",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "waf",
      Lude._svcVersion = "2015-08-24",
      Lude._svcEndpoint = Lude.defaultEndpoint wafService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "WAF",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
