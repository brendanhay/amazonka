-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types
  ( -- * Service configuration
    wAFRegionalService,

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
import Network.AWS.WAFRegional.Types.XSSMatchSet
import Network.AWS.WAFRegional.Types.XSSMatchSetSummary
import Network.AWS.WAFRegional.Types.XSSMatchSetUpdate
import Network.AWS.WAFRegional.Types.XSSMatchTuple

-- | API version @2016-11-28@ of the Amazon WAF Regional SDK configuration.
wAFRegionalService :: Lude.Service
wAFRegionalService =
  Lude.Service
    { Lude._svcAbbrev = "WAFRegional",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "waf-regional",
      Lude._svcVersion = "2016-11-28",
      Lude._svcEndpoint = Lude.defaultEndpoint wAFRegionalService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "WAFRegional",
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
