{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types
  ( -- * Service Configuration
    wAFRegional,

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
    ActivatedRule,
    activatedRule,
    arOverrideAction,
    arAction,
    arExcludedRules,
    arType,
    arPriority,
    arRuleId,

    -- * ByteMatchSet
    ByteMatchSet,
    byteMatchSet,
    bmsName,
    bmsByteMatchSetId,
    bmsByteMatchTuples,

    -- * ByteMatchSetSummary
    ByteMatchSetSummary,
    byteMatchSetSummary,
    bmssByteMatchSetId,
    bmssName,

    -- * ByteMatchSetUpdate
    ByteMatchSetUpdate,
    byteMatchSetUpdate,
    bmsuAction,
    bmsuByteMatchTuple,

    -- * ByteMatchTuple
    ByteMatchTuple,
    byteMatchTuple,
    bmtFieldToMatch,
    bmtTargetString,
    bmtTextTransformation,
    bmtPositionalConstraint,

    -- * ExcludedRule
    ExcludedRule,
    excludedRule,
    erRuleId,

    -- * FieldToMatch
    FieldToMatch,
    fieldToMatch,
    ftmData,
    ftmType,

    -- * GeoMatchConstraint
    GeoMatchConstraint,
    geoMatchConstraint,
    gmcType,
    gmcValue,

    -- * GeoMatchSet
    GeoMatchSet,
    geoMatchSet,
    gmsName,
    gmsGeoMatchSetId,
    gmsGeoMatchConstraints,

    -- * GeoMatchSetSummary
    GeoMatchSetSummary,
    geoMatchSetSummary,
    gmssGeoMatchSetId,
    gmssName,

    -- * GeoMatchSetUpdate
    GeoMatchSetUpdate,
    geoMatchSetUpdate,
    gmsuAction,
    gmsuGeoMatchConstraint,

    -- * HTTPHeader
    HTTPHeader,
    hTTPHeader,
    httphValue,
    httphName,

    -- * HTTPRequest
    HTTPRequest,
    hTTPRequest,
    httprHTTPVersion,
    httprCountry,
    httprURI,
    httprHeaders,
    httprMethod,
    httprClientIP,

    -- * IPSet
    IPSet,
    ipSet,
    isName,
    isIPSetId,
    isIPSetDescriptors,

    -- * IPSetDescriptor
    IPSetDescriptor,
    ipSetDescriptor,
    isdType,
    isdValue,

    -- * IPSetSummary
    IPSetSummary,
    ipSetSummary,
    issIPSetId,
    issName,

    -- * IPSetUpdate
    IPSetUpdate,
    ipSetUpdate,
    isuAction,
    isuIPSetDescriptor,

    -- * LoggingConfiguration
    LoggingConfiguration,
    loggingConfiguration,
    lcRedactedFields,
    lcResourceARN,
    lcLogDestinationConfigs,

    -- * Predicate
    Predicate,
    predicate,
    pNegated,
    pType,
    pDataId,

    -- * RateBasedRule
    RateBasedRule,
    rateBasedRule,
    rbrMetricName,
    rbrName,
    rbrRuleId,
    rbrMatchPredicates,
    rbrRateKey,
    rbrRateLimit,

    -- * RegexMatchSet
    RegexMatchSet,
    regexMatchSet,
    rmsName,
    rmsRegexMatchTuples,
    rmsRegexMatchSetId,

    -- * RegexMatchSetSummary
    RegexMatchSetSummary,
    regexMatchSetSummary,
    rmssRegexMatchSetId,
    rmssName,

    -- * RegexMatchSetUpdate
    RegexMatchSetUpdate,
    regexMatchSetUpdate,
    rmsuAction,
    rmsuRegexMatchTuple,

    -- * RegexMatchTuple
    RegexMatchTuple,
    regexMatchTuple,
    rmtFieldToMatch,
    rmtTextTransformation,
    rmtRegexPatternSetId,

    -- * RegexPatternSet
    RegexPatternSet,
    regexPatternSet,
    rpsName,
    rpsRegexPatternSetId,
    rpsRegexPatternStrings,

    -- * RegexPatternSetSummary
    RegexPatternSetSummary,
    regexPatternSetSummary,
    rpssRegexPatternSetId,
    rpssName,

    -- * RegexPatternSetUpdate
    RegexPatternSetUpdate,
    regexPatternSetUpdate,
    rpsuAction,
    rpsuRegexPatternString,

    -- * Rule
    Rule,
    rule,
    rMetricName,
    rName,
    rRuleId,
    rPredicates,

    -- * RuleGroup
    RuleGroup,
    ruleGroup,
    rgMetricName,
    rgName,
    rgRuleGroupId,

    -- * RuleGroupSummary
    RuleGroupSummary,
    ruleGroupSummary,
    rgsRuleGroupId,
    rgsName,

    -- * RuleGroupUpdate
    RuleGroupUpdate,
    ruleGroupUpdate,
    rguAction,
    rguActivatedRule,

    -- * RuleSummary
    RuleSummary,
    ruleSummary,
    rsRuleId,
    rsName,

    -- * RuleUpdate
    RuleUpdate,
    ruleUpdate,
    ruAction,
    ruPredicate,

    -- * SampledHTTPRequest
    SampledHTTPRequest,
    sampledHTTPRequest,
    shttprRuleWithinRuleGroup,
    shttprAction,
    shttprTimestamp,
    shttprRequest,
    shttprWeight,

    -- * SizeConstraint
    SizeConstraint,
    sizeConstraint,
    scFieldToMatch,
    scTextTransformation,
    scComparisonOperator,
    scSize,

    -- * SizeConstraintSet
    SizeConstraintSet,
    sizeConstraintSet,
    scsName,
    scsSizeConstraintSetId,
    scsSizeConstraints,

    -- * SizeConstraintSetSummary
    SizeConstraintSetSummary,
    sizeConstraintSetSummary,
    scssSizeConstraintSetId,
    scssName,

    -- * SizeConstraintSetUpdate
    SizeConstraintSetUpdate,
    sizeConstraintSetUpdate,
    scsuAction,
    scsuSizeConstraint,

    -- * SqlInjectionMatchSet
    SqlInjectionMatchSet,
    sqlInjectionMatchSet,
    simsName,
    simsSqlInjectionMatchSetId,
    simsSqlInjectionMatchTuples,

    -- * SqlInjectionMatchSetSummary
    SqlInjectionMatchSetSummary,
    sqlInjectionMatchSetSummary,
    simssSqlInjectionMatchSetId,
    simssName,

    -- * SqlInjectionMatchSetUpdate
    SqlInjectionMatchSetUpdate,
    sqlInjectionMatchSetUpdate,
    simsuAction,
    simsuSqlInjectionMatchTuple,

    -- * SqlInjectionMatchTuple
    SqlInjectionMatchTuple,
    sqlInjectionMatchTuple,
    simtFieldToMatch,
    simtTextTransformation,

    -- * SubscribedRuleGroupSummary
    SubscribedRuleGroupSummary,
    subscribedRuleGroupSummary,
    srgsRuleGroupId,
    srgsName,
    srgsMetricName,

    -- * Tag
    Tag,
    tag,
    tagKey,
    tagValue,

    -- * TagInfoForResource
    TagInfoForResource,
    tagInfoForResource,
    tifrTagList,
    tifrResourceARN,

    -- * TimeWindow
    TimeWindow,
    timeWindow,
    twStartTime,
    twEndTime,

    -- * WafAction
    WafAction,
    wafAction,
    waType,

    -- * WafOverrideAction
    WafOverrideAction,
    wafOverrideAction,
    woaType,

    -- * WebACL
    WebACL,
    webACL,
    waMetricName,
    waName,
    waWebACLARN,
    waWebACLId,
    waDefaultAction,
    waRules,

    -- * WebACLSummary
    WebACLSummary,
    webACLSummary,
    wasWebACLId,
    wasName,

    -- * WebACLUpdate
    WebACLUpdate,
    webACLUpdate,
    wauAction,
    wauActivatedRule,

    -- * XSSMatchSet
    XSSMatchSet,
    xssMatchSet,
    xmsName,
    xmsXSSMatchSetId,
    xmsXSSMatchTuples,

    -- * XSSMatchSetSummary
    XSSMatchSetSummary,
    xssMatchSetSummary,
    xmssXSSMatchSetId,
    xmssName,

    -- * XSSMatchSetUpdate
    XSSMatchSetUpdate,
    xssMatchSetUpdate,
    xmsuAction,
    xmsuXSSMatchTuple,

    -- * XSSMatchTuple
    XSSMatchTuple,
    xssMatchTuple,
    xmtFieldToMatch,
    xmtTextTransformation,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
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
wAFRegional :: Service
wAFRegional =
  Service
    { _svcAbbrev = "WAFRegional",
      _svcSigner = v4,
      _svcPrefix = "waf-regional",
      _svcVersion = "2016-11-28",
      _svcEndpoint = defaultEndpoint wAFRegional,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "WAFRegional",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
