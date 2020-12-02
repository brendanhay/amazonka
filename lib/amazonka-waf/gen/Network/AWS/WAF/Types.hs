{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types
  ( -- * Service Configuration
    waf,

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
waf :: Service
waf =
  Service
    { _svcAbbrev = "WAF",
      _svcSigner = v4,
      _svcPrefix = "waf",
      _svcVersion = "2015-08-24",
      _svcEndpoint = defaultEndpoint waf,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "WAF",
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
