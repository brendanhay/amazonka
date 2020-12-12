{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is the /AWS WAF Classic API Reference/ for using AWS WAF Classic with Amazon CloudFront. The AWS WAF Classic actions and data types listed in the reference are available for protecting Amazon CloudFront distributions. You can use these actions and data types via the endpoint /waf.amazonaws.com/ . This guide is for developers who need detailed information about the AWS WAF Classic API actions, data types, and errors. For detailed information about AWS WAF Classic features and an overview of how to use the AWS WAF Classic API, see the <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic> in the developer guide.
module Network.AWS.WAF
  ( -- * Service configuration
    wafService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListActivatedRulesInRuleGroup (Paginated)
    module Network.AWS.WAF.ListActivatedRulesInRuleGroup,

    -- ** ListRateBasedRules (Paginated)
    module Network.AWS.WAF.ListRateBasedRules,

    -- ** GetSizeConstraintSet
    module Network.AWS.WAF.GetSizeConstraintSet,

    -- ** DeleteRateBasedRule
    module Network.AWS.WAF.DeleteRateBasedRule,

    -- ** UpdateRateBasedRule
    module Network.AWS.WAF.UpdateRateBasedRule,

    -- ** UpdateRule
    module Network.AWS.WAF.UpdateRule,

    -- ** DeleteRule
    module Network.AWS.WAF.DeleteRule,

    -- ** CreateIPSet
    module Network.AWS.WAF.CreateIPSet,

    -- ** GetRuleGroup
    module Network.AWS.WAF.GetRuleGroup,

    -- ** GetChangeTokenStatus
    module Network.AWS.WAF.GetChangeTokenStatus,

    -- ** DeleteWebACL
    module Network.AWS.WAF.DeleteWebACL,

    -- ** UpdateWebACL
    module Network.AWS.WAF.UpdateWebACL,

    -- ** ListWebACLs (Paginated)
    module Network.AWS.WAF.ListWebACLs,

    -- ** ListRules (Paginated)
    module Network.AWS.WAF.ListRules,

    -- ** CreateRule
    module Network.AWS.WAF.CreateRule,

    -- ** DeleteLoggingConfiguration
    module Network.AWS.WAF.DeleteLoggingConfiguration,

    -- ** CreateWebACL
    module Network.AWS.WAF.CreateWebACL,

    -- ** GetGeoMatchSet
    module Network.AWS.WAF.GetGeoMatchSet,

    -- ** PutLoggingConfiguration
    module Network.AWS.WAF.PutLoggingConfiguration,

    -- ** ListTagsForResource
    module Network.AWS.WAF.ListTagsForResource,

    -- ** ListByteMatchSets (Paginated)
    module Network.AWS.WAF.ListByteMatchSets,

    -- ** ListGeoMatchSets (Paginated)
    module Network.AWS.WAF.ListGeoMatchSets,

    -- ** GetLoggingConfiguration
    module Network.AWS.WAF.GetLoggingConfiguration,

    -- ** CreateRuleGroup
    module Network.AWS.WAF.CreateRuleGroup,

    -- ** DeleteRegexMatchSet
    module Network.AWS.WAF.DeleteRegexMatchSet,

    -- ** UpdateRegexMatchSet
    module Network.AWS.WAF.UpdateRegexMatchSet,

    -- ** GetIPSet
    module Network.AWS.WAF.GetIPSet,

    -- ** GetWebACL
    module Network.AWS.WAF.GetWebACL,

    -- ** GetRule
    module Network.AWS.WAF.GetRule,

    -- ** DeleteXSSMatchSet
    module Network.AWS.WAF.DeleteXSSMatchSet,

    -- ** UpdateXSSMatchSet
    module Network.AWS.WAF.UpdateXSSMatchSet,

    -- ** CreateWebACLMigrationStack
    module Network.AWS.WAF.CreateWebACLMigrationStack,

    -- ** ListXSSMatchSets (Paginated)
    module Network.AWS.WAF.ListXSSMatchSets,

    -- ** CreateGeoMatchSet
    module Network.AWS.WAF.CreateGeoMatchSet,

    -- ** GetChangeToken
    module Network.AWS.WAF.GetChangeToken,

    -- ** ListSizeConstraintSets (Paginated)
    module Network.AWS.WAF.ListSizeConstraintSets,

    -- ** GetSampledRequests
    module Network.AWS.WAF.GetSampledRequests,

    -- ** GetSqlInjectionMatchSet
    module Network.AWS.WAF.GetSqlInjectionMatchSet,

    -- ** ListSubscribedRuleGroups (Paginated)
    module Network.AWS.WAF.ListSubscribedRuleGroups,

    -- ** CreateSqlInjectionMatchSet
    module Network.AWS.WAF.CreateSqlInjectionMatchSet,

    -- ** GetXSSMatchSet
    module Network.AWS.WAF.GetXSSMatchSet,

    -- ** CreateByteMatchSet
    module Network.AWS.WAF.CreateByteMatchSet,

    -- ** UpdateByteMatchSet
    module Network.AWS.WAF.UpdateByteMatchSet,

    -- ** DeleteByteMatchSet
    module Network.AWS.WAF.DeleteByteMatchSet,

    -- ** PutPermissionPolicy
    module Network.AWS.WAF.PutPermissionPolicy,

    -- ** ListLoggingConfigurations (Paginated)
    module Network.AWS.WAF.ListLoggingConfigurations,

    -- ** GetRateBasedRuleManagedKeys (Paginated)
    module Network.AWS.WAF.GetRateBasedRuleManagedKeys,

    -- ** DeletePermissionPolicy
    module Network.AWS.WAF.DeletePermissionPolicy,

    -- ** GetRegexMatchSet
    module Network.AWS.WAF.GetRegexMatchSet,

    -- ** DeleteIPSet
    module Network.AWS.WAF.DeleteIPSet,

    -- ** UpdateIPSet
    module Network.AWS.WAF.UpdateIPSet,

    -- ** ListIPSets (Paginated)
    module Network.AWS.WAF.ListIPSets,

    -- ** ListRegexMatchSets (Paginated)
    module Network.AWS.WAF.ListRegexMatchSets,

    -- ** CreateXSSMatchSet
    module Network.AWS.WAF.CreateXSSMatchSet,

    -- ** DeleteGeoMatchSet
    module Network.AWS.WAF.DeleteGeoMatchSet,

    -- ** UpdateGeoMatchSet
    module Network.AWS.WAF.UpdateGeoMatchSet,

    -- ** GetByteMatchSet
    module Network.AWS.WAF.GetByteMatchSet,

    -- ** GetPermissionPolicy
    module Network.AWS.WAF.GetPermissionPolicy,

    -- ** ListRuleGroups (Paginated)
    module Network.AWS.WAF.ListRuleGroups,

    -- ** TagResource
    module Network.AWS.WAF.TagResource,

    -- ** DeleteRuleGroup
    module Network.AWS.WAF.DeleteRuleGroup,

    -- ** UpdateRuleGroup
    module Network.AWS.WAF.UpdateRuleGroup,

    -- ** CreateRegexMatchSet
    module Network.AWS.WAF.CreateRegexMatchSet,

    -- ** GetRateBasedRule
    module Network.AWS.WAF.GetRateBasedRule,

    -- ** CreateRegexPatternSet
    module Network.AWS.WAF.CreateRegexPatternSet,

    -- ** DeleteSizeConstraintSet
    module Network.AWS.WAF.DeleteSizeConstraintSet,

    -- ** UpdateSizeConstraintSet
    module Network.AWS.WAF.UpdateSizeConstraintSet,

    -- ** UntagResource
    module Network.AWS.WAF.UntagResource,

    -- ** DeleteRegexPatternSet
    module Network.AWS.WAF.DeleteRegexPatternSet,

    -- ** UpdateRegexPatternSet
    module Network.AWS.WAF.UpdateRegexPatternSet,

    -- ** CreateSizeConstraintSet
    module Network.AWS.WAF.CreateSizeConstraintSet,

    -- ** ListRegexPatternSets (Paginated)
    module Network.AWS.WAF.ListRegexPatternSets,

    -- ** ListSqlInjectionMatchSets (Paginated)
    module Network.AWS.WAF.ListSqlInjectionMatchSets,

    -- ** GetRegexPatternSet
    module Network.AWS.WAF.GetRegexPatternSet,

    -- ** CreateRateBasedRule
    module Network.AWS.WAF.CreateRateBasedRule,

    -- ** DeleteSqlInjectionMatchSet
    module Network.AWS.WAF.DeleteSqlInjectionMatchSet,

    -- ** UpdateSqlInjectionMatchSet
    module Network.AWS.WAF.UpdateSqlInjectionMatchSet,

    -- * Types

    -- ** ChangeAction
    ChangeAction (..),

    -- ** ChangeTokenStatus
    ChangeTokenStatus (..),

    -- ** ComparisonOperator
    ComparisonOperator (..),

    -- ** GeoMatchConstraintType
    GeoMatchConstraintType (..),

    -- ** GeoMatchConstraintValue
    GeoMatchConstraintValue (..),

    -- ** IPSetDescriptorType
    IPSetDescriptorType (..),

    -- ** MatchFieldType
    MatchFieldType (..),

    -- ** PositionalConstraint
    PositionalConstraint (..),

    -- ** PredicateType
    PredicateType (..),

    -- ** RateKey
    RateKey (..),

    -- ** TextTransformation
    TextTransformation (..),

    -- ** WafActionType
    WafActionType (..),

    -- ** WafOverrideActionType
    WafOverrideActionType (..),

    -- ** WafRuleType
    WafRuleType (..),

    -- ** ActivatedRule
    ActivatedRule (..),
    mkActivatedRule,
    arOverrideAction,
    arAction,
    arExcludedRules,
    arType,
    arPriority,
    arRuleId,

    -- ** ByteMatchSet
    ByteMatchSet (..),
    mkByteMatchSet,
    bmsName,
    bmsByteMatchSetId,
    bmsByteMatchTuples,

    -- ** ByteMatchSetSummary
    ByteMatchSetSummary (..),
    mkByteMatchSetSummary,
    bmssByteMatchSetId,
    bmssName,

    -- ** ByteMatchSetUpdate
    ByteMatchSetUpdate (..),
    mkByteMatchSetUpdate,
    bmsuAction,
    bmsuByteMatchTuple,

    -- ** ByteMatchTuple
    ByteMatchTuple (..),
    mkByteMatchTuple,
    bmtFieldToMatch,
    bmtTargetString,
    bmtTextTransformation,
    bmtPositionalConstraint,

    -- ** ExcludedRule
    ExcludedRule (..),
    mkExcludedRule,
    erRuleId,

    -- ** FieldToMatch
    FieldToMatch (..),
    mkFieldToMatch,
    ftmData,
    ftmType,

    -- ** GeoMatchConstraint
    GeoMatchConstraint (..),
    mkGeoMatchConstraint,
    gmcType,
    gmcValue,

    -- ** GeoMatchSet
    GeoMatchSet (..),
    mkGeoMatchSet,
    gmsName,
    gmsGeoMatchSetId,
    gmsGeoMatchConstraints,

    -- ** GeoMatchSetSummary
    GeoMatchSetSummary (..),
    mkGeoMatchSetSummary,
    gmssGeoMatchSetId,
    gmssName,

    -- ** GeoMatchSetUpdate
    GeoMatchSetUpdate (..),
    mkGeoMatchSetUpdate,
    gmsuAction,
    gmsuGeoMatchConstraint,

    -- ** HTTPHeader
    HTTPHeader (..),
    mkHTTPHeader,
    httphValue,
    httphName,

    -- ** HTTPRequest
    HTTPRequest (..),
    mkHTTPRequest,
    httprHTTPVersion,
    httprCountry,
    httprURI,
    httprHeaders,
    httprMethod,
    httprClientIP,

    -- ** IPSet
    IPSet (..),
    mkIPSet,
    isName,
    isIPSetId,
    isIPSetDescriptors,

    -- ** IPSetDescriptor
    IPSetDescriptor (..),
    mkIPSetDescriptor,
    isdType,
    isdValue,

    -- ** IPSetSummary
    IPSetSummary (..),
    mkIPSetSummary,
    issIPSetId,
    issName,

    -- ** IPSetUpdate
    IPSetUpdate (..),
    mkIPSetUpdate,
    isuAction,
    isuIPSetDescriptor,

    -- ** LoggingConfiguration
    LoggingConfiguration (..),
    mkLoggingConfiguration,
    lcRedactedFields,
    lcResourceARN,
    lcLogDestinationConfigs,

    -- ** Predicate
    Predicate (..),
    mkPredicate,
    pNegated,
    pType,
    pDataId,

    -- ** RateBasedRule
    RateBasedRule (..),
    mkRateBasedRule,
    rbrMetricName,
    rbrName,
    rbrRuleId,
    rbrMatchPredicates,
    rbrRateKey,
    rbrRateLimit,

    -- ** RegexMatchSet
    RegexMatchSet (..),
    mkRegexMatchSet,
    rmsName,
    rmsRegexMatchTuples,
    rmsRegexMatchSetId,

    -- ** RegexMatchSetSummary
    RegexMatchSetSummary (..),
    mkRegexMatchSetSummary,
    rmssRegexMatchSetId,
    rmssName,

    -- ** RegexMatchSetUpdate
    RegexMatchSetUpdate (..),
    mkRegexMatchSetUpdate,
    rmsuAction,
    rmsuRegexMatchTuple,

    -- ** RegexMatchTuple
    RegexMatchTuple (..),
    mkRegexMatchTuple,
    rmtFieldToMatch,
    rmtTextTransformation,
    rmtRegexPatternSetId,

    -- ** RegexPatternSet
    RegexPatternSet (..),
    mkRegexPatternSet,
    rpsName,
    rpsRegexPatternSetId,
    rpsRegexPatternStrings,

    -- ** RegexPatternSetSummary
    RegexPatternSetSummary (..),
    mkRegexPatternSetSummary,
    rpssRegexPatternSetId,
    rpssName,

    -- ** RegexPatternSetUpdate
    RegexPatternSetUpdate (..),
    mkRegexPatternSetUpdate,
    rpsuAction,
    rpsuRegexPatternString,

    -- ** Rule
    Rule (..),
    mkRule,
    rMetricName,
    rName,
    rRuleId,
    rPredicates,

    -- ** RuleGroup
    RuleGroup (..),
    mkRuleGroup,
    rgMetricName,
    rgName,
    rgRuleGroupId,

    -- ** RuleGroupSummary
    RuleGroupSummary (..),
    mkRuleGroupSummary,
    rgsRuleGroupId,
    rgsName,

    -- ** RuleGroupUpdate
    RuleGroupUpdate (..),
    mkRuleGroupUpdate,
    rguAction,
    rguActivatedRule,

    -- ** RuleSummary
    RuleSummary (..),
    mkRuleSummary,
    rsRuleId,
    rsName,

    -- ** RuleUpdate
    RuleUpdate (..),
    mkRuleUpdate,
    ruAction,
    ruPredicate,

    -- ** SampledHTTPRequest
    SampledHTTPRequest (..),
    mkSampledHTTPRequest,
    shttprRuleWithinRuleGroup,
    shttprAction,
    shttprTimestamp,
    shttprRequest,
    shttprWeight,

    -- ** SizeConstraint
    SizeConstraint (..),
    mkSizeConstraint,
    scFieldToMatch,
    scTextTransformation,
    scComparisonOperator,
    scSize,

    -- ** SizeConstraintSet
    SizeConstraintSet (..),
    mkSizeConstraintSet,
    scsName,
    scsSizeConstraintSetId,
    scsSizeConstraints,

    -- ** SizeConstraintSetSummary
    SizeConstraintSetSummary (..),
    mkSizeConstraintSetSummary,
    scssSizeConstraintSetId,
    scssName,

    -- ** SizeConstraintSetUpdate
    SizeConstraintSetUpdate (..),
    mkSizeConstraintSetUpdate,
    scsuAction,
    scsuSizeConstraint,

    -- ** SqlInjectionMatchSet
    SqlInjectionMatchSet (..),
    mkSqlInjectionMatchSet,
    simsName,
    simsSqlInjectionMatchSetId,
    simsSqlInjectionMatchTuples,

    -- ** SqlInjectionMatchSetSummary
    SqlInjectionMatchSetSummary (..),
    mkSqlInjectionMatchSetSummary,
    simssSqlInjectionMatchSetId,
    simssName,

    -- ** SqlInjectionMatchSetUpdate
    SqlInjectionMatchSetUpdate (..),
    mkSqlInjectionMatchSetUpdate,
    simsuAction,
    simsuSqlInjectionMatchTuple,

    -- ** SqlInjectionMatchTuple
    SqlInjectionMatchTuple (..),
    mkSqlInjectionMatchTuple,
    simtFieldToMatch,
    simtTextTransformation,

    -- ** SubscribedRuleGroupSummary
    SubscribedRuleGroupSummary (..),
    mkSubscribedRuleGroupSummary,
    srgsRuleGroupId,
    srgsName,
    srgsMetricName,

    -- ** Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- ** TagInfoForResource
    TagInfoForResource (..),
    mkTagInfoForResource,
    tifrTagList,
    tifrResourceARN,

    -- ** TimeWindow
    TimeWindow (..),
    mkTimeWindow,
    twStartTime,
    twEndTime,

    -- ** WafAction
    WafAction (..),
    mkWafAction,
    waType,

    -- ** WafOverrideAction
    WafOverrideAction (..),
    mkWafOverrideAction,
    woaType,

    -- ** WebACL
    WebACL (..),
    mkWebACL,
    waMetricName,
    waName,
    waWebACLARN,
    waWebACLId,
    waDefaultAction,
    waRules,

    -- ** WebACLSummary
    WebACLSummary (..),
    mkWebACLSummary,
    wasWebACLId,
    wasName,

    -- ** WebACLUpdate
    WebACLUpdate (..),
    mkWebACLUpdate,
    wauAction,
    wauActivatedRule,

    -- ** XSSMatchSet
    XSSMatchSet (..),
    mkXSSMatchSet,
    xmsName,
    xmsXSSMatchSetId,
    xmsXSSMatchTuples,

    -- ** XSSMatchSetSummary
    XSSMatchSetSummary (..),
    mkXSSMatchSetSummary,
    xmssXSSMatchSetId,
    xmssName,

    -- ** XSSMatchSetUpdate
    XSSMatchSetUpdate (..),
    mkXSSMatchSetUpdate,
    xmsuAction,
    xmsuXSSMatchTuple,

    -- ** XSSMatchTuple
    XSSMatchTuple (..),
    mkXSSMatchTuple,
    xmtFieldToMatch,
    xmtTextTransformation,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.WAF.CreateByteMatchSet
import Network.AWS.WAF.CreateGeoMatchSet
import Network.AWS.WAF.CreateIPSet
import Network.AWS.WAF.CreateRateBasedRule
import Network.AWS.WAF.CreateRegexMatchSet
import Network.AWS.WAF.CreateRegexPatternSet
import Network.AWS.WAF.CreateRule
import Network.AWS.WAF.CreateRuleGroup
import Network.AWS.WAF.CreateSizeConstraintSet
import Network.AWS.WAF.CreateSqlInjectionMatchSet
import Network.AWS.WAF.CreateWebACL
import Network.AWS.WAF.CreateWebACLMigrationStack
import Network.AWS.WAF.CreateXSSMatchSet
import Network.AWS.WAF.DeleteByteMatchSet
import Network.AWS.WAF.DeleteGeoMatchSet
import Network.AWS.WAF.DeleteIPSet
import Network.AWS.WAF.DeleteLoggingConfiguration
import Network.AWS.WAF.DeletePermissionPolicy
import Network.AWS.WAF.DeleteRateBasedRule
import Network.AWS.WAF.DeleteRegexMatchSet
import Network.AWS.WAF.DeleteRegexPatternSet
import Network.AWS.WAF.DeleteRule
import Network.AWS.WAF.DeleteRuleGroup
import Network.AWS.WAF.DeleteSizeConstraintSet
import Network.AWS.WAF.DeleteSqlInjectionMatchSet
import Network.AWS.WAF.DeleteWebACL
import Network.AWS.WAF.DeleteXSSMatchSet
import Network.AWS.WAF.GetByteMatchSet
import Network.AWS.WAF.GetChangeToken
import Network.AWS.WAF.GetChangeTokenStatus
import Network.AWS.WAF.GetGeoMatchSet
import Network.AWS.WAF.GetIPSet
import Network.AWS.WAF.GetLoggingConfiguration
import Network.AWS.WAF.GetPermissionPolicy
import Network.AWS.WAF.GetRateBasedRule
import Network.AWS.WAF.GetRateBasedRuleManagedKeys
import Network.AWS.WAF.GetRegexMatchSet
import Network.AWS.WAF.GetRegexPatternSet
import Network.AWS.WAF.GetRule
import Network.AWS.WAF.GetRuleGroup
import Network.AWS.WAF.GetSampledRequests
import Network.AWS.WAF.GetSizeConstraintSet
import Network.AWS.WAF.GetSqlInjectionMatchSet
import Network.AWS.WAF.GetWebACL
import Network.AWS.WAF.GetXSSMatchSet
import Network.AWS.WAF.ListActivatedRulesInRuleGroup
import Network.AWS.WAF.ListByteMatchSets
import Network.AWS.WAF.ListGeoMatchSets
import Network.AWS.WAF.ListIPSets
import Network.AWS.WAF.ListLoggingConfigurations
import Network.AWS.WAF.ListRateBasedRules
import Network.AWS.WAF.ListRegexMatchSets
import Network.AWS.WAF.ListRegexPatternSets
import Network.AWS.WAF.ListRuleGroups
import Network.AWS.WAF.ListRules
import Network.AWS.WAF.ListSizeConstraintSets
import Network.AWS.WAF.ListSqlInjectionMatchSets
import Network.AWS.WAF.ListSubscribedRuleGroups
import Network.AWS.WAF.ListTagsForResource
import Network.AWS.WAF.ListWebACLs
import Network.AWS.WAF.ListXSSMatchSets
import Network.AWS.WAF.PutLoggingConfiguration
import Network.AWS.WAF.PutPermissionPolicy
import Network.AWS.WAF.TagResource
import Network.AWS.WAF.Types
import Network.AWS.WAF.UntagResource
import Network.AWS.WAF.UpdateByteMatchSet
import Network.AWS.WAF.UpdateGeoMatchSet
import Network.AWS.WAF.UpdateIPSet
import Network.AWS.WAF.UpdateRateBasedRule
import Network.AWS.WAF.UpdateRegexMatchSet
import Network.AWS.WAF.UpdateRegexPatternSet
import Network.AWS.WAF.UpdateRule
import Network.AWS.WAF.UpdateRuleGroup
import Network.AWS.WAF.UpdateSizeConstraintSet
import Network.AWS.WAF.UpdateSqlInjectionMatchSet
import Network.AWS.WAF.UpdateWebACL
import Network.AWS.WAF.UpdateXSSMatchSet
import Network.AWS.WAF.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'WAF'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
