{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is the /AWS WAF Regional Classic API Reference/ for using AWS WAF Classic with the AWS resources, Elastic Load Balancing (ELB) Application Load Balancers and API Gateway APIs. The AWS WAF Classic actions and data types listed in the reference are available for protecting Elastic Load Balancing (ELB) Application Load Balancers and API Gateway APIs. You can use these actions and data types by means of the endpoints listed in <https://docs.aws.amazon.com/general/latest/gr/rande.html#waf_region AWS Regions and Endpoints> . This guide is for developers who need detailed information about the AWS WAF Classic API actions, data types, and errors. For detailed information about AWS WAF Classic features and an overview of how to use the AWS WAF Classic API, see the <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic> in the developer guide.
module Network.AWS.WAFRegional
  ( -- * Service configuration
    wAFRegionalService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListActivatedRulesInRuleGroup
    module Network.AWS.WAFRegional.ListActivatedRulesInRuleGroup,

    -- ** ListRateBasedRules
    module Network.AWS.WAFRegional.ListRateBasedRules,

    -- ** GetSizeConstraintSet
    module Network.AWS.WAFRegional.GetSizeConstraintSet,

    -- ** DeleteRateBasedRule
    module Network.AWS.WAFRegional.DeleteRateBasedRule,

    -- ** UpdateRateBasedRule
    module Network.AWS.WAFRegional.UpdateRateBasedRule,

    -- ** UpdateRule
    module Network.AWS.WAFRegional.UpdateRule,

    -- ** DeleteRule
    module Network.AWS.WAFRegional.DeleteRule,

    -- ** CreateIPSet
    module Network.AWS.WAFRegional.CreateIPSet,

    -- ** GetRuleGroup
    module Network.AWS.WAFRegional.GetRuleGroup,

    -- ** GetChangeTokenStatus
    module Network.AWS.WAFRegional.GetChangeTokenStatus,

    -- ** DeleteWebACL
    module Network.AWS.WAFRegional.DeleteWebACL,

    -- ** UpdateWebACL
    module Network.AWS.WAFRegional.UpdateWebACL,

    -- ** ListWebACLs
    module Network.AWS.WAFRegional.ListWebACLs,

    -- ** ListRules
    module Network.AWS.WAFRegional.ListRules,

    -- ** CreateRule
    module Network.AWS.WAFRegional.CreateRule,

    -- ** DeleteLoggingConfiguration
    module Network.AWS.WAFRegional.DeleteLoggingConfiguration,

    -- ** CreateWebACL
    module Network.AWS.WAFRegional.CreateWebACL,

    -- ** GetGeoMatchSet
    module Network.AWS.WAFRegional.GetGeoMatchSet,

    -- ** PutLoggingConfiguration
    module Network.AWS.WAFRegional.PutLoggingConfiguration,

    -- ** ListTagsForResource
    module Network.AWS.WAFRegional.ListTagsForResource,

    -- ** ListByteMatchSets
    module Network.AWS.WAFRegional.ListByteMatchSets,

    -- ** ListGeoMatchSets
    module Network.AWS.WAFRegional.ListGeoMatchSets,

    -- ** GetLoggingConfiguration
    module Network.AWS.WAFRegional.GetLoggingConfiguration,

    -- ** CreateRuleGroup
    module Network.AWS.WAFRegional.CreateRuleGroup,

    -- ** DeleteRegexMatchSet
    module Network.AWS.WAFRegional.DeleteRegexMatchSet,

    -- ** UpdateRegexMatchSet
    module Network.AWS.WAFRegional.UpdateRegexMatchSet,

    -- ** GetIPSet
    module Network.AWS.WAFRegional.GetIPSet,

    -- ** GetWebACL
    module Network.AWS.WAFRegional.GetWebACL,

    -- ** GetRule
    module Network.AWS.WAFRegional.GetRule,

    -- ** DeleteXSSMatchSet
    module Network.AWS.WAFRegional.DeleteXSSMatchSet,

    -- ** UpdateXSSMatchSet
    module Network.AWS.WAFRegional.UpdateXSSMatchSet,

    -- ** CreateWebACLMigrationStack
    module Network.AWS.WAFRegional.CreateWebACLMigrationStack,

    -- ** ListXSSMatchSets
    module Network.AWS.WAFRegional.ListXSSMatchSets,

    -- ** CreateGeoMatchSet
    module Network.AWS.WAFRegional.CreateGeoMatchSet,

    -- ** GetChangeToken
    module Network.AWS.WAFRegional.GetChangeToken,

    -- ** ListSizeConstraintSets
    module Network.AWS.WAFRegional.ListSizeConstraintSets,

    -- ** ListResourcesForWebACL
    module Network.AWS.WAFRegional.ListResourcesForWebACL,

    -- ** GetSampledRequests
    module Network.AWS.WAFRegional.GetSampledRequests,

    -- ** GetSqlInjectionMatchSet
    module Network.AWS.WAFRegional.GetSqlInjectionMatchSet,

    -- ** GetWebACLForResource
    module Network.AWS.WAFRegional.GetWebACLForResource,

    -- ** DisassociateWebACL
    module Network.AWS.WAFRegional.DisassociateWebACL,

    -- ** ListSubscribedRuleGroups
    module Network.AWS.WAFRegional.ListSubscribedRuleGroups,

    -- ** CreateSqlInjectionMatchSet
    module Network.AWS.WAFRegional.CreateSqlInjectionMatchSet,

    -- ** GetXSSMatchSet
    module Network.AWS.WAFRegional.GetXSSMatchSet,

    -- ** CreateByteMatchSet
    module Network.AWS.WAFRegional.CreateByteMatchSet,

    -- ** UpdateByteMatchSet
    module Network.AWS.WAFRegional.UpdateByteMatchSet,

    -- ** DeleteByteMatchSet
    module Network.AWS.WAFRegional.DeleteByteMatchSet,

    -- ** PutPermissionPolicy
    module Network.AWS.WAFRegional.PutPermissionPolicy,

    -- ** ListLoggingConfigurations
    module Network.AWS.WAFRegional.ListLoggingConfigurations,

    -- ** GetRateBasedRuleManagedKeys
    module Network.AWS.WAFRegional.GetRateBasedRuleManagedKeys,

    -- ** AssociateWebACL
    module Network.AWS.WAFRegional.AssociateWebACL,

    -- ** DeletePermissionPolicy
    module Network.AWS.WAFRegional.DeletePermissionPolicy,

    -- ** GetRegexMatchSet
    module Network.AWS.WAFRegional.GetRegexMatchSet,

    -- ** DeleteIPSet
    module Network.AWS.WAFRegional.DeleteIPSet,

    -- ** UpdateIPSet
    module Network.AWS.WAFRegional.UpdateIPSet,

    -- ** ListIPSets
    module Network.AWS.WAFRegional.ListIPSets,

    -- ** ListRegexMatchSets
    module Network.AWS.WAFRegional.ListRegexMatchSets,

    -- ** CreateXSSMatchSet
    module Network.AWS.WAFRegional.CreateXSSMatchSet,

    -- ** DeleteGeoMatchSet
    module Network.AWS.WAFRegional.DeleteGeoMatchSet,

    -- ** UpdateGeoMatchSet
    module Network.AWS.WAFRegional.UpdateGeoMatchSet,

    -- ** GetByteMatchSet
    module Network.AWS.WAFRegional.GetByteMatchSet,

    -- ** GetPermissionPolicy
    module Network.AWS.WAFRegional.GetPermissionPolicy,

    -- ** ListRuleGroups
    module Network.AWS.WAFRegional.ListRuleGroups,

    -- ** TagResource
    module Network.AWS.WAFRegional.TagResource,

    -- ** DeleteRuleGroup
    module Network.AWS.WAFRegional.DeleteRuleGroup,

    -- ** UpdateRuleGroup
    module Network.AWS.WAFRegional.UpdateRuleGroup,

    -- ** CreateRegexMatchSet
    module Network.AWS.WAFRegional.CreateRegexMatchSet,

    -- ** GetRateBasedRule
    module Network.AWS.WAFRegional.GetRateBasedRule,

    -- ** CreateRegexPatternSet
    module Network.AWS.WAFRegional.CreateRegexPatternSet,

    -- ** DeleteSizeConstraintSet
    module Network.AWS.WAFRegional.DeleteSizeConstraintSet,

    -- ** UpdateSizeConstraintSet
    module Network.AWS.WAFRegional.UpdateSizeConstraintSet,

    -- ** UntagResource
    module Network.AWS.WAFRegional.UntagResource,

    -- ** DeleteRegexPatternSet
    module Network.AWS.WAFRegional.DeleteRegexPatternSet,

    -- ** UpdateRegexPatternSet
    module Network.AWS.WAFRegional.UpdateRegexPatternSet,

    -- ** CreateSizeConstraintSet
    module Network.AWS.WAFRegional.CreateSizeConstraintSet,

    -- ** ListRegexPatternSets
    module Network.AWS.WAFRegional.ListRegexPatternSets,

    -- ** ListSqlInjectionMatchSets
    module Network.AWS.WAFRegional.ListSqlInjectionMatchSets,

    -- ** GetRegexPatternSet
    module Network.AWS.WAFRegional.GetRegexPatternSet,

    -- ** CreateRateBasedRule
    module Network.AWS.WAFRegional.CreateRateBasedRule,

    -- ** DeleteSqlInjectionMatchSet
    module Network.AWS.WAFRegional.DeleteSqlInjectionMatchSet,

    -- ** UpdateSqlInjectionMatchSet
    module Network.AWS.WAFRegional.UpdateSqlInjectionMatchSet,

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

    -- ** ResourceType
    ResourceType (..),

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
    Lude.ISO8601,
    Lude.Timestamp,
    Lude.UTCTime,
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.WAFRegional.AssociateWebACL
import Network.AWS.WAFRegional.CreateByteMatchSet
import Network.AWS.WAFRegional.CreateGeoMatchSet
import Network.AWS.WAFRegional.CreateIPSet
import Network.AWS.WAFRegional.CreateRateBasedRule
import Network.AWS.WAFRegional.CreateRegexMatchSet
import Network.AWS.WAFRegional.CreateRegexPatternSet
import Network.AWS.WAFRegional.CreateRule
import Network.AWS.WAFRegional.CreateRuleGroup
import Network.AWS.WAFRegional.CreateSizeConstraintSet
import Network.AWS.WAFRegional.CreateSqlInjectionMatchSet
import Network.AWS.WAFRegional.CreateWebACL
import Network.AWS.WAFRegional.CreateWebACLMigrationStack
import Network.AWS.WAFRegional.CreateXSSMatchSet
import Network.AWS.WAFRegional.DeleteByteMatchSet
import Network.AWS.WAFRegional.DeleteGeoMatchSet
import Network.AWS.WAFRegional.DeleteIPSet
import Network.AWS.WAFRegional.DeleteLoggingConfiguration
import Network.AWS.WAFRegional.DeletePermissionPolicy
import Network.AWS.WAFRegional.DeleteRateBasedRule
import Network.AWS.WAFRegional.DeleteRegexMatchSet
import Network.AWS.WAFRegional.DeleteRegexPatternSet
import Network.AWS.WAFRegional.DeleteRule
import Network.AWS.WAFRegional.DeleteRuleGroup
import Network.AWS.WAFRegional.DeleteSizeConstraintSet
import Network.AWS.WAFRegional.DeleteSqlInjectionMatchSet
import Network.AWS.WAFRegional.DeleteWebACL
import Network.AWS.WAFRegional.DeleteXSSMatchSet
import Network.AWS.WAFRegional.DisassociateWebACL
import Network.AWS.WAFRegional.GetByteMatchSet
import Network.AWS.WAFRegional.GetChangeToken
import Network.AWS.WAFRegional.GetChangeTokenStatus
import Network.AWS.WAFRegional.GetGeoMatchSet
import Network.AWS.WAFRegional.GetIPSet
import Network.AWS.WAFRegional.GetLoggingConfiguration
import Network.AWS.WAFRegional.GetPermissionPolicy
import Network.AWS.WAFRegional.GetRateBasedRule
import Network.AWS.WAFRegional.GetRateBasedRuleManagedKeys
import Network.AWS.WAFRegional.GetRegexMatchSet
import Network.AWS.WAFRegional.GetRegexPatternSet
import Network.AWS.WAFRegional.GetRule
import Network.AWS.WAFRegional.GetRuleGroup
import Network.AWS.WAFRegional.GetSampledRequests
import Network.AWS.WAFRegional.GetSizeConstraintSet
import Network.AWS.WAFRegional.GetSqlInjectionMatchSet
import Network.AWS.WAFRegional.GetWebACL
import Network.AWS.WAFRegional.GetWebACLForResource
import Network.AWS.WAFRegional.GetXSSMatchSet
import Network.AWS.WAFRegional.ListActivatedRulesInRuleGroup
import Network.AWS.WAFRegional.ListByteMatchSets
import Network.AWS.WAFRegional.ListGeoMatchSets
import Network.AWS.WAFRegional.ListIPSets
import Network.AWS.WAFRegional.ListLoggingConfigurations
import Network.AWS.WAFRegional.ListRateBasedRules
import Network.AWS.WAFRegional.ListRegexMatchSets
import Network.AWS.WAFRegional.ListRegexPatternSets
import Network.AWS.WAFRegional.ListResourcesForWebACL
import Network.AWS.WAFRegional.ListRuleGroups
import Network.AWS.WAFRegional.ListRules
import Network.AWS.WAFRegional.ListSizeConstraintSets
import Network.AWS.WAFRegional.ListSqlInjectionMatchSets
import Network.AWS.WAFRegional.ListSubscribedRuleGroups
import Network.AWS.WAFRegional.ListTagsForResource
import Network.AWS.WAFRegional.ListWebACLs
import Network.AWS.WAFRegional.ListXSSMatchSets
import Network.AWS.WAFRegional.PutLoggingConfiguration
import Network.AWS.WAFRegional.PutPermissionPolicy
import Network.AWS.WAFRegional.TagResource
import Network.AWS.WAFRegional.Types
import Network.AWS.WAFRegional.UntagResource
import Network.AWS.WAFRegional.UpdateByteMatchSet
import Network.AWS.WAFRegional.UpdateGeoMatchSet
import Network.AWS.WAFRegional.UpdateIPSet
import Network.AWS.WAFRegional.UpdateRateBasedRule
import Network.AWS.WAFRegional.UpdateRegexMatchSet
import Network.AWS.WAFRegional.UpdateRegexPatternSet
import Network.AWS.WAFRegional.UpdateRule
import Network.AWS.WAFRegional.UpdateRuleGroup
import Network.AWS.WAFRegional.UpdateSizeConstraintSet
import Network.AWS.WAFRegional.UpdateSqlInjectionMatchSet
import Network.AWS.WAFRegional.UpdateWebACL
import Network.AWS.WAFRegional.UpdateXSSMatchSet
import Network.AWS.WAFRegional.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'WAFRegional'.

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
