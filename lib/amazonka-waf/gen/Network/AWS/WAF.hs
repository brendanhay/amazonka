{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is the /AWS WAF API Reference/ for using AWS WAF with Amazon CloudFront. The AWS WAF actions and data types listed in the reference are available for protecting Amazon CloudFront distributions. You can use these actions and data types via the endpoint /waf.amazonaws.com/ . This guide is for developers who need detailed information about the AWS WAF API actions, data types, and errors. For detailed information about AWS WAF features and an overview of how to use the AWS WAF API, see the <http://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
--
--
module Network.AWS.WAF
    (
    -- * Service Configuration
      waf

    -- * Errors
    -- $errors

    -- ** WAFInvalidAccountException
    , _WAFInvalidAccountException

    -- ** WAFSubscriptionNotFoundException
    , _WAFSubscriptionNotFoundException

    -- ** WAFReferencedItemException
    , _WAFReferencedItemException

    -- ** WAFInvalidRegexPatternException
    , _WAFInvalidRegexPatternException

    -- ** WAFInvalidOperationException
    , _WAFInvalidOperationException

    -- ** WAFNonexistentItemException
    , _WAFNonexistentItemException

    -- ** WAFInvalidParameterException
    , _WAFInvalidParameterException

    -- ** WAFLimitsExceededException
    , _WAFLimitsExceededException

    -- ** WAFInvalidPermissionPolicyException
    , _WAFInvalidPermissionPolicyException

    -- ** WAFStaleDataException
    , _WAFStaleDataException

    -- ** WAFInternalErrorException
    , _WAFInternalErrorException

    -- ** WAFNonexistentContainerException
    , _WAFNonexistentContainerException

    -- ** WAFDisallowedNameException
    , _WAFDisallowedNameException

    -- ** WAFNonEmptyEntityException
    , _WAFNonEmptyEntityException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListActivatedRulesInRuleGroup
    , module Network.AWS.WAF.ListActivatedRulesInRuleGroup

    -- ** ListRateBasedRules
    , module Network.AWS.WAF.ListRateBasedRules

    -- ** GetSizeConstraintSet
    , module Network.AWS.WAF.GetSizeConstraintSet

    -- ** DeleteRateBasedRule
    , module Network.AWS.WAF.DeleteRateBasedRule

    -- ** UpdateRateBasedRule
    , module Network.AWS.WAF.UpdateRateBasedRule

    -- ** UpdateRule
    , module Network.AWS.WAF.UpdateRule

    -- ** DeleteRule
    , module Network.AWS.WAF.DeleteRule

    -- ** CreateIPSet
    , module Network.AWS.WAF.CreateIPSet

    -- ** GetRuleGroup
    , module Network.AWS.WAF.GetRuleGroup

    -- ** GetChangeTokenStatus
    , module Network.AWS.WAF.GetChangeTokenStatus

    -- ** DeleteWebACL
    , module Network.AWS.WAF.DeleteWebACL

    -- ** UpdateWebACL
    , module Network.AWS.WAF.UpdateWebACL

    -- ** ListWebACLs (Paginated)
    , module Network.AWS.WAF.ListWebACLs

    -- ** ListRules (Paginated)
    , module Network.AWS.WAF.ListRules

    -- ** CreateRule
    , module Network.AWS.WAF.CreateRule

    -- ** CreateWebACL
    , module Network.AWS.WAF.CreateWebACL

    -- ** GetGeoMatchSet
    , module Network.AWS.WAF.GetGeoMatchSet

    -- ** ListByteMatchSets (Paginated)
    , module Network.AWS.WAF.ListByteMatchSets

    -- ** ListGeoMatchSets
    , module Network.AWS.WAF.ListGeoMatchSets

    -- ** CreateRuleGroup
    , module Network.AWS.WAF.CreateRuleGroup

    -- ** DeleteRegexMatchSet
    , module Network.AWS.WAF.DeleteRegexMatchSet

    -- ** UpdateRegexMatchSet
    , module Network.AWS.WAF.UpdateRegexMatchSet

    -- ** GetIPSet
    , module Network.AWS.WAF.GetIPSet

    -- ** GetWebACL
    , module Network.AWS.WAF.GetWebACL

    -- ** GetRule
    , module Network.AWS.WAF.GetRule

    -- ** DeleteXSSMatchSet
    , module Network.AWS.WAF.DeleteXSSMatchSet

    -- ** UpdateXSSMatchSet
    , module Network.AWS.WAF.UpdateXSSMatchSet

    -- ** ListXSSMatchSets (Paginated)
    , module Network.AWS.WAF.ListXSSMatchSets

    -- ** CreateGeoMatchSet
    , module Network.AWS.WAF.CreateGeoMatchSet

    -- ** GetChangeToken
    , module Network.AWS.WAF.GetChangeToken

    -- ** ListSizeConstraintSets (Paginated)
    , module Network.AWS.WAF.ListSizeConstraintSets

    -- ** GetSampledRequests
    , module Network.AWS.WAF.GetSampledRequests

    -- ** GetSqlInjectionMatchSet
    , module Network.AWS.WAF.GetSqlInjectionMatchSet

    -- ** ListSubscribedRuleGroups
    , module Network.AWS.WAF.ListSubscribedRuleGroups

    -- ** CreateSqlInjectionMatchSet
    , module Network.AWS.WAF.CreateSqlInjectionMatchSet

    -- ** GetXSSMatchSet
    , module Network.AWS.WAF.GetXSSMatchSet

    -- ** CreateByteMatchSet
    , module Network.AWS.WAF.CreateByteMatchSet

    -- ** UpdateByteMatchSet
    , module Network.AWS.WAF.UpdateByteMatchSet

    -- ** DeleteByteMatchSet
    , module Network.AWS.WAF.DeleteByteMatchSet

    -- ** PutPermissionPolicy
    , module Network.AWS.WAF.PutPermissionPolicy

    -- ** GetRateBasedRuleManagedKeys
    , module Network.AWS.WAF.GetRateBasedRuleManagedKeys

    -- ** DeletePermissionPolicy
    , module Network.AWS.WAF.DeletePermissionPolicy

    -- ** GetRegexMatchSet
    , module Network.AWS.WAF.GetRegexMatchSet

    -- ** DeleteIPSet
    , module Network.AWS.WAF.DeleteIPSet

    -- ** UpdateIPSet
    , module Network.AWS.WAF.UpdateIPSet

    -- ** ListIPSets (Paginated)
    , module Network.AWS.WAF.ListIPSets

    -- ** ListRegexMatchSets
    , module Network.AWS.WAF.ListRegexMatchSets

    -- ** CreateXSSMatchSet
    , module Network.AWS.WAF.CreateXSSMatchSet

    -- ** DeleteGeoMatchSet
    , module Network.AWS.WAF.DeleteGeoMatchSet

    -- ** UpdateGeoMatchSet
    , module Network.AWS.WAF.UpdateGeoMatchSet

    -- ** GetByteMatchSet
    , module Network.AWS.WAF.GetByteMatchSet

    -- ** GetPermissionPolicy
    , module Network.AWS.WAF.GetPermissionPolicy

    -- ** ListRuleGroups
    , module Network.AWS.WAF.ListRuleGroups

    -- ** DeleteRuleGroup
    , module Network.AWS.WAF.DeleteRuleGroup

    -- ** UpdateRuleGroup
    , module Network.AWS.WAF.UpdateRuleGroup

    -- ** CreateRegexMatchSet
    , module Network.AWS.WAF.CreateRegexMatchSet

    -- ** GetRateBasedRule
    , module Network.AWS.WAF.GetRateBasedRule

    -- ** CreateRegexPatternSet
    , module Network.AWS.WAF.CreateRegexPatternSet

    -- ** DeleteSizeConstraintSet
    , module Network.AWS.WAF.DeleteSizeConstraintSet

    -- ** UpdateSizeConstraintSet
    , module Network.AWS.WAF.UpdateSizeConstraintSet

    -- ** DeleteRegexPatternSet
    , module Network.AWS.WAF.DeleteRegexPatternSet

    -- ** UpdateRegexPatternSet
    , module Network.AWS.WAF.UpdateRegexPatternSet

    -- ** CreateSizeConstraintSet
    , module Network.AWS.WAF.CreateSizeConstraintSet

    -- ** ListRegexPatternSets
    , module Network.AWS.WAF.ListRegexPatternSets

    -- ** ListSqlInjectionMatchSets (Paginated)
    , module Network.AWS.WAF.ListSqlInjectionMatchSets

    -- ** GetRegexPatternSet
    , module Network.AWS.WAF.GetRegexPatternSet

    -- ** CreateRateBasedRule
    , module Network.AWS.WAF.CreateRateBasedRule

    -- ** DeleteSqlInjectionMatchSet
    , module Network.AWS.WAF.DeleteSqlInjectionMatchSet

    -- ** UpdateSqlInjectionMatchSet
    , module Network.AWS.WAF.UpdateSqlInjectionMatchSet

    -- * Types

    -- ** ChangeAction
    , ChangeAction (..)

    -- ** ChangeTokenStatus
    , ChangeTokenStatus (..)

    -- ** ComparisonOperator
    , ComparisonOperator (..)

    -- ** GeoMatchConstraintType
    , GeoMatchConstraintType (..)

    -- ** GeoMatchConstraintValue
    , GeoMatchConstraintValue (..)

    -- ** IPSetDescriptorType
    , IPSetDescriptorType (..)

    -- ** MatchFieldType
    , MatchFieldType (..)

    -- ** PositionalConstraint
    , PositionalConstraint (..)

    -- ** PredicateType
    , PredicateType (..)

    -- ** RateKey
    , RateKey (..)

    -- ** TextTransformation
    , TextTransformation (..)

    -- ** WafActionType
    , WafActionType (..)

    -- ** WafOverrideActionType
    , WafOverrideActionType (..)

    -- ** WafRuleType
    , WafRuleType (..)

    -- ** ActivatedRule
    , ActivatedRule
    , activatedRule
    , arOverrideAction
    , arAction
    , arType
    , arPriority
    , arRuleId

    -- ** ByteMatchSet
    , ByteMatchSet
    , byteMatchSet
    , bmsName
    , bmsByteMatchSetId
    , bmsByteMatchTuples

    -- ** ByteMatchSetSummary
    , ByteMatchSetSummary
    , byteMatchSetSummary
    , bmssByteMatchSetId
    , bmssName

    -- ** ByteMatchSetUpdate
    , ByteMatchSetUpdate
    , byteMatchSetUpdate
    , bmsuAction
    , bmsuByteMatchTuple

    -- ** ByteMatchTuple
    , ByteMatchTuple
    , byteMatchTuple
    , bmtFieldToMatch
    , bmtTargetString
    , bmtTextTransformation
    , bmtPositionalConstraint

    -- ** FieldToMatch
    , FieldToMatch
    , fieldToMatch
    , ftmData
    , ftmType

    -- ** GeoMatchConstraint
    , GeoMatchConstraint
    , geoMatchConstraint
    , gmcType
    , gmcValue

    -- ** GeoMatchSet
    , GeoMatchSet
    , geoMatchSet
    , gmsName
    , gmsGeoMatchSetId
    , gmsGeoMatchConstraints

    -- ** GeoMatchSetSummary
    , GeoMatchSetSummary
    , geoMatchSetSummary
    , gmssGeoMatchSetId
    , gmssName

    -- ** GeoMatchSetUpdate
    , GeoMatchSetUpdate
    , geoMatchSetUpdate
    , gmsuAction
    , gmsuGeoMatchConstraint

    -- ** HTTPHeader
    , HTTPHeader
    , hTTPHeader
    , httphValue
    , httphName

    -- ** HTTPRequest
    , HTTPRequest
    , hTTPRequest
    , httprHTTPVersion
    , httprCountry
    , httprURI
    , httprHeaders
    , httprMethod
    , httprClientIP

    -- ** IPSet
    , IPSet
    , ipSet
    , isName
    , isIPSetId
    , isIPSetDescriptors

    -- ** IPSetDescriptor
    , IPSetDescriptor
    , ipSetDescriptor
    , isdType
    , isdValue

    -- ** IPSetSummary
    , IPSetSummary
    , ipSetSummary
    , issIPSetId
    , issName

    -- ** IPSetUpdate
    , IPSetUpdate
    , ipSetUpdate
    , isuAction
    , isuIPSetDescriptor

    -- ** Predicate
    , Predicate
    , predicate
    , pNegated
    , pType
    , pDataId

    -- ** RateBasedRule
    , RateBasedRule
    , rateBasedRule
    , rbrMetricName
    , rbrName
    , rbrRuleId
    , rbrMatchPredicates
    , rbrRateKey
    , rbrRateLimit

    -- ** RegexMatchSet
    , RegexMatchSet
    , regexMatchSet
    , rmsName
    , rmsRegexMatchTuples
    , rmsRegexMatchSetId

    -- ** RegexMatchSetSummary
    , RegexMatchSetSummary
    , regexMatchSetSummary
    , rmssRegexMatchSetId
    , rmssName

    -- ** RegexMatchSetUpdate
    , RegexMatchSetUpdate
    , regexMatchSetUpdate
    , rmsuAction
    , rmsuRegexMatchTuple

    -- ** RegexMatchTuple
    , RegexMatchTuple
    , regexMatchTuple
    , rmtFieldToMatch
    , rmtTextTransformation
    , rmtRegexPatternSetId

    -- ** RegexPatternSet
    , RegexPatternSet
    , regexPatternSet
    , rpsName
    , rpsRegexPatternSetId
    , rpsRegexPatternStrings

    -- ** RegexPatternSetSummary
    , RegexPatternSetSummary
    , regexPatternSetSummary
    , rpssRegexPatternSetId
    , rpssName

    -- ** RegexPatternSetUpdate
    , RegexPatternSetUpdate
    , regexPatternSetUpdate
    , rpsuAction
    , rpsuRegexPatternString

    -- ** Rule
    , Rule
    , rule
    , rMetricName
    , rName
    , rRuleId
    , rPredicates

    -- ** RuleGroup
    , RuleGroup
    , ruleGroup
    , rgMetricName
    , rgName
    , rgRuleGroupId

    -- ** RuleGroupSummary
    , RuleGroupSummary
    , ruleGroupSummary
    , rgsRuleGroupId
    , rgsName

    -- ** RuleGroupUpdate
    , RuleGroupUpdate
    , ruleGroupUpdate
    , rguAction
    , rguActivatedRule

    -- ** RuleSummary
    , RuleSummary
    , ruleSummary
    , rsRuleId
    , rsName

    -- ** RuleUpdate
    , RuleUpdate
    , ruleUpdate
    , ruAction
    , ruPredicate

    -- ** SampledHTTPRequest
    , SampledHTTPRequest
    , sampledHTTPRequest
    , shttprRuleWithinRuleGroup
    , shttprAction
    , shttprTimestamp
    , shttprRequest
    , shttprWeight

    -- ** SizeConstraint
    , SizeConstraint
    , sizeConstraint
    , scFieldToMatch
    , scTextTransformation
    , scComparisonOperator
    , scSize

    -- ** SizeConstraintSet
    , SizeConstraintSet
    , sizeConstraintSet
    , scsName
    , scsSizeConstraintSetId
    , scsSizeConstraints

    -- ** SizeConstraintSetSummary
    , SizeConstraintSetSummary
    , sizeConstraintSetSummary
    , scssSizeConstraintSetId
    , scssName

    -- ** SizeConstraintSetUpdate
    , SizeConstraintSetUpdate
    , sizeConstraintSetUpdate
    , scsuAction
    , scsuSizeConstraint

    -- ** SqlInjectionMatchSet
    , SqlInjectionMatchSet
    , sqlInjectionMatchSet
    , simsName
    , simsSqlInjectionMatchSetId
    , simsSqlInjectionMatchTuples

    -- ** SqlInjectionMatchSetSummary
    , SqlInjectionMatchSetSummary
    , sqlInjectionMatchSetSummary
    , simssSqlInjectionMatchSetId
    , simssName

    -- ** SqlInjectionMatchSetUpdate
    , SqlInjectionMatchSetUpdate
    , sqlInjectionMatchSetUpdate
    , simsuAction
    , simsuSqlInjectionMatchTuple

    -- ** SqlInjectionMatchTuple
    , SqlInjectionMatchTuple
    , sqlInjectionMatchTuple
    , simtFieldToMatch
    , simtTextTransformation

    -- ** SubscribedRuleGroupSummary
    , SubscribedRuleGroupSummary
    , subscribedRuleGroupSummary
    , srgsRuleGroupId
    , srgsName
    , srgsMetricName

    -- ** TimeWindow
    , TimeWindow
    , timeWindow
    , twStartTime
    , twEndTime

    -- ** WafAction
    , WafAction
    , wafAction
    , waType

    -- ** WafOverrideAction
    , WafOverrideAction
    , wafOverrideAction
    , woaType

    -- ** WebACL
    , WebACL
    , webACL
    , waMetricName
    , waName
    , waWebACLId
    , waDefaultAction
    , waRules

    -- ** WebACLSummary
    , WebACLSummary
    , webACLSummary
    , wasWebACLId
    , wasName

    -- ** WebACLUpdate
    , WebACLUpdate
    , webACLUpdate
    , wauAction
    , wauActivatedRule

    -- ** XSSMatchSet
    , XSSMatchSet
    , xssMatchSet
    , xmsName
    , xmsXSSMatchSetId
    , xmsXSSMatchTuples

    -- ** XSSMatchSetSummary
    , XSSMatchSetSummary
    , xssMatchSetSummary
    , xmssXSSMatchSetId
    , xmssName

    -- ** XSSMatchSetUpdate
    , XSSMatchSetUpdate
    , xssMatchSetUpdate
    , xmsuAction
    , xmsuXSSMatchTuple

    -- ** XSSMatchTuple
    , XSSMatchTuple
    , xssMatchTuple
    , xmtFieldToMatch
    , xmtTextTransformation
    ) where

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
import Network.AWS.WAF.CreateXSSMatchSet
import Network.AWS.WAF.DeleteByteMatchSet
import Network.AWS.WAF.DeleteGeoMatchSet
import Network.AWS.WAF.DeleteIPSet
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
import Network.AWS.WAF.ListRateBasedRules
import Network.AWS.WAF.ListRegexMatchSets
import Network.AWS.WAF.ListRegexPatternSets
import Network.AWS.WAF.ListRuleGroups
import Network.AWS.WAF.ListRules
import Network.AWS.WAF.ListSizeConstraintSets
import Network.AWS.WAF.ListSqlInjectionMatchSets
import Network.AWS.WAF.ListSubscribedRuleGroups
import Network.AWS.WAF.ListWebACLs
import Network.AWS.WAF.ListXSSMatchSets
import Network.AWS.WAF.PutPermissionPolicy
import Network.AWS.WAF.Types
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

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'WAF'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
