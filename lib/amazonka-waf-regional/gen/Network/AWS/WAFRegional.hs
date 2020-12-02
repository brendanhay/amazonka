{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is the /AWS WAF Regional API Reference/ for using AWS WAF with Elastic Load Balancing (ELB) Application Load Balancers. The AWS WAF actions and data types listed in the reference are available for protecting Application Load Balancers. You can use these actions and data types by means of the endpoints listed in <http://docs.aws.amazon.com/general/latest/gr/rande.html#waf_region AWS Regions and Endpoints> . This guide is for developers who need detailed information about the AWS WAF API actions, data types, and errors. For detailed information about AWS WAF features and an overview of how to use the AWS WAF API, see the <http://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
--
--
module Network.AWS.WAFRegional
    (
    -- * Service Configuration
      wAFRegional

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

    -- ** WAFUnavailableEntityException
    , _WAFUnavailableEntityException

    -- ** WAFDisallowedNameException
    , _WAFDisallowedNameException

    -- ** WAFNonEmptyEntityException
    , _WAFNonEmptyEntityException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListActivatedRulesInRuleGroup
    , module Network.AWS.WAFRegional.ListActivatedRulesInRuleGroup

    -- ** ListRateBasedRules
    , module Network.AWS.WAFRegional.ListRateBasedRules

    -- ** GetSizeConstraintSet
    , module Network.AWS.WAFRegional.GetSizeConstraintSet

    -- ** DeleteRateBasedRule
    , module Network.AWS.WAFRegional.DeleteRateBasedRule

    -- ** UpdateRateBasedRule
    , module Network.AWS.WAFRegional.UpdateRateBasedRule

    -- ** UpdateRule
    , module Network.AWS.WAFRegional.UpdateRule

    -- ** DeleteRule
    , module Network.AWS.WAFRegional.DeleteRule

    -- ** CreateIPSet
    , module Network.AWS.WAFRegional.CreateIPSet

    -- ** GetRuleGroup
    , module Network.AWS.WAFRegional.GetRuleGroup

    -- ** GetChangeTokenStatus
    , module Network.AWS.WAFRegional.GetChangeTokenStatus

    -- ** DeleteWebACL
    , module Network.AWS.WAFRegional.DeleteWebACL

    -- ** UpdateWebACL
    , module Network.AWS.WAFRegional.UpdateWebACL

    -- ** ListWebACLs
    , module Network.AWS.WAFRegional.ListWebACLs

    -- ** ListRules
    , module Network.AWS.WAFRegional.ListRules

    -- ** CreateRule
    , module Network.AWS.WAFRegional.CreateRule

    -- ** CreateWebACL
    , module Network.AWS.WAFRegional.CreateWebACL

    -- ** GetGeoMatchSet
    , module Network.AWS.WAFRegional.GetGeoMatchSet

    -- ** ListByteMatchSets
    , module Network.AWS.WAFRegional.ListByteMatchSets

    -- ** ListGeoMatchSets
    , module Network.AWS.WAFRegional.ListGeoMatchSets

    -- ** CreateRuleGroup
    , module Network.AWS.WAFRegional.CreateRuleGroup

    -- ** DeleteRegexMatchSet
    , module Network.AWS.WAFRegional.DeleteRegexMatchSet

    -- ** UpdateRegexMatchSet
    , module Network.AWS.WAFRegional.UpdateRegexMatchSet

    -- ** GetIPSet
    , module Network.AWS.WAFRegional.GetIPSet

    -- ** GetWebACL
    , module Network.AWS.WAFRegional.GetWebACL

    -- ** GetRule
    , module Network.AWS.WAFRegional.GetRule

    -- ** DeleteXSSMatchSet
    , module Network.AWS.WAFRegional.DeleteXSSMatchSet

    -- ** UpdateXSSMatchSet
    , module Network.AWS.WAFRegional.UpdateXSSMatchSet

    -- ** ListXSSMatchSets
    , module Network.AWS.WAFRegional.ListXSSMatchSets

    -- ** CreateGeoMatchSet
    , module Network.AWS.WAFRegional.CreateGeoMatchSet

    -- ** GetChangeToken
    , module Network.AWS.WAFRegional.GetChangeToken

    -- ** ListSizeConstraintSets
    , module Network.AWS.WAFRegional.ListSizeConstraintSets

    -- ** ListResourcesForWebACL
    , module Network.AWS.WAFRegional.ListResourcesForWebACL

    -- ** GetSampledRequests
    , module Network.AWS.WAFRegional.GetSampledRequests

    -- ** GetSqlInjectionMatchSet
    , module Network.AWS.WAFRegional.GetSqlInjectionMatchSet

    -- ** GetWebACLForResource
    , module Network.AWS.WAFRegional.GetWebACLForResource

    -- ** DisassociateWebACL
    , module Network.AWS.WAFRegional.DisassociateWebACL

    -- ** ListSubscribedRuleGroups
    , module Network.AWS.WAFRegional.ListSubscribedRuleGroups

    -- ** CreateSqlInjectionMatchSet
    , module Network.AWS.WAFRegional.CreateSqlInjectionMatchSet

    -- ** GetXSSMatchSet
    , module Network.AWS.WAFRegional.GetXSSMatchSet

    -- ** CreateByteMatchSet
    , module Network.AWS.WAFRegional.CreateByteMatchSet

    -- ** UpdateByteMatchSet
    , module Network.AWS.WAFRegional.UpdateByteMatchSet

    -- ** DeleteByteMatchSet
    , module Network.AWS.WAFRegional.DeleteByteMatchSet

    -- ** PutPermissionPolicy
    , module Network.AWS.WAFRegional.PutPermissionPolicy

    -- ** GetRateBasedRuleManagedKeys
    , module Network.AWS.WAFRegional.GetRateBasedRuleManagedKeys

    -- ** AssociateWebACL
    , module Network.AWS.WAFRegional.AssociateWebACL

    -- ** DeletePermissionPolicy
    , module Network.AWS.WAFRegional.DeletePermissionPolicy

    -- ** GetRegexMatchSet
    , module Network.AWS.WAFRegional.GetRegexMatchSet

    -- ** DeleteIPSet
    , module Network.AWS.WAFRegional.DeleteIPSet

    -- ** UpdateIPSet
    , module Network.AWS.WAFRegional.UpdateIPSet

    -- ** ListIPSets
    , module Network.AWS.WAFRegional.ListIPSets

    -- ** ListRegexMatchSets
    , module Network.AWS.WAFRegional.ListRegexMatchSets

    -- ** CreateXSSMatchSet
    , module Network.AWS.WAFRegional.CreateXSSMatchSet

    -- ** DeleteGeoMatchSet
    , module Network.AWS.WAFRegional.DeleteGeoMatchSet

    -- ** UpdateGeoMatchSet
    , module Network.AWS.WAFRegional.UpdateGeoMatchSet

    -- ** GetByteMatchSet
    , module Network.AWS.WAFRegional.GetByteMatchSet

    -- ** GetPermissionPolicy
    , module Network.AWS.WAFRegional.GetPermissionPolicy

    -- ** ListRuleGroups
    , module Network.AWS.WAFRegional.ListRuleGroups

    -- ** DeleteRuleGroup
    , module Network.AWS.WAFRegional.DeleteRuleGroup

    -- ** UpdateRuleGroup
    , module Network.AWS.WAFRegional.UpdateRuleGroup

    -- ** CreateRegexMatchSet
    , module Network.AWS.WAFRegional.CreateRegexMatchSet

    -- ** GetRateBasedRule
    , module Network.AWS.WAFRegional.GetRateBasedRule

    -- ** CreateRegexPatternSet
    , module Network.AWS.WAFRegional.CreateRegexPatternSet

    -- ** DeleteSizeConstraintSet
    , module Network.AWS.WAFRegional.DeleteSizeConstraintSet

    -- ** UpdateSizeConstraintSet
    , module Network.AWS.WAFRegional.UpdateSizeConstraintSet

    -- ** DeleteRegexPatternSet
    , module Network.AWS.WAFRegional.DeleteRegexPatternSet

    -- ** UpdateRegexPatternSet
    , module Network.AWS.WAFRegional.UpdateRegexPatternSet

    -- ** CreateSizeConstraintSet
    , module Network.AWS.WAFRegional.CreateSizeConstraintSet

    -- ** ListRegexPatternSets
    , module Network.AWS.WAFRegional.ListRegexPatternSets

    -- ** ListSqlInjectionMatchSets
    , module Network.AWS.WAFRegional.ListSqlInjectionMatchSets

    -- ** GetRegexPatternSet
    , module Network.AWS.WAFRegional.GetRegexPatternSet

    -- ** CreateRateBasedRule
    , module Network.AWS.WAFRegional.CreateRateBasedRule

    -- ** DeleteSqlInjectionMatchSet
    , module Network.AWS.WAFRegional.DeleteSqlInjectionMatchSet

    -- ** UpdateSqlInjectionMatchSet
    , module Network.AWS.WAFRegional.UpdateSqlInjectionMatchSet

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
import Network.AWS.WAFRegional.CreateXSSMatchSet
import Network.AWS.WAFRegional.DeleteByteMatchSet
import Network.AWS.WAFRegional.DeleteGeoMatchSet
import Network.AWS.WAFRegional.DeleteIPSet
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
import Network.AWS.WAFRegional.ListRateBasedRules
import Network.AWS.WAFRegional.ListRegexMatchSets
import Network.AWS.WAFRegional.ListRegexPatternSets
import Network.AWS.WAFRegional.ListResourcesForWebACL
import Network.AWS.WAFRegional.ListRuleGroups
import Network.AWS.WAFRegional.ListRules
import Network.AWS.WAFRegional.ListSizeConstraintSets
import Network.AWS.WAFRegional.ListSqlInjectionMatchSets
import Network.AWS.WAFRegional.ListSubscribedRuleGroups
import Network.AWS.WAFRegional.ListWebACLs
import Network.AWS.WAFRegional.ListXSSMatchSets
import Network.AWS.WAFRegional.PutPermissionPolicy
import Network.AWS.WAFRegional.Types
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

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'WAFRegional'.
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
