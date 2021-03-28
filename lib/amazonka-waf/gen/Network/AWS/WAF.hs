{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

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
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** WAFInvalidAccountException
    , _WAFInvalidAccountException

    -- ** WAFSubscriptionNotFoundException
    , _WAFSubscriptionNotFoundException

    -- ** WAFReferencedItemException
    , _WAFReferencedItemException

    -- ** WAFTagOperationException
    , _WAFTagOperationException

    -- ** WAFEntityMigrationException
    , _WAFEntityMigrationException

    -- ** WAFInvalidRegexPatternException
    , _WAFInvalidRegexPatternException

    -- ** WAFInvalidOperationException
    , _WAFInvalidOperationException

    -- ** WAFBadRequestException
    , _WAFBadRequestException

    -- ** WAFNonexistentItemException
    , _WAFNonexistentItemException

    -- ** WAFInvalidParameterException
    , _WAFInvalidParameterException

    -- ** WAFTagOperationInternalErrorException
    , _WAFTagOperationInternalErrorException

    -- ** WAFServiceLinkedRoleErrorException
    , _WAFServiceLinkedRoleErrorException

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

    -- ** ListActivatedRulesInRuleGroup (Paginated)
    , module Network.AWS.WAF.ListActivatedRulesInRuleGroup

    -- ** ListRateBasedRules (Paginated)
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

    -- ** DeleteLoggingConfiguration 
    , module Network.AWS.WAF.DeleteLoggingConfiguration

    -- ** CreateWebACL 
    , module Network.AWS.WAF.CreateWebACL

    -- ** GetGeoMatchSet 
    , module Network.AWS.WAF.GetGeoMatchSet

    -- ** PutLoggingConfiguration 
    , module Network.AWS.WAF.PutLoggingConfiguration

    -- ** ListTagsForResource 
    , module Network.AWS.WAF.ListTagsForResource

    -- ** ListByteMatchSets (Paginated)
    , module Network.AWS.WAF.ListByteMatchSets

    -- ** ListGeoMatchSets (Paginated)
    , module Network.AWS.WAF.ListGeoMatchSets

    -- ** GetLoggingConfiguration 
    , module Network.AWS.WAF.GetLoggingConfiguration

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

    -- ** DeleteXssMatchSet 
    , module Network.AWS.WAF.DeleteXssMatchSet

    -- ** UpdateXssMatchSet 
    , module Network.AWS.WAF.UpdateXssMatchSet

    -- ** CreateWebACLMigrationStack 
    , module Network.AWS.WAF.CreateWebACLMigrationStack

    -- ** ListXssMatchSets (Paginated)
    , module Network.AWS.WAF.ListXssMatchSets

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

    -- ** ListSubscribedRuleGroups (Paginated)
    , module Network.AWS.WAF.ListSubscribedRuleGroups

    -- ** CreateSqlInjectionMatchSet 
    , module Network.AWS.WAF.CreateSqlInjectionMatchSet

    -- ** GetXssMatchSet 
    , module Network.AWS.WAF.GetXssMatchSet

    -- ** CreateByteMatchSet 
    , module Network.AWS.WAF.CreateByteMatchSet

    -- ** UpdateByteMatchSet 
    , module Network.AWS.WAF.UpdateByteMatchSet

    -- ** DeleteByteMatchSet 
    , module Network.AWS.WAF.DeleteByteMatchSet

    -- ** PutPermissionPolicy 
    , module Network.AWS.WAF.PutPermissionPolicy

    -- ** ListLoggingConfigurations (Paginated)
    , module Network.AWS.WAF.ListLoggingConfigurations

    -- ** GetRateBasedRuleManagedKeys (Paginated)
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

    -- ** ListRegexMatchSets (Paginated)
    , module Network.AWS.WAF.ListRegexMatchSets

    -- ** CreateXssMatchSet 
    , module Network.AWS.WAF.CreateXssMatchSet

    -- ** DeleteGeoMatchSet 
    , module Network.AWS.WAF.DeleteGeoMatchSet

    -- ** UpdateGeoMatchSet 
    , module Network.AWS.WAF.UpdateGeoMatchSet

    -- ** GetByteMatchSet 
    , module Network.AWS.WAF.GetByteMatchSet

    -- ** GetPermissionPolicy 
    , module Network.AWS.WAF.GetPermissionPolicy

    -- ** ListRuleGroups (Paginated)
    , module Network.AWS.WAF.ListRuleGroups

    -- ** TagResource 
    , module Network.AWS.WAF.TagResource

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

    -- ** UntagResource 
    , module Network.AWS.WAF.UntagResource

    -- ** DeleteRegexPatternSet 
    , module Network.AWS.WAF.DeleteRegexPatternSet

    -- ** UpdateRegexPatternSet 
    , module Network.AWS.WAF.UpdateRegexPatternSet

    -- ** CreateSizeConstraintSet 
    , module Network.AWS.WAF.CreateSizeConstraintSet

    -- ** ListRegexPatternSets (Paginated)
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

    -- ** IPString
    , IPString (..)

    -- ** RegexMatchSet
    , RegexMatchSet (..)
    , mkRegexMatchSet
    , rmsName
    , rmsRegexMatchSetId
    , rmsRegexMatchTuples

    -- ** ExcludedRule
    , ExcludedRule (..)
    , mkExcludedRule
    , erRuleId

    -- ** SqlInjectionMatchSetSummary
    , SqlInjectionMatchSetSummary (..)
    , mkSqlInjectionMatchSetSummary
    , simssSqlInjectionMatchSetId
    , simssName

    -- ** HeaderValue
    , HeaderValue (..)

    -- ** RuleUpdate
    , RuleUpdate (..)
    , mkRuleUpdate
    , ruAction
    , ruPredicate

    -- ** WebACLUpdate
    , WebACLUpdate (..)
    , mkWebACLUpdate
    , wacluAction
    , wacluActivatedRule

    -- ** RegexPatternSet
    , RegexPatternSet (..)
    , mkRegexPatternSet
    , rpsRegexPatternSetId
    , rpsRegexPatternStrings
    , rpsName

    -- ** HTTPHeader
    , HTTPHeader (..)
    , mkHTTPHeader
    , httphName
    , httphValue

    -- ** IPSetSummary
    , IPSetSummary (..)
    , mkIPSetSummary
    , ipssIPSetId
    , ipssName

    -- ** ResourceId
    , ResourceId (..)

    -- ** HTTPMethod
    , HTTPMethod (..)

    -- ** HTTPVersion
    , HTTPVersion (..)

    -- ** Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- ** WafOverrideActionType
    , WafOverrideActionType (..)

    -- ** ManagedKey
    , ManagedKey (..)

    -- ** SqlInjectionMatchTuple
    , SqlInjectionMatchTuple (..)
    , mkSqlInjectionMatchTuple
    , simtFieldToMatch
    , simtTextTransformation

    -- ** IPSetUpdate
    , IPSetUpdate (..)
    , mkIPSetUpdate
    , ipsuAction
    , ipsuIPSetDescriptor

    -- ** FieldToMatch
    , FieldToMatch (..)
    , mkFieldToMatch
    , ftmType
    , ftmData

    -- ** GeoMatchSetUpdate
    , GeoMatchSetUpdate (..)
    , mkGeoMatchSetUpdate
    , gmsuAction
    , gmsuGeoMatchConstraint

    -- ** RateKey
    , RateKey (..)

    -- ** TagInfoForResource
    , TagInfoForResource (..)
    , mkTagInfoForResource
    , tifrResourceARN
    , tifrTagList

    -- ** ResourceName
    , ResourceName (..)

    -- ** RateBasedRule
    , RateBasedRule (..)
    , mkRateBasedRule
    , rbrRuleId
    , rbrMatchPredicates
    , rbrRateKey
    , rbrRateLimit
    , rbrMetricName
    , rbrName

    -- ** RuleGroupSummary
    , RuleGroupSummary (..)
    , mkRuleGroupSummary
    , rgsRuleGroupId
    , rgsName

    -- ** ChangeAction
    , ChangeAction (..)

    -- ** RegexMatchSetSummary
    , RegexMatchSetSummary (..)
    , mkRegexMatchSetSummary
    , rmssRegexMatchSetId
    , rmssName

    -- ** SqlInjectionMatchSet
    , SqlInjectionMatchSet (..)
    , mkSqlInjectionMatchSet
    , simsSqlInjectionMatchSetId
    , simsSqlInjectionMatchTuples
    , simsName

    -- ** Country
    , Country (..)

    -- ** IPSetDescriptor
    , IPSetDescriptor (..)
    , mkIPSetDescriptor
    , ipsdType
    , ipsdValue

    -- ** RuleGroupUpdate
    , RuleGroupUpdate (..)
    , mkRuleGroupUpdate
    , rguAction
    , rguActivatedRule

    -- ** MetricName
    , MetricName (..)

    -- ** ByteMatchSet
    , ByteMatchSet (..)
    , mkByteMatchSet
    , bmsByteMatchSetId
    , bmsByteMatchTuples
    , bmsName

    -- ** SizeConstraintSetUpdate
    , SizeConstraintSetUpdate (..)
    , mkSizeConstraintSetUpdate
    , scsuAction
    , scsuSizeConstraint

    -- ** XssMatchTuple
    , XssMatchTuple (..)
    , mkXssMatchTuple
    , xmtFieldToMatch
    , xmtTextTransformation

    -- ** GeoMatchConstraint
    , GeoMatchConstraint (..)
    , mkGeoMatchConstraint
    , gmcType
    , gmcValue

    -- ** Action
    , Action (..)

    -- ** WafActionType
    , WafActionType (..)

    -- ** S3ObjectUrl
    , S3ObjectUrl (..)

    -- ** SizeConstraintSetSummary
    , SizeConstraintSetSummary (..)
    , mkSizeConstraintSetSummary
    , scssSizeConstraintSetId
    , scssName

    -- ** Rule
    , Rule (..)
    , mkRule
    , rRuleId
    , rPredicates
    , rMetricName
    , rName

    -- ** RegexPatternSetUpdate
    , RegexPatternSetUpdate (..)
    , mkRegexPatternSetUpdate
    , rpsuAction
    , rpsuRegexPatternString

    -- ** WafRuleType
    , WafRuleType (..)

    -- ** WebACL
    , WebACL (..)
    , mkWebACL
    , waclWebACLId
    , waclDefaultAction
    , waclRules
    , waclMetricName
    , waclName
    , waclWebACLArn

    -- ** HTTPRequest
    , HTTPRequest (..)
    , mkHTTPRequest
    , httprClientIP
    , httprCountry
    , httprHTTPVersion
    , httprHeaders
    , httprMethod
    , httprURI

    -- ** ComparisonOperator
    , ComparisonOperator (..)

    -- ** SubscribedRuleGroupSummary
    , SubscribedRuleGroupSummary (..)
    , mkSubscribedRuleGroupSummary
    , srgsRuleGroupId
    , srgsName
    , srgsMetricName

    -- ** Predicate
    , Predicate (..)
    , mkPredicate
    , pNegated
    , pType
    , pDataId

    -- ** XssMatchSet
    , XssMatchSet (..)
    , mkXssMatchSet
    , xmsXssMatchSetId
    , xmsXssMatchTuples
    , xmsName

    -- ** ByteMatchTuple
    , ByteMatchTuple (..)
    , mkByteMatchTuple
    , bmtFieldToMatch
    , bmtTargetString
    , bmtTextTransformation
    , bmtPositionalConstraint

    -- ** TimeWindow
    , TimeWindow (..)
    , mkTimeWindow
    , twStartTime
    , twEndTime

    -- ** TextTransformation
    , TextTransformation (..)

    -- ** GeoMatchSet
    , GeoMatchSet (..)
    , mkGeoMatchSet
    , gmsGeoMatchSetId
    , gmsGeoMatchConstraints
    , gmsName

    -- ** ByteMatchSetSummary
    , ByteMatchSetSummary (..)
    , mkByteMatchSetSummary
    , bmssByteMatchSetId
    , bmssName

    -- ** ResourceArn
    , ResourceArn (..)

    -- ** SizeConstraintSet
    , SizeConstraintSet (..)
    , mkSizeConstraintSet
    , scsSizeConstraintSetId
    , scsSizeConstraints
    , scsName

    -- ** WebACLSummary
    , WebACLSummary (..)
    , mkWebACLSummary
    , waclsWebACLId
    , waclsName

    -- ** ByteMatchSetUpdate
    , ByteMatchSetUpdate (..)
    , mkByteMatchSetUpdate
    , bmsuAction
    , bmsuByteMatchTuple

    -- ** WafOverrideAction
    , WafOverrideAction (..)
    , mkWafOverrideAction
    , woaType

    -- ** RuleSummary
    , RuleSummary (..)
    , mkRuleSummary
    , rsRuleId
    , rsName

    -- ** NextMarker
    , NextMarker (..)

    -- ** RegexMatchSetUpdate
    , RegexMatchSetUpdate (..)
    , mkRegexMatchSetUpdate
    , rmsuAction
    , rmsuRegexMatchTuple

    -- ** XssMatchSetSummary
    , XssMatchSetSummary (..)
    , mkXssMatchSetSummary
    , xmssXssMatchSetId
    , xmssName

    -- ** ChangeToken
    , ChangeToken (..)

    -- ** TagKey
    , TagKey (..)

    -- ** XssMatchSetUpdate
    , XssMatchSetUpdate (..)
    , mkXssMatchSetUpdate
    , xmsuAction
    , xmsuXssMatchTuple

    -- ** SizeConstraint
    , SizeConstraint (..)
    , mkSizeConstraint
    , scFieldToMatch
    , scTextTransformation
    , scComparisonOperator
    , scSize

    -- ** RegexPatternString
    , RegexPatternString (..)

    -- ** IPSetDescriptorType
    , IPSetDescriptorType (..)

    -- ** GeoMatchConstraintValue
    , GeoMatchConstraintValue (..)

    -- ** GeoMatchSetSummary
    , GeoMatchSetSummary (..)
    , mkGeoMatchSetSummary
    , gmssGeoMatchSetId
    , gmssName

    -- ** ActivatedRule
    , ActivatedRule (..)
    , mkActivatedRule
    , arPriority
    , arRuleId
    , arAction
    , arExcludedRules
    , arOverrideAction
    , arType

    -- ** LoggingConfiguration
    , LoggingConfiguration (..)
    , mkLoggingConfiguration
    , lcResourceArn
    , lcLogDestinationConfigs
    , lcRedactedFields

    -- ** SampledHTTPRequest
    , SampledHTTPRequest (..)
    , mkSampledHTTPRequest
    , shttprRequest
    , shttprWeight
    , shttprAction
    , shttprRuleWithinRuleGroup
    , shttprTimestamp

    -- ** RegexPatternSetSummary
    , RegexPatternSetSummary (..)
    , mkRegexPatternSetSummary
    , rpssRegexPatternSetId
    , rpssName

    -- ** GeoMatchConstraintType
    , GeoMatchConstraintType (..)

    -- ** IPSet
    , IPSet (..)
    , mkIPSet
    , ipsIPSetId
    , ipsIPSetDescriptors
    , ipsName

    -- ** RegexMatchTuple
    , RegexMatchTuple (..)
    , mkRegexMatchTuple
    , rmtFieldToMatch
    , rmtTextTransformation
    , rmtRegexPatternSetId

    -- ** WafAction
    , WafAction (..)
    , mkWafAction
    , waType

    -- ** S3BucketName
    , S3BucketName (..)

    -- ** PositionalConstraint
    , PositionalConstraint (..)

    -- ** SqlInjectionMatchSetUpdate
    , SqlInjectionMatchSetUpdate (..)
    , mkSqlInjectionMatchSetUpdate
    , simsuAction
    , simsuSqlInjectionMatchTuple

    -- ** PredicateType
    , PredicateType (..)

    -- ** ChangeTokenStatus
    , ChangeTokenStatus (..)

    -- ** RuleGroup
    , RuleGroup (..)
    , mkRuleGroup
    , rgRuleGroupId
    , rgMetricName
    , rgName

    -- ** MatchFieldType
    , MatchFieldType (..)

    -- ** Name
    , Name (..)

    -- ** RegexMatchSetId
    , RegexMatchSetId (..)

    -- ** RuleId
    , RuleId (..)

    -- ** IPSetId
    , IPSetId (..)

    -- ** SqlInjectionMatchSetId
    , SqlInjectionMatchSetId (..)

    -- ** WebAclId
    , WebAclId (..)

    -- ** RegexPatternSetId
    , RegexPatternSetId (..)

    -- ** Key
    , Key (..)

    -- ** Value
    , Value (..)

    -- ** Data
    , Data (..)

    -- ** ResourceARN
    , ResourceARN (..)

    -- ** Policy
    , Policy (..)

    -- ** WebACLArn
    , WebACLArn (..)

    -- ** URI
    , URI (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.WAF.Types
import Network.AWS.WAF.Waiters
import Network.AWS.WAF.ListActivatedRulesInRuleGroup
import Network.AWS.WAF.ListRateBasedRules
import Network.AWS.WAF.GetSizeConstraintSet
import Network.AWS.WAF.DeleteRateBasedRule
import Network.AWS.WAF.UpdateRateBasedRule
import Network.AWS.WAF.UpdateRule
import Network.AWS.WAF.DeleteRule
import Network.AWS.WAF.CreateIPSet
import Network.AWS.WAF.GetRuleGroup
import Network.AWS.WAF.GetChangeTokenStatus
import Network.AWS.WAF.DeleteWebACL
import Network.AWS.WAF.UpdateWebACL
import Network.AWS.WAF.ListWebACLs
import Network.AWS.WAF.ListRules
import Network.AWS.WAF.CreateRule
import Network.AWS.WAF.DeleteLoggingConfiguration
import Network.AWS.WAF.CreateWebACL
import Network.AWS.WAF.GetGeoMatchSet
import Network.AWS.WAF.PutLoggingConfiguration
import Network.AWS.WAF.ListTagsForResource
import Network.AWS.WAF.ListByteMatchSets
import Network.AWS.WAF.ListGeoMatchSets
import Network.AWS.WAF.GetLoggingConfiguration
import Network.AWS.WAF.CreateRuleGroup
import Network.AWS.WAF.DeleteRegexMatchSet
import Network.AWS.WAF.UpdateRegexMatchSet
import Network.AWS.WAF.GetIPSet
import Network.AWS.WAF.GetWebACL
import Network.AWS.WAF.GetRule
import Network.AWS.WAF.DeleteXssMatchSet
import Network.AWS.WAF.UpdateXssMatchSet
import Network.AWS.WAF.CreateWebACLMigrationStack
import Network.AWS.WAF.ListXssMatchSets
import Network.AWS.WAF.CreateGeoMatchSet
import Network.AWS.WAF.GetChangeToken
import Network.AWS.WAF.ListSizeConstraintSets
import Network.AWS.WAF.GetSampledRequests
import Network.AWS.WAF.GetSqlInjectionMatchSet
import Network.AWS.WAF.ListSubscribedRuleGroups
import Network.AWS.WAF.CreateSqlInjectionMatchSet
import Network.AWS.WAF.GetXssMatchSet
import Network.AWS.WAF.CreateByteMatchSet
import Network.AWS.WAF.UpdateByteMatchSet
import Network.AWS.WAF.DeleteByteMatchSet
import Network.AWS.WAF.PutPermissionPolicy
import Network.AWS.WAF.ListLoggingConfigurations
import Network.AWS.WAF.GetRateBasedRuleManagedKeys
import Network.AWS.WAF.DeletePermissionPolicy
import Network.AWS.WAF.GetRegexMatchSet
import Network.AWS.WAF.DeleteIPSet
import Network.AWS.WAF.UpdateIPSet
import Network.AWS.WAF.ListIPSets
import Network.AWS.WAF.ListRegexMatchSets
import Network.AWS.WAF.CreateXssMatchSet
import Network.AWS.WAF.DeleteGeoMatchSet
import Network.AWS.WAF.UpdateGeoMatchSet
import Network.AWS.WAF.GetByteMatchSet
import Network.AWS.WAF.GetPermissionPolicy
import Network.AWS.WAF.ListRuleGroups
import Network.AWS.WAF.TagResource
import Network.AWS.WAF.DeleteRuleGroup
import Network.AWS.WAF.UpdateRuleGroup
import Network.AWS.WAF.CreateRegexMatchSet
import Network.AWS.WAF.GetRateBasedRule
import Network.AWS.WAF.CreateRegexPatternSet
import Network.AWS.WAF.DeleteSizeConstraintSet
import Network.AWS.WAF.UpdateSizeConstraintSet
import Network.AWS.WAF.UntagResource
import Network.AWS.WAF.DeleteRegexPatternSet
import Network.AWS.WAF.UpdateRegexPatternSet
import Network.AWS.WAF.CreateSizeConstraintSet
import Network.AWS.WAF.ListRegexPatternSets
import Network.AWS.WAF.ListSqlInjectionMatchSets
import Network.AWS.WAF.GetRegexPatternSet
import Network.AWS.WAF.CreateRateBasedRule
import Network.AWS.WAF.DeleteSqlInjectionMatchSet
import Network.AWS.WAF.UpdateSqlInjectionMatchSet
import qualified Network.AWS.Prelude as Lude

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
