{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

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

    -- ** DeleteLoggingConfiguration 
    , module Network.AWS.WAFRegional.DeleteLoggingConfiguration

    -- ** CreateWebACL 
    , module Network.AWS.WAFRegional.CreateWebACL

    -- ** GetGeoMatchSet 
    , module Network.AWS.WAFRegional.GetGeoMatchSet

    -- ** PutLoggingConfiguration 
    , module Network.AWS.WAFRegional.PutLoggingConfiguration

    -- ** ListTagsForResource 
    , module Network.AWS.WAFRegional.ListTagsForResource

    -- ** ListByteMatchSets 
    , module Network.AWS.WAFRegional.ListByteMatchSets

    -- ** ListGeoMatchSets 
    , module Network.AWS.WAFRegional.ListGeoMatchSets

    -- ** GetLoggingConfiguration 
    , module Network.AWS.WAFRegional.GetLoggingConfiguration

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

    -- ** DeleteXssMatchSet 
    , module Network.AWS.WAFRegional.DeleteXssMatchSet

    -- ** UpdateXssMatchSet 
    , module Network.AWS.WAFRegional.UpdateXssMatchSet

    -- ** CreateWebACLMigrationStack 
    , module Network.AWS.WAFRegional.CreateWebACLMigrationStack

    -- ** ListXssMatchSets 
    , module Network.AWS.WAFRegional.ListXssMatchSets

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

    -- ** GetXssMatchSet 
    , module Network.AWS.WAFRegional.GetXssMatchSet

    -- ** CreateByteMatchSet 
    , module Network.AWS.WAFRegional.CreateByteMatchSet

    -- ** UpdateByteMatchSet 
    , module Network.AWS.WAFRegional.UpdateByteMatchSet

    -- ** DeleteByteMatchSet 
    , module Network.AWS.WAFRegional.DeleteByteMatchSet

    -- ** PutPermissionPolicy 
    , module Network.AWS.WAFRegional.PutPermissionPolicy

    -- ** ListLoggingConfigurations 
    , module Network.AWS.WAFRegional.ListLoggingConfigurations

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

    -- ** CreateXssMatchSet 
    , module Network.AWS.WAFRegional.CreateXssMatchSet

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

    -- ** TagResource 
    , module Network.AWS.WAFRegional.TagResource

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

    -- ** UntagResource 
    , module Network.AWS.WAFRegional.UntagResource

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

    -- ** ResourceType
    , ResourceType (..)

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

    -- ** WebACLId
    , WebACLId (..)

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

import Network.AWS.WAFRegional.Types
import Network.AWS.WAFRegional.Waiters
import Network.AWS.WAFRegional.ListActivatedRulesInRuleGroup
import Network.AWS.WAFRegional.ListRateBasedRules
import Network.AWS.WAFRegional.GetSizeConstraintSet
import Network.AWS.WAFRegional.DeleteRateBasedRule
import Network.AWS.WAFRegional.UpdateRateBasedRule
import Network.AWS.WAFRegional.UpdateRule
import Network.AWS.WAFRegional.DeleteRule
import Network.AWS.WAFRegional.CreateIPSet
import Network.AWS.WAFRegional.GetRuleGroup
import Network.AWS.WAFRegional.GetChangeTokenStatus
import Network.AWS.WAFRegional.DeleteWebACL
import Network.AWS.WAFRegional.UpdateWebACL
import Network.AWS.WAFRegional.ListWebACLs
import Network.AWS.WAFRegional.ListRules
import Network.AWS.WAFRegional.CreateRule
import Network.AWS.WAFRegional.DeleteLoggingConfiguration
import Network.AWS.WAFRegional.CreateWebACL
import Network.AWS.WAFRegional.GetGeoMatchSet
import Network.AWS.WAFRegional.PutLoggingConfiguration
import Network.AWS.WAFRegional.ListTagsForResource
import Network.AWS.WAFRegional.ListByteMatchSets
import Network.AWS.WAFRegional.ListGeoMatchSets
import Network.AWS.WAFRegional.GetLoggingConfiguration
import Network.AWS.WAFRegional.CreateRuleGroup
import Network.AWS.WAFRegional.DeleteRegexMatchSet
import Network.AWS.WAFRegional.UpdateRegexMatchSet
import Network.AWS.WAFRegional.GetIPSet
import Network.AWS.WAFRegional.GetWebACL
import Network.AWS.WAFRegional.GetRule
import Network.AWS.WAFRegional.DeleteXssMatchSet
import Network.AWS.WAFRegional.UpdateXssMatchSet
import Network.AWS.WAFRegional.CreateWebACLMigrationStack
import Network.AWS.WAFRegional.ListXssMatchSets
import Network.AWS.WAFRegional.CreateGeoMatchSet
import Network.AWS.WAFRegional.GetChangeToken
import Network.AWS.WAFRegional.ListSizeConstraintSets
import Network.AWS.WAFRegional.ListResourcesForWebACL
import Network.AWS.WAFRegional.GetSampledRequests
import Network.AWS.WAFRegional.GetSqlInjectionMatchSet
import Network.AWS.WAFRegional.GetWebACLForResource
import Network.AWS.WAFRegional.DisassociateWebACL
import Network.AWS.WAFRegional.ListSubscribedRuleGroups
import Network.AWS.WAFRegional.CreateSqlInjectionMatchSet
import Network.AWS.WAFRegional.GetXssMatchSet
import Network.AWS.WAFRegional.CreateByteMatchSet
import Network.AWS.WAFRegional.UpdateByteMatchSet
import Network.AWS.WAFRegional.DeleteByteMatchSet
import Network.AWS.WAFRegional.PutPermissionPolicy
import Network.AWS.WAFRegional.ListLoggingConfigurations
import Network.AWS.WAFRegional.GetRateBasedRuleManagedKeys
import Network.AWS.WAFRegional.AssociateWebACL
import Network.AWS.WAFRegional.DeletePermissionPolicy
import Network.AWS.WAFRegional.GetRegexMatchSet
import Network.AWS.WAFRegional.DeleteIPSet
import Network.AWS.WAFRegional.UpdateIPSet
import Network.AWS.WAFRegional.ListIPSets
import Network.AWS.WAFRegional.ListRegexMatchSets
import Network.AWS.WAFRegional.CreateXssMatchSet
import Network.AWS.WAFRegional.DeleteGeoMatchSet
import Network.AWS.WAFRegional.UpdateGeoMatchSet
import Network.AWS.WAFRegional.GetByteMatchSet
import Network.AWS.WAFRegional.GetPermissionPolicy
import Network.AWS.WAFRegional.ListRuleGroups
import Network.AWS.WAFRegional.TagResource
import Network.AWS.WAFRegional.DeleteRuleGroup
import Network.AWS.WAFRegional.UpdateRuleGroup
import Network.AWS.WAFRegional.CreateRegexMatchSet
import Network.AWS.WAFRegional.GetRateBasedRule
import Network.AWS.WAFRegional.CreateRegexPatternSet
import Network.AWS.WAFRegional.DeleteSizeConstraintSet
import Network.AWS.WAFRegional.UpdateSizeConstraintSet
import Network.AWS.WAFRegional.UntagResource
import Network.AWS.WAFRegional.DeleteRegexPatternSet
import Network.AWS.WAFRegional.UpdateRegexPatternSet
import Network.AWS.WAFRegional.CreateSizeConstraintSet
import Network.AWS.WAFRegional.ListRegexPatternSets
import Network.AWS.WAFRegional.ListSqlInjectionMatchSets
import Network.AWS.WAFRegional.GetRegexPatternSet
import Network.AWS.WAFRegional.CreateRateBasedRule
import Network.AWS.WAFRegional.DeleteSqlInjectionMatchSet
import Network.AWS.WAFRegional.UpdateSqlInjectionMatchSet
import qualified Network.AWS.Prelude as Lude

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
