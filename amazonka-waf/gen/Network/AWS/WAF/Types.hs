{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAF.Types
    (
    -- * Service Configuration
      waf

    -- * Errors
    , _WAFInvalidAccountException
    , _WAFSubscriptionNotFoundException
    , _WAFReferencedItemException
    , _WAFInvalidRegexPatternException
    , _WAFInvalidOperationException
    , _WAFNonexistentItemException
    , _WAFInvalidParameterException
    , _WAFLimitsExceededException
    , _WAFInvalidPermissionPolicyException
    , _WAFStaleDataException
    , _WAFInternalErrorException
    , _WAFNonexistentContainerException
    , _WAFDisallowedNameException
    , _WAFNonEmptyEntityException

    -- * ChangeAction
    , ChangeAction (..)

    -- * ChangeTokenStatus
    , ChangeTokenStatus (..)

    -- * ComparisonOperator
    , ComparisonOperator (..)

    -- * GeoMatchConstraintType
    , GeoMatchConstraintType (..)

    -- * GeoMatchConstraintValue
    , GeoMatchConstraintValue (..)

    -- * IPSetDescriptorType
    , IPSetDescriptorType (..)

    -- * MatchFieldType
    , MatchFieldType (..)

    -- * PositionalConstraint
    , PositionalConstraint (..)

    -- * PredicateType
    , PredicateType (..)

    -- * RateKey
    , RateKey (..)

    -- * TextTransformation
    , TextTransformation (..)

    -- * WafActionType
    , WafActionType (..)

    -- * WafOverrideActionType
    , WafOverrideActionType (..)

    -- * WafRuleType
    , WafRuleType (..)

    -- * ActivatedRule
    , ActivatedRule
    , activatedRule
    , arOverrideAction
    , arAction
    , arType
    , arPriority
    , arRuleId

    -- * ByteMatchSet
    , ByteMatchSet
    , byteMatchSet
    , bmsName
    , bmsByteMatchSetId
    , bmsByteMatchTuples

    -- * ByteMatchSetSummary
    , ByteMatchSetSummary
    , byteMatchSetSummary
    , bmssByteMatchSetId
    , bmssName

    -- * ByteMatchSetUpdate
    , ByteMatchSetUpdate
    , byteMatchSetUpdate
    , bmsuAction
    , bmsuByteMatchTuple

    -- * ByteMatchTuple
    , ByteMatchTuple
    , byteMatchTuple
    , bmtFieldToMatch
    , bmtTargetString
    , bmtTextTransformation
    , bmtPositionalConstraint

    -- * FieldToMatch
    , FieldToMatch
    , fieldToMatch
    , ftmData
    , ftmType

    -- * GeoMatchConstraint
    , GeoMatchConstraint
    , geoMatchConstraint
    , gmcType
    , gmcValue

    -- * GeoMatchSet
    , GeoMatchSet
    , geoMatchSet
    , gmsName
    , gmsGeoMatchSetId
    , gmsGeoMatchConstraints

    -- * GeoMatchSetSummary
    , GeoMatchSetSummary
    , geoMatchSetSummary
    , gmssGeoMatchSetId
    , gmssName

    -- * GeoMatchSetUpdate
    , GeoMatchSetUpdate
    , geoMatchSetUpdate
    , gmsuAction
    , gmsuGeoMatchConstraint

    -- * HTTPHeader
    , HTTPHeader
    , hTTPHeader
    , httphValue
    , httphName

    -- * HTTPRequest
    , HTTPRequest
    , hTTPRequest
    , httprHTTPVersion
    , httprCountry
    , httprURI
    , httprHeaders
    , httprMethod
    , httprClientIP

    -- * IPSet
    , IPSet
    , ipSet
    , isName
    , isIPSetId
    , isIPSetDescriptors

    -- * IPSetDescriptor
    , IPSetDescriptor
    , ipSetDescriptor
    , isdType
    , isdValue

    -- * IPSetSummary
    , IPSetSummary
    , ipSetSummary
    , issIPSetId
    , issName

    -- * IPSetUpdate
    , IPSetUpdate
    , ipSetUpdate
    , isuAction
    , isuIPSetDescriptor

    -- * Predicate
    , Predicate
    , predicate
    , pNegated
    , pType
    , pDataId

    -- * RateBasedRule
    , RateBasedRule
    , rateBasedRule
    , rbrMetricName
    , rbrName
    , rbrRuleId
    , rbrMatchPredicates
    , rbrRateKey
    , rbrRateLimit

    -- * RegexMatchSet
    , RegexMatchSet
    , regexMatchSet
    , rmsName
    , rmsRegexMatchTuples
    , rmsRegexMatchSetId

    -- * RegexMatchSetSummary
    , RegexMatchSetSummary
    , regexMatchSetSummary
    , rmssRegexMatchSetId
    , rmssName

    -- * RegexMatchSetUpdate
    , RegexMatchSetUpdate
    , regexMatchSetUpdate
    , rmsuAction
    , rmsuRegexMatchTuple

    -- * RegexMatchTuple
    , RegexMatchTuple
    , regexMatchTuple
    , rmtFieldToMatch
    , rmtTextTransformation
    , rmtRegexPatternSetId

    -- * RegexPatternSet
    , RegexPatternSet
    , regexPatternSet
    , rpsName
    , rpsRegexPatternSetId
    , rpsRegexPatternStrings

    -- * RegexPatternSetSummary
    , RegexPatternSetSummary
    , regexPatternSetSummary
    , rpssRegexPatternSetId
    , rpssName

    -- * RegexPatternSetUpdate
    , RegexPatternSetUpdate
    , regexPatternSetUpdate
    , rpsuAction
    , rpsuRegexPatternString

    -- * Rule
    , Rule
    , rule
    , rMetricName
    , rName
    , rRuleId
    , rPredicates

    -- * RuleGroup
    , RuleGroup
    , ruleGroup
    , rgMetricName
    , rgName
    , rgRuleGroupId

    -- * RuleGroupSummary
    , RuleGroupSummary
    , ruleGroupSummary
    , rgsRuleGroupId
    , rgsName

    -- * RuleGroupUpdate
    , RuleGroupUpdate
    , ruleGroupUpdate
    , rguAction
    , rguActivatedRule

    -- * RuleSummary
    , RuleSummary
    , ruleSummary
    , rsRuleId
    , rsName

    -- * RuleUpdate
    , RuleUpdate
    , ruleUpdate
    , ruAction
    , ruPredicate

    -- * SampledHTTPRequest
    , SampledHTTPRequest
    , sampledHTTPRequest
    , shttprRuleWithinRuleGroup
    , shttprAction
    , shttprTimestamp
    , shttprRequest
    , shttprWeight

    -- * SizeConstraint
    , SizeConstraint
    , sizeConstraint
    , scFieldToMatch
    , scTextTransformation
    , scComparisonOperator
    , scSize

    -- * SizeConstraintSet
    , SizeConstraintSet
    , sizeConstraintSet
    , scsName
    , scsSizeConstraintSetId
    , scsSizeConstraints

    -- * SizeConstraintSetSummary
    , SizeConstraintSetSummary
    , sizeConstraintSetSummary
    , scssSizeConstraintSetId
    , scssName

    -- * SizeConstraintSetUpdate
    , SizeConstraintSetUpdate
    , sizeConstraintSetUpdate
    , scsuAction
    , scsuSizeConstraint

    -- * SqlInjectionMatchSet
    , SqlInjectionMatchSet
    , sqlInjectionMatchSet
    , simsName
    , simsSqlInjectionMatchSetId
    , simsSqlInjectionMatchTuples

    -- * SqlInjectionMatchSetSummary
    , SqlInjectionMatchSetSummary
    , sqlInjectionMatchSetSummary
    , simssSqlInjectionMatchSetId
    , simssName

    -- * SqlInjectionMatchSetUpdate
    , SqlInjectionMatchSetUpdate
    , sqlInjectionMatchSetUpdate
    , simsuAction
    , simsuSqlInjectionMatchTuple

    -- * SqlInjectionMatchTuple
    , SqlInjectionMatchTuple
    , sqlInjectionMatchTuple
    , simtFieldToMatch
    , simtTextTransformation

    -- * SubscribedRuleGroupSummary
    , SubscribedRuleGroupSummary
    , subscribedRuleGroupSummary
    , srgsRuleGroupId
    , srgsName
    , srgsMetricName

    -- * TimeWindow
    , TimeWindow
    , timeWindow
    , twStartTime
    , twEndTime

    -- * WafAction
    , WafAction
    , wafAction
    , waType

    -- * WafOverrideAction
    , WafOverrideAction
    , wafOverrideAction
    , woaType

    -- * WebACL
    , WebACL
    , webACL
    , waMetricName
    , waName
    , waWebACLId
    , waDefaultAction
    , waRules

    -- * WebACLSummary
    , WebACLSummary
    , webACLSummary
    , wasWebACLId
    , wasName

    -- * WebACLUpdate
    , WebACLUpdate
    , webACLUpdate
    , wauAction
    , wauActivatedRule

    -- * XSSMatchSet
    , XSSMatchSet
    , xssMatchSet
    , xmsName
    , xmsXSSMatchSetId
    , xmsXSSMatchTuples

    -- * XSSMatchSetSummary
    , XSSMatchSetSummary
    , xssMatchSetSummary
    , xmssXSSMatchSetId
    , xmssName

    -- * XSSMatchSetUpdate
    , XSSMatchSetUpdate
    , xssMatchSetUpdate
    , xmsuAction
    , xmsuXSSMatchTuple

    -- * XSSMatchTuple
    , XSSMatchTuple
    , xssMatchTuple
    , xmtFieldToMatch
    , xmtTextTransformation
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.WAF.Types.Product
import Network.AWS.WAF.Types.Sum

-- | API version @2015-08-24@ of the Amazon WAF SDK configuration.
waf :: Service
waf =
  Service
    { _svcAbbrev = "WAF"
    , _svcSigner = v4
    , _svcPrefix = "waf"
    , _svcVersion = "2015-08-24"
    , _svcEndpoint = defaultEndpoint waf
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "WAF"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | The operation failed because you tried to create, update, or delete an object by using an invalid account identifier.
--
--
_WAFInvalidAccountException :: AsError a => Getting (First ServiceError) a ServiceError
_WAFInvalidAccountException =
  _MatchServiceError waf "WAFInvalidAccountException"


-- | The specified subscription does not exist.
--
--
_WAFSubscriptionNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_WAFSubscriptionNotFoundException =
  _MatchServiceError waf "WAFSubscriptionNotFoundException"


-- | The operation failed because you tried to delete an object that is still in use. For example:
--
--
--     * You tried to delete a @ByteMatchSet@ that is still referenced by a @Rule@ .
--
--     * You tried to delete a @Rule@ that is still referenced by a @WebACL@ .
--
--
--
_WAFReferencedItemException :: AsError a => Getting (First ServiceError) a ServiceError
_WAFReferencedItemException =
  _MatchServiceError waf "WAFReferencedItemException"


-- | The regular expression (regex) you specified in @RegexPatternString@ is invalid.
--
--
_WAFInvalidRegexPatternException :: AsError a => Getting (First ServiceError) a ServiceError
_WAFInvalidRegexPatternException =
  _MatchServiceError waf "WAFInvalidRegexPatternException"


-- | The operation failed because there was nothing to do. For example:
--
--
--     * You tried to remove a @Rule@ from a @WebACL@ , but the @Rule@ isn't in the specified @WebACL@ .
--
--     * You tried to remove an IP address from an @IPSet@ , but the IP address isn't in the specified @IPSet@ .
--
--     * You tried to remove a @ByteMatchTuple@ from a @ByteMatchSet@ , but the @ByteMatchTuple@ isn't in the specified @WebACL@ .
--
--     * You tried to add a @Rule@ to a @WebACL@ , but the @Rule@ already exists in the specified @WebACL@ .
--
--     * You tried to add an IP address to an @IPSet@ , but the IP address already exists in the specified @IPSet@ .
--
--     * You tried to add a @ByteMatchTuple@ to a @ByteMatchSet@ , but the @ByteMatchTuple@ already exists in the specified @WebACL@ .
--
--
--
_WAFInvalidOperationException :: AsError a => Getting (First ServiceError) a ServiceError
_WAFInvalidOperationException =
  _MatchServiceError waf "WAFInvalidOperationException"


-- | The operation failed because the referenced object doesn't exist.
--
--
_WAFNonexistentItemException :: AsError a => Getting (First ServiceError) a ServiceError
_WAFNonexistentItemException =
  _MatchServiceError waf "WAFNonexistentItemException"


-- | The operation failed because AWS WAF didn't recognize a parameter in the request. For example:
--
--
--     * You specified an invalid parameter name.
--
--     * You specified an invalid value.
--
--     * You tried to update an object (@ByteMatchSet@ , @IPSet@ , @Rule@ , or @WebACL@ ) using an action other than @INSERT@ or @DELETE@ .
--
--     * You tried to create a @WebACL@ with a @DefaultAction@ @Type@ other than @ALLOW@ , @BLOCK@ , or @COUNT@ .
--
--     * You tried to create a @RateBasedRule@ with a @RateKey@ value other than @IP@ .
--
--     * You tried to update a @WebACL@ with a @WafAction@ @Type@ other than @ALLOW@ , @BLOCK@ , or @COUNT@ .
--
--     * You tried to update a @ByteMatchSet@ with a @FieldToMatch@ @Type@ other than HEADER, METHOD, QUERY_STRING, URI, or BODY.
--
--     * You tried to update a @ByteMatchSet@ with a @Field@ of @HEADER@ but no value for @Data@ .
--
--     * Your request references an ARN that is malformed, or corresponds to a resource with which a web ACL cannot be associated.
--
--
--
_WAFInvalidParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_WAFInvalidParameterException =
  _MatchServiceError waf "WAFInvalidParameterException"


-- | The operation exceeds a resource limit, for example, the maximum number of @WebACL@ objects that you can create for an AWS account. For more information, see <http://docs.aws.amazon.com/waf/latest/developerguide/limits.html Limits> in the /AWS WAF Developer Guide/ .
--
--
_WAFLimitsExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_WAFLimitsExceededException =
  _MatchServiceError waf "WAFLimitsExceededException"


-- | The operation failed because the specified policy is not in the proper format.
--
--
-- The policy is subject to the following restrictions:
--
--     * You can attach only one policy with each @PutPermissionPolicy@ request.
--
--     * The policy must include an @Effect@ , @Action@ and @Principal@ .
--
--     * @Effect@ must specify @Allow@ .
--
--     * The @Action@ in the policy must be @waf:UpdateWebACL@ or @waf-regional:UpdateWebACL@ . Any extra or wildcard actions in the policy will be rejected.
--
--     * The policy cannot include a @Resource@ parameter.
--
--     * The ARN in the request must be a valid WAF RuleGroup ARN and the RuleGroup must exist in the same region.
--
--     * The user making the request must be the owner of the RuleGroup.
--
--     * Your policy must be composed using IAM Policy version 2012-10-17.
--
--
--
_WAFInvalidPermissionPolicyException :: AsError a => Getting (First ServiceError) a ServiceError
_WAFInvalidPermissionPolicyException =
  _MatchServiceError waf "WAFInvalidPermissionPolicyException"


-- | The operation failed because you tried to create, update, or delete an object by using a change token that has already been used.
--
--
_WAFStaleDataException :: AsError a => Getting (First ServiceError) a ServiceError
_WAFStaleDataException = _MatchServiceError waf "WAFStaleDataException"


-- | The operation failed because of a system problem, even though the request was valid. Retry your request.
--
--
_WAFInternalErrorException :: AsError a => Getting (First ServiceError) a ServiceError
_WAFInternalErrorException = _MatchServiceError waf "WAFInternalErrorException"


-- | The operation failed because you tried to add an object to or delete an object from another object that doesn't exist. For example:
--
--
--     * You tried to add a @Rule@ to or delete a @Rule@ from a @WebACL@ that doesn't exist.
--
--     * You tried to add a @ByteMatchSet@ to or delete a @ByteMatchSet@ from a @Rule@ that doesn't exist.
--
--     * You tried to add an IP address to or delete an IP address from an @IPSet@ that doesn't exist.
--
--     * You tried to add a @ByteMatchTuple@ to or delete a @ByteMatchTuple@ from a @ByteMatchSet@ that doesn't exist.
--
--
--
_WAFNonexistentContainerException :: AsError a => Getting (First ServiceError) a ServiceError
_WAFNonexistentContainerException =
  _MatchServiceError waf "WAFNonexistentContainerException"


-- | The name specified is invalid.
--
--
_WAFDisallowedNameException :: AsError a => Getting (First ServiceError) a ServiceError
_WAFDisallowedNameException =
  _MatchServiceError waf "WAFDisallowedNameException"


-- | The operation failed because you tried to delete an object that isn't empty. For example:
--
--
--     * You tried to delete a @WebACL@ that still contains one or more @Rule@ objects.
--
--     * You tried to delete a @Rule@ that still contains one or more @ByteMatchSet@ objects or other predicates.
--
--     * You tried to delete a @ByteMatchSet@ that contains one or more @ByteMatchTuple@ objects.
--
--     * You tried to delete an @IPSet@ that references one or more IP addresses.
--
--
--
_WAFNonEmptyEntityException :: AsError a => Getting (First ServiceError) a ServiceError
_WAFNonEmptyEntityException =
  _MatchServiceError waf "WAFNonEmptyEntityException"

