{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.WAF
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2015-08-24@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- This is the /AWS WAF Classic API Reference/ for using AWS WAF Classic
-- with Amazon CloudFront. The AWS WAF Classic actions and data types
-- listed in the reference are available for protecting Amazon CloudFront
-- distributions. You can use these actions and data types via the endpoint
-- /waf.amazonaws.com/. This guide is for developers who need detailed
-- information about the AWS WAF Classic API actions, data types, and
-- errors. For detailed information about AWS WAF Classic features and an
-- overview of how to use the AWS WAF Classic API, see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
module Network.AWS.WAF
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** WAFInvalidAccountException
    _WAFInvalidAccountException,

    -- ** WAFSubscriptionNotFoundException
    _WAFSubscriptionNotFoundException,

    -- ** WAFReferencedItemException
    _WAFReferencedItemException,

    -- ** WAFTagOperationException
    _WAFTagOperationException,

    -- ** WAFEntityMigrationException
    _WAFEntityMigrationException,

    -- ** WAFInvalidRegexPatternException
    _WAFInvalidRegexPatternException,

    -- ** WAFInvalidOperationException
    _WAFInvalidOperationException,

    -- ** WAFBadRequestException
    _WAFBadRequestException,

    -- ** WAFNonexistentItemException
    _WAFNonexistentItemException,

    -- ** WAFInvalidParameterException
    _WAFInvalidParameterException,

    -- ** WAFTagOperationInternalErrorException
    _WAFTagOperationInternalErrorException,

    -- ** WAFServiceLinkedRoleErrorException
    _WAFServiceLinkedRoleErrorException,

    -- ** WAFLimitsExceededException
    _WAFLimitsExceededException,

    -- ** WAFInvalidPermissionPolicyException
    _WAFInvalidPermissionPolicyException,

    -- ** WAFStaleDataException
    _WAFStaleDataException,

    -- ** WAFInternalErrorException
    _WAFInternalErrorException,

    -- ** WAFNonexistentContainerException
    _WAFNonexistentContainerException,

    -- ** WAFDisallowedNameException
    _WAFDisallowedNameException,

    -- ** WAFNonEmptyEntityException
    _WAFNonEmptyEntityException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListActivatedRulesInRuleGroup (Paginated)
    ListActivatedRulesInRuleGroup (ListActivatedRulesInRuleGroup'),
    newListActivatedRulesInRuleGroup,
    ListActivatedRulesInRuleGroupResponse (ListActivatedRulesInRuleGroupResponse'),
    newListActivatedRulesInRuleGroupResponse,

    -- ** ListRateBasedRules (Paginated)
    ListRateBasedRules (ListRateBasedRules'),
    newListRateBasedRules,
    ListRateBasedRulesResponse (ListRateBasedRulesResponse'),
    newListRateBasedRulesResponse,

    -- ** GetSizeConstraintSet
    GetSizeConstraintSet (GetSizeConstraintSet'),
    newGetSizeConstraintSet,
    GetSizeConstraintSetResponse (GetSizeConstraintSetResponse'),
    newGetSizeConstraintSetResponse,

    -- ** DeleteRateBasedRule
    DeleteRateBasedRule (DeleteRateBasedRule'),
    newDeleteRateBasedRule,
    DeleteRateBasedRuleResponse (DeleteRateBasedRuleResponse'),
    newDeleteRateBasedRuleResponse,

    -- ** UpdateRateBasedRule
    UpdateRateBasedRule (UpdateRateBasedRule'),
    newUpdateRateBasedRule,
    UpdateRateBasedRuleResponse (UpdateRateBasedRuleResponse'),
    newUpdateRateBasedRuleResponse,

    -- ** UpdateRule
    UpdateRule (UpdateRule'),
    newUpdateRule,
    UpdateRuleResponse (UpdateRuleResponse'),
    newUpdateRuleResponse,

    -- ** DeleteRule
    DeleteRule (DeleteRule'),
    newDeleteRule,
    DeleteRuleResponse (DeleteRuleResponse'),
    newDeleteRuleResponse,

    -- ** CreateIPSet
    CreateIPSet (CreateIPSet'),
    newCreateIPSet,
    CreateIPSetResponse (CreateIPSetResponse'),
    newCreateIPSetResponse,

    -- ** GetRuleGroup
    GetRuleGroup (GetRuleGroup'),
    newGetRuleGroup,
    GetRuleGroupResponse (GetRuleGroupResponse'),
    newGetRuleGroupResponse,

    -- ** GetChangeTokenStatus
    GetChangeTokenStatus (GetChangeTokenStatus'),
    newGetChangeTokenStatus,
    GetChangeTokenStatusResponse (GetChangeTokenStatusResponse'),
    newGetChangeTokenStatusResponse,

    -- ** DeleteWebACL
    DeleteWebACL (DeleteWebACL'),
    newDeleteWebACL,
    DeleteWebACLResponse (DeleteWebACLResponse'),
    newDeleteWebACLResponse,

    -- ** UpdateWebACL
    UpdateWebACL (UpdateWebACL'),
    newUpdateWebACL,
    UpdateWebACLResponse (UpdateWebACLResponse'),
    newUpdateWebACLResponse,

    -- ** ListWebACLs (Paginated)
    ListWebACLs (ListWebACLs'),
    newListWebACLs,
    ListWebACLsResponse (ListWebACLsResponse'),
    newListWebACLsResponse,

    -- ** ListRules (Paginated)
    ListRules (ListRules'),
    newListRules,
    ListRulesResponse (ListRulesResponse'),
    newListRulesResponse,

    -- ** CreateRule
    CreateRule (CreateRule'),
    newCreateRule,
    CreateRuleResponse (CreateRuleResponse'),
    newCreateRuleResponse,

    -- ** DeleteLoggingConfiguration
    DeleteLoggingConfiguration (DeleteLoggingConfiguration'),
    newDeleteLoggingConfiguration,
    DeleteLoggingConfigurationResponse (DeleteLoggingConfigurationResponse'),
    newDeleteLoggingConfigurationResponse,

    -- ** CreateWebACL
    CreateWebACL (CreateWebACL'),
    newCreateWebACL,
    CreateWebACLResponse (CreateWebACLResponse'),
    newCreateWebACLResponse,

    -- ** GetGeoMatchSet
    GetGeoMatchSet (GetGeoMatchSet'),
    newGetGeoMatchSet,
    GetGeoMatchSetResponse (GetGeoMatchSetResponse'),
    newGetGeoMatchSetResponse,

    -- ** PutLoggingConfiguration
    PutLoggingConfiguration (PutLoggingConfiguration'),
    newPutLoggingConfiguration,
    PutLoggingConfigurationResponse (PutLoggingConfigurationResponse'),
    newPutLoggingConfigurationResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListByteMatchSets (Paginated)
    ListByteMatchSets (ListByteMatchSets'),
    newListByteMatchSets,
    ListByteMatchSetsResponse (ListByteMatchSetsResponse'),
    newListByteMatchSetsResponse,

    -- ** ListGeoMatchSets (Paginated)
    ListGeoMatchSets (ListGeoMatchSets'),
    newListGeoMatchSets,
    ListGeoMatchSetsResponse (ListGeoMatchSetsResponse'),
    newListGeoMatchSetsResponse,

    -- ** GetLoggingConfiguration
    GetLoggingConfiguration (GetLoggingConfiguration'),
    newGetLoggingConfiguration,
    GetLoggingConfigurationResponse (GetLoggingConfigurationResponse'),
    newGetLoggingConfigurationResponse,

    -- ** CreateRuleGroup
    CreateRuleGroup (CreateRuleGroup'),
    newCreateRuleGroup,
    CreateRuleGroupResponse (CreateRuleGroupResponse'),
    newCreateRuleGroupResponse,

    -- ** DeleteRegexMatchSet
    DeleteRegexMatchSet (DeleteRegexMatchSet'),
    newDeleteRegexMatchSet,
    DeleteRegexMatchSetResponse (DeleteRegexMatchSetResponse'),
    newDeleteRegexMatchSetResponse,

    -- ** UpdateRegexMatchSet
    UpdateRegexMatchSet (UpdateRegexMatchSet'),
    newUpdateRegexMatchSet,
    UpdateRegexMatchSetResponse (UpdateRegexMatchSetResponse'),
    newUpdateRegexMatchSetResponse,

    -- ** GetIPSet
    GetIPSet (GetIPSet'),
    newGetIPSet,
    GetIPSetResponse (GetIPSetResponse'),
    newGetIPSetResponse,

    -- ** GetWebACL
    GetWebACL (GetWebACL'),
    newGetWebACL,
    GetWebACLResponse (GetWebACLResponse'),
    newGetWebACLResponse,

    -- ** GetRule
    GetRule (GetRule'),
    newGetRule,
    GetRuleResponse (GetRuleResponse'),
    newGetRuleResponse,

    -- ** DeleteXssMatchSet
    DeleteXssMatchSet (DeleteXssMatchSet'),
    newDeleteXssMatchSet,
    DeleteXssMatchSetResponse (DeleteXssMatchSetResponse'),
    newDeleteXssMatchSetResponse,

    -- ** UpdateXssMatchSet
    UpdateXssMatchSet (UpdateXssMatchSet'),
    newUpdateXssMatchSet,
    UpdateXssMatchSetResponse (UpdateXssMatchSetResponse'),
    newUpdateXssMatchSetResponse,

    -- ** CreateWebACLMigrationStack
    CreateWebACLMigrationStack (CreateWebACLMigrationStack'),
    newCreateWebACLMigrationStack,
    CreateWebACLMigrationStackResponse (CreateWebACLMigrationStackResponse'),
    newCreateWebACLMigrationStackResponse,

    -- ** ListXssMatchSets (Paginated)
    ListXssMatchSets (ListXssMatchSets'),
    newListXssMatchSets,
    ListXssMatchSetsResponse (ListXssMatchSetsResponse'),
    newListXssMatchSetsResponse,

    -- ** CreateGeoMatchSet
    CreateGeoMatchSet (CreateGeoMatchSet'),
    newCreateGeoMatchSet,
    CreateGeoMatchSetResponse (CreateGeoMatchSetResponse'),
    newCreateGeoMatchSetResponse,

    -- ** GetChangeToken
    GetChangeToken (GetChangeToken'),
    newGetChangeToken,
    GetChangeTokenResponse (GetChangeTokenResponse'),
    newGetChangeTokenResponse,

    -- ** ListSizeConstraintSets (Paginated)
    ListSizeConstraintSets (ListSizeConstraintSets'),
    newListSizeConstraintSets,
    ListSizeConstraintSetsResponse (ListSizeConstraintSetsResponse'),
    newListSizeConstraintSetsResponse,

    -- ** GetSampledRequests
    GetSampledRequests (GetSampledRequests'),
    newGetSampledRequests,
    GetSampledRequestsResponse (GetSampledRequestsResponse'),
    newGetSampledRequestsResponse,

    -- ** GetSqlInjectionMatchSet
    GetSqlInjectionMatchSet (GetSqlInjectionMatchSet'),
    newGetSqlInjectionMatchSet,
    GetSqlInjectionMatchSetResponse (GetSqlInjectionMatchSetResponse'),
    newGetSqlInjectionMatchSetResponse,

    -- ** ListSubscribedRuleGroups (Paginated)
    ListSubscribedRuleGroups (ListSubscribedRuleGroups'),
    newListSubscribedRuleGroups,
    ListSubscribedRuleGroupsResponse (ListSubscribedRuleGroupsResponse'),
    newListSubscribedRuleGroupsResponse,

    -- ** CreateSqlInjectionMatchSet
    CreateSqlInjectionMatchSet (CreateSqlInjectionMatchSet'),
    newCreateSqlInjectionMatchSet,
    CreateSqlInjectionMatchSetResponse (CreateSqlInjectionMatchSetResponse'),
    newCreateSqlInjectionMatchSetResponse,

    -- ** GetXssMatchSet
    GetXssMatchSet (GetXssMatchSet'),
    newGetXssMatchSet,
    GetXssMatchSetResponse (GetXssMatchSetResponse'),
    newGetXssMatchSetResponse,

    -- ** CreateByteMatchSet
    CreateByteMatchSet (CreateByteMatchSet'),
    newCreateByteMatchSet,
    CreateByteMatchSetResponse (CreateByteMatchSetResponse'),
    newCreateByteMatchSetResponse,

    -- ** UpdateByteMatchSet
    UpdateByteMatchSet (UpdateByteMatchSet'),
    newUpdateByteMatchSet,
    UpdateByteMatchSetResponse (UpdateByteMatchSetResponse'),
    newUpdateByteMatchSetResponse,

    -- ** DeleteByteMatchSet
    DeleteByteMatchSet (DeleteByteMatchSet'),
    newDeleteByteMatchSet,
    DeleteByteMatchSetResponse (DeleteByteMatchSetResponse'),
    newDeleteByteMatchSetResponse,

    -- ** PutPermissionPolicy
    PutPermissionPolicy (PutPermissionPolicy'),
    newPutPermissionPolicy,
    PutPermissionPolicyResponse (PutPermissionPolicyResponse'),
    newPutPermissionPolicyResponse,

    -- ** ListLoggingConfigurations (Paginated)
    ListLoggingConfigurations (ListLoggingConfigurations'),
    newListLoggingConfigurations,
    ListLoggingConfigurationsResponse (ListLoggingConfigurationsResponse'),
    newListLoggingConfigurationsResponse,

    -- ** GetRateBasedRuleManagedKeys (Paginated)
    GetRateBasedRuleManagedKeys (GetRateBasedRuleManagedKeys'),
    newGetRateBasedRuleManagedKeys,
    GetRateBasedRuleManagedKeysResponse (GetRateBasedRuleManagedKeysResponse'),
    newGetRateBasedRuleManagedKeysResponse,

    -- ** DeletePermissionPolicy
    DeletePermissionPolicy (DeletePermissionPolicy'),
    newDeletePermissionPolicy,
    DeletePermissionPolicyResponse (DeletePermissionPolicyResponse'),
    newDeletePermissionPolicyResponse,

    -- ** GetRegexMatchSet
    GetRegexMatchSet (GetRegexMatchSet'),
    newGetRegexMatchSet,
    GetRegexMatchSetResponse (GetRegexMatchSetResponse'),
    newGetRegexMatchSetResponse,

    -- ** DeleteIPSet
    DeleteIPSet (DeleteIPSet'),
    newDeleteIPSet,
    DeleteIPSetResponse (DeleteIPSetResponse'),
    newDeleteIPSetResponse,

    -- ** UpdateIPSet
    UpdateIPSet (UpdateIPSet'),
    newUpdateIPSet,
    UpdateIPSetResponse (UpdateIPSetResponse'),
    newUpdateIPSetResponse,

    -- ** ListIPSets (Paginated)
    ListIPSets (ListIPSets'),
    newListIPSets,
    ListIPSetsResponse (ListIPSetsResponse'),
    newListIPSetsResponse,

    -- ** ListRegexMatchSets (Paginated)
    ListRegexMatchSets (ListRegexMatchSets'),
    newListRegexMatchSets,
    ListRegexMatchSetsResponse (ListRegexMatchSetsResponse'),
    newListRegexMatchSetsResponse,

    -- ** CreateXssMatchSet
    CreateXssMatchSet (CreateXssMatchSet'),
    newCreateXssMatchSet,
    CreateXssMatchSetResponse (CreateXssMatchSetResponse'),
    newCreateXssMatchSetResponse,

    -- ** DeleteGeoMatchSet
    DeleteGeoMatchSet (DeleteGeoMatchSet'),
    newDeleteGeoMatchSet,
    DeleteGeoMatchSetResponse (DeleteGeoMatchSetResponse'),
    newDeleteGeoMatchSetResponse,

    -- ** UpdateGeoMatchSet
    UpdateGeoMatchSet (UpdateGeoMatchSet'),
    newUpdateGeoMatchSet,
    UpdateGeoMatchSetResponse (UpdateGeoMatchSetResponse'),
    newUpdateGeoMatchSetResponse,

    -- ** GetByteMatchSet
    GetByteMatchSet (GetByteMatchSet'),
    newGetByteMatchSet,
    GetByteMatchSetResponse (GetByteMatchSetResponse'),
    newGetByteMatchSetResponse,

    -- ** GetPermissionPolicy
    GetPermissionPolicy (GetPermissionPolicy'),
    newGetPermissionPolicy,
    GetPermissionPolicyResponse (GetPermissionPolicyResponse'),
    newGetPermissionPolicyResponse,

    -- ** ListRuleGroups (Paginated)
    ListRuleGroups (ListRuleGroups'),
    newListRuleGroups,
    ListRuleGroupsResponse (ListRuleGroupsResponse'),
    newListRuleGroupsResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** DeleteRuleGroup
    DeleteRuleGroup (DeleteRuleGroup'),
    newDeleteRuleGroup,
    DeleteRuleGroupResponse (DeleteRuleGroupResponse'),
    newDeleteRuleGroupResponse,

    -- ** UpdateRuleGroup
    UpdateRuleGroup (UpdateRuleGroup'),
    newUpdateRuleGroup,
    UpdateRuleGroupResponse (UpdateRuleGroupResponse'),
    newUpdateRuleGroupResponse,

    -- ** CreateRegexMatchSet
    CreateRegexMatchSet (CreateRegexMatchSet'),
    newCreateRegexMatchSet,
    CreateRegexMatchSetResponse (CreateRegexMatchSetResponse'),
    newCreateRegexMatchSetResponse,

    -- ** GetRateBasedRule
    GetRateBasedRule (GetRateBasedRule'),
    newGetRateBasedRule,
    GetRateBasedRuleResponse (GetRateBasedRuleResponse'),
    newGetRateBasedRuleResponse,

    -- ** CreateRegexPatternSet
    CreateRegexPatternSet (CreateRegexPatternSet'),
    newCreateRegexPatternSet,
    CreateRegexPatternSetResponse (CreateRegexPatternSetResponse'),
    newCreateRegexPatternSetResponse,

    -- ** DeleteSizeConstraintSet
    DeleteSizeConstraintSet (DeleteSizeConstraintSet'),
    newDeleteSizeConstraintSet,
    DeleteSizeConstraintSetResponse (DeleteSizeConstraintSetResponse'),
    newDeleteSizeConstraintSetResponse,

    -- ** UpdateSizeConstraintSet
    UpdateSizeConstraintSet (UpdateSizeConstraintSet'),
    newUpdateSizeConstraintSet,
    UpdateSizeConstraintSetResponse (UpdateSizeConstraintSetResponse'),
    newUpdateSizeConstraintSetResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** DeleteRegexPatternSet
    DeleteRegexPatternSet (DeleteRegexPatternSet'),
    newDeleteRegexPatternSet,
    DeleteRegexPatternSetResponse (DeleteRegexPatternSetResponse'),
    newDeleteRegexPatternSetResponse,

    -- ** UpdateRegexPatternSet
    UpdateRegexPatternSet (UpdateRegexPatternSet'),
    newUpdateRegexPatternSet,
    UpdateRegexPatternSetResponse (UpdateRegexPatternSetResponse'),
    newUpdateRegexPatternSetResponse,

    -- ** CreateSizeConstraintSet
    CreateSizeConstraintSet (CreateSizeConstraintSet'),
    newCreateSizeConstraintSet,
    CreateSizeConstraintSetResponse (CreateSizeConstraintSetResponse'),
    newCreateSizeConstraintSetResponse,

    -- ** ListRegexPatternSets (Paginated)
    ListRegexPatternSets (ListRegexPatternSets'),
    newListRegexPatternSets,
    ListRegexPatternSetsResponse (ListRegexPatternSetsResponse'),
    newListRegexPatternSetsResponse,

    -- ** ListSqlInjectionMatchSets (Paginated)
    ListSqlInjectionMatchSets (ListSqlInjectionMatchSets'),
    newListSqlInjectionMatchSets,
    ListSqlInjectionMatchSetsResponse (ListSqlInjectionMatchSetsResponse'),
    newListSqlInjectionMatchSetsResponse,

    -- ** GetRegexPatternSet
    GetRegexPatternSet (GetRegexPatternSet'),
    newGetRegexPatternSet,
    GetRegexPatternSetResponse (GetRegexPatternSetResponse'),
    newGetRegexPatternSetResponse,

    -- ** CreateRateBasedRule
    CreateRateBasedRule (CreateRateBasedRule'),
    newCreateRateBasedRule,
    CreateRateBasedRuleResponse (CreateRateBasedRuleResponse'),
    newCreateRateBasedRuleResponse,

    -- ** DeleteSqlInjectionMatchSet
    DeleteSqlInjectionMatchSet (DeleteSqlInjectionMatchSet'),
    newDeleteSqlInjectionMatchSet,
    DeleteSqlInjectionMatchSetResponse (DeleteSqlInjectionMatchSetResponse'),
    newDeleteSqlInjectionMatchSetResponse,

    -- ** UpdateSqlInjectionMatchSet
    UpdateSqlInjectionMatchSet (UpdateSqlInjectionMatchSet'),
    newUpdateSqlInjectionMatchSet,
    UpdateSqlInjectionMatchSetResponse (UpdateSqlInjectionMatchSetResponse'),
    newUpdateSqlInjectionMatchSetResponse,

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
    ActivatedRule (ActivatedRule'),
    newActivatedRule,

    -- ** ByteMatchSet
    ByteMatchSet (ByteMatchSet'),
    newByteMatchSet,

    -- ** ByteMatchSetSummary
    ByteMatchSetSummary (ByteMatchSetSummary'),
    newByteMatchSetSummary,

    -- ** ByteMatchSetUpdate
    ByteMatchSetUpdate (ByteMatchSetUpdate'),
    newByteMatchSetUpdate,

    -- ** ByteMatchTuple
    ByteMatchTuple (ByteMatchTuple'),
    newByteMatchTuple,

    -- ** ExcludedRule
    ExcludedRule (ExcludedRule'),
    newExcludedRule,

    -- ** FieldToMatch
    FieldToMatch (FieldToMatch'),
    newFieldToMatch,

    -- ** GeoMatchConstraint
    GeoMatchConstraint (GeoMatchConstraint'),
    newGeoMatchConstraint,

    -- ** GeoMatchSet
    GeoMatchSet (GeoMatchSet'),
    newGeoMatchSet,

    -- ** GeoMatchSetSummary
    GeoMatchSetSummary (GeoMatchSetSummary'),
    newGeoMatchSetSummary,

    -- ** GeoMatchSetUpdate
    GeoMatchSetUpdate (GeoMatchSetUpdate'),
    newGeoMatchSetUpdate,

    -- ** HTTPHeader
    HTTPHeader (HTTPHeader'),
    newHTTPHeader,

    -- ** HTTPRequest
    HTTPRequest (HTTPRequest'),
    newHTTPRequest,

    -- ** IPSet
    IPSet (IPSet'),
    newIPSet,

    -- ** IPSetDescriptor
    IPSetDescriptor (IPSetDescriptor'),
    newIPSetDescriptor,

    -- ** IPSetSummary
    IPSetSummary (IPSetSummary'),
    newIPSetSummary,

    -- ** IPSetUpdate
    IPSetUpdate (IPSetUpdate'),
    newIPSetUpdate,

    -- ** LoggingConfiguration
    LoggingConfiguration (LoggingConfiguration'),
    newLoggingConfiguration,

    -- ** Predicate
    Predicate (Predicate'),
    newPredicate,

    -- ** RateBasedRule
    RateBasedRule (RateBasedRule'),
    newRateBasedRule,

    -- ** RegexMatchSet
    RegexMatchSet (RegexMatchSet'),
    newRegexMatchSet,

    -- ** RegexMatchSetSummary
    RegexMatchSetSummary (RegexMatchSetSummary'),
    newRegexMatchSetSummary,

    -- ** RegexMatchSetUpdate
    RegexMatchSetUpdate (RegexMatchSetUpdate'),
    newRegexMatchSetUpdate,

    -- ** RegexMatchTuple
    RegexMatchTuple (RegexMatchTuple'),
    newRegexMatchTuple,

    -- ** RegexPatternSet
    RegexPatternSet (RegexPatternSet'),
    newRegexPatternSet,

    -- ** RegexPatternSetSummary
    RegexPatternSetSummary (RegexPatternSetSummary'),
    newRegexPatternSetSummary,

    -- ** RegexPatternSetUpdate
    RegexPatternSetUpdate (RegexPatternSetUpdate'),
    newRegexPatternSetUpdate,

    -- ** Rule
    Rule (Rule'),
    newRule,

    -- ** RuleGroup
    RuleGroup (RuleGroup'),
    newRuleGroup,

    -- ** RuleGroupSummary
    RuleGroupSummary (RuleGroupSummary'),
    newRuleGroupSummary,

    -- ** RuleGroupUpdate
    RuleGroupUpdate (RuleGroupUpdate'),
    newRuleGroupUpdate,

    -- ** RuleSummary
    RuleSummary (RuleSummary'),
    newRuleSummary,

    -- ** RuleUpdate
    RuleUpdate (RuleUpdate'),
    newRuleUpdate,

    -- ** SampledHTTPRequest
    SampledHTTPRequest (SampledHTTPRequest'),
    newSampledHTTPRequest,

    -- ** SizeConstraint
    SizeConstraint (SizeConstraint'),
    newSizeConstraint,

    -- ** SizeConstraintSet
    SizeConstraintSet (SizeConstraintSet'),
    newSizeConstraintSet,

    -- ** SizeConstraintSetSummary
    SizeConstraintSetSummary (SizeConstraintSetSummary'),
    newSizeConstraintSetSummary,

    -- ** SizeConstraintSetUpdate
    SizeConstraintSetUpdate (SizeConstraintSetUpdate'),
    newSizeConstraintSetUpdate,

    -- ** SqlInjectionMatchSet
    SqlInjectionMatchSet (SqlInjectionMatchSet'),
    newSqlInjectionMatchSet,

    -- ** SqlInjectionMatchSetSummary
    SqlInjectionMatchSetSummary (SqlInjectionMatchSetSummary'),
    newSqlInjectionMatchSetSummary,

    -- ** SqlInjectionMatchSetUpdate
    SqlInjectionMatchSetUpdate (SqlInjectionMatchSetUpdate'),
    newSqlInjectionMatchSetUpdate,

    -- ** SqlInjectionMatchTuple
    SqlInjectionMatchTuple (SqlInjectionMatchTuple'),
    newSqlInjectionMatchTuple,

    -- ** SubscribedRuleGroupSummary
    SubscribedRuleGroupSummary (SubscribedRuleGroupSummary'),
    newSubscribedRuleGroupSummary,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TagInfoForResource
    TagInfoForResource (TagInfoForResource'),
    newTagInfoForResource,

    -- ** TimeWindow
    TimeWindow (TimeWindow'),
    newTimeWindow,

    -- ** WafAction
    WafAction (WafAction'),
    newWafAction,

    -- ** WafOverrideAction
    WafOverrideAction (WafOverrideAction'),
    newWafOverrideAction,

    -- ** WebACL
    WebACL (WebACL'),
    newWebACL,

    -- ** WebACLSummary
    WebACLSummary (WebACLSummary'),
    newWebACLSummary,

    -- ** WebACLUpdate
    WebACLUpdate (WebACLUpdate'),
    newWebACLUpdate,

    -- ** XssMatchSet
    XssMatchSet (XssMatchSet'),
    newXssMatchSet,

    -- ** XssMatchSetSummary
    XssMatchSetSummary (XssMatchSetSummary'),
    newXssMatchSetSummary,

    -- ** XssMatchSetUpdate
    XssMatchSetUpdate (XssMatchSetUpdate'),
    newXssMatchSetUpdate,

    -- ** XssMatchTuple
    XssMatchTuple (XssMatchTuple'),
    newXssMatchTuple,
  )
where

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
import Network.AWS.WAF.CreateXssMatchSet
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
import Network.AWS.WAF.DeleteXssMatchSet
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
import Network.AWS.WAF.GetXssMatchSet
import Network.AWS.WAF.Lens
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
import Network.AWS.WAF.ListXssMatchSets
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
import Network.AWS.WAF.UpdateXssMatchSet
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
