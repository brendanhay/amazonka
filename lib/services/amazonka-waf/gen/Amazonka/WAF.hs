{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.WAF
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.WAF
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** WAFBadRequestException
    _WAFBadRequestException,

    -- ** WAFDisallowedNameException
    _WAFDisallowedNameException,

    -- ** WAFEntityMigrationException
    _WAFEntityMigrationException,

    -- ** WAFInternalErrorException
    _WAFInternalErrorException,

    -- ** WAFInvalidAccountException
    _WAFInvalidAccountException,

    -- ** WAFInvalidOperationException
    _WAFInvalidOperationException,

    -- ** WAFInvalidParameterException
    _WAFInvalidParameterException,

    -- ** WAFInvalidPermissionPolicyException
    _WAFInvalidPermissionPolicyException,

    -- ** WAFInvalidRegexPatternException
    _WAFInvalidRegexPatternException,

    -- ** WAFLimitsExceededException
    _WAFLimitsExceededException,

    -- ** WAFNonEmptyEntityException
    _WAFNonEmptyEntityException,

    -- ** WAFNonexistentContainerException
    _WAFNonexistentContainerException,

    -- ** WAFNonexistentItemException
    _WAFNonexistentItemException,

    -- ** WAFReferencedItemException
    _WAFReferencedItemException,

    -- ** WAFServiceLinkedRoleErrorException
    _WAFServiceLinkedRoleErrorException,

    -- ** WAFStaleDataException
    _WAFStaleDataException,

    -- ** WAFSubscriptionNotFoundException
    _WAFSubscriptionNotFoundException,

    -- ** WAFTagOperationException
    _WAFTagOperationException,

    -- ** WAFTagOperationInternalErrorException
    _WAFTagOperationInternalErrorException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateByteMatchSet
    CreateByteMatchSet (CreateByteMatchSet'),
    newCreateByteMatchSet,
    CreateByteMatchSetResponse (CreateByteMatchSetResponse'),
    newCreateByteMatchSetResponse,

    -- ** CreateGeoMatchSet
    CreateGeoMatchSet (CreateGeoMatchSet'),
    newCreateGeoMatchSet,
    CreateGeoMatchSetResponse (CreateGeoMatchSetResponse'),
    newCreateGeoMatchSetResponse,

    -- ** CreateIPSet
    CreateIPSet (CreateIPSet'),
    newCreateIPSet,
    CreateIPSetResponse (CreateIPSetResponse'),
    newCreateIPSetResponse,

    -- ** CreateRateBasedRule
    CreateRateBasedRule (CreateRateBasedRule'),
    newCreateRateBasedRule,
    CreateRateBasedRuleResponse (CreateRateBasedRuleResponse'),
    newCreateRateBasedRuleResponse,

    -- ** CreateRegexMatchSet
    CreateRegexMatchSet (CreateRegexMatchSet'),
    newCreateRegexMatchSet,
    CreateRegexMatchSetResponse (CreateRegexMatchSetResponse'),
    newCreateRegexMatchSetResponse,

    -- ** CreateRegexPatternSet
    CreateRegexPatternSet (CreateRegexPatternSet'),
    newCreateRegexPatternSet,
    CreateRegexPatternSetResponse (CreateRegexPatternSetResponse'),
    newCreateRegexPatternSetResponse,

    -- ** CreateRule
    CreateRule (CreateRule'),
    newCreateRule,
    CreateRuleResponse (CreateRuleResponse'),
    newCreateRuleResponse,

    -- ** CreateRuleGroup
    CreateRuleGroup (CreateRuleGroup'),
    newCreateRuleGroup,
    CreateRuleGroupResponse (CreateRuleGroupResponse'),
    newCreateRuleGroupResponse,

    -- ** CreateSizeConstraintSet
    CreateSizeConstraintSet (CreateSizeConstraintSet'),
    newCreateSizeConstraintSet,
    CreateSizeConstraintSetResponse (CreateSizeConstraintSetResponse'),
    newCreateSizeConstraintSetResponse,

    -- ** CreateSqlInjectionMatchSet
    CreateSqlInjectionMatchSet (CreateSqlInjectionMatchSet'),
    newCreateSqlInjectionMatchSet,
    CreateSqlInjectionMatchSetResponse (CreateSqlInjectionMatchSetResponse'),
    newCreateSqlInjectionMatchSetResponse,

    -- ** CreateWebACL
    CreateWebACL (CreateWebACL'),
    newCreateWebACL,
    CreateWebACLResponse (CreateWebACLResponse'),
    newCreateWebACLResponse,

    -- ** CreateWebACLMigrationStack
    CreateWebACLMigrationStack (CreateWebACLMigrationStack'),
    newCreateWebACLMigrationStack,
    CreateWebACLMigrationStackResponse (CreateWebACLMigrationStackResponse'),
    newCreateWebACLMigrationStackResponse,

    -- ** CreateXssMatchSet
    CreateXssMatchSet (CreateXssMatchSet'),
    newCreateXssMatchSet,
    CreateXssMatchSetResponse (CreateXssMatchSetResponse'),
    newCreateXssMatchSetResponse,

    -- ** DeleteByteMatchSet
    DeleteByteMatchSet (DeleteByteMatchSet'),
    newDeleteByteMatchSet,
    DeleteByteMatchSetResponse (DeleteByteMatchSetResponse'),
    newDeleteByteMatchSetResponse,

    -- ** DeleteGeoMatchSet
    DeleteGeoMatchSet (DeleteGeoMatchSet'),
    newDeleteGeoMatchSet,
    DeleteGeoMatchSetResponse (DeleteGeoMatchSetResponse'),
    newDeleteGeoMatchSetResponse,

    -- ** DeleteIPSet
    DeleteIPSet (DeleteIPSet'),
    newDeleteIPSet,
    DeleteIPSetResponse (DeleteIPSetResponse'),
    newDeleteIPSetResponse,

    -- ** DeleteLoggingConfiguration
    DeleteLoggingConfiguration (DeleteLoggingConfiguration'),
    newDeleteLoggingConfiguration,
    DeleteLoggingConfigurationResponse (DeleteLoggingConfigurationResponse'),
    newDeleteLoggingConfigurationResponse,

    -- ** DeletePermissionPolicy
    DeletePermissionPolicy (DeletePermissionPolicy'),
    newDeletePermissionPolicy,
    DeletePermissionPolicyResponse (DeletePermissionPolicyResponse'),
    newDeletePermissionPolicyResponse,

    -- ** DeleteRateBasedRule
    DeleteRateBasedRule (DeleteRateBasedRule'),
    newDeleteRateBasedRule,
    DeleteRateBasedRuleResponse (DeleteRateBasedRuleResponse'),
    newDeleteRateBasedRuleResponse,

    -- ** DeleteRegexMatchSet
    DeleteRegexMatchSet (DeleteRegexMatchSet'),
    newDeleteRegexMatchSet,
    DeleteRegexMatchSetResponse (DeleteRegexMatchSetResponse'),
    newDeleteRegexMatchSetResponse,

    -- ** DeleteRegexPatternSet
    DeleteRegexPatternSet (DeleteRegexPatternSet'),
    newDeleteRegexPatternSet,
    DeleteRegexPatternSetResponse (DeleteRegexPatternSetResponse'),
    newDeleteRegexPatternSetResponse,

    -- ** DeleteRule
    DeleteRule (DeleteRule'),
    newDeleteRule,
    DeleteRuleResponse (DeleteRuleResponse'),
    newDeleteRuleResponse,

    -- ** DeleteRuleGroup
    DeleteRuleGroup (DeleteRuleGroup'),
    newDeleteRuleGroup,
    DeleteRuleGroupResponse (DeleteRuleGroupResponse'),
    newDeleteRuleGroupResponse,

    -- ** DeleteSizeConstraintSet
    DeleteSizeConstraintSet (DeleteSizeConstraintSet'),
    newDeleteSizeConstraintSet,
    DeleteSizeConstraintSetResponse (DeleteSizeConstraintSetResponse'),
    newDeleteSizeConstraintSetResponse,

    -- ** DeleteSqlInjectionMatchSet
    DeleteSqlInjectionMatchSet (DeleteSqlInjectionMatchSet'),
    newDeleteSqlInjectionMatchSet,
    DeleteSqlInjectionMatchSetResponse (DeleteSqlInjectionMatchSetResponse'),
    newDeleteSqlInjectionMatchSetResponse,

    -- ** DeleteWebACL
    DeleteWebACL (DeleteWebACL'),
    newDeleteWebACL,
    DeleteWebACLResponse (DeleteWebACLResponse'),
    newDeleteWebACLResponse,

    -- ** DeleteXssMatchSet
    DeleteXssMatchSet (DeleteXssMatchSet'),
    newDeleteXssMatchSet,
    DeleteXssMatchSetResponse (DeleteXssMatchSetResponse'),
    newDeleteXssMatchSetResponse,

    -- ** GetByteMatchSet
    GetByteMatchSet (GetByteMatchSet'),
    newGetByteMatchSet,
    GetByteMatchSetResponse (GetByteMatchSetResponse'),
    newGetByteMatchSetResponse,

    -- ** GetChangeToken
    GetChangeToken (GetChangeToken'),
    newGetChangeToken,
    GetChangeTokenResponse (GetChangeTokenResponse'),
    newGetChangeTokenResponse,

    -- ** GetChangeTokenStatus
    GetChangeTokenStatus (GetChangeTokenStatus'),
    newGetChangeTokenStatus,
    GetChangeTokenStatusResponse (GetChangeTokenStatusResponse'),
    newGetChangeTokenStatusResponse,

    -- ** GetGeoMatchSet
    GetGeoMatchSet (GetGeoMatchSet'),
    newGetGeoMatchSet,
    GetGeoMatchSetResponse (GetGeoMatchSetResponse'),
    newGetGeoMatchSetResponse,

    -- ** GetIPSet
    GetIPSet (GetIPSet'),
    newGetIPSet,
    GetIPSetResponse (GetIPSetResponse'),
    newGetIPSetResponse,

    -- ** GetLoggingConfiguration
    GetLoggingConfiguration (GetLoggingConfiguration'),
    newGetLoggingConfiguration,
    GetLoggingConfigurationResponse (GetLoggingConfigurationResponse'),
    newGetLoggingConfigurationResponse,

    -- ** GetPermissionPolicy
    GetPermissionPolicy (GetPermissionPolicy'),
    newGetPermissionPolicy,
    GetPermissionPolicyResponse (GetPermissionPolicyResponse'),
    newGetPermissionPolicyResponse,

    -- ** GetRateBasedRule
    GetRateBasedRule (GetRateBasedRule'),
    newGetRateBasedRule,
    GetRateBasedRuleResponse (GetRateBasedRuleResponse'),
    newGetRateBasedRuleResponse,

    -- ** GetRateBasedRuleManagedKeys (Paginated)
    GetRateBasedRuleManagedKeys (GetRateBasedRuleManagedKeys'),
    newGetRateBasedRuleManagedKeys,
    GetRateBasedRuleManagedKeysResponse (GetRateBasedRuleManagedKeysResponse'),
    newGetRateBasedRuleManagedKeysResponse,

    -- ** GetRegexMatchSet
    GetRegexMatchSet (GetRegexMatchSet'),
    newGetRegexMatchSet,
    GetRegexMatchSetResponse (GetRegexMatchSetResponse'),
    newGetRegexMatchSetResponse,

    -- ** GetRegexPatternSet
    GetRegexPatternSet (GetRegexPatternSet'),
    newGetRegexPatternSet,
    GetRegexPatternSetResponse (GetRegexPatternSetResponse'),
    newGetRegexPatternSetResponse,

    -- ** GetRule
    GetRule (GetRule'),
    newGetRule,
    GetRuleResponse (GetRuleResponse'),
    newGetRuleResponse,

    -- ** GetRuleGroup
    GetRuleGroup (GetRuleGroup'),
    newGetRuleGroup,
    GetRuleGroupResponse (GetRuleGroupResponse'),
    newGetRuleGroupResponse,

    -- ** GetSampledRequests
    GetSampledRequests (GetSampledRequests'),
    newGetSampledRequests,
    GetSampledRequestsResponse (GetSampledRequestsResponse'),
    newGetSampledRequestsResponse,

    -- ** GetSizeConstraintSet
    GetSizeConstraintSet (GetSizeConstraintSet'),
    newGetSizeConstraintSet,
    GetSizeConstraintSetResponse (GetSizeConstraintSetResponse'),
    newGetSizeConstraintSetResponse,

    -- ** GetSqlInjectionMatchSet
    GetSqlInjectionMatchSet (GetSqlInjectionMatchSet'),
    newGetSqlInjectionMatchSet,
    GetSqlInjectionMatchSetResponse (GetSqlInjectionMatchSetResponse'),
    newGetSqlInjectionMatchSetResponse,

    -- ** GetWebACL
    GetWebACL (GetWebACL'),
    newGetWebACL,
    GetWebACLResponse (GetWebACLResponse'),
    newGetWebACLResponse,

    -- ** GetXssMatchSet
    GetXssMatchSet (GetXssMatchSet'),
    newGetXssMatchSet,
    GetXssMatchSetResponse (GetXssMatchSetResponse'),
    newGetXssMatchSetResponse,

    -- ** ListActivatedRulesInRuleGroup (Paginated)
    ListActivatedRulesInRuleGroup (ListActivatedRulesInRuleGroup'),
    newListActivatedRulesInRuleGroup,
    ListActivatedRulesInRuleGroupResponse (ListActivatedRulesInRuleGroupResponse'),
    newListActivatedRulesInRuleGroupResponse,

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

    -- ** ListIPSets (Paginated)
    ListIPSets (ListIPSets'),
    newListIPSets,
    ListIPSetsResponse (ListIPSetsResponse'),
    newListIPSetsResponse,

    -- ** ListLoggingConfigurations (Paginated)
    ListLoggingConfigurations (ListLoggingConfigurations'),
    newListLoggingConfigurations,
    ListLoggingConfigurationsResponse (ListLoggingConfigurationsResponse'),
    newListLoggingConfigurationsResponse,

    -- ** ListRateBasedRules (Paginated)
    ListRateBasedRules (ListRateBasedRules'),
    newListRateBasedRules,
    ListRateBasedRulesResponse (ListRateBasedRulesResponse'),
    newListRateBasedRulesResponse,

    -- ** ListRegexMatchSets (Paginated)
    ListRegexMatchSets (ListRegexMatchSets'),
    newListRegexMatchSets,
    ListRegexMatchSetsResponse (ListRegexMatchSetsResponse'),
    newListRegexMatchSetsResponse,

    -- ** ListRegexPatternSets (Paginated)
    ListRegexPatternSets (ListRegexPatternSets'),
    newListRegexPatternSets,
    ListRegexPatternSetsResponse (ListRegexPatternSetsResponse'),
    newListRegexPatternSetsResponse,

    -- ** ListRuleGroups (Paginated)
    ListRuleGroups (ListRuleGroups'),
    newListRuleGroups,
    ListRuleGroupsResponse (ListRuleGroupsResponse'),
    newListRuleGroupsResponse,

    -- ** ListRules (Paginated)
    ListRules (ListRules'),
    newListRules,
    ListRulesResponse (ListRulesResponse'),
    newListRulesResponse,

    -- ** ListSizeConstraintSets (Paginated)
    ListSizeConstraintSets (ListSizeConstraintSets'),
    newListSizeConstraintSets,
    ListSizeConstraintSetsResponse (ListSizeConstraintSetsResponse'),
    newListSizeConstraintSetsResponse,

    -- ** ListSqlInjectionMatchSets (Paginated)
    ListSqlInjectionMatchSets (ListSqlInjectionMatchSets'),
    newListSqlInjectionMatchSets,
    ListSqlInjectionMatchSetsResponse (ListSqlInjectionMatchSetsResponse'),
    newListSqlInjectionMatchSetsResponse,

    -- ** ListSubscribedRuleGroups (Paginated)
    ListSubscribedRuleGroups (ListSubscribedRuleGroups'),
    newListSubscribedRuleGroups,
    ListSubscribedRuleGroupsResponse (ListSubscribedRuleGroupsResponse'),
    newListSubscribedRuleGroupsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListWebACLs (Paginated)
    ListWebACLs (ListWebACLs'),
    newListWebACLs,
    ListWebACLsResponse (ListWebACLsResponse'),
    newListWebACLsResponse,

    -- ** ListXssMatchSets (Paginated)
    ListXssMatchSets (ListXssMatchSets'),
    newListXssMatchSets,
    ListXssMatchSetsResponse (ListXssMatchSetsResponse'),
    newListXssMatchSetsResponse,

    -- ** PutLoggingConfiguration
    PutLoggingConfiguration (PutLoggingConfiguration'),
    newPutLoggingConfiguration,
    PutLoggingConfigurationResponse (PutLoggingConfigurationResponse'),
    newPutLoggingConfigurationResponse,

    -- ** PutPermissionPolicy
    PutPermissionPolicy (PutPermissionPolicy'),
    newPutPermissionPolicy,
    PutPermissionPolicyResponse (PutPermissionPolicyResponse'),
    newPutPermissionPolicyResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateByteMatchSet
    UpdateByteMatchSet (UpdateByteMatchSet'),
    newUpdateByteMatchSet,
    UpdateByteMatchSetResponse (UpdateByteMatchSetResponse'),
    newUpdateByteMatchSetResponse,

    -- ** UpdateGeoMatchSet
    UpdateGeoMatchSet (UpdateGeoMatchSet'),
    newUpdateGeoMatchSet,
    UpdateGeoMatchSetResponse (UpdateGeoMatchSetResponse'),
    newUpdateGeoMatchSetResponse,

    -- ** UpdateIPSet
    UpdateIPSet (UpdateIPSet'),
    newUpdateIPSet,
    UpdateIPSetResponse (UpdateIPSetResponse'),
    newUpdateIPSetResponse,

    -- ** UpdateRateBasedRule
    UpdateRateBasedRule (UpdateRateBasedRule'),
    newUpdateRateBasedRule,
    UpdateRateBasedRuleResponse (UpdateRateBasedRuleResponse'),
    newUpdateRateBasedRuleResponse,

    -- ** UpdateRegexMatchSet
    UpdateRegexMatchSet (UpdateRegexMatchSet'),
    newUpdateRegexMatchSet,
    UpdateRegexMatchSetResponse (UpdateRegexMatchSetResponse'),
    newUpdateRegexMatchSetResponse,

    -- ** UpdateRegexPatternSet
    UpdateRegexPatternSet (UpdateRegexPatternSet'),
    newUpdateRegexPatternSet,
    UpdateRegexPatternSetResponse (UpdateRegexPatternSetResponse'),
    newUpdateRegexPatternSetResponse,

    -- ** UpdateRule
    UpdateRule (UpdateRule'),
    newUpdateRule,
    UpdateRuleResponse (UpdateRuleResponse'),
    newUpdateRuleResponse,

    -- ** UpdateRuleGroup
    UpdateRuleGroup (UpdateRuleGroup'),
    newUpdateRuleGroup,
    UpdateRuleGroupResponse (UpdateRuleGroupResponse'),
    newUpdateRuleGroupResponse,

    -- ** UpdateSizeConstraintSet
    UpdateSizeConstraintSet (UpdateSizeConstraintSet'),
    newUpdateSizeConstraintSet,
    UpdateSizeConstraintSetResponse (UpdateSizeConstraintSetResponse'),
    newUpdateSizeConstraintSetResponse,

    -- ** UpdateSqlInjectionMatchSet
    UpdateSqlInjectionMatchSet (UpdateSqlInjectionMatchSet'),
    newUpdateSqlInjectionMatchSet,
    UpdateSqlInjectionMatchSetResponse (UpdateSqlInjectionMatchSetResponse'),
    newUpdateSqlInjectionMatchSetResponse,

    -- ** UpdateWebACL
    UpdateWebACL (UpdateWebACL'),
    newUpdateWebACL,
    UpdateWebACLResponse (UpdateWebACLResponse'),
    newUpdateWebACLResponse,

    -- ** UpdateXssMatchSet
    UpdateXssMatchSet (UpdateXssMatchSet'),
    newUpdateXssMatchSet,
    UpdateXssMatchSetResponse (UpdateXssMatchSetResponse'),
    newUpdateXssMatchSetResponse,

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

import Amazonka.WAF.CreateByteMatchSet
import Amazonka.WAF.CreateGeoMatchSet
import Amazonka.WAF.CreateIPSet
import Amazonka.WAF.CreateRateBasedRule
import Amazonka.WAF.CreateRegexMatchSet
import Amazonka.WAF.CreateRegexPatternSet
import Amazonka.WAF.CreateRule
import Amazonka.WAF.CreateRuleGroup
import Amazonka.WAF.CreateSizeConstraintSet
import Amazonka.WAF.CreateSqlInjectionMatchSet
import Amazonka.WAF.CreateWebACL
import Amazonka.WAF.CreateWebACLMigrationStack
import Amazonka.WAF.CreateXssMatchSet
import Amazonka.WAF.DeleteByteMatchSet
import Amazonka.WAF.DeleteGeoMatchSet
import Amazonka.WAF.DeleteIPSet
import Amazonka.WAF.DeleteLoggingConfiguration
import Amazonka.WAF.DeletePermissionPolicy
import Amazonka.WAF.DeleteRateBasedRule
import Amazonka.WAF.DeleteRegexMatchSet
import Amazonka.WAF.DeleteRegexPatternSet
import Amazonka.WAF.DeleteRule
import Amazonka.WAF.DeleteRuleGroup
import Amazonka.WAF.DeleteSizeConstraintSet
import Amazonka.WAF.DeleteSqlInjectionMatchSet
import Amazonka.WAF.DeleteWebACL
import Amazonka.WAF.DeleteXssMatchSet
import Amazonka.WAF.GetByteMatchSet
import Amazonka.WAF.GetChangeToken
import Amazonka.WAF.GetChangeTokenStatus
import Amazonka.WAF.GetGeoMatchSet
import Amazonka.WAF.GetIPSet
import Amazonka.WAF.GetLoggingConfiguration
import Amazonka.WAF.GetPermissionPolicy
import Amazonka.WAF.GetRateBasedRule
import Amazonka.WAF.GetRateBasedRuleManagedKeys
import Amazonka.WAF.GetRegexMatchSet
import Amazonka.WAF.GetRegexPatternSet
import Amazonka.WAF.GetRule
import Amazonka.WAF.GetRuleGroup
import Amazonka.WAF.GetSampledRequests
import Amazonka.WAF.GetSizeConstraintSet
import Amazonka.WAF.GetSqlInjectionMatchSet
import Amazonka.WAF.GetWebACL
import Amazonka.WAF.GetXssMatchSet
import Amazonka.WAF.Lens
import Amazonka.WAF.ListActivatedRulesInRuleGroup
import Amazonka.WAF.ListByteMatchSets
import Amazonka.WAF.ListGeoMatchSets
import Amazonka.WAF.ListIPSets
import Amazonka.WAF.ListLoggingConfigurations
import Amazonka.WAF.ListRateBasedRules
import Amazonka.WAF.ListRegexMatchSets
import Amazonka.WAF.ListRegexPatternSets
import Amazonka.WAF.ListRuleGroups
import Amazonka.WAF.ListRules
import Amazonka.WAF.ListSizeConstraintSets
import Amazonka.WAF.ListSqlInjectionMatchSets
import Amazonka.WAF.ListSubscribedRuleGroups
import Amazonka.WAF.ListTagsForResource
import Amazonka.WAF.ListWebACLs
import Amazonka.WAF.ListXssMatchSets
import Amazonka.WAF.PutLoggingConfiguration
import Amazonka.WAF.PutPermissionPolicy
import Amazonka.WAF.TagResource
import Amazonka.WAF.Types
import Amazonka.WAF.UntagResource
import Amazonka.WAF.UpdateByteMatchSet
import Amazonka.WAF.UpdateGeoMatchSet
import Amazonka.WAF.UpdateIPSet
import Amazonka.WAF.UpdateRateBasedRule
import Amazonka.WAF.UpdateRegexMatchSet
import Amazonka.WAF.UpdateRegexPatternSet
import Amazonka.WAF.UpdateRule
import Amazonka.WAF.UpdateRuleGroup
import Amazonka.WAF.UpdateSizeConstraintSet
import Amazonka.WAF.UpdateSqlInjectionMatchSet
import Amazonka.WAF.UpdateWebACL
import Amazonka.WAF.UpdateXssMatchSet
import Amazonka.WAF.Waiters

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
