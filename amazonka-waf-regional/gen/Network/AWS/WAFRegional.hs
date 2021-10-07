{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.WAFRegional
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2016-11-28@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- This is __AWS WAF Classic Regional__ documentation. For more
-- information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- This is the /AWS WAF Regional Classic API Reference/ for using AWS WAF
-- Classic with the AWS resources, Elastic Load Balancing (ELB) Application
-- Load Balancers and API Gateway APIs. The AWS WAF Classic actions and
-- data types listed in the reference are available for protecting Elastic
-- Load Balancing (ELB) Application Load Balancers and API Gateway APIs.
-- You can use these actions and data types by means of the endpoints
-- listed in
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html#waf_region AWS Regions and Endpoints>.
-- This guide is for developers who need detailed information about the AWS
-- WAF Classic API actions, data types, and errors. For detailed
-- information about AWS WAF Classic features and an overview of how to use
-- the AWS WAF Classic API, see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
module Network.AWS.WAFRegional
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** WAFSubscriptionNotFoundException
    _WAFSubscriptionNotFoundException,

    -- ** WAFTagOperationInternalErrorException
    _WAFTagOperationInternalErrorException,

    -- ** WAFInvalidAccountException
    _WAFInvalidAccountException,

    -- ** WAFBadRequestException
    _WAFBadRequestException,

    -- ** WAFDisallowedNameException
    _WAFDisallowedNameException,

    -- ** WAFNonEmptyEntityException
    _WAFNonEmptyEntityException,

    -- ** WAFInvalidOperationException
    _WAFInvalidOperationException,

    -- ** WAFStaleDataException
    _WAFStaleDataException,

    -- ** WAFTagOperationException
    _WAFTagOperationException,

    -- ** WAFInternalErrorException
    _WAFInternalErrorException,

    -- ** WAFServiceLinkedRoleErrorException
    _WAFServiceLinkedRoleErrorException,

    -- ** WAFInvalidParameterException
    _WAFInvalidParameterException,

    -- ** WAFNonexistentItemException
    _WAFNonexistentItemException,

    -- ** WAFUnavailableEntityException
    _WAFUnavailableEntityException,

    -- ** WAFInvalidRegexPatternException
    _WAFInvalidRegexPatternException,

    -- ** WAFNonexistentContainerException
    _WAFNonexistentContainerException,

    -- ** WAFEntityMigrationException
    _WAFEntityMigrationException,

    -- ** WAFReferencedItemException
    _WAFReferencedItemException,

    -- ** WAFInvalidPermissionPolicyException
    _WAFInvalidPermissionPolicyException,

    -- ** WAFLimitsExceededException
    _WAFLimitsExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetChangeTokenStatus
    GetChangeTokenStatus (GetChangeTokenStatus'),
    newGetChangeTokenStatus,
    GetChangeTokenStatusResponse (GetChangeTokenStatusResponse'),
    newGetChangeTokenStatusResponse,

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

    -- ** GetRuleGroup
    GetRuleGroup (GetRuleGroup'),
    newGetRuleGroup,
    GetRuleGroupResponse (GetRuleGroupResponse'),
    newGetRuleGroupResponse,

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

    -- ** GetSizeConstraintSet
    GetSizeConstraintSet (GetSizeConstraintSet'),
    newGetSizeConstraintSet,
    GetSizeConstraintSetResponse (GetSizeConstraintSetResponse'),
    newGetSizeConstraintSetResponse,

    -- ** ListRateBasedRules
    ListRateBasedRules (ListRateBasedRules'),
    newListRateBasedRules,
    ListRateBasedRulesResponse (ListRateBasedRulesResponse'),
    newListRateBasedRulesResponse,

    -- ** GetWebACLForResource
    GetWebACLForResource (GetWebACLForResource'),
    newGetWebACLForResource,
    GetWebACLForResourceResponse (GetWebACLForResourceResponse'),
    newGetWebACLForResourceResponse,

    -- ** CreateRateBasedRule
    CreateRateBasedRule (CreateRateBasedRule'),
    newCreateRateBasedRule,
    CreateRateBasedRuleResponse (CreateRateBasedRuleResponse'),
    newCreateRateBasedRuleResponse,

    -- ** ListSqlInjectionMatchSets
    ListSqlInjectionMatchSets (ListSqlInjectionMatchSets'),
    newListSqlInjectionMatchSets,
    ListSqlInjectionMatchSetsResponse (ListSqlInjectionMatchSetsResponse'),
    newListSqlInjectionMatchSetsResponse,

    -- ** ListRegexPatternSets
    ListRegexPatternSets (ListRegexPatternSets'),
    newListRegexPatternSets,
    ListRegexPatternSetsResponse (ListRegexPatternSetsResponse'),
    newListRegexPatternSetsResponse,

    -- ** GetSqlInjectionMatchSet
    GetSqlInjectionMatchSet (GetSqlInjectionMatchSet'),
    newGetSqlInjectionMatchSet,
    GetSqlInjectionMatchSetResponse (GetSqlInjectionMatchSetResponse'),
    newGetSqlInjectionMatchSetResponse,

    -- ** ListSizeConstraintSets
    ListSizeConstraintSets (ListSizeConstraintSets'),
    newListSizeConstraintSets,
    ListSizeConstraintSetsResponse (ListSizeConstraintSetsResponse'),
    newListSizeConstraintSetsResponse,

    -- ** UpdateSizeConstraintSet
    UpdateSizeConstraintSet (UpdateSizeConstraintSet'),
    newUpdateSizeConstraintSet,
    UpdateSizeConstraintSetResponse (UpdateSizeConstraintSetResponse'),
    newUpdateSizeConstraintSetResponse,

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

    -- ** GetChangeToken
    GetChangeToken (GetChangeToken'),
    newGetChangeToken,
    GetChangeTokenResponse (GetChangeTokenResponse'),
    newGetChangeTokenResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** CreateWebACLMigrationStack
    CreateWebACLMigrationStack (CreateWebACLMigrationStack'),
    newCreateWebACLMigrationStack,
    CreateWebACLMigrationStackResponse (CreateWebACLMigrationStackResponse'),
    newCreateWebACLMigrationStackResponse,

    -- ** UpdateRuleGroup
    UpdateRuleGroup (UpdateRuleGroup'),
    newUpdateRuleGroup,
    UpdateRuleGroupResponse (UpdateRuleGroupResponse'),
    newUpdateRuleGroupResponse,

    -- ** ListXssMatchSets
    ListXssMatchSets (ListXssMatchSets'),
    newListXssMatchSets,
    ListXssMatchSetsResponse (ListXssMatchSetsResponse'),
    newListXssMatchSetsResponse,

    -- ** DeleteRuleGroup
    DeleteRuleGroup (DeleteRuleGroup'),
    newDeleteRuleGroup,
    DeleteRuleGroupResponse (DeleteRuleGroupResponse'),
    newDeleteRuleGroupResponse,

    -- ** CreateRegexMatchSet
    CreateRegexMatchSet (CreateRegexMatchSet'),
    newCreateRegexMatchSet,
    CreateRegexMatchSetResponse (CreateRegexMatchSetResponse'),
    newCreateRegexMatchSetResponse,

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

    -- ** ListRegexMatchSets
    ListRegexMatchSets (ListRegexMatchSets'),
    newListRegexMatchSets,
    ListRegexMatchSetsResponse (ListRegexMatchSetsResponse'),
    newListRegexMatchSetsResponse,

    -- ** GetLoggingConfiguration
    GetLoggingConfiguration (GetLoggingConfiguration'),
    newGetLoggingConfiguration,
    GetLoggingConfigurationResponse (GetLoggingConfigurationResponse'),
    newGetLoggingConfigurationResponse,

    -- ** UpdateRegexMatchSet
    UpdateRegexMatchSet (UpdateRegexMatchSet'),
    newUpdateRegexMatchSet,
    UpdateRegexMatchSetResponse (UpdateRegexMatchSetResponse'),
    newUpdateRegexMatchSetResponse,

    -- ** DeleteLoggingConfiguration
    DeleteLoggingConfiguration (DeleteLoggingConfiguration'),
    newDeleteLoggingConfiguration,
    DeleteLoggingConfigurationResponse (DeleteLoggingConfigurationResponse'),
    newDeleteLoggingConfigurationResponse,

    -- ** CreateRule
    CreateRule (CreateRule'),
    newCreateRule,
    CreateRuleResponse (CreateRuleResponse'),
    newCreateRuleResponse,

    -- ** AssociateWebACL
    AssociateWebACL (AssociateWebACL'),
    newAssociateWebACL,
    AssociateWebACLResponse (AssociateWebACLResponse'),
    newAssociateWebACLResponse,

    -- ** ListLoggingConfigurations
    ListLoggingConfigurations (ListLoggingConfigurations'),
    newListLoggingConfigurations,
    ListLoggingConfigurationsResponse (ListLoggingConfigurationsResponse'),
    newListLoggingConfigurationsResponse,

    -- ** UpdateIPSet
    UpdateIPSet (UpdateIPSet'),
    newUpdateIPSet,
    UpdateIPSetResponse (UpdateIPSetResponse'),
    newUpdateIPSetResponse,

    -- ** CreateWebACL
    CreateWebACL (CreateWebACL'),
    newCreateWebACL,
    CreateWebACLResponse (CreateWebACLResponse'),
    newCreateWebACLResponse,

    -- ** PutPermissionPolicy
    PutPermissionPolicy (PutPermissionPolicy'),
    newPutPermissionPolicy,
    PutPermissionPolicyResponse (PutPermissionPolicyResponse'),
    newPutPermissionPolicyResponse,

    -- ** DeleteIPSet
    DeleteIPSet (DeleteIPSet'),
    newDeleteIPSet,
    DeleteIPSetResponse (DeleteIPSetResponse'),
    newDeleteIPSetResponse,

    -- ** GetGeoMatchSet
    GetGeoMatchSet (GetGeoMatchSet'),
    newGetGeoMatchSet,
    GetGeoMatchSetResponse (GetGeoMatchSetResponse'),
    newGetGeoMatchSetResponse,

    -- ** GetRateBasedRuleManagedKeys
    GetRateBasedRuleManagedKeys (GetRateBasedRuleManagedKeys'),
    newGetRateBasedRuleManagedKeys,
    GetRateBasedRuleManagedKeysResponse (GetRateBasedRuleManagedKeysResponse'),
    newGetRateBasedRuleManagedKeysResponse,

    -- ** CreateIPSet
    CreateIPSet (CreateIPSet'),
    newCreateIPSet,
    CreateIPSetResponse (CreateIPSetResponse'),
    newCreateIPSetResponse,

    -- ** ListRules
    ListRules (ListRules'),
    newListRules,
    ListRulesResponse (ListRulesResponse'),
    newListRulesResponse,

    -- ** ListWebACLs
    ListWebACLs (ListWebACLs'),
    newListWebACLs,
    ListWebACLsResponse (ListWebACLsResponse'),
    newListWebACLsResponse,

    -- ** CreateByteMatchSet
    CreateByteMatchSet (CreateByteMatchSet'),
    newCreateByteMatchSet,
    CreateByteMatchSetResponse (CreateByteMatchSetResponse'),
    newCreateByteMatchSetResponse,

    -- ** GetXssMatchSet
    GetXssMatchSet (GetXssMatchSet'),
    newGetXssMatchSet,
    GetXssMatchSetResponse (GetXssMatchSetResponse'),
    newGetXssMatchSetResponse,

    -- ** UpdateRateBasedRule
    UpdateRateBasedRule (UpdateRateBasedRule'),
    newUpdateRateBasedRule,
    UpdateRateBasedRuleResponse (UpdateRateBasedRuleResponse'),
    newUpdateRateBasedRuleResponse,

    -- ** DeleteRateBasedRule
    DeleteRateBasedRule (DeleteRateBasedRule'),
    newDeleteRateBasedRule,
    DeleteRateBasedRuleResponse (DeleteRateBasedRuleResponse'),
    newDeleteRateBasedRuleResponse,

    -- ** ListActivatedRulesInRuleGroup
    ListActivatedRulesInRuleGroup (ListActivatedRulesInRuleGroup'),
    newListActivatedRulesInRuleGroup,
    ListActivatedRulesInRuleGroupResponse (ListActivatedRulesInRuleGroupResponse'),
    newListActivatedRulesInRuleGroupResponse,

    -- ** ListSubscribedRuleGroups
    ListSubscribedRuleGroups (ListSubscribedRuleGroups'),
    newListSubscribedRuleGroups,
    ListSubscribedRuleGroupsResponse (ListSubscribedRuleGroupsResponse'),
    newListSubscribedRuleGroupsResponse,

    -- ** CreateSqlInjectionMatchSet
    CreateSqlInjectionMatchSet (CreateSqlInjectionMatchSet'),
    newCreateSqlInjectionMatchSet,
    CreateSqlInjectionMatchSetResponse (CreateSqlInjectionMatchSetResponse'),
    newCreateSqlInjectionMatchSetResponse,

    -- ** DisassociateWebACL
    DisassociateWebACL (DisassociateWebACL'),
    newDisassociateWebACL,
    DisassociateWebACLResponse (DisassociateWebACLResponse'),
    newDisassociateWebACLResponse,

    -- ** DeleteSqlInjectionMatchSet
    DeleteSqlInjectionMatchSet (DeleteSqlInjectionMatchSet'),
    newDeleteSqlInjectionMatchSet,
    DeleteSqlInjectionMatchSetResponse (DeleteSqlInjectionMatchSetResponse'),
    newDeleteSqlInjectionMatchSetResponse,

    -- ** GetRegexPatternSet
    GetRegexPatternSet (GetRegexPatternSet'),
    newGetRegexPatternSet,
    GetRegexPatternSetResponse (GetRegexPatternSetResponse'),
    newGetRegexPatternSetResponse,

    -- ** UpdateSqlInjectionMatchSet
    UpdateSqlInjectionMatchSet (UpdateSqlInjectionMatchSet'),
    newUpdateSqlInjectionMatchSet,
    UpdateSqlInjectionMatchSetResponse (UpdateSqlInjectionMatchSetResponse'),
    newUpdateSqlInjectionMatchSetResponse,

    -- ** DeleteRegexPatternSet
    DeleteRegexPatternSet (DeleteRegexPatternSet'),
    newDeleteRegexPatternSet,
    DeleteRegexPatternSetResponse (DeleteRegexPatternSetResponse'),
    newDeleteRegexPatternSetResponse,

    -- ** CreateSizeConstraintSet
    CreateSizeConstraintSet (CreateSizeConstraintSet'),
    newCreateSizeConstraintSet,
    CreateSizeConstraintSetResponse (CreateSizeConstraintSetResponse'),
    newCreateSizeConstraintSetResponse,

    -- ** UpdateRegexPatternSet
    UpdateRegexPatternSet (UpdateRegexPatternSet'),
    newUpdateRegexPatternSet,
    UpdateRegexPatternSetResponse (UpdateRegexPatternSetResponse'),
    newUpdateRegexPatternSetResponse,

    -- ** ListResourcesForWebACL
    ListResourcesForWebACL (ListResourcesForWebACL'),
    newListResourcesForWebACL,
    ListResourcesForWebACLResponse (ListResourcesForWebACLResponse'),
    newListResourcesForWebACLResponse,

    -- ** GetSampledRequests
    GetSampledRequests (GetSampledRequests'),
    newGetSampledRequests,
    GetSampledRequestsResponse (GetSampledRequestsResponse'),
    newGetSampledRequestsResponse,

    -- ** GetRateBasedRule
    GetRateBasedRule (GetRateBasedRule'),
    newGetRateBasedRule,
    GetRateBasedRuleResponse (GetRateBasedRuleResponse'),
    newGetRateBasedRuleResponse,

    -- ** DeleteXssMatchSet
    DeleteXssMatchSet (DeleteXssMatchSet'),
    newDeleteXssMatchSet,
    DeleteXssMatchSetResponse (DeleteXssMatchSetResponse'),
    newDeleteXssMatchSetResponse,

    -- ** ListRuleGroups
    ListRuleGroups (ListRuleGroups'),
    newListRuleGroups,
    ListRuleGroupsResponse (ListRuleGroupsResponse'),
    newListRuleGroupsResponse,

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

    -- ** CreateGeoMatchSet
    CreateGeoMatchSet (CreateGeoMatchSet'),
    newCreateGeoMatchSet,
    CreateGeoMatchSetResponse (CreateGeoMatchSetResponse'),
    newCreateGeoMatchSetResponse,

    -- ** UpdateXssMatchSet
    UpdateXssMatchSet (UpdateXssMatchSet'),
    newUpdateXssMatchSet,
    UpdateXssMatchSetResponse (UpdateXssMatchSetResponse'),
    newUpdateXssMatchSetResponse,

    -- ** GetIPSet
    GetIPSet (GetIPSet'),
    newGetIPSet,
    GetIPSetResponse (GetIPSetResponse'),
    newGetIPSetResponse,

    -- ** CreateXssMatchSet
    CreateXssMatchSet (CreateXssMatchSet'),
    newCreateXssMatchSet,
    CreateXssMatchSetResponse (CreateXssMatchSetResponse'),
    newCreateXssMatchSetResponse,

    -- ** GetByteMatchSet
    GetByteMatchSet (GetByteMatchSet'),
    newGetByteMatchSet,
    GetByteMatchSetResponse (GetByteMatchSetResponse'),
    newGetByteMatchSetResponse,

    -- ** UpdateGeoMatchSet
    UpdateGeoMatchSet (UpdateGeoMatchSet'),
    newUpdateGeoMatchSet,
    UpdateGeoMatchSetResponse (UpdateGeoMatchSetResponse'),
    newUpdateGeoMatchSetResponse,

    -- ** ListGeoMatchSets
    ListGeoMatchSets (ListGeoMatchSets'),
    newListGeoMatchSets,
    ListGeoMatchSetsResponse (ListGeoMatchSetsResponse'),
    newListGeoMatchSetsResponse,

    -- ** DeleteGeoMatchSet
    DeleteGeoMatchSet (DeleteGeoMatchSet'),
    newDeleteGeoMatchSet,
    DeleteGeoMatchSetResponse (DeleteGeoMatchSetResponse'),
    newDeleteGeoMatchSetResponse,

    -- ** GetPermissionPolicy
    GetPermissionPolicy (GetPermissionPolicy'),
    newGetPermissionPolicy,
    GetPermissionPolicyResponse (GetPermissionPolicyResponse'),
    newGetPermissionPolicyResponse,

    -- ** DeleteByteMatchSet
    DeleteByteMatchSet (DeleteByteMatchSet'),
    newDeleteByteMatchSet,
    DeleteByteMatchSetResponse (DeleteByteMatchSetResponse'),
    newDeleteByteMatchSetResponse,

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

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** UpdateByteMatchSet
    UpdateByteMatchSet (UpdateByteMatchSet'),
    newUpdateByteMatchSet,
    UpdateByteMatchSetResponse (UpdateByteMatchSetResponse'),
    newUpdateByteMatchSetResponse,

    -- ** PutLoggingConfiguration
    PutLoggingConfiguration (PutLoggingConfiguration'),
    newPutLoggingConfiguration,
    PutLoggingConfigurationResponse (PutLoggingConfigurationResponse'),
    newPutLoggingConfigurationResponse,

    -- ** ListIPSets
    ListIPSets (ListIPSets'),
    newListIPSets,
    ListIPSetsResponse (ListIPSetsResponse'),
    newListIPSetsResponse,

    -- ** ListByteMatchSets
    ListByteMatchSets (ListByteMatchSets'),
    newListByteMatchSets,
    ListByteMatchSetsResponse (ListByteMatchSetsResponse'),
    newListByteMatchSetsResponse,

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
import Network.AWS.WAFRegional.CreateXssMatchSet
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
import Network.AWS.WAFRegional.DeleteXssMatchSet
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
import Network.AWS.WAFRegional.GetXssMatchSet
import Network.AWS.WAFRegional.Lens
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
import Network.AWS.WAFRegional.ListXssMatchSets
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
import Network.AWS.WAFRegional.UpdateXssMatchSet
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
