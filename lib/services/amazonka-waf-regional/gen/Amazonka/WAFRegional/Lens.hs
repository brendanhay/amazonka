{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WAFRegional.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFRegional.Lens
  ( -- * Operations

    -- ** AssociateWebACL
    associateWebACL_webACLId,
    associateWebACL_resourceArn,
    associateWebACLResponse_httpStatus,

    -- ** CreateByteMatchSet
    createByteMatchSet_name,
    createByteMatchSet_changeToken,
    createByteMatchSetResponse_byteMatchSet,
    createByteMatchSetResponse_changeToken,
    createByteMatchSetResponse_httpStatus,

    -- ** CreateGeoMatchSet
    createGeoMatchSet_name,
    createGeoMatchSet_changeToken,
    createGeoMatchSetResponse_changeToken,
    createGeoMatchSetResponse_geoMatchSet,
    createGeoMatchSetResponse_httpStatus,

    -- ** CreateIPSet
    createIPSet_name,
    createIPSet_changeToken,
    createIPSetResponse_changeToken,
    createIPSetResponse_iPSet,
    createIPSetResponse_httpStatus,

    -- ** CreateRateBasedRule
    createRateBasedRule_tags,
    createRateBasedRule_name,
    createRateBasedRule_metricName,
    createRateBasedRule_rateKey,
    createRateBasedRule_rateLimit,
    createRateBasedRule_changeToken,
    createRateBasedRuleResponse_changeToken,
    createRateBasedRuleResponse_rule,
    createRateBasedRuleResponse_httpStatus,

    -- ** CreateRegexMatchSet
    createRegexMatchSet_name,
    createRegexMatchSet_changeToken,
    createRegexMatchSetResponse_changeToken,
    createRegexMatchSetResponse_regexMatchSet,
    createRegexMatchSetResponse_httpStatus,

    -- ** CreateRegexPatternSet
    createRegexPatternSet_name,
    createRegexPatternSet_changeToken,
    createRegexPatternSetResponse_changeToken,
    createRegexPatternSetResponse_regexPatternSet,
    createRegexPatternSetResponse_httpStatus,

    -- ** CreateRule
    createRule_tags,
    createRule_name,
    createRule_metricName,
    createRule_changeToken,
    createRuleResponse_changeToken,
    createRuleResponse_rule,
    createRuleResponse_httpStatus,

    -- ** CreateRuleGroup
    createRuleGroup_tags,
    createRuleGroup_name,
    createRuleGroup_metricName,
    createRuleGroup_changeToken,
    createRuleGroupResponse_changeToken,
    createRuleGroupResponse_ruleGroup,
    createRuleGroupResponse_httpStatus,

    -- ** CreateSizeConstraintSet
    createSizeConstraintSet_name,
    createSizeConstraintSet_changeToken,
    createSizeConstraintSetResponse_changeToken,
    createSizeConstraintSetResponse_sizeConstraintSet,
    createSizeConstraintSetResponse_httpStatus,

    -- ** CreateSqlInjectionMatchSet
    createSqlInjectionMatchSet_name,
    createSqlInjectionMatchSet_changeToken,
    createSqlInjectionMatchSetResponse_changeToken,
    createSqlInjectionMatchSetResponse_sqlInjectionMatchSet,
    createSqlInjectionMatchSetResponse_httpStatus,

    -- ** CreateWebACL
    createWebACL_tags,
    createWebACL_name,
    createWebACL_metricName,
    createWebACL_defaultAction,
    createWebACL_changeToken,
    createWebACLResponse_changeToken,
    createWebACLResponse_webACL,
    createWebACLResponse_httpStatus,

    -- ** CreateWebACLMigrationStack
    createWebACLMigrationStack_webACLId,
    createWebACLMigrationStack_s3BucketName,
    createWebACLMigrationStack_ignoreUnsupportedType,
    createWebACLMigrationStackResponse_httpStatus,
    createWebACLMigrationStackResponse_s3ObjectUrl,

    -- ** CreateXssMatchSet
    createXssMatchSet_name,
    createXssMatchSet_changeToken,
    createXssMatchSetResponse_changeToken,
    createXssMatchSetResponse_xssMatchSet,
    createXssMatchSetResponse_httpStatus,

    -- ** DeleteByteMatchSet
    deleteByteMatchSet_byteMatchSetId,
    deleteByteMatchSet_changeToken,
    deleteByteMatchSetResponse_changeToken,
    deleteByteMatchSetResponse_httpStatus,

    -- ** DeleteGeoMatchSet
    deleteGeoMatchSet_geoMatchSetId,
    deleteGeoMatchSet_changeToken,
    deleteGeoMatchSetResponse_changeToken,
    deleteGeoMatchSetResponse_httpStatus,

    -- ** DeleteIPSet
    deleteIPSet_iPSetId,
    deleteIPSet_changeToken,
    deleteIPSetResponse_changeToken,
    deleteIPSetResponse_httpStatus,

    -- ** DeleteLoggingConfiguration
    deleteLoggingConfiguration_resourceArn,
    deleteLoggingConfigurationResponse_httpStatus,

    -- ** DeletePermissionPolicy
    deletePermissionPolicy_resourceArn,
    deletePermissionPolicyResponse_httpStatus,

    -- ** DeleteRateBasedRule
    deleteRateBasedRule_ruleId,
    deleteRateBasedRule_changeToken,
    deleteRateBasedRuleResponse_changeToken,
    deleteRateBasedRuleResponse_httpStatus,

    -- ** DeleteRegexMatchSet
    deleteRegexMatchSet_regexMatchSetId,
    deleteRegexMatchSet_changeToken,
    deleteRegexMatchSetResponse_changeToken,
    deleteRegexMatchSetResponse_httpStatus,

    -- ** DeleteRegexPatternSet
    deleteRegexPatternSet_regexPatternSetId,
    deleteRegexPatternSet_changeToken,
    deleteRegexPatternSetResponse_changeToken,
    deleteRegexPatternSetResponse_httpStatus,

    -- ** DeleteRule
    deleteRule_ruleId,
    deleteRule_changeToken,
    deleteRuleResponse_changeToken,
    deleteRuleResponse_httpStatus,

    -- ** DeleteRuleGroup
    deleteRuleGroup_ruleGroupId,
    deleteRuleGroup_changeToken,
    deleteRuleGroupResponse_changeToken,
    deleteRuleGroupResponse_httpStatus,

    -- ** DeleteSizeConstraintSet
    deleteSizeConstraintSet_sizeConstraintSetId,
    deleteSizeConstraintSet_changeToken,
    deleteSizeConstraintSetResponse_changeToken,
    deleteSizeConstraintSetResponse_httpStatus,

    -- ** DeleteSqlInjectionMatchSet
    deleteSqlInjectionMatchSet_sqlInjectionMatchSetId,
    deleteSqlInjectionMatchSet_changeToken,
    deleteSqlInjectionMatchSetResponse_changeToken,
    deleteSqlInjectionMatchSetResponse_httpStatus,

    -- ** DeleteWebACL
    deleteWebACL_webACLId,
    deleteWebACL_changeToken,
    deleteWebACLResponse_changeToken,
    deleteWebACLResponse_httpStatus,

    -- ** DeleteXssMatchSet
    deleteXssMatchSet_xssMatchSetId,
    deleteXssMatchSet_changeToken,
    deleteXssMatchSetResponse_changeToken,
    deleteXssMatchSetResponse_httpStatus,

    -- ** DisassociateWebACL
    disassociateWebACL_resourceArn,
    disassociateWebACLResponse_httpStatus,

    -- ** GetByteMatchSet
    getByteMatchSet_byteMatchSetId,
    getByteMatchSetResponse_byteMatchSet,
    getByteMatchSetResponse_httpStatus,

    -- ** GetChangeToken
    getChangeTokenResponse_changeToken,
    getChangeTokenResponse_httpStatus,

    -- ** GetChangeTokenStatus
    getChangeTokenStatus_changeToken,
    getChangeTokenStatusResponse_changeTokenStatus,
    getChangeTokenStatusResponse_httpStatus,

    -- ** GetGeoMatchSet
    getGeoMatchSet_geoMatchSetId,
    getGeoMatchSetResponse_geoMatchSet,
    getGeoMatchSetResponse_httpStatus,

    -- ** GetIPSet
    getIPSet_iPSetId,
    getIPSetResponse_iPSet,
    getIPSetResponse_httpStatus,

    -- ** GetLoggingConfiguration
    getLoggingConfiguration_resourceArn,
    getLoggingConfigurationResponse_loggingConfiguration,
    getLoggingConfigurationResponse_httpStatus,

    -- ** GetPermissionPolicy
    getPermissionPolicy_resourceArn,
    getPermissionPolicyResponse_policy,
    getPermissionPolicyResponse_httpStatus,

    -- ** GetRateBasedRule
    getRateBasedRule_ruleId,
    getRateBasedRuleResponse_rule,
    getRateBasedRuleResponse_httpStatus,

    -- ** GetRateBasedRuleManagedKeys
    getRateBasedRuleManagedKeys_nextMarker,
    getRateBasedRuleManagedKeys_ruleId,
    getRateBasedRuleManagedKeysResponse_managedKeys,
    getRateBasedRuleManagedKeysResponse_nextMarker,
    getRateBasedRuleManagedKeysResponse_httpStatus,

    -- ** GetRegexMatchSet
    getRegexMatchSet_regexMatchSetId,
    getRegexMatchSetResponse_regexMatchSet,
    getRegexMatchSetResponse_httpStatus,

    -- ** GetRegexPatternSet
    getRegexPatternSet_regexPatternSetId,
    getRegexPatternSetResponse_regexPatternSet,
    getRegexPatternSetResponse_httpStatus,

    -- ** GetRule
    getRule_ruleId,
    getRuleResponse_rule,
    getRuleResponse_httpStatus,

    -- ** GetRuleGroup
    getRuleGroup_ruleGroupId,
    getRuleGroupResponse_ruleGroup,
    getRuleGroupResponse_httpStatus,

    -- ** GetSampledRequests
    getSampledRequests_webAclId,
    getSampledRequests_ruleId,
    getSampledRequests_timeWindow,
    getSampledRequests_maxItems,
    getSampledRequestsResponse_populationSize,
    getSampledRequestsResponse_sampledRequests,
    getSampledRequestsResponse_timeWindow,
    getSampledRequestsResponse_httpStatus,

    -- ** GetSizeConstraintSet
    getSizeConstraintSet_sizeConstraintSetId,
    getSizeConstraintSetResponse_sizeConstraintSet,
    getSizeConstraintSetResponse_httpStatus,

    -- ** GetSqlInjectionMatchSet
    getSqlInjectionMatchSet_sqlInjectionMatchSetId,
    getSqlInjectionMatchSetResponse_sqlInjectionMatchSet,
    getSqlInjectionMatchSetResponse_httpStatus,

    -- ** GetWebACL
    getWebACL_webACLId,
    getWebACLResponse_webACL,
    getWebACLResponse_httpStatus,

    -- ** GetWebACLForResource
    getWebACLForResource_resourceArn,
    getWebACLForResourceResponse_webACLSummary,
    getWebACLForResourceResponse_httpStatus,

    -- ** GetXssMatchSet
    getXssMatchSet_xssMatchSetId,
    getXssMatchSetResponse_xssMatchSet,
    getXssMatchSetResponse_httpStatus,

    -- ** ListActivatedRulesInRuleGroup
    listActivatedRulesInRuleGroup_limit,
    listActivatedRulesInRuleGroup_nextMarker,
    listActivatedRulesInRuleGroup_ruleGroupId,
    listActivatedRulesInRuleGroupResponse_activatedRules,
    listActivatedRulesInRuleGroupResponse_nextMarker,
    listActivatedRulesInRuleGroupResponse_httpStatus,

    -- ** ListByteMatchSets
    listByteMatchSets_limit,
    listByteMatchSets_nextMarker,
    listByteMatchSetsResponse_byteMatchSets,
    listByteMatchSetsResponse_nextMarker,
    listByteMatchSetsResponse_httpStatus,

    -- ** ListGeoMatchSets
    listGeoMatchSets_limit,
    listGeoMatchSets_nextMarker,
    listGeoMatchSetsResponse_geoMatchSets,
    listGeoMatchSetsResponse_nextMarker,
    listGeoMatchSetsResponse_httpStatus,

    -- ** ListIPSets
    listIPSets_limit,
    listIPSets_nextMarker,
    listIPSetsResponse_iPSets,
    listIPSetsResponse_nextMarker,
    listIPSetsResponse_httpStatus,

    -- ** ListLoggingConfigurations
    listLoggingConfigurations_limit,
    listLoggingConfigurations_nextMarker,
    listLoggingConfigurationsResponse_loggingConfigurations,
    listLoggingConfigurationsResponse_nextMarker,
    listLoggingConfigurationsResponse_httpStatus,

    -- ** ListRateBasedRules
    listRateBasedRules_limit,
    listRateBasedRules_nextMarker,
    listRateBasedRulesResponse_nextMarker,
    listRateBasedRulesResponse_rules,
    listRateBasedRulesResponse_httpStatus,

    -- ** ListRegexMatchSets
    listRegexMatchSets_limit,
    listRegexMatchSets_nextMarker,
    listRegexMatchSetsResponse_nextMarker,
    listRegexMatchSetsResponse_regexMatchSets,
    listRegexMatchSetsResponse_httpStatus,

    -- ** ListRegexPatternSets
    listRegexPatternSets_limit,
    listRegexPatternSets_nextMarker,
    listRegexPatternSetsResponse_nextMarker,
    listRegexPatternSetsResponse_regexPatternSets,
    listRegexPatternSetsResponse_httpStatus,

    -- ** ListResourcesForWebACL
    listResourcesForWebACL_resourceType,
    listResourcesForWebACL_webACLId,
    listResourcesForWebACLResponse_resourceArns,
    listResourcesForWebACLResponse_httpStatus,

    -- ** ListRuleGroups
    listRuleGroups_limit,
    listRuleGroups_nextMarker,
    listRuleGroupsResponse_nextMarker,
    listRuleGroupsResponse_ruleGroups,
    listRuleGroupsResponse_httpStatus,

    -- ** ListRules
    listRules_limit,
    listRules_nextMarker,
    listRulesResponse_nextMarker,
    listRulesResponse_rules,
    listRulesResponse_httpStatus,

    -- ** ListSizeConstraintSets
    listSizeConstraintSets_limit,
    listSizeConstraintSets_nextMarker,
    listSizeConstraintSetsResponse_nextMarker,
    listSizeConstraintSetsResponse_sizeConstraintSets,
    listSizeConstraintSetsResponse_httpStatus,

    -- ** ListSqlInjectionMatchSets
    listSqlInjectionMatchSets_limit,
    listSqlInjectionMatchSets_nextMarker,
    listSqlInjectionMatchSetsResponse_nextMarker,
    listSqlInjectionMatchSetsResponse_sqlInjectionMatchSets,
    listSqlInjectionMatchSetsResponse_httpStatus,

    -- ** ListSubscribedRuleGroups
    listSubscribedRuleGroups_limit,
    listSubscribedRuleGroups_nextMarker,
    listSubscribedRuleGroupsResponse_nextMarker,
    listSubscribedRuleGroupsResponse_ruleGroups,
    listSubscribedRuleGroupsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_limit,
    listTagsForResource_nextMarker,
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_nextMarker,
    listTagsForResourceResponse_tagInfoForResource,
    listTagsForResourceResponse_httpStatus,

    -- ** ListWebACLs
    listWebACLs_limit,
    listWebACLs_nextMarker,
    listWebACLsResponse_nextMarker,
    listWebACLsResponse_webACLs,
    listWebACLsResponse_httpStatus,

    -- ** ListXssMatchSets
    listXssMatchSets_limit,
    listXssMatchSets_nextMarker,
    listXssMatchSetsResponse_nextMarker,
    listXssMatchSetsResponse_xssMatchSets,
    listXssMatchSetsResponse_httpStatus,

    -- ** PutLoggingConfiguration
    putLoggingConfiguration_loggingConfiguration,
    putLoggingConfigurationResponse_loggingConfiguration,
    putLoggingConfigurationResponse_httpStatus,

    -- ** PutPermissionPolicy
    putPermissionPolicy_resourceArn,
    putPermissionPolicy_policy,
    putPermissionPolicyResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateByteMatchSet
    updateByteMatchSet_byteMatchSetId,
    updateByteMatchSet_changeToken,
    updateByteMatchSet_updates,
    updateByteMatchSetResponse_changeToken,
    updateByteMatchSetResponse_httpStatus,

    -- ** UpdateGeoMatchSet
    updateGeoMatchSet_geoMatchSetId,
    updateGeoMatchSet_changeToken,
    updateGeoMatchSet_updates,
    updateGeoMatchSetResponse_changeToken,
    updateGeoMatchSetResponse_httpStatus,

    -- ** UpdateIPSet
    updateIPSet_iPSetId,
    updateIPSet_changeToken,
    updateIPSet_updates,
    updateIPSetResponse_changeToken,
    updateIPSetResponse_httpStatus,

    -- ** UpdateRateBasedRule
    updateRateBasedRule_ruleId,
    updateRateBasedRule_changeToken,
    updateRateBasedRule_updates,
    updateRateBasedRule_rateLimit,
    updateRateBasedRuleResponse_changeToken,
    updateRateBasedRuleResponse_httpStatus,

    -- ** UpdateRegexMatchSet
    updateRegexMatchSet_regexMatchSetId,
    updateRegexMatchSet_updates,
    updateRegexMatchSet_changeToken,
    updateRegexMatchSetResponse_changeToken,
    updateRegexMatchSetResponse_httpStatus,

    -- ** UpdateRegexPatternSet
    updateRegexPatternSet_regexPatternSetId,
    updateRegexPatternSet_updates,
    updateRegexPatternSet_changeToken,
    updateRegexPatternSetResponse_changeToken,
    updateRegexPatternSetResponse_httpStatus,

    -- ** UpdateRule
    updateRule_ruleId,
    updateRule_changeToken,
    updateRule_updates,
    updateRuleResponse_changeToken,
    updateRuleResponse_httpStatus,

    -- ** UpdateRuleGroup
    updateRuleGroup_ruleGroupId,
    updateRuleGroup_updates,
    updateRuleGroup_changeToken,
    updateRuleGroupResponse_changeToken,
    updateRuleGroupResponse_httpStatus,

    -- ** UpdateSizeConstraintSet
    updateSizeConstraintSet_sizeConstraintSetId,
    updateSizeConstraintSet_changeToken,
    updateSizeConstraintSet_updates,
    updateSizeConstraintSetResponse_changeToken,
    updateSizeConstraintSetResponse_httpStatus,

    -- ** UpdateSqlInjectionMatchSet
    updateSqlInjectionMatchSet_sqlInjectionMatchSetId,
    updateSqlInjectionMatchSet_changeToken,
    updateSqlInjectionMatchSet_updates,
    updateSqlInjectionMatchSetResponse_changeToken,
    updateSqlInjectionMatchSetResponse_httpStatus,

    -- ** UpdateWebACL
    updateWebACL_defaultAction,
    updateWebACL_updates,
    updateWebACL_webACLId,
    updateWebACL_changeToken,
    updateWebACLResponse_changeToken,
    updateWebACLResponse_httpStatus,

    -- ** UpdateXssMatchSet
    updateXssMatchSet_xssMatchSetId,
    updateXssMatchSet_changeToken,
    updateXssMatchSet_updates,
    updateXssMatchSetResponse_changeToken,
    updateXssMatchSetResponse_httpStatus,

    -- * Types

    -- ** ActivatedRule
    activatedRule_action,
    activatedRule_excludedRules,
    activatedRule_overrideAction,
    activatedRule_type,
    activatedRule_priority,
    activatedRule_ruleId,

    -- ** ByteMatchSet
    byteMatchSet_name,
    byteMatchSet_byteMatchSetId,
    byteMatchSet_byteMatchTuples,

    -- ** ByteMatchSetSummary
    byteMatchSetSummary_byteMatchSetId,
    byteMatchSetSummary_name,

    -- ** ByteMatchSetUpdate
    byteMatchSetUpdate_action,
    byteMatchSetUpdate_byteMatchTuple,

    -- ** ByteMatchTuple
    byteMatchTuple_fieldToMatch,
    byteMatchTuple_targetString,
    byteMatchTuple_textTransformation,
    byteMatchTuple_positionalConstraint,

    -- ** ExcludedRule
    excludedRule_ruleId,

    -- ** FieldToMatch
    fieldToMatch_data,
    fieldToMatch_type,

    -- ** GeoMatchConstraint
    geoMatchConstraint_type,
    geoMatchConstraint_value,

    -- ** GeoMatchSet
    geoMatchSet_name,
    geoMatchSet_geoMatchSetId,
    geoMatchSet_geoMatchConstraints,

    -- ** GeoMatchSetSummary
    geoMatchSetSummary_geoMatchSetId,
    geoMatchSetSummary_name,

    -- ** GeoMatchSetUpdate
    geoMatchSetUpdate_action,
    geoMatchSetUpdate_geoMatchConstraint,

    -- ** HTTPHeader
    hTTPHeader_name,
    hTTPHeader_value,

    -- ** HTTPRequest
    hTTPRequest_clientIP,
    hTTPRequest_country,
    hTTPRequest_hTTPVersion,
    hTTPRequest_headers,
    hTTPRequest_method,
    hTTPRequest_uri,

    -- ** IPSet
    iPSet_name,
    iPSet_iPSetId,
    iPSet_iPSetDescriptors,

    -- ** IPSetDescriptor
    iPSetDescriptor_type,
    iPSetDescriptor_value,

    -- ** IPSetSummary
    iPSetSummary_iPSetId,
    iPSetSummary_name,

    -- ** IPSetUpdate
    iPSetUpdate_action,
    iPSetUpdate_iPSetDescriptor,

    -- ** LoggingConfiguration
    loggingConfiguration_redactedFields,
    loggingConfiguration_resourceArn,
    loggingConfiguration_logDestinationConfigs,

    -- ** Predicate
    predicate_negated,
    predicate_type,
    predicate_dataId,

    -- ** RateBasedRule
    rateBasedRule_metricName,
    rateBasedRule_name,
    rateBasedRule_ruleId,
    rateBasedRule_matchPredicates,
    rateBasedRule_rateKey,
    rateBasedRule_rateLimit,

    -- ** RegexMatchSet
    regexMatchSet_name,
    regexMatchSet_regexMatchSetId,
    regexMatchSet_regexMatchTuples,

    -- ** RegexMatchSetSummary
    regexMatchSetSummary_regexMatchSetId,
    regexMatchSetSummary_name,

    -- ** RegexMatchSetUpdate
    regexMatchSetUpdate_action,
    regexMatchSetUpdate_regexMatchTuple,

    -- ** RegexMatchTuple
    regexMatchTuple_fieldToMatch,
    regexMatchTuple_textTransformation,
    regexMatchTuple_regexPatternSetId,

    -- ** RegexPatternSet
    regexPatternSet_name,
    regexPatternSet_regexPatternSetId,
    regexPatternSet_regexPatternStrings,

    -- ** RegexPatternSetSummary
    regexPatternSetSummary_regexPatternSetId,
    regexPatternSetSummary_name,

    -- ** RegexPatternSetUpdate
    regexPatternSetUpdate_action,
    regexPatternSetUpdate_regexPatternString,

    -- ** Rule
    rule_metricName,
    rule_name,
    rule_ruleId,
    rule_predicates,

    -- ** RuleGroup
    ruleGroup_metricName,
    ruleGroup_name,
    ruleGroup_ruleGroupId,

    -- ** RuleGroupSummary
    ruleGroupSummary_ruleGroupId,
    ruleGroupSummary_name,

    -- ** RuleGroupUpdate
    ruleGroupUpdate_action,
    ruleGroupUpdate_activatedRule,

    -- ** RuleSummary
    ruleSummary_ruleId,
    ruleSummary_name,

    -- ** RuleUpdate
    ruleUpdate_action,
    ruleUpdate_predicate,

    -- ** SampledHTTPRequest
    sampledHTTPRequest_action,
    sampledHTTPRequest_ruleWithinRuleGroup,
    sampledHTTPRequest_timestamp,
    sampledHTTPRequest_request,
    sampledHTTPRequest_weight,

    -- ** SizeConstraint
    sizeConstraint_fieldToMatch,
    sizeConstraint_textTransformation,
    sizeConstraint_comparisonOperator,
    sizeConstraint_size,

    -- ** SizeConstraintSet
    sizeConstraintSet_name,
    sizeConstraintSet_sizeConstraintSetId,
    sizeConstraintSet_sizeConstraints,

    -- ** SizeConstraintSetSummary
    sizeConstraintSetSummary_sizeConstraintSetId,
    sizeConstraintSetSummary_name,

    -- ** SizeConstraintSetUpdate
    sizeConstraintSetUpdate_action,
    sizeConstraintSetUpdate_sizeConstraint,

    -- ** SqlInjectionMatchSet
    sqlInjectionMatchSet_name,
    sqlInjectionMatchSet_sqlInjectionMatchSetId,
    sqlInjectionMatchSet_sqlInjectionMatchTuples,

    -- ** SqlInjectionMatchSetSummary
    sqlInjectionMatchSetSummary_sqlInjectionMatchSetId,
    sqlInjectionMatchSetSummary_name,

    -- ** SqlInjectionMatchSetUpdate
    sqlInjectionMatchSetUpdate_action,
    sqlInjectionMatchSetUpdate_sqlInjectionMatchTuple,

    -- ** SqlInjectionMatchTuple
    sqlInjectionMatchTuple_fieldToMatch,
    sqlInjectionMatchTuple_textTransformation,

    -- ** SubscribedRuleGroupSummary
    subscribedRuleGroupSummary_ruleGroupId,
    subscribedRuleGroupSummary_name,
    subscribedRuleGroupSummary_metricName,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TagInfoForResource
    tagInfoForResource_resourceARN,
    tagInfoForResource_tagList,

    -- ** TimeWindow
    timeWindow_startTime,
    timeWindow_endTime,

    -- ** WafAction
    wafAction_type,

    -- ** WafOverrideAction
    wafOverrideAction_type,

    -- ** WebACL
    webACL_metricName,
    webACL_name,
    webACL_webACLArn,
    webACL_webACLId,
    webACL_defaultAction,
    webACL_rules,

    -- ** WebACLSummary
    webACLSummary_webACLId,
    webACLSummary_name,

    -- ** WebACLUpdate
    webACLUpdate_action,
    webACLUpdate_activatedRule,

    -- ** XssMatchSet
    xssMatchSet_name,
    xssMatchSet_xssMatchSetId,
    xssMatchSet_xssMatchTuples,

    -- ** XssMatchSetSummary
    xssMatchSetSummary_xssMatchSetId,
    xssMatchSetSummary_name,

    -- ** XssMatchSetUpdate
    xssMatchSetUpdate_action,
    xssMatchSetUpdate_xssMatchTuple,

    -- ** XssMatchTuple
    xssMatchTuple_fieldToMatch,
    xssMatchTuple_textTransformation,
  )
where

import Amazonka.WAFRegional.AssociateWebACL
import Amazonka.WAFRegional.CreateByteMatchSet
import Amazonka.WAFRegional.CreateGeoMatchSet
import Amazonka.WAFRegional.CreateIPSet
import Amazonka.WAFRegional.CreateRateBasedRule
import Amazonka.WAFRegional.CreateRegexMatchSet
import Amazonka.WAFRegional.CreateRegexPatternSet
import Amazonka.WAFRegional.CreateRule
import Amazonka.WAFRegional.CreateRuleGroup
import Amazonka.WAFRegional.CreateSizeConstraintSet
import Amazonka.WAFRegional.CreateSqlInjectionMatchSet
import Amazonka.WAFRegional.CreateWebACL
import Amazonka.WAFRegional.CreateWebACLMigrationStack
import Amazonka.WAFRegional.CreateXssMatchSet
import Amazonka.WAFRegional.DeleteByteMatchSet
import Amazonka.WAFRegional.DeleteGeoMatchSet
import Amazonka.WAFRegional.DeleteIPSet
import Amazonka.WAFRegional.DeleteLoggingConfiguration
import Amazonka.WAFRegional.DeletePermissionPolicy
import Amazonka.WAFRegional.DeleteRateBasedRule
import Amazonka.WAFRegional.DeleteRegexMatchSet
import Amazonka.WAFRegional.DeleteRegexPatternSet
import Amazonka.WAFRegional.DeleteRule
import Amazonka.WAFRegional.DeleteRuleGroup
import Amazonka.WAFRegional.DeleteSizeConstraintSet
import Amazonka.WAFRegional.DeleteSqlInjectionMatchSet
import Amazonka.WAFRegional.DeleteWebACL
import Amazonka.WAFRegional.DeleteXssMatchSet
import Amazonka.WAFRegional.DisassociateWebACL
import Amazonka.WAFRegional.GetByteMatchSet
import Amazonka.WAFRegional.GetChangeToken
import Amazonka.WAFRegional.GetChangeTokenStatus
import Amazonka.WAFRegional.GetGeoMatchSet
import Amazonka.WAFRegional.GetIPSet
import Amazonka.WAFRegional.GetLoggingConfiguration
import Amazonka.WAFRegional.GetPermissionPolicy
import Amazonka.WAFRegional.GetRateBasedRule
import Amazonka.WAFRegional.GetRateBasedRuleManagedKeys
import Amazonka.WAFRegional.GetRegexMatchSet
import Amazonka.WAFRegional.GetRegexPatternSet
import Amazonka.WAFRegional.GetRule
import Amazonka.WAFRegional.GetRuleGroup
import Amazonka.WAFRegional.GetSampledRequests
import Amazonka.WAFRegional.GetSizeConstraintSet
import Amazonka.WAFRegional.GetSqlInjectionMatchSet
import Amazonka.WAFRegional.GetWebACL
import Amazonka.WAFRegional.GetWebACLForResource
import Amazonka.WAFRegional.GetXssMatchSet
import Amazonka.WAFRegional.ListActivatedRulesInRuleGroup
import Amazonka.WAFRegional.ListByteMatchSets
import Amazonka.WAFRegional.ListGeoMatchSets
import Amazonka.WAFRegional.ListIPSets
import Amazonka.WAFRegional.ListLoggingConfigurations
import Amazonka.WAFRegional.ListRateBasedRules
import Amazonka.WAFRegional.ListRegexMatchSets
import Amazonka.WAFRegional.ListRegexPatternSets
import Amazonka.WAFRegional.ListResourcesForWebACL
import Amazonka.WAFRegional.ListRuleGroups
import Amazonka.WAFRegional.ListRules
import Amazonka.WAFRegional.ListSizeConstraintSets
import Amazonka.WAFRegional.ListSqlInjectionMatchSets
import Amazonka.WAFRegional.ListSubscribedRuleGroups
import Amazonka.WAFRegional.ListTagsForResource
import Amazonka.WAFRegional.ListWebACLs
import Amazonka.WAFRegional.ListXssMatchSets
import Amazonka.WAFRegional.PutLoggingConfiguration
import Amazonka.WAFRegional.PutPermissionPolicy
import Amazonka.WAFRegional.TagResource
import Amazonka.WAFRegional.Types.ActivatedRule
import Amazonka.WAFRegional.Types.ByteMatchSet
import Amazonka.WAFRegional.Types.ByteMatchSetSummary
import Amazonka.WAFRegional.Types.ByteMatchSetUpdate
import Amazonka.WAFRegional.Types.ByteMatchTuple
import Amazonka.WAFRegional.Types.ExcludedRule
import Amazonka.WAFRegional.Types.FieldToMatch
import Amazonka.WAFRegional.Types.GeoMatchConstraint
import Amazonka.WAFRegional.Types.GeoMatchSet
import Amazonka.WAFRegional.Types.GeoMatchSetSummary
import Amazonka.WAFRegional.Types.GeoMatchSetUpdate
import Amazonka.WAFRegional.Types.HTTPHeader
import Amazonka.WAFRegional.Types.HTTPRequest
import Amazonka.WAFRegional.Types.IPSet
import Amazonka.WAFRegional.Types.IPSetDescriptor
import Amazonka.WAFRegional.Types.IPSetSummary
import Amazonka.WAFRegional.Types.IPSetUpdate
import Amazonka.WAFRegional.Types.LoggingConfiguration
import Amazonka.WAFRegional.Types.Predicate
import Amazonka.WAFRegional.Types.RateBasedRule
import Amazonka.WAFRegional.Types.RegexMatchSet
import Amazonka.WAFRegional.Types.RegexMatchSetSummary
import Amazonka.WAFRegional.Types.RegexMatchSetUpdate
import Amazonka.WAFRegional.Types.RegexMatchTuple
import Amazonka.WAFRegional.Types.RegexPatternSet
import Amazonka.WAFRegional.Types.RegexPatternSetSummary
import Amazonka.WAFRegional.Types.RegexPatternSetUpdate
import Amazonka.WAFRegional.Types.Rule
import Amazonka.WAFRegional.Types.RuleGroup
import Amazonka.WAFRegional.Types.RuleGroupSummary
import Amazonka.WAFRegional.Types.RuleGroupUpdate
import Amazonka.WAFRegional.Types.RuleSummary
import Amazonka.WAFRegional.Types.RuleUpdate
import Amazonka.WAFRegional.Types.SampledHTTPRequest
import Amazonka.WAFRegional.Types.SizeConstraint
import Amazonka.WAFRegional.Types.SizeConstraintSet
import Amazonka.WAFRegional.Types.SizeConstraintSetSummary
import Amazonka.WAFRegional.Types.SizeConstraintSetUpdate
import Amazonka.WAFRegional.Types.SqlInjectionMatchSet
import Amazonka.WAFRegional.Types.SqlInjectionMatchSetSummary
import Amazonka.WAFRegional.Types.SqlInjectionMatchSetUpdate
import Amazonka.WAFRegional.Types.SqlInjectionMatchTuple
import Amazonka.WAFRegional.Types.SubscribedRuleGroupSummary
import Amazonka.WAFRegional.Types.Tag
import Amazonka.WAFRegional.Types.TagInfoForResource
import Amazonka.WAFRegional.Types.TimeWindow
import Amazonka.WAFRegional.Types.WafAction
import Amazonka.WAFRegional.Types.WafOverrideAction
import Amazonka.WAFRegional.Types.WebACL
import Amazonka.WAFRegional.Types.WebACLSummary
import Amazonka.WAFRegional.Types.WebACLUpdate
import Amazonka.WAFRegional.Types.XssMatchSet
import Amazonka.WAFRegional.Types.XssMatchSetSummary
import Amazonka.WAFRegional.Types.XssMatchSetUpdate
import Amazonka.WAFRegional.Types.XssMatchTuple
import Amazonka.WAFRegional.UntagResource
import Amazonka.WAFRegional.UpdateByteMatchSet
import Amazonka.WAFRegional.UpdateGeoMatchSet
import Amazonka.WAFRegional.UpdateIPSet
import Amazonka.WAFRegional.UpdateRateBasedRule
import Amazonka.WAFRegional.UpdateRegexMatchSet
import Amazonka.WAFRegional.UpdateRegexPatternSet
import Amazonka.WAFRegional.UpdateRule
import Amazonka.WAFRegional.UpdateRuleGroup
import Amazonka.WAFRegional.UpdateSizeConstraintSet
import Amazonka.WAFRegional.UpdateSqlInjectionMatchSet
import Amazonka.WAFRegional.UpdateWebACL
import Amazonka.WAFRegional.UpdateXssMatchSet
