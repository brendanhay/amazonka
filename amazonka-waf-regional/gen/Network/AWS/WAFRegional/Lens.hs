{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Lens
  ( -- * Operations

    -- ** DeleteWebACL
    deleteWebACL_webACLId,
    deleteWebACL_changeToken,
    deleteWebACLResponse_changeToken,
    deleteWebACLResponse_httpStatus,

    -- ** GetChangeTokenStatus
    getChangeTokenStatus_changeToken,
    getChangeTokenStatusResponse_changeTokenStatus,
    getChangeTokenStatusResponse_httpStatus,

    -- ** UpdateRule
    updateRule_ruleId,
    updateRule_changeToken,
    updateRule_updates,
    updateRuleResponse_changeToken,
    updateRuleResponse_httpStatus,

    -- ** GetRuleGroup
    getRuleGroup_ruleGroupId,
    getRuleGroupResponse_ruleGroup,
    getRuleGroupResponse_httpStatus,

    -- ** DeleteRule
    deleteRule_ruleId,
    deleteRule_changeToken,
    deleteRuleResponse_changeToken,
    deleteRuleResponse_httpStatus,

    -- ** UpdateWebACL
    updateWebACL_updates,
    updateWebACL_defaultAction,
    updateWebACL_webACLId,
    updateWebACL_changeToken,
    updateWebACLResponse_changeToken,
    updateWebACLResponse_httpStatus,

    -- ** ListRateBasedRules
    listRateBasedRules_nextMarker,
    listRateBasedRules_limit,
    listRateBasedRulesResponse_nextMarker,
    listRateBasedRulesResponse_rules,
    listRateBasedRulesResponse_httpStatus,

    -- ** GetSizeConstraintSet
    getSizeConstraintSet_sizeConstraintSetId,
    getSizeConstraintSetResponse_sizeConstraintSet,
    getSizeConstraintSetResponse_httpStatus,

    -- ** GetWebACLForResource
    getWebACLForResource_resourceArn,
    getWebACLForResourceResponse_webACLSummary,
    getWebACLForResourceResponse_httpStatus,

    -- ** ListSqlInjectionMatchSets
    listSqlInjectionMatchSets_nextMarker,
    listSqlInjectionMatchSets_limit,
    listSqlInjectionMatchSetsResponse_sqlInjectionMatchSets,
    listSqlInjectionMatchSetsResponse_nextMarker,
    listSqlInjectionMatchSetsResponse_httpStatus,

    -- ** CreateRateBasedRule
    createRateBasedRule_tags,
    createRateBasedRule_name,
    createRateBasedRule_metricName,
    createRateBasedRule_rateKey,
    createRateBasedRule_rateLimit,
    createRateBasedRule_changeToken,
    createRateBasedRuleResponse_rule,
    createRateBasedRuleResponse_changeToken,
    createRateBasedRuleResponse_httpStatus,

    -- ** ListRegexPatternSets
    listRegexPatternSets_nextMarker,
    listRegexPatternSets_limit,
    listRegexPatternSetsResponse_regexPatternSets,
    listRegexPatternSetsResponse_nextMarker,
    listRegexPatternSetsResponse_httpStatus,

    -- ** GetSqlInjectionMatchSet
    getSqlInjectionMatchSet_sqlInjectionMatchSetId,
    getSqlInjectionMatchSetResponse_sqlInjectionMatchSet,
    getSqlInjectionMatchSetResponse_httpStatus,

    -- ** CreateRegexPatternSet
    createRegexPatternSet_name,
    createRegexPatternSet_changeToken,
    createRegexPatternSetResponse_regexPatternSet,
    createRegexPatternSetResponse_changeToken,
    createRegexPatternSetResponse_httpStatus,

    -- ** UpdateSizeConstraintSet
    updateSizeConstraintSet_sizeConstraintSetId,
    updateSizeConstraintSet_changeToken,
    updateSizeConstraintSet_updates,
    updateSizeConstraintSetResponse_changeToken,
    updateSizeConstraintSetResponse_httpStatus,

    -- ** GetChangeToken
    getChangeTokenResponse_changeToken,
    getChangeTokenResponse_httpStatus,

    -- ** ListSizeConstraintSets
    listSizeConstraintSets_nextMarker,
    listSizeConstraintSets_limit,
    listSizeConstraintSetsResponse_sizeConstraintSets,
    listSizeConstraintSetsResponse_nextMarker,
    listSizeConstraintSetsResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DeleteSizeConstraintSet
    deleteSizeConstraintSet_sizeConstraintSetId,
    deleteSizeConstraintSet_changeToken,
    deleteSizeConstraintSetResponse_changeToken,
    deleteSizeConstraintSetResponse_httpStatus,

    -- ** ListXssMatchSets
    listXssMatchSets_nextMarker,
    listXssMatchSets_limit,
    listXssMatchSetsResponse_nextMarker,
    listXssMatchSetsResponse_xssMatchSets,
    listXssMatchSetsResponse_httpStatus,

    -- ** DeleteRuleGroup
    deleteRuleGroup_ruleGroupId,
    deleteRuleGroup_changeToken,
    deleteRuleGroupResponse_changeToken,
    deleteRuleGroupResponse_httpStatus,

    -- ** UpdateRuleGroup
    updateRuleGroup_ruleGroupId,
    updateRuleGroup_updates,
    updateRuleGroup_changeToken,
    updateRuleGroupResponse_changeToken,
    updateRuleGroupResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** CreateWebACLMigrationStack
    createWebACLMigrationStack_webACLId,
    createWebACLMigrationStack_s3BucketName,
    createWebACLMigrationStack_ignoreUnsupportedType,
    createWebACLMigrationStackResponse_httpStatus,
    createWebACLMigrationStackResponse_s3ObjectUrl,

    -- ** CreateRegexMatchSet
    createRegexMatchSet_name,
    createRegexMatchSet_changeToken,
    createRegexMatchSetResponse_regexMatchSet,
    createRegexMatchSetResponse_changeToken,
    createRegexMatchSetResponse_httpStatus,

    -- ** CreateRuleGroup
    createRuleGroup_tags,
    createRuleGroup_name,
    createRuleGroup_metricName,
    createRuleGroup_changeToken,
    createRuleGroupResponse_ruleGroup,
    createRuleGroupResponse_changeToken,
    createRuleGroupResponse_httpStatus,

    -- ** ListRegexMatchSets
    listRegexMatchSets_nextMarker,
    listRegexMatchSets_limit,
    listRegexMatchSetsResponse_nextMarker,
    listRegexMatchSetsResponse_regexMatchSets,
    listRegexMatchSetsResponse_httpStatus,

    -- ** UpdateRegexMatchSet
    updateRegexMatchSet_regexMatchSetId,
    updateRegexMatchSet_updates,
    updateRegexMatchSet_changeToken,
    updateRegexMatchSetResponse_changeToken,
    updateRegexMatchSetResponse_httpStatus,

    -- ** DeleteRegexMatchSet
    deleteRegexMatchSet_regexMatchSetId,
    deleteRegexMatchSet_changeToken,
    deleteRegexMatchSetResponse_changeToken,
    deleteRegexMatchSetResponse_httpStatus,

    -- ** GetLoggingConfiguration
    getLoggingConfiguration_resourceArn,
    getLoggingConfigurationResponse_loggingConfiguration,
    getLoggingConfigurationResponse_httpStatus,

    -- ** AssociateWebACL
    associateWebACL_webACLId,
    associateWebACL_resourceArn,
    associateWebACLResponse_httpStatus,

    -- ** DeleteLoggingConfiguration
    deleteLoggingConfiguration_resourceArn,
    deleteLoggingConfigurationResponse_httpStatus,

    -- ** PutPermissionPolicy
    putPermissionPolicy_resourceArn,
    putPermissionPolicy_policy,
    putPermissionPolicyResponse_httpStatus,

    -- ** DeleteIPSet
    deleteIPSet_iPSetId,
    deleteIPSet_changeToken,
    deleteIPSetResponse_changeToken,
    deleteIPSetResponse_httpStatus,

    -- ** CreateRule
    createRule_tags,
    createRule_name,
    createRule_metricName,
    createRule_changeToken,
    createRuleResponse_rule,
    createRuleResponse_changeToken,
    createRuleResponse_httpStatus,

    -- ** ListLoggingConfigurations
    listLoggingConfigurations_nextMarker,
    listLoggingConfigurations_limit,
    listLoggingConfigurationsResponse_loggingConfigurations,
    listLoggingConfigurationsResponse_nextMarker,
    listLoggingConfigurationsResponse_httpStatus,

    -- ** UpdateIPSet
    updateIPSet_iPSetId,
    updateIPSet_changeToken,
    updateIPSet_updates,
    updateIPSetResponse_changeToken,
    updateIPSetResponse_httpStatus,

    -- ** GetRateBasedRuleManagedKeys
    getRateBasedRuleManagedKeys_nextMarker,
    getRateBasedRuleManagedKeys_ruleId,
    getRateBasedRuleManagedKeysResponse_managedKeys,
    getRateBasedRuleManagedKeysResponse_nextMarker,
    getRateBasedRuleManagedKeysResponse_httpStatus,

    -- ** GetGeoMatchSet
    getGeoMatchSet_geoMatchSetId,
    getGeoMatchSetResponse_geoMatchSet,
    getGeoMatchSetResponse_httpStatus,

    -- ** CreateWebACL
    createWebACL_tags,
    createWebACL_name,
    createWebACL_metricName,
    createWebACL_defaultAction,
    createWebACL_changeToken,
    createWebACLResponse_webACL,
    createWebACLResponse_changeToken,
    createWebACLResponse_httpStatus,

    -- ** ListWebACLs
    listWebACLs_nextMarker,
    listWebACLs_limit,
    listWebACLsResponse_nextMarker,
    listWebACLsResponse_webACLs,
    listWebACLsResponse_httpStatus,

    -- ** ListRules
    listRules_nextMarker,
    listRules_limit,
    listRulesResponse_nextMarker,
    listRulesResponse_rules,
    listRulesResponse_httpStatus,

    -- ** CreateByteMatchSet
    createByteMatchSet_name,
    createByteMatchSet_changeToken,
    createByteMatchSetResponse_byteMatchSet,
    createByteMatchSetResponse_changeToken,
    createByteMatchSetResponse_httpStatus,

    -- ** GetXssMatchSet
    getXssMatchSet_xssMatchSetId,
    getXssMatchSetResponse_xssMatchSet,
    getXssMatchSetResponse_httpStatus,

    -- ** CreateIPSet
    createIPSet_name,
    createIPSet_changeToken,
    createIPSetResponse_iPSet,
    createIPSetResponse_changeToken,
    createIPSetResponse_httpStatus,

    -- ** ListSubscribedRuleGroups
    listSubscribedRuleGroups_nextMarker,
    listSubscribedRuleGroups_limit,
    listSubscribedRuleGroupsResponse_nextMarker,
    listSubscribedRuleGroupsResponse_ruleGroups,
    listSubscribedRuleGroupsResponse_httpStatus,

    -- ** ListActivatedRulesInRuleGroup
    listActivatedRulesInRuleGroup_nextMarker,
    listActivatedRulesInRuleGroup_ruleGroupId,
    listActivatedRulesInRuleGroup_limit,
    listActivatedRulesInRuleGroupResponse_nextMarker,
    listActivatedRulesInRuleGroupResponse_activatedRules,
    listActivatedRulesInRuleGroupResponse_httpStatus,

    -- ** DisassociateWebACL
    disassociateWebACL_resourceArn,
    disassociateWebACLResponse_httpStatus,

    -- ** DeleteRateBasedRule
    deleteRateBasedRule_ruleId,
    deleteRateBasedRule_changeToken,
    deleteRateBasedRuleResponse_changeToken,
    deleteRateBasedRuleResponse_httpStatus,

    -- ** UpdateRateBasedRule
    updateRateBasedRule_ruleId,
    updateRateBasedRule_changeToken,
    updateRateBasedRule_updates,
    updateRateBasedRule_rateLimit,
    updateRateBasedRuleResponse_changeToken,
    updateRateBasedRuleResponse_httpStatus,

    -- ** CreateSqlInjectionMatchSet
    createSqlInjectionMatchSet_name,
    createSqlInjectionMatchSet_changeToken,
    createSqlInjectionMatchSetResponse_sqlInjectionMatchSet,
    createSqlInjectionMatchSetResponse_changeToken,
    createSqlInjectionMatchSetResponse_httpStatus,

    -- ** GetRegexPatternSet
    getRegexPatternSet_regexPatternSetId,
    getRegexPatternSetResponse_regexPatternSet,
    getRegexPatternSetResponse_httpStatus,

    -- ** UpdateSqlInjectionMatchSet
    updateSqlInjectionMatchSet_sqlInjectionMatchSetId,
    updateSqlInjectionMatchSet_changeToken,
    updateSqlInjectionMatchSet_updates,
    updateSqlInjectionMatchSetResponse_changeToken,
    updateSqlInjectionMatchSetResponse_httpStatus,

    -- ** DeleteSqlInjectionMatchSet
    deleteSqlInjectionMatchSet_sqlInjectionMatchSetId,
    deleteSqlInjectionMatchSet_changeToken,
    deleteSqlInjectionMatchSetResponse_changeToken,
    deleteSqlInjectionMatchSetResponse_httpStatus,

    -- ** UpdateRegexPatternSet
    updateRegexPatternSet_regexPatternSetId,
    updateRegexPatternSet_updates,
    updateRegexPatternSet_changeToken,
    updateRegexPatternSetResponse_changeToken,
    updateRegexPatternSetResponse_httpStatus,

    -- ** DeleteRegexPatternSet
    deleteRegexPatternSet_regexPatternSetId,
    deleteRegexPatternSet_changeToken,
    deleteRegexPatternSetResponse_changeToken,
    deleteRegexPatternSetResponse_httpStatus,

    -- ** GetSampledRequests
    getSampledRequests_webAclId,
    getSampledRequests_ruleId,
    getSampledRequests_timeWindow,
    getSampledRequests_maxItems,
    getSampledRequestsResponse_timeWindow,
    getSampledRequestsResponse_populationSize,
    getSampledRequestsResponse_sampledRequests,
    getSampledRequestsResponse_httpStatus,

    -- ** ListResourcesForWebACL
    listResourcesForWebACL_resourceType,
    listResourcesForWebACL_webACLId,
    listResourcesForWebACLResponse_resourceArns,
    listResourcesForWebACLResponse_httpStatus,

    -- ** CreateSizeConstraintSet
    createSizeConstraintSet_name,
    createSizeConstraintSet_changeToken,
    createSizeConstraintSetResponse_sizeConstraintSet,
    createSizeConstraintSetResponse_changeToken,
    createSizeConstraintSetResponse_httpStatus,

    -- ** GetRateBasedRule
    getRateBasedRule_ruleId,
    getRateBasedRuleResponse_rule,
    getRateBasedRuleResponse_httpStatus,

    -- ** CreateGeoMatchSet
    createGeoMatchSet_name,
    createGeoMatchSet_changeToken,
    createGeoMatchSetResponse_geoMatchSet,
    createGeoMatchSetResponse_changeToken,
    createGeoMatchSetResponse_httpStatus,

    -- ** DeleteXssMatchSet
    deleteXssMatchSet_xssMatchSetId,
    deleteXssMatchSet_changeToken,
    deleteXssMatchSetResponse_changeToken,
    deleteXssMatchSetResponse_httpStatus,

    -- ** GetRule
    getRule_ruleId,
    getRuleResponse_rule,
    getRuleResponse_httpStatus,

    -- ** ListRuleGroups
    listRuleGroups_nextMarker,
    listRuleGroups_limit,
    listRuleGroupsResponse_nextMarker,
    listRuleGroupsResponse_ruleGroups,
    listRuleGroupsResponse_httpStatus,

    -- ** UpdateXssMatchSet
    updateXssMatchSet_xssMatchSetId,
    updateXssMatchSet_changeToken,
    updateXssMatchSet_updates,
    updateXssMatchSetResponse_changeToken,
    updateXssMatchSetResponse_httpStatus,

    -- ** GetWebACL
    getWebACL_webACLId,
    getWebACLResponse_webACL,
    getWebACLResponse_httpStatus,

    -- ** UpdateGeoMatchSet
    updateGeoMatchSet_geoMatchSetId,
    updateGeoMatchSet_changeToken,
    updateGeoMatchSet_updates,
    updateGeoMatchSetResponse_changeToken,
    updateGeoMatchSetResponse_httpStatus,

    -- ** GetPermissionPolicy
    getPermissionPolicy_resourceArn,
    getPermissionPolicyResponse_policy,
    getPermissionPolicyResponse_httpStatus,

    -- ** ListGeoMatchSets
    listGeoMatchSets_nextMarker,
    listGeoMatchSets_limit,
    listGeoMatchSetsResponse_geoMatchSets,
    listGeoMatchSetsResponse_nextMarker,
    listGeoMatchSetsResponse_httpStatus,

    -- ** GetByteMatchSet
    getByteMatchSet_byteMatchSetId,
    getByteMatchSetResponse_byteMatchSet,
    getByteMatchSetResponse_httpStatus,

    -- ** CreateXssMatchSet
    createXssMatchSet_name,
    createXssMatchSet_changeToken,
    createXssMatchSetResponse_xssMatchSet,
    createXssMatchSetResponse_changeToken,
    createXssMatchSetResponse_httpStatus,

    -- ** GetIPSet
    getIPSet_iPSetId,
    getIPSetResponse_iPSet,
    getIPSetResponse_httpStatus,

    -- ** DeleteGeoMatchSet
    deleteGeoMatchSet_geoMatchSetId,
    deleteGeoMatchSet_changeToken,
    deleteGeoMatchSetResponse_changeToken,
    deleteGeoMatchSetResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextMarker,
    listTagsForResource_limit,
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_nextMarker,
    listTagsForResourceResponse_tagInfoForResource,
    listTagsForResourceResponse_httpStatus,

    -- ** UpdateByteMatchSet
    updateByteMatchSet_byteMatchSetId,
    updateByteMatchSet_changeToken,
    updateByteMatchSet_updates,
    updateByteMatchSetResponse_changeToken,
    updateByteMatchSetResponse_httpStatus,

    -- ** DeleteByteMatchSet
    deleteByteMatchSet_byteMatchSetId,
    deleteByteMatchSet_changeToken,
    deleteByteMatchSetResponse_changeToken,
    deleteByteMatchSetResponse_httpStatus,

    -- ** GetRegexMatchSet
    getRegexMatchSet_regexMatchSetId,
    getRegexMatchSetResponse_regexMatchSet,
    getRegexMatchSetResponse_httpStatus,

    -- ** ListByteMatchSets
    listByteMatchSets_nextMarker,
    listByteMatchSets_limit,
    listByteMatchSetsResponse_nextMarker,
    listByteMatchSetsResponse_byteMatchSets,
    listByteMatchSetsResponse_httpStatus,

    -- ** DeletePermissionPolicy
    deletePermissionPolicy_resourceArn,
    deletePermissionPolicyResponse_httpStatus,

    -- ** ListIPSets
    listIPSets_nextMarker,
    listIPSets_limit,
    listIPSetsResponse_nextMarker,
    listIPSetsResponse_iPSets,
    listIPSetsResponse_httpStatus,

    -- ** PutLoggingConfiguration
    putLoggingConfiguration_loggingConfiguration,
    putLoggingConfigurationResponse_loggingConfiguration,
    putLoggingConfigurationResponse_httpStatus,

    -- * Types

    -- ** ActivatedRule
    activatedRule_excludedRules,
    activatedRule_overrideAction,
    activatedRule_action,
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
    hTTPRequest_headers,
    hTTPRequest_uri,
    hTTPRequest_method,
    hTTPRequest_clientIP,
    hTTPRequest_country,
    hTTPRequest_hTTPVersion,

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
    regexMatchSet_regexMatchSetId,
    regexMatchSet_regexMatchTuples,
    regexMatchSet_name,

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
    sampledHTTPRequest_timestamp,
    sampledHTTPRequest_action,
    sampledHTTPRequest_ruleWithinRuleGroup,
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
    webACL_webACLArn,
    webACL_name,
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
import Network.AWS.WAFRegional.Types.ActivatedRule
import Network.AWS.WAFRegional.Types.ByteMatchSet
import Network.AWS.WAFRegional.Types.ByteMatchSetSummary
import Network.AWS.WAFRegional.Types.ByteMatchSetUpdate
import Network.AWS.WAFRegional.Types.ByteMatchTuple
import Network.AWS.WAFRegional.Types.ExcludedRule
import Network.AWS.WAFRegional.Types.FieldToMatch
import Network.AWS.WAFRegional.Types.GeoMatchConstraint
import Network.AWS.WAFRegional.Types.GeoMatchSet
import Network.AWS.WAFRegional.Types.GeoMatchSetSummary
import Network.AWS.WAFRegional.Types.GeoMatchSetUpdate
import Network.AWS.WAFRegional.Types.HTTPHeader
import Network.AWS.WAFRegional.Types.HTTPRequest
import Network.AWS.WAFRegional.Types.IPSet
import Network.AWS.WAFRegional.Types.IPSetDescriptor
import Network.AWS.WAFRegional.Types.IPSetSummary
import Network.AWS.WAFRegional.Types.IPSetUpdate
import Network.AWS.WAFRegional.Types.LoggingConfiguration
import Network.AWS.WAFRegional.Types.Predicate
import Network.AWS.WAFRegional.Types.RateBasedRule
import Network.AWS.WAFRegional.Types.RegexMatchSet
import Network.AWS.WAFRegional.Types.RegexMatchSetSummary
import Network.AWS.WAFRegional.Types.RegexMatchSetUpdate
import Network.AWS.WAFRegional.Types.RegexMatchTuple
import Network.AWS.WAFRegional.Types.RegexPatternSet
import Network.AWS.WAFRegional.Types.RegexPatternSetSummary
import Network.AWS.WAFRegional.Types.RegexPatternSetUpdate
import Network.AWS.WAFRegional.Types.Rule
import Network.AWS.WAFRegional.Types.RuleGroup
import Network.AWS.WAFRegional.Types.RuleGroupSummary
import Network.AWS.WAFRegional.Types.RuleGroupUpdate
import Network.AWS.WAFRegional.Types.RuleSummary
import Network.AWS.WAFRegional.Types.RuleUpdate
import Network.AWS.WAFRegional.Types.SampledHTTPRequest
import Network.AWS.WAFRegional.Types.SizeConstraint
import Network.AWS.WAFRegional.Types.SizeConstraintSet
import Network.AWS.WAFRegional.Types.SizeConstraintSetSummary
import Network.AWS.WAFRegional.Types.SizeConstraintSetUpdate
import Network.AWS.WAFRegional.Types.SqlInjectionMatchSet
import Network.AWS.WAFRegional.Types.SqlInjectionMatchSetSummary
import Network.AWS.WAFRegional.Types.SqlInjectionMatchSetUpdate
import Network.AWS.WAFRegional.Types.SqlInjectionMatchTuple
import Network.AWS.WAFRegional.Types.SubscribedRuleGroupSummary
import Network.AWS.WAFRegional.Types.Tag
import Network.AWS.WAFRegional.Types.TagInfoForResource
import Network.AWS.WAFRegional.Types.TimeWindow
import Network.AWS.WAFRegional.Types.WafAction
import Network.AWS.WAFRegional.Types.WafOverrideAction
import Network.AWS.WAFRegional.Types.WebACL
import Network.AWS.WAFRegional.Types.WebACLSummary
import Network.AWS.WAFRegional.Types.WebACLUpdate
import Network.AWS.WAFRegional.Types.XssMatchSet
import Network.AWS.WAFRegional.Types.XssMatchSetSummary
import Network.AWS.WAFRegional.Types.XssMatchSetUpdate
import Network.AWS.WAFRegional.Types.XssMatchTuple
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
