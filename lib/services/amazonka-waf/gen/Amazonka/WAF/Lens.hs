{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WAF.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAF.Lens
  ( -- * Operations

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
import Amazonka.WAF.Types.ActivatedRule
import Amazonka.WAF.Types.ByteMatchSet
import Amazonka.WAF.Types.ByteMatchSetSummary
import Amazonka.WAF.Types.ByteMatchSetUpdate
import Amazonka.WAF.Types.ByteMatchTuple
import Amazonka.WAF.Types.ExcludedRule
import Amazonka.WAF.Types.FieldToMatch
import Amazonka.WAF.Types.GeoMatchConstraint
import Amazonka.WAF.Types.GeoMatchSet
import Amazonka.WAF.Types.GeoMatchSetSummary
import Amazonka.WAF.Types.GeoMatchSetUpdate
import Amazonka.WAF.Types.HTTPHeader
import Amazonka.WAF.Types.HTTPRequest
import Amazonka.WAF.Types.IPSet
import Amazonka.WAF.Types.IPSetDescriptor
import Amazonka.WAF.Types.IPSetSummary
import Amazonka.WAF.Types.IPSetUpdate
import Amazonka.WAF.Types.LoggingConfiguration
import Amazonka.WAF.Types.Predicate
import Amazonka.WAF.Types.RateBasedRule
import Amazonka.WAF.Types.RegexMatchSet
import Amazonka.WAF.Types.RegexMatchSetSummary
import Amazonka.WAF.Types.RegexMatchSetUpdate
import Amazonka.WAF.Types.RegexMatchTuple
import Amazonka.WAF.Types.RegexPatternSet
import Amazonka.WAF.Types.RegexPatternSetSummary
import Amazonka.WAF.Types.RegexPatternSetUpdate
import Amazonka.WAF.Types.Rule
import Amazonka.WAF.Types.RuleGroup
import Amazonka.WAF.Types.RuleGroupSummary
import Amazonka.WAF.Types.RuleGroupUpdate
import Amazonka.WAF.Types.RuleSummary
import Amazonka.WAF.Types.RuleUpdate
import Amazonka.WAF.Types.SampledHTTPRequest
import Amazonka.WAF.Types.SizeConstraint
import Amazonka.WAF.Types.SizeConstraintSet
import Amazonka.WAF.Types.SizeConstraintSetSummary
import Amazonka.WAF.Types.SizeConstraintSetUpdate
import Amazonka.WAF.Types.SqlInjectionMatchSet
import Amazonka.WAF.Types.SqlInjectionMatchSetSummary
import Amazonka.WAF.Types.SqlInjectionMatchSetUpdate
import Amazonka.WAF.Types.SqlInjectionMatchTuple
import Amazonka.WAF.Types.SubscribedRuleGroupSummary
import Amazonka.WAF.Types.Tag
import Amazonka.WAF.Types.TagInfoForResource
import Amazonka.WAF.Types.TimeWindow
import Amazonka.WAF.Types.WafAction
import Amazonka.WAF.Types.WafOverrideAction
import Amazonka.WAF.Types.WebACL
import Amazonka.WAF.Types.WebACLSummary
import Amazonka.WAF.Types.WebACLUpdate
import Amazonka.WAF.Types.XssMatchSet
import Amazonka.WAF.Types.XssMatchSetSummary
import Amazonka.WAF.Types.XssMatchSetUpdate
import Amazonka.WAF.Types.XssMatchTuple
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
