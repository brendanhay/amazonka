{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Lens
  ( -- * Operations

    -- ** ListActivatedRulesInRuleGroup
    listActivatedRulesInRuleGroup_ruleGroupId,
    listActivatedRulesInRuleGroup_nextMarker,
    listActivatedRulesInRuleGroup_limit,
    listActivatedRulesInRuleGroupResponse_nextMarker,
    listActivatedRulesInRuleGroupResponse_activatedRules,
    listActivatedRulesInRuleGroupResponse_httpStatus,

    -- ** ListRateBasedRules
    listRateBasedRules_nextMarker,
    listRateBasedRules_limit,
    listRateBasedRulesResponse_rules,
    listRateBasedRulesResponse_nextMarker,
    listRateBasedRulesResponse_httpStatus,

    -- ** GetSizeConstraintSet
    getSizeConstraintSet_sizeConstraintSetId,
    getSizeConstraintSetResponse_sizeConstraintSet,
    getSizeConstraintSetResponse_httpStatus,

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

    -- ** UpdateRule
    updateRule_ruleId,
    updateRule_changeToken,
    updateRule_updates,
    updateRuleResponse_changeToken,
    updateRuleResponse_httpStatus,

    -- ** DeleteRule
    deleteRule_ruleId,
    deleteRule_changeToken,
    deleteRuleResponse_changeToken,
    deleteRuleResponse_httpStatus,

    -- ** CreateIPSet
    createIPSet_name,
    createIPSet_changeToken,
    createIPSetResponse_changeToken,
    createIPSetResponse_iPSet,
    createIPSetResponse_httpStatus,

    -- ** GetRuleGroup
    getRuleGroup_ruleGroupId,
    getRuleGroupResponse_ruleGroup,
    getRuleGroupResponse_httpStatus,

    -- ** GetChangeTokenStatus
    getChangeTokenStatus_changeToken,
    getChangeTokenStatusResponse_changeTokenStatus,
    getChangeTokenStatusResponse_httpStatus,

    -- ** DeleteWebACL
    deleteWebACL_webACLId,
    deleteWebACL_changeToken,
    deleteWebACLResponse_changeToken,
    deleteWebACLResponse_httpStatus,

    -- ** UpdateWebACL
    updateWebACL_updates,
    updateWebACL_defaultAction,
    updateWebACL_webACLId,
    updateWebACL_changeToken,
    updateWebACLResponse_changeToken,
    updateWebACLResponse_httpStatus,

    -- ** ListWebACLs
    listWebACLs_nextMarker,
    listWebACLs_limit,
    listWebACLsResponse_webACLs,
    listWebACLsResponse_nextMarker,
    listWebACLsResponse_httpStatus,

    -- ** ListRules
    listRules_nextMarker,
    listRules_limit,
    listRulesResponse_rules,
    listRulesResponse_nextMarker,
    listRulesResponse_httpStatus,

    -- ** CreateRule
    createRule_tags,
    createRule_name,
    createRule_metricName,
    createRule_changeToken,
    createRuleResponse_rule,
    createRuleResponse_changeToken,
    createRuleResponse_httpStatus,

    -- ** DeleteLoggingConfiguration
    deleteLoggingConfiguration_resourceArn,
    deleteLoggingConfigurationResponse_httpStatus,

    -- ** CreateWebACL
    createWebACL_tags,
    createWebACL_name,
    createWebACL_metricName,
    createWebACL_defaultAction,
    createWebACL_changeToken,
    createWebACLResponse_webACL,
    createWebACLResponse_changeToken,
    createWebACLResponse_httpStatus,

    -- ** GetGeoMatchSet
    getGeoMatchSet_geoMatchSetId,
    getGeoMatchSetResponse_geoMatchSet,
    getGeoMatchSetResponse_httpStatus,

    -- ** PutLoggingConfiguration
    putLoggingConfiguration_loggingConfiguration,
    putLoggingConfigurationResponse_loggingConfiguration,
    putLoggingConfigurationResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextMarker,
    listTagsForResource_limit,
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tagInfoForResource,
    listTagsForResourceResponse_nextMarker,
    listTagsForResourceResponse_httpStatus,

    -- ** ListByteMatchSets
    listByteMatchSets_nextMarker,
    listByteMatchSets_limit,
    listByteMatchSetsResponse_byteMatchSets,
    listByteMatchSetsResponse_nextMarker,
    listByteMatchSetsResponse_httpStatus,

    -- ** ListGeoMatchSets
    listGeoMatchSets_nextMarker,
    listGeoMatchSets_limit,
    listGeoMatchSetsResponse_geoMatchSets,
    listGeoMatchSetsResponse_nextMarker,
    listGeoMatchSetsResponse_httpStatus,

    -- ** GetLoggingConfiguration
    getLoggingConfiguration_resourceArn,
    getLoggingConfigurationResponse_loggingConfiguration,
    getLoggingConfigurationResponse_httpStatus,

    -- ** CreateRuleGroup
    createRuleGroup_tags,
    createRuleGroup_name,
    createRuleGroup_metricName,
    createRuleGroup_changeToken,
    createRuleGroupResponse_changeToken,
    createRuleGroupResponse_ruleGroup,
    createRuleGroupResponse_httpStatus,

    -- ** DeleteRegexMatchSet
    deleteRegexMatchSet_regexMatchSetId,
    deleteRegexMatchSet_changeToken,
    deleteRegexMatchSetResponse_changeToken,
    deleteRegexMatchSetResponse_httpStatus,

    -- ** UpdateRegexMatchSet
    updateRegexMatchSet_regexMatchSetId,
    updateRegexMatchSet_updates,
    updateRegexMatchSet_changeToken,
    updateRegexMatchSetResponse_changeToken,
    updateRegexMatchSetResponse_httpStatus,

    -- ** GetIPSet
    getIPSet_iPSetId,
    getIPSetResponse_iPSet,
    getIPSetResponse_httpStatus,

    -- ** GetWebACL
    getWebACL_webACLId,
    getWebACLResponse_webACL,
    getWebACLResponse_httpStatus,

    -- ** GetRule
    getRule_ruleId,
    getRuleResponse_rule,
    getRuleResponse_httpStatus,

    -- ** DeleteXssMatchSet
    deleteXssMatchSet_xssMatchSetId,
    deleteXssMatchSet_changeToken,
    deleteXssMatchSetResponse_changeToken,
    deleteXssMatchSetResponse_httpStatus,

    -- ** UpdateXssMatchSet
    updateXssMatchSet_xssMatchSetId,
    updateXssMatchSet_changeToken,
    updateXssMatchSet_updates,
    updateXssMatchSetResponse_changeToken,
    updateXssMatchSetResponse_httpStatus,

    -- ** CreateWebACLMigrationStack
    createWebACLMigrationStack_webACLId,
    createWebACLMigrationStack_s3BucketName,
    createWebACLMigrationStack_ignoreUnsupportedType,
    createWebACLMigrationStackResponse_httpStatus,
    createWebACLMigrationStackResponse_s3ObjectUrl,

    -- ** ListXssMatchSets
    listXssMatchSets_nextMarker,
    listXssMatchSets_limit,
    listXssMatchSetsResponse_xssMatchSets,
    listXssMatchSetsResponse_nextMarker,
    listXssMatchSetsResponse_httpStatus,

    -- ** CreateGeoMatchSet
    createGeoMatchSet_name,
    createGeoMatchSet_changeToken,
    createGeoMatchSetResponse_geoMatchSet,
    createGeoMatchSetResponse_changeToken,
    createGeoMatchSetResponse_httpStatus,

    -- ** GetChangeToken
    getChangeTokenResponse_changeToken,
    getChangeTokenResponse_httpStatus,

    -- ** ListSizeConstraintSets
    listSizeConstraintSets_nextMarker,
    listSizeConstraintSets_limit,
    listSizeConstraintSetsResponse_sizeConstraintSets,
    listSizeConstraintSetsResponse_nextMarker,
    listSizeConstraintSetsResponse_httpStatus,

    -- ** GetSampledRequests
    getSampledRequests_webAclId,
    getSampledRequests_ruleId,
    getSampledRequests_timeWindow,
    getSampledRequests_maxItems,
    getSampledRequestsResponse_sampledRequests,
    getSampledRequestsResponse_populationSize,
    getSampledRequestsResponse_timeWindow,
    getSampledRequestsResponse_httpStatus,

    -- ** GetSqlInjectionMatchSet
    getSqlInjectionMatchSet_sqlInjectionMatchSetId,
    getSqlInjectionMatchSetResponse_sqlInjectionMatchSet,
    getSqlInjectionMatchSetResponse_httpStatus,

    -- ** ListSubscribedRuleGroups
    listSubscribedRuleGroups_nextMarker,
    listSubscribedRuleGroups_limit,
    listSubscribedRuleGroupsResponse_ruleGroups,
    listSubscribedRuleGroupsResponse_nextMarker,
    listSubscribedRuleGroupsResponse_httpStatus,

    -- ** CreateSqlInjectionMatchSet
    createSqlInjectionMatchSet_name,
    createSqlInjectionMatchSet_changeToken,
    createSqlInjectionMatchSetResponse_sqlInjectionMatchSet,
    createSqlInjectionMatchSetResponse_changeToken,
    createSqlInjectionMatchSetResponse_httpStatus,

    -- ** GetXssMatchSet
    getXssMatchSet_xssMatchSetId,
    getXssMatchSetResponse_xssMatchSet,
    getXssMatchSetResponse_httpStatus,

    -- ** CreateByteMatchSet
    createByteMatchSet_name,
    createByteMatchSet_changeToken,
    createByteMatchSetResponse_byteMatchSet,
    createByteMatchSetResponse_changeToken,
    createByteMatchSetResponse_httpStatus,

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

    -- ** PutPermissionPolicy
    putPermissionPolicy_resourceArn,
    putPermissionPolicy_policy,
    putPermissionPolicyResponse_httpStatus,

    -- ** ListLoggingConfigurations
    listLoggingConfigurations_nextMarker,
    listLoggingConfigurations_limit,
    listLoggingConfigurationsResponse_nextMarker,
    listLoggingConfigurationsResponse_loggingConfigurations,
    listLoggingConfigurationsResponse_httpStatus,

    -- ** GetRateBasedRuleManagedKeys
    getRateBasedRuleManagedKeys_nextMarker,
    getRateBasedRuleManagedKeys_ruleId,
    getRateBasedRuleManagedKeysResponse_nextMarker,
    getRateBasedRuleManagedKeysResponse_managedKeys,
    getRateBasedRuleManagedKeysResponse_httpStatus,

    -- ** DeletePermissionPolicy
    deletePermissionPolicy_resourceArn,
    deletePermissionPolicyResponse_httpStatus,

    -- ** GetRegexMatchSet
    getRegexMatchSet_regexMatchSetId,
    getRegexMatchSetResponse_regexMatchSet,
    getRegexMatchSetResponse_httpStatus,

    -- ** DeleteIPSet
    deleteIPSet_iPSetId,
    deleteIPSet_changeToken,
    deleteIPSetResponse_changeToken,
    deleteIPSetResponse_httpStatus,

    -- ** UpdateIPSet
    updateIPSet_iPSetId,
    updateIPSet_changeToken,
    updateIPSet_updates,
    updateIPSetResponse_changeToken,
    updateIPSetResponse_httpStatus,

    -- ** ListIPSets
    listIPSets_nextMarker,
    listIPSets_limit,
    listIPSetsResponse_nextMarker,
    listIPSetsResponse_iPSets,
    listIPSetsResponse_httpStatus,

    -- ** ListRegexMatchSets
    listRegexMatchSets_nextMarker,
    listRegexMatchSets_limit,
    listRegexMatchSetsResponse_regexMatchSets,
    listRegexMatchSetsResponse_nextMarker,
    listRegexMatchSetsResponse_httpStatus,

    -- ** CreateXssMatchSet
    createXssMatchSet_name,
    createXssMatchSet_changeToken,
    createXssMatchSetResponse_xssMatchSet,
    createXssMatchSetResponse_changeToken,
    createXssMatchSetResponse_httpStatus,

    -- ** DeleteGeoMatchSet
    deleteGeoMatchSet_geoMatchSetId,
    deleteGeoMatchSet_changeToken,
    deleteGeoMatchSetResponse_changeToken,
    deleteGeoMatchSetResponse_httpStatus,

    -- ** UpdateGeoMatchSet
    updateGeoMatchSet_geoMatchSetId,
    updateGeoMatchSet_changeToken,
    updateGeoMatchSet_updates,
    updateGeoMatchSetResponse_changeToken,
    updateGeoMatchSetResponse_httpStatus,

    -- ** GetByteMatchSet
    getByteMatchSet_byteMatchSetId,
    getByteMatchSetResponse_byteMatchSet,
    getByteMatchSetResponse_httpStatus,

    -- ** GetPermissionPolicy
    getPermissionPolicy_resourceArn,
    getPermissionPolicyResponse_policy,
    getPermissionPolicyResponse_httpStatus,

    -- ** ListRuleGroups
    listRuleGroups_nextMarker,
    listRuleGroups_limit,
    listRuleGroupsResponse_ruleGroups,
    listRuleGroupsResponse_nextMarker,
    listRuleGroupsResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

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

    -- ** CreateRegexMatchSet
    createRegexMatchSet_name,
    createRegexMatchSet_changeToken,
    createRegexMatchSetResponse_regexMatchSet,
    createRegexMatchSetResponse_changeToken,
    createRegexMatchSetResponse_httpStatus,

    -- ** GetRateBasedRule
    getRateBasedRule_ruleId,
    getRateBasedRuleResponse_rule,
    getRateBasedRuleResponse_httpStatus,

    -- ** CreateRegexPatternSet
    createRegexPatternSet_name,
    createRegexPatternSet_changeToken,
    createRegexPatternSetResponse_regexPatternSet,
    createRegexPatternSetResponse_changeToken,
    createRegexPatternSetResponse_httpStatus,

    -- ** DeleteSizeConstraintSet
    deleteSizeConstraintSet_sizeConstraintSetId,
    deleteSizeConstraintSet_changeToken,
    deleteSizeConstraintSetResponse_changeToken,
    deleteSizeConstraintSetResponse_httpStatus,

    -- ** UpdateSizeConstraintSet
    updateSizeConstraintSet_sizeConstraintSetId,
    updateSizeConstraintSet_changeToken,
    updateSizeConstraintSet_updates,
    updateSizeConstraintSetResponse_changeToken,
    updateSizeConstraintSetResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DeleteRegexPatternSet
    deleteRegexPatternSet_regexPatternSetId,
    deleteRegexPatternSet_changeToken,
    deleteRegexPatternSetResponse_changeToken,
    deleteRegexPatternSetResponse_httpStatus,

    -- ** UpdateRegexPatternSet
    updateRegexPatternSet_regexPatternSetId,
    updateRegexPatternSet_updates,
    updateRegexPatternSet_changeToken,
    updateRegexPatternSetResponse_changeToken,
    updateRegexPatternSetResponse_httpStatus,

    -- ** CreateSizeConstraintSet
    createSizeConstraintSet_name,
    createSizeConstraintSet_changeToken,
    createSizeConstraintSetResponse_sizeConstraintSet,
    createSizeConstraintSetResponse_changeToken,
    createSizeConstraintSetResponse_httpStatus,

    -- ** ListRegexPatternSets
    listRegexPatternSets_nextMarker,
    listRegexPatternSets_limit,
    listRegexPatternSetsResponse_regexPatternSets,
    listRegexPatternSetsResponse_nextMarker,
    listRegexPatternSetsResponse_httpStatus,

    -- ** ListSqlInjectionMatchSets
    listSqlInjectionMatchSets_nextMarker,
    listSqlInjectionMatchSets_limit,
    listSqlInjectionMatchSetsResponse_nextMarker,
    listSqlInjectionMatchSetsResponse_sqlInjectionMatchSets,
    listSqlInjectionMatchSetsResponse_httpStatus,

    -- ** GetRegexPatternSet
    getRegexPatternSet_regexPatternSetId,
    getRegexPatternSetResponse_regexPatternSet,
    getRegexPatternSetResponse_httpStatus,

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

    -- ** DeleteSqlInjectionMatchSet
    deleteSqlInjectionMatchSet_sqlInjectionMatchSetId,
    deleteSqlInjectionMatchSet_changeToken,
    deleteSqlInjectionMatchSetResponse_changeToken,
    deleteSqlInjectionMatchSetResponse_httpStatus,

    -- ** UpdateSqlInjectionMatchSet
    updateSqlInjectionMatchSet_sqlInjectionMatchSetId,
    updateSqlInjectionMatchSet_changeToken,
    updateSqlInjectionMatchSet_updates,
    updateSqlInjectionMatchSetResponse_changeToken,
    updateSqlInjectionMatchSetResponse_httpStatus,

    -- * Types

    -- ** ActivatedRule
    activatedRule_overrideAction,
    activatedRule_action,
    activatedRule_excludedRules,
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
    hTTPHeader_value,
    hTTPHeader_name,

    -- ** HTTPRequest
    hTTPRequest_hTTPVersion,
    hTTPRequest_country,
    hTTPRequest_uri,
    hTTPRequest_headers,
    hTTPRequest_method,
    hTTPRequest_clientIP,

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
    regexMatchSet_regexMatchTuples,
    regexMatchSet_regexMatchSetId,

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
    sampledHTTPRequest_ruleWithinRuleGroup,
    sampledHTTPRequest_action,
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
    tagInfoForResource_tagList,
    tagInfoForResource_resourceARN,

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
import Network.AWS.WAF.Types.ActivatedRule
import Network.AWS.WAF.Types.ByteMatchSet
import Network.AWS.WAF.Types.ByteMatchSetSummary
import Network.AWS.WAF.Types.ByteMatchSetUpdate
import Network.AWS.WAF.Types.ByteMatchTuple
import Network.AWS.WAF.Types.ExcludedRule
import Network.AWS.WAF.Types.FieldToMatch
import Network.AWS.WAF.Types.GeoMatchConstraint
import Network.AWS.WAF.Types.GeoMatchSet
import Network.AWS.WAF.Types.GeoMatchSetSummary
import Network.AWS.WAF.Types.GeoMatchSetUpdate
import Network.AWS.WAF.Types.HTTPHeader
import Network.AWS.WAF.Types.HTTPRequest
import Network.AWS.WAF.Types.IPSet
import Network.AWS.WAF.Types.IPSetDescriptor
import Network.AWS.WAF.Types.IPSetSummary
import Network.AWS.WAF.Types.IPSetUpdate
import Network.AWS.WAF.Types.LoggingConfiguration
import Network.AWS.WAF.Types.Predicate
import Network.AWS.WAF.Types.RateBasedRule
import Network.AWS.WAF.Types.RegexMatchSet
import Network.AWS.WAF.Types.RegexMatchSetSummary
import Network.AWS.WAF.Types.RegexMatchSetUpdate
import Network.AWS.WAF.Types.RegexMatchTuple
import Network.AWS.WAF.Types.RegexPatternSet
import Network.AWS.WAF.Types.RegexPatternSetSummary
import Network.AWS.WAF.Types.RegexPatternSetUpdate
import Network.AWS.WAF.Types.Rule
import Network.AWS.WAF.Types.RuleGroup
import Network.AWS.WAF.Types.RuleGroupSummary
import Network.AWS.WAF.Types.RuleGroupUpdate
import Network.AWS.WAF.Types.RuleSummary
import Network.AWS.WAF.Types.RuleUpdate
import Network.AWS.WAF.Types.SampledHTTPRequest
import Network.AWS.WAF.Types.SizeConstraint
import Network.AWS.WAF.Types.SizeConstraintSet
import Network.AWS.WAF.Types.SizeConstraintSetSummary
import Network.AWS.WAF.Types.SizeConstraintSetUpdate
import Network.AWS.WAF.Types.SqlInjectionMatchSet
import Network.AWS.WAF.Types.SqlInjectionMatchSetSummary
import Network.AWS.WAF.Types.SqlInjectionMatchSetUpdate
import Network.AWS.WAF.Types.SqlInjectionMatchTuple
import Network.AWS.WAF.Types.SubscribedRuleGroupSummary
import Network.AWS.WAF.Types.Tag
import Network.AWS.WAF.Types.TagInfoForResource
import Network.AWS.WAF.Types.TimeWindow
import Network.AWS.WAF.Types.WafAction
import Network.AWS.WAF.Types.WafOverrideAction
import Network.AWS.WAF.Types.WebACL
import Network.AWS.WAF.Types.WebACLSummary
import Network.AWS.WAF.Types.WebACLUpdate
import Network.AWS.WAF.Types.XssMatchSet
import Network.AWS.WAF.Types.XssMatchSetSummary
import Network.AWS.WAF.Types.XssMatchSetUpdate
import Network.AWS.WAF.Types.XssMatchTuple
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
