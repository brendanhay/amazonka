{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DLM.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DLM.Lens
  ( -- * Operations

    -- ** DeleteLifecyclePolicy
    deleteLifecyclePolicy_policyId,
    deleteLifecyclePolicyResponse_httpStatus,

    -- ** UpdateLifecyclePolicy
    updateLifecyclePolicy_state,
    updateLifecyclePolicy_policyDetails,
    updateLifecyclePolicy_executionRoleArn,
    updateLifecyclePolicy_description,
    updateLifecyclePolicy_policyId,
    updateLifecyclePolicyResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** CreateLifecyclePolicy
    createLifecyclePolicy_tags,
    createLifecyclePolicy_executionRoleArn,
    createLifecyclePolicy_description,
    createLifecyclePolicy_state,
    createLifecyclePolicy_policyDetails,
    createLifecyclePolicyResponse_policyId,
    createLifecyclePolicyResponse_httpStatus,

    -- ** GetLifecyclePolicy
    getLifecyclePolicy_policyId,
    getLifecyclePolicyResponse_policy,
    getLifecyclePolicyResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** GetLifecyclePolicies
    getLifecyclePolicies_state,
    getLifecyclePolicies_targetTags,
    getLifecyclePolicies_tagsToAdd,
    getLifecyclePolicies_policyIds,
    getLifecyclePolicies_resourceTypes,
    getLifecyclePoliciesResponse_policies,
    getLifecyclePoliciesResponse_httpStatus,

    -- * Types

    -- ** Action
    action_name,
    action_crossRegionCopy,

    -- ** CreateRule
    createRule_location,
    createRule_interval,
    createRule_cronExpression,
    createRule_times,
    createRule_intervalUnit,

    -- ** CrossRegionCopyAction
    crossRegionCopyAction_retainRule,
    crossRegionCopyAction_target,
    crossRegionCopyAction_encryptionConfiguration,

    -- ** CrossRegionCopyDeprecateRule
    crossRegionCopyDeprecateRule_interval,
    crossRegionCopyDeprecateRule_intervalUnit,

    -- ** CrossRegionCopyRetainRule
    crossRegionCopyRetainRule_interval,
    crossRegionCopyRetainRule_intervalUnit,

    -- ** CrossRegionCopyRule
    crossRegionCopyRule_deprecateRule,
    crossRegionCopyRule_targetRegion,
    crossRegionCopyRule_copyTags,
    crossRegionCopyRule_cmkArn,
    crossRegionCopyRule_retainRule,
    crossRegionCopyRule_target,
    crossRegionCopyRule_encrypted,

    -- ** DeprecateRule
    deprecateRule_count,
    deprecateRule_interval,
    deprecateRule_intervalUnit,

    -- ** EncryptionConfiguration
    encryptionConfiguration_cmkArn,
    encryptionConfiguration_encrypted,

    -- ** EventParameters
    eventParameters_eventType,
    eventParameters_snapshotOwner,
    eventParameters_descriptionRegex,

    -- ** EventSource
    eventSource_parameters,
    eventSource_type,

    -- ** FastRestoreRule
    fastRestoreRule_count,
    fastRestoreRule_interval,
    fastRestoreRule_intervalUnit,
    fastRestoreRule_availabilityZones,

    -- ** LifecyclePolicy
    lifecyclePolicy_state,
    lifecyclePolicy_policyDetails,
    lifecyclePolicy_policyId,
    lifecyclePolicy_executionRoleArn,
    lifecyclePolicy_dateCreated,
    lifecyclePolicy_statusMessage,
    lifecyclePolicy_dateModified,
    lifecyclePolicy_policyArn,
    lifecyclePolicy_description,
    lifecyclePolicy_tags,

    -- ** LifecyclePolicySummary
    lifecyclePolicySummary_state,
    lifecyclePolicySummary_policyId,
    lifecyclePolicySummary_policyType,
    lifecyclePolicySummary_description,
    lifecyclePolicySummary_tags,

    -- ** Parameters
    parameters_noReboot,
    parameters_excludeBootVolume,

    -- ** PolicyDetails
    policyDetails_actions,
    policyDetails_targetTags,
    policyDetails_policyType,
    policyDetails_resourceLocations,
    policyDetails_parameters,
    policyDetails_schedules,
    policyDetails_eventSource,
    policyDetails_resourceTypes,

    -- ** RetainRule
    retainRule_count,
    retainRule_interval,
    retainRule_intervalUnit,

    -- ** Schedule
    schedule_variableTags,
    schedule_createRule,
    schedule_deprecateRule,
    schedule_copyTags,
    schedule_name,
    schedule_shareRules,
    schedule_tagsToAdd,
    schedule_retainRule,
    schedule_crossRegionCopyRules,
    schedule_fastRestoreRule,

    -- ** ShareRule
    shareRule_unshareIntervalUnit,
    shareRule_unshareInterval,
    shareRule_targetAccounts,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Network.AWS.DLM.CreateLifecyclePolicy
import Network.AWS.DLM.DeleteLifecyclePolicy
import Network.AWS.DLM.GetLifecyclePolicies
import Network.AWS.DLM.GetLifecyclePolicy
import Network.AWS.DLM.ListTagsForResource
import Network.AWS.DLM.TagResource
import Network.AWS.DLM.Types.Action
import Network.AWS.DLM.Types.CreateRule
import Network.AWS.DLM.Types.CrossRegionCopyAction
import Network.AWS.DLM.Types.CrossRegionCopyDeprecateRule
import Network.AWS.DLM.Types.CrossRegionCopyRetainRule
import Network.AWS.DLM.Types.CrossRegionCopyRule
import Network.AWS.DLM.Types.DeprecateRule
import Network.AWS.DLM.Types.EncryptionConfiguration
import Network.AWS.DLM.Types.EventParameters
import Network.AWS.DLM.Types.EventSource
import Network.AWS.DLM.Types.FastRestoreRule
import Network.AWS.DLM.Types.LifecyclePolicy
import Network.AWS.DLM.Types.LifecyclePolicySummary
import Network.AWS.DLM.Types.Parameters
import Network.AWS.DLM.Types.PolicyDetails
import Network.AWS.DLM.Types.RetainRule
import Network.AWS.DLM.Types.Schedule
import Network.AWS.DLM.Types.ShareRule
import Network.AWS.DLM.Types.Tag
import Network.AWS.DLM.UntagResource
import Network.AWS.DLM.UpdateLifecyclePolicy
