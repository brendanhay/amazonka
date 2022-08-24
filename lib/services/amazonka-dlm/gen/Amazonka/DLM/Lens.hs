{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DLM.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DLM.Lens
  ( -- * Operations

    -- ** CreateLifecyclePolicy
    createLifecyclePolicy_tags,
    createLifecyclePolicy_executionRoleArn,
    createLifecyclePolicy_description,
    createLifecyclePolicy_state,
    createLifecyclePolicy_policyDetails,
    createLifecyclePolicyResponse_policyId,
    createLifecyclePolicyResponse_httpStatus,

    -- ** DeleteLifecyclePolicy
    deleteLifecyclePolicy_policyId,
    deleteLifecyclePolicyResponse_httpStatus,

    -- ** GetLifecyclePolicies
    getLifecyclePolicies_tagsToAdd,
    getLifecyclePolicies_targetTags,
    getLifecyclePolicies_state,
    getLifecyclePolicies_policyIds,
    getLifecyclePolicies_resourceTypes,
    getLifecyclePoliciesResponse_policies,
    getLifecyclePoliciesResponse_httpStatus,

    -- ** GetLifecyclePolicy
    getLifecyclePolicy_policyId,
    getLifecyclePolicyResponse_policy,
    getLifecyclePolicyResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateLifecyclePolicy
    updateLifecyclePolicy_policyDetails,
    updateLifecyclePolicy_state,
    updateLifecyclePolicy_description,
    updateLifecyclePolicy_executionRoleArn,
    updateLifecyclePolicy_policyId,
    updateLifecyclePolicyResponse_httpStatus,

    -- * Types

    -- ** Action
    action_name,
    action_crossRegionCopy,

    -- ** CreateRule
    createRule_interval,
    createRule_location,
    createRule_intervalUnit,
    createRule_times,
    createRule_cronExpression,

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
    crossRegionCopyRule_cmkArn,
    crossRegionCopyRule_copyTags,
    crossRegionCopyRule_retainRule,
    crossRegionCopyRule_target,
    crossRegionCopyRule_targetRegion,
    crossRegionCopyRule_deprecateRule,
    crossRegionCopyRule_encrypted,

    -- ** DeprecateRule
    deprecateRule_interval,
    deprecateRule_count,
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
    fastRestoreRule_interval,
    fastRestoreRule_count,
    fastRestoreRule_intervalUnit,
    fastRestoreRule_availabilityZones,

    -- ** LifecyclePolicy
    lifecyclePolicy_tags,
    lifecyclePolicy_policyId,
    lifecyclePolicy_policyDetails,
    lifecyclePolicy_state,
    lifecyclePolicy_description,
    lifecyclePolicy_policyArn,
    lifecyclePolicy_dateCreated,
    lifecyclePolicy_executionRoleArn,
    lifecyclePolicy_statusMessage,
    lifecyclePolicy_dateModified,

    -- ** LifecyclePolicySummary
    lifecyclePolicySummary_tags,
    lifecyclePolicySummary_policyId,
    lifecyclePolicySummary_policyType,
    lifecyclePolicySummary_state,
    lifecyclePolicySummary_description,

    -- ** Parameters
    parameters_excludeDataVolumeTags,
    parameters_excludeBootVolume,
    parameters_noReboot,

    -- ** PolicyDetails
    policyDetails_policyType,
    policyDetails_targetTags,
    policyDetails_resourceLocations,
    policyDetails_schedules,
    policyDetails_resourceTypes,
    policyDetails_actions,
    policyDetails_eventSource,
    policyDetails_parameters,

    -- ** RetainRule
    retainRule_interval,
    retainRule_count,
    retainRule_intervalUnit,

    -- ** Schedule
    schedule_name,
    schedule_fastRestoreRule,
    schedule_variableTags,
    schedule_tagsToAdd,
    schedule_createRule,
    schedule_copyTags,
    schedule_retainRule,
    schedule_deprecateRule,
    schedule_crossRegionCopyRules,
    schedule_shareRules,

    -- ** ShareRule
    shareRule_unshareInterval,
    shareRule_unshareIntervalUnit,
    shareRule_targetAccounts,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Amazonka.DLM.CreateLifecyclePolicy
import Amazonka.DLM.DeleteLifecyclePolicy
import Amazonka.DLM.GetLifecyclePolicies
import Amazonka.DLM.GetLifecyclePolicy
import Amazonka.DLM.ListTagsForResource
import Amazonka.DLM.TagResource
import Amazonka.DLM.Types.Action
import Amazonka.DLM.Types.CreateRule
import Amazonka.DLM.Types.CrossRegionCopyAction
import Amazonka.DLM.Types.CrossRegionCopyDeprecateRule
import Amazonka.DLM.Types.CrossRegionCopyRetainRule
import Amazonka.DLM.Types.CrossRegionCopyRule
import Amazonka.DLM.Types.DeprecateRule
import Amazonka.DLM.Types.EncryptionConfiguration
import Amazonka.DLM.Types.EventParameters
import Amazonka.DLM.Types.EventSource
import Amazonka.DLM.Types.FastRestoreRule
import Amazonka.DLM.Types.LifecyclePolicy
import Amazonka.DLM.Types.LifecyclePolicySummary
import Amazonka.DLM.Types.Parameters
import Amazonka.DLM.Types.PolicyDetails
import Amazonka.DLM.Types.RetainRule
import Amazonka.DLM.Types.Schedule
import Amazonka.DLM.Types.ShareRule
import Amazonka.DLM.Types.Tag
import Amazonka.DLM.UntagResource
import Amazonka.DLM.UpdateLifecyclePolicy
