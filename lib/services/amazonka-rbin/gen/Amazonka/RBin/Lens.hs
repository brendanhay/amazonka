{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RBin.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RBin.Lens
  ( -- * Operations

    -- ** CreateRule
    createRule_description,
    createRule_lockConfiguration,
    createRule_resourceTags,
    createRule_tags,
    createRule_retentionPeriod,
    createRule_resourceType,
    createRuleResponse_description,
    createRuleResponse_identifier,
    createRuleResponse_lockConfiguration,
    createRuleResponse_lockState,
    createRuleResponse_resourceTags,
    createRuleResponse_resourceType,
    createRuleResponse_retentionPeriod,
    createRuleResponse_status,
    createRuleResponse_tags,
    createRuleResponse_httpStatus,

    -- ** DeleteRule
    deleteRule_identifier,
    deleteRuleResponse_httpStatus,

    -- ** GetRule
    getRule_identifier,
    getRuleResponse_description,
    getRuleResponse_identifier,
    getRuleResponse_lockConfiguration,
    getRuleResponse_lockEndTime,
    getRuleResponse_lockState,
    getRuleResponse_resourceTags,
    getRuleResponse_resourceType,
    getRuleResponse_retentionPeriod,
    getRuleResponse_status,
    getRuleResponse_httpStatus,

    -- ** ListRules
    listRules_lockState,
    listRules_maxResults,
    listRules_nextToken,
    listRules_resourceTags,
    listRules_resourceType,
    listRulesResponse_nextToken,
    listRulesResponse_rules,
    listRulesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** LockRule
    lockRule_identifier,
    lockRule_lockConfiguration,
    lockRuleResponse_description,
    lockRuleResponse_identifier,
    lockRuleResponse_lockConfiguration,
    lockRuleResponse_lockState,
    lockRuleResponse_resourceTags,
    lockRuleResponse_resourceType,
    lockRuleResponse_retentionPeriod,
    lockRuleResponse_status,
    lockRuleResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UnlockRule
    unlockRule_identifier,
    unlockRuleResponse_description,
    unlockRuleResponse_identifier,
    unlockRuleResponse_lockConfiguration,
    unlockRuleResponse_lockEndTime,
    unlockRuleResponse_lockState,
    unlockRuleResponse_resourceTags,
    unlockRuleResponse_resourceType,
    unlockRuleResponse_retentionPeriod,
    unlockRuleResponse_status,
    unlockRuleResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateRule
    updateRule_description,
    updateRule_resourceTags,
    updateRule_resourceType,
    updateRule_retentionPeriod,
    updateRule_identifier,
    updateRuleResponse_description,
    updateRuleResponse_identifier,
    updateRuleResponse_lockEndTime,
    updateRuleResponse_lockState,
    updateRuleResponse_resourceTags,
    updateRuleResponse_resourceType,
    updateRuleResponse_retentionPeriod,
    updateRuleResponse_status,
    updateRuleResponse_httpStatus,

    -- * Types

    -- ** LockConfiguration
    lockConfiguration_unlockDelay,

    -- ** ResourceTag
    resourceTag_resourceTagValue,
    resourceTag_resourceTagKey,

    -- ** RetentionPeriod
    retentionPeriod_retentionPeriodValue,
    retentionPeriod_retentionPeriodUnit,

    -- ** RuleSummary
    ruleSummary_description,
    ruleSummary_identifier,
    ruleSummary_lockState,
    ruleSummary_retentionPeriod,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** UnlockDelay
    unlockDelay_unlockDelayValue,
    unlockDelay_unlockDelayUnit,
  )
where

import Amazonka.RBin.CreateRule
import Amazonka.RBin.DeleteRule
import Amazonka.RBin.GetRule
import Amazonka.RBin.ListRules
import Amazonka.RBin.ListTagsForResource
import Amazonka.RBin.LockRule
import Amazonka.RBin.TagResource
import Amazonka.RBin.Types.LockConfiguration
import Amazonka.RBin.Types.ResourceTag
import Amazonka.RBin.Types.RetentionPeriod
import Amazonka.RBin.Types.RuleSummary
import Amazonka.RBin.Types.Tag
import Amazonka.RBin.Types.UnlockDelay
import Amazonka.RBin.UnlockRule
import Amazonka.RBin.UntagResource
import Amazonka.RBin.UpdateRule
