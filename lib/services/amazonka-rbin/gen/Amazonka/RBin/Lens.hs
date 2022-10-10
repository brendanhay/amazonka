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
    createRule_tags,
    createRule_resourceTags,
    createRule_description,
    createRule_retentionPeriod,
    createRule_resourceType,
    createRuleResponse_tags,
    createRuleResponse_resourceType,
    createRuleResponse_status,
    createRuleResponse_resourceTags,
    createRuleResponse_description,
    createRuleResponse_retentionPeriod,
    createRuleResponse_identifier,
    createRuleResponse_httpStatus,

    -- ** DeleteRule
    deleteRule_identifier,
    deleteRuleResponse_httpStatus,

    -- ** GetRule
    getRule_identifier,
    getRuleResponse_resourceType,
    getRuleResponse_status,
    getRuleResponse_resourceTags,
    getRuleResponse_description,
    getRuleResponse_retentionPeriod,
    getRuleResponse_identifier,
    getRuleResponse_httpStatus,

    -- ** ListRules
    listRules_nextToken,
    listRules_resourceTags,
    listRules_maxResults,
    listRules_resourceType,
    listRulesResponse_nextToken,
    listRulesResponse_rules,
    listRulesResponse_httpStatus,

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

    -- ** UpdateRule
    updateRule_resourceType,
    updateRule_resourceTags,
    updateRule_description,
    updateRule_retentionPeriod,
    updateRule_identifier,
    updateRuleResponse_resourceType,
    updateRuleResponse_status,
    updateRuleResponse_resourceTags,
    updateRuleResponse_description,
    updateRuleResponse_retentionPeriod,
    updateRuleResponse_identifier,
    updateRuleResponse_httpStatus,

    -- * Types

    -- ** ResourceTag
    resourceTag_resourceTagValue,
    resourceTag_resourceTagKey,

    -- ** RetentionPeriod
    retentionPeriod_retentionPeriodValue,
    retentionPeriod_retentionPeriodUnit,

    -- ** RuleSummary
    ruleSummary_description,
    ruleSummary_retentionPeriod,
    ruleSummary_identifier,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Amazonka.RBin.CreateRule
import Amazonka.RBin.DeleteRule
import Amazonka.RBin.GetRule
import Amazonka.RBin.ListRules
import Amazonka.RBin.ListTagsForResource
import Amazonka.RBin.TagResource
import Amazonka.RBin.Types.ResourceTag
import Amazonka.RBin.Types.RetentionPeriod
import Amazonka.RBin.Types.RuleSummary
import Amazonka.RBin.Types.Tag
import Amazonka.RBin.UntagResource
import Amazonka.RBin.UpdateRule
