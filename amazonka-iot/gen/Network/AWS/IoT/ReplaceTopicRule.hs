{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ReplaceTopicRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the rule. You must specify all parameters for the new rule.
-- Creating rules is an administrator-level action. Any user who has
-- permission to create rules will be able to access data processed by the
-- rule.
module Network.AWS.IoT.ReplaceTopicRule
  ( -- * Creating a Request
    ReplaceTopicRule (..),
    newReplaceTopicRule,

    -- * Request Lenses
    replaceTopicRule_ruleName,
    replaceTopicRule_topicRulePayload,

    -- * Destructuring the Response
    ReplaceTopicRuleResponse (..),
    newReplaceTopicRuleResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the ReplaceTopicRule operation.
--
-- /See:/ 'newReplaceTopicRule' smart constructor.
data ReplaceTopicRule = ReplaceTopicRule'
  { -- | The name of the rule.
    ruleName :: Core.Text,
    -- | The rule payload.
    topicRulePayload :: TopicRulePayload
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReplaceTopicRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleName', 'replaceTopicRule_ruleName' - The name of the rule.
--
-- 'topicRulePayload', 'replaceTopicRule_topicRulePayload' - The rule payload.
newReplaceTopicRule ::
  -- | 'ruleName'
  Core.Text ->
  -- | 'topicRulePayload'
  TopicRulePayload ->
  ReplaceTopicRule
newReplaceTopicRule pRuleName_ pTopicRulePayload_ =
  ReplaceTopicRule'
    { ruleName = pRuleName_,
      topicRulePayload = pTopicRulePayload_
    }

-- | The name of the rule.
replaceTopicRule_ruleName :: Lens.Lens' ReplaceTopicRule Core.Text
replaceTopicRule_ruleName = Lens.lens (\ReplaceTopicRule' {ruleName} -> ruleName) (\s@ReplaceTopicRule' {} a -> s {ruleName = a} :: ReplaceTopicRule)

-- | The rule payload.
replaceTopicRule_topicRulePayload :: Lens.Lens' ReplaceTopicRule TopicRulePayload
replaceTopicRule_topicRulePayload = Lens.lens (\ReplaceTopicRule' {topicRulePayload} -> topicRulePayload) (\s@ReplaceTopicRule' {} a -> s {topicRulePayload = a} :: ReplaceTopicRule)

instance Core.AWSRequest ReplaceTopicRule where
  type
    AWSResponse ReplaceTopicRule =
      ReplaceTopicRuleResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveNull ReplaceTopicRuleResponse'

instance Core.Hashable ReplaceTopicRule

instance Core.NFData ReplaceTopicRule

instance Core.ToHeaders ReplaceTopicRule where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON ReplaceTopicRule where
  toJSON ReplaceTopicRule' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("topicRulePayload" Core..= topicRulePayload)
          ]
      )

instance Core.ToPath ReplaceTopicRule where
  toPath ReplaceTopicRule' {..} =
    Core.mconcat ["/rules/", Core.toBS ruleName]

instance Core.ToQuery ReplaceTopicRule where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newReplaceTopicRuleResponse' smart constructor.
data ReplaceTopicRuleResponse = ReplaceTopicRuleResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReplaceTopicRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newReplaceTopicRuleResponse ::
  ReplaceTopicRuleResponse
newReplaceTopicRuleResponse =
  ReplaceTopicRuleResponse'

instance Core.NFData ReplaceTopicRuleResponse
