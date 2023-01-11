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
-- Module      : Amazonka.IoT.ReplaceTopicRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the rule. You must specify all parameters for the new rule.
-- Creating rules is an administrator-level action. Any user who has
-- permission to create rules will be able to access data processed by the
-- rule.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ReplaceTopicRule>
-- action.
module Amazonka.IoT.ReplaceTopicRule
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the ReplaceTopicRule operation.
--
-- /See:/ 'newReplaceTopicRule' smart constructor.
data ReplaceTopicRule = ReplaceTopicRule'
  { -- | The name of the rule.
    ruleName :: Prelude.Text,
    -- | The rule payload.
    topicRulePayload :: TopicRulePayload
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'topicRulePayload'
  TopicRulePayload ->
  ReplaceTopicRule
newReplaceTopicRule pRuleName_ pTopicRulePayload_ =
  ReplaceTopicRule'
    { ruleName = pRuleName_,
      topicRulePayload = pTopicRulePayload_
    }

-- | The name of the rule.
replaceTopicRule_ruleName :: Lens.Lens' ReplaceTopicRule Prelude.Text
replaceTopicRule_ruleName = Lens.lens (\ReplaceTopicRule' {ruleName} -> ruleName) (\s@ReplaceTopicRule' {} a -> s {ruleName = a} :: ReplaceTopicRule)

-- | The rule payload.
replaceTopicRule_topicRulePayload :: Lens.Lens' ReplaceTopicRule TopicRulePayload
replaceTopicRule_topicRulePayload = Lens.lens (\ReplaceTopicRule' {topicRulePayload} -> topicRulePayload) (\s@ReplaceTopicRule' {} a -> s {topicRulePayload = a} :: ReplaceTopicRule)

instance Core.AWSRequest ReplaceTopicRule where
  type
    AWSResponse ReplaceTopicRule =
      ReplaceTopicRuleResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveNull ReplaceTopicRuleResponse'

instance Prelude.Hashable ReplaceTopicRule where
  hashWithSalt _salt ReplaceTopicRule' {..} =
    _salt `Prelude.hashWithSalt` ruleName
      `Prelude.hashWithSalt` topicRulePayload

instance Prelude.NFData ReplaceTopicRule where
  rnf ReplaceTopicRule' {..} =
    Prelude.rnf ruleName
      `Prelude.seq` Prelude.rnf topicRulePayload

instance Data.ToHeaders ReplaceTopicRule where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON ReplaceTopicRule where
  toJSON ReplaceTopicRule' {..} =
    Data.toJSON topicRulePayload

instance Data.ToPath ReplaceTopicRule where
  toPath ReplaceTopicRule' {..} =
    Prelude.mconcat ["/rules/", Data.toBS ruleName]

instance Data.ToQuery ReplaceTopicRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newReplaceTopicRuleResponse' smart constructor.
data ReplaceTopicRuleResponse = ReplaceTopicRuleResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplaceTopicRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newReplaceTopicRuleResponse ::
  ReplaceTopicRuleResponse
newReplaceTopicRuleResponse =
  ReplaceTopicRuleResponse'

instance Prelude.NFData ReplaceTopicRuleResponse where
  rnf _ = ()
