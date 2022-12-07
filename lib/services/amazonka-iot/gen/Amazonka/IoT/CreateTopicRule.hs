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
-- Module      : Amazonka.IoT.CreateTopicRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a rule. Creating rules is an administrator-level action. Any
-- user who has permission to create rules will be able to access data
-- processed by the rule.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions CreateTopicRule>
-- action.
module Amazonka.IoT.CreateTopicRule
  ( -- * Creating a Request
    CreateTopicRule (..),
    newCreateTopicRule,

    -- * Request Lenses
    createTopicRule_tags,
    createTopicRule_ruleName,
    createTopicRule_topicRulePayload,

    -- * Destructuring the Response
    CreateTopicRuleResponse (..),
    newCreateTopicRuleResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the CreateTopicRule operation.
--
-- /See:/ 'newCreateTopicRule' smart constructor.
data CreateTopicRule = CreateTopicRule'
  { -- | Metadata which can be used to manage the topic rule.
    --
    -- For URI Request parameters use format: ...key1=value1&key2=value2...
    --
    -- For the CLI command-line parameter use format: --tags
    -- \"key1=value1&key2=value2...\"
    --
    -- For the cli-input-json file use format: \"tags\":
    -- \"key1=value1&key2=value2...\"
    tags :: Prelude.Maybe Prelude.Text,
    -- | The name of the rule.
    ruleName :: Prelude.Text,
    -- | The rule payload.
    topicRulePayload :: TopicRulePayload
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTopicRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createTopicRule_tags' - Metadata which can be used to manage the topic rule.
--
-- For URI Request parameters use format: ...key1=value1&key2=value2...
--
-- For the CLI command-line parameter use format: --tags
-- \"key1=value1&key2=value2...\"
--
-- For the cli-input-json file use format: \"tags\":
-- \"key1=value1&key2=value2...\"
--
-- 'ruleName', 'createTopicRule_ruleName' - The name of the rule.
--
-- 'topicRulePayload', 'createTopicRule_topicRulePayload' - The rule payload.
newCreateTopicRule ::
  -- | 'ruleName'
  Prelude.Text ->
  -- | 'topicRulePayload'
  TopicRulePayload ->
  CreateTopicRule
newCreateTopicRule pRuleName_ pTopicRulePayload_ =
  CreateTopicRule'
    { tags = Prelude.Nothing,
      ruleName = pRuleName_,
      topicRulePayload = pTopicRulePayload_
    }

-- | Metadata which can be used to manage the topic rule.
--
-- For URI Request parameters use format: ...key1=value1&key2=value2...
--
-- For the CLI command-line parameter use format: --tags
-- \"key1=value1&key2=value2...\"
--
-- For the cli-input-json file use format: \"tags\":
-- \"key1=value1&key2=value2...\"
createTopicRule_tags :: Lens.Lens' CreateTopicRule (Prelude.Maybe Prelude.Text)
createTopicRule_tags = Lens.lens (\CreateTopicRule' {tags} -> tags) (\s@CreateTopicRule' {} a -> s {tags = a} :: CreateTopicRule)

-- | The name of the rule.
createTopicRule_ruleName :: Lens.Lens' CreateTopicRule Prelude.Text
createTopicRule_ruleName = Lens.lens (\CreateTopicRule' {ruleName} -> ruleName) (\s@CreateTopicRule' {} a -> s {ruleName = a} :: CreateTopicRule)

-- | The rule payload.
createTopicRule_topicRulePayload :: Lens.Lens' CreateTopicRule TopicRulePayload
createTopicRule_topicRulePayload = Lens.lens (\CreateTopicRule' {topicRulePayload} -> topicRulePayload) (\s@CreateTopicRule' {} a -> s {topicRulePayload = a} :: CreateTopicRule)

instance Core.AWSRequest CreateTopicRule where
  type
    AWSResponse CreateTopicRule =
      CreateTopicRuleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull CreateTopicRuleResponse'

instance Prelude.Hashable CreateTopicRule where
  hashWithSalt _salt CreateTopicRule' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` ruleName
      `Prelude.hashWithSalt` topicRulePayload

instance Prelude.NFData CreateTopicRule where
  rnf CreateTopicRule' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf ruleName
      `Prelude.seq` Prelude.rnf topicRulePayload

instance Data.ToHeaders CreateTopicRule where
  toHeaders CreateTopicRule' {..} =
    Prelude.mconcat ["x-amz-tagging" Data.=# tags]

instance Data.ToJSON CreateTopicRule where
  toJSON CreateTopicRule' {..} =
    Data.toJSON topicRulePayload

instance Data.ToPath CreateTopicRule where
  toPath CreateTopicRule' {..} =
    Prelude.mconcat ["/rules/", Data.toBS ruleName]

instance Data.ToQuery CreateTopicRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTopicRuleResponse' smart constructor.
data CreateTopicRuleResponse = CreateTopicRuleResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTopicRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreateTopicRuleResponse ::
  CreateTopicRuleResponse
newCreateTopicRuleResponse = CreateTopicRuleResponse'

instance Prelude.NFData CreateTopicRuleResponse where
  rnf _ = ()
