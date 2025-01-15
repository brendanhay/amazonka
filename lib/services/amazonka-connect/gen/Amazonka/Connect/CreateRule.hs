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
-- Module      : Amazonka.Connect.CreateRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a rule for the specified Amazon Connect instance.
--
-- Use the
-- <https://docs.aws.amazon.com/connect/latest/APIReference/connect-rules-language.html Rules Function language>
-- to code conditions for the rule.
module Amazonka.Connect.CreateRule
  ( -- * Creating a Request
    CreateRule (..),
    newCreateRule,

    -- * Request Lenses
    createRule_clientToken,
    createRule_instanceId,
    createRule_name,
    createRule_triggerEventSource,
    createRule_function,
    createRule_actions,
    createRule_publishStatus,

    -- * Destructuring the Response
    CreateRuleResponse (..),
    newCreateRuleResponse,

    -- * Response Lenses
    createRuleResponse_httpStatus,
    createRuleResponse_ruleArn,
    createRuleResponse_ruleId,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateRule' smart constructor.
data CreateRule = CreateRule'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If not provided, the Amazon Web Services SDK
    -- populates this field. For more information about idempotency, see
    -- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | A unique name for the rule.
    name :: Prelude.Text,
    -- | The event source to trigger the rule.
    triggerEventSource :: RuleTriggerEventSource,
    -- | The conditions of the rule.
    function :: Prelude.Text,
    -- | A list of actions to be run when the rule is triggered.
    actions :: [RuleAction],
    -- | The publish status of the rule.
    publishStatus :: RulePublishStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createRule_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
--
-- 'instanceId', 'createRule_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'name', 'createRule_name' - A unique name for the rule.
--
-- 'triggerEventSource', 'createRule_triggerEventSource' - The event source to trigger the rule.
--
-- 'function', 'createRule_function' - The conditions of the rule.
--
-- 'actions', 'createRule_actions' - A list of actions to be run when the rule is triggered.
--
-- 'publishStatus', 'createRule_publishStatus' - The publish status of the rule.
newCreateRule ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'triggerEventSource'
  RuleTriggerEventSource ->
  -- | 'function'
  Prelude.Text ->
  -- | 'publishStatus'
  RulePublishStatus ->
  CreateRule
newCreateRule
  pInstanceId_
  pName_
  pTriggerEventSource_
  pFunction_
  pPublishStatus_ =
    CreateRule'
      { clientToken = Prelude.Nothing,
        instanceId = pInstanceId_,
        name = pName_,
        triggerEventSource = pTriggerEventSource_,
        function = pFunction_,
        actions = Prelude.mempty,
        publishStatus = pPublishStatus_
      }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
createRule_clientToken :: Lens.Lens' CreateRule (Prelude.Maybe Prelude.Text)
createRule_clientToken = Lens.lens (\CreateRule' {clientToken} -> clientToken) (\s@CreateRule' {} a -> s {clientToken = a} :: CreateRule)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
createRule_instanceId :: Lens.Lens' CreateRule Prelude.Text
createRule_instanceId = Lens.lens (\CreateRule' {instanceId} -> instanceId) (\s@CreateRule' {} a -> s {instanceId = a} :: CreateRule)

-- | A unique name for the rule.
createRule_name :: Lens.Lens' CreateRule Prelude.Text
createRule_name = Lens.lens (\CreateRule' {name} -> name) (\s@CreateRule' {} a -> s {name = a} :: CreateRule)

-- | The event source to trigger the rule.
createRule_triggerEventSource :: Lens.Lens' CreateRule RuleTriggerEventSource
createRule_triggerEventSource = Lens.lens (\CreateRule' {triggerEventSource} -> triggerEventSource) (\s@CreateRule' {} a -> s {triggerEventSource = a} :: CreateRule)

-- | The conditions of the rule.
createRule_function :: Lens.Lens' CreateRule Prelude.Text
createRule_function = Lens.lens (\CreateRule' {function} -> function) (\s@CreateRule' {} a -> s {function = a} :: CreateRule)

-- | A list of actions to be run when the rule is triggered.
createRule_actions :: Lens.Lens' CreateRule [RuleAction]
createRule_actions = Lens.lens (\CreateRule' {actions} -> actions) (\s@CreateRule' {} a -> s {actions = a} :: CreateRule) Prelude.. Lens.coerced

-- | The publish status of the rule.
createRule_publishStatus :: Lens.Lens' CreateRule RulePublishStatus
createRule_publishStatus = Lens.lens (\CreateRule' {publishStatus} -> publishStatus) (\s@CreateRule' {} a -> s {publishStatus = a} :: CreateRule)

instance Core.AWSRequest CreateRule where
  type AWSResponse CreateRule = CreateRuleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRuleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "RuleArn")
            Prelude.<*> (x Data..:> "RuleId")
      )

instance Prelude.Hashable CreateRule where
  hashWithSalt _salt CreateRule' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` triggerEventSource
      `Prelude.hashWithSalt` function
      `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` publishStatus

instance Prelude.NFData CreateRule where
  rnf CreateRule' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf instanceId `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf triggerEventSource `Prelude.seq`
            Prelude.rnf function `Prelude.seq`
              Prelude.rnf actions `Prelude.seq`
                Prelude.rnf publishStatus

instance Data.ToHeaders CreateRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateRule where
  toJSON CreateRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("TriggerEventSource" Data..= triggerEventSource),
            Prelude.Just ("Function" Data..= function),
            Prelude.Just ("Actions" Data..= actions),
            Prelude.Just
              ("PublishStatus" Data..= publishStatus)
          ]
      )

instance Data.ToPath CreateRule where
  toPath CreateRule' {..} =
    Prelude.mconcat ["/rules/", Data.toBS instanceId]

instance Data.ToQuery CreateRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateRuleResponse' smart constructor.
data CreateRuleResponse = CreateRuleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the rule.
    ruleArn :: Prelude.Text,
    -- | A unique identifier for the rule.
    ruleId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createRuleResponse_httpStatus' - The response's http status code.
--
-- 'ruleArn', 'createRuleResponse_ruleArn' - The Amazon Resource Name (ARN) of the rule.
--
-- 'ruleId', 'createRuleResponse_ruleId' - A unique identifier for the rule.
newCreateRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'ruleArn'
  Prelude.Text ->
  -- | 'ruleId'
  Prelude.Text ->
  CreateRuleResponse
newCreateRuleResponse pHttpStatus_ pRuleArn_ pRuleId_ =
  CreateRuleResponse'
    { httpStatus = pHttpStatus_,
      ruleArn = pRuleArn_,
      ruleId = pRuleId_
    }

-- | The response's http status code.
createRuleResponse_httpStatus :: Lens.Lens' CreateRuleResponse Prelude.Int
createRuleResponse_httpStatus = Lens.lens (\CreateRuleResponse' {httpStatus} -> httpStatus) (\s@CreateRuleResponse' {} a -> s {httpStatus = a} :: CreateRuleResponse)

-- | The Amazon Resource Name (ARN) of the rule.
createRuleResponse_ruleArn :: Lens.Lens' CreateRuleResponse Prelude.Text
createRuleResponse_ruleArn = Lens.lens (\CreateRuleResponse' {ruleArn} -> ruleArn) (\s@CreateRuleResponse' {} a -> s {ruleArn = a} :: CreateRuleResponse)

-- | A unique identifier for the rule.
createRuleResponse_ruleId :: Lens.Lens' CreateRuleResponse Prelude.Text
createRuleResponse_ruleId = Lens.lens (\CreateRuleResponse' {ruleId} -> ruleId) (\s@CreateRuleResponse' {} a -> s {ruleId = a} :: CreateRuleResponse)

instance Prelude.NFData CreateRuleResponse where
  rnf CreateRuleResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf ruleArn `Prelude.seq`
        Prelude.rnf ruleId
