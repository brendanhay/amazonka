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
-- Module      : Amazonka.Connect.UpdateRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a rule for the specified Amazon Connect instance.
module Amazonka.Connect.UpdateRule
  ( -- * Creating a Request
    UpdateRule (..),
    newUpdateRule,

    -- * Request Lenses
    updateRule_ruleId,
    updateRule_instanceId,
    updateRule_name,
    updateRule_function,
    updateRule_actions,
    updateRule_publishStatus,

    -- * Destructuring the Response
    UpdateRuleResponse (..),
    newUpdateRuleResponse,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateRule' smart constructor.
data UpdateRule = UpdateRule'
  { -- | A unique identifier for the rule.
    ruleId :: Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The name of the rule. You can change the name only if
    -- @TriggerEventSource@ is one of the following values:
    -- @OnZendeskTicketCreate@ | @OnZendeskTicketStatusUpdate@ |
    -- @OnSalesforceCaseCreate@
    name :: Prelude.Text,
    -- | The conditions of the rule.
    function :: Prelude.Text,
    -- | A list of actions to be run when the rule is triggered.
    actions :: [RuleAction],
    -- | The publish status of the rule.
    publishStatus :: RulePublishStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleId', 'updateRule_ruleId' - A unique identifier for the rule.
--
-- 'instanceId', 'updateRule_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'name', 'updateRule_name' - The name of the rule. You can change the name only if
-- @TriggerEventSource@ is one of the following values:
-- @OnZendeskTicketCreate@ | @OnZendeskTicketStatusUpdate@ |
-- @OnSalesforceCaseCreate@
--
-- 'function', 'updateRule_function' - The conditions of the rule.
--
-- 'actions', 'updateRule_actions' - A list of actions to be run when the rule is triggered.
--
-- 'publishStatus', 'updateRule_publishStatus' - The publish status of the rule.
newUpdateRule ::
  -- | 'ruleId'
  Prelude.Text ->
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'function'
  Prelude.Text ->
  -- | 'publishStatus'
  RulePublishStatus ->
  UpdateRule
newUpdateRule
  pRuleId_
  pInstanceId_
  pName_
  pFunction_
  pPublishStatus_ =
    UpdateRule'
      { ruleId = pRuleId_,
        instanceId = pInstanceId_,
        name = pName_,
        function = pFunction_,
        actions = Prelude.mempty,
        publishStatus = pPublishStatus_
      }

-- | A unique identifier for the rule.
updateRule_ruleId :: Lens.Lens' UpdateRule Prelude.Text
updateRule_ruleId = Lens.lens (\UpdateRule' {ruleId} -> ruleId) (\s@UpdateRule' {} a -> s {ruleId = a} :: UpdateRule)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
updateRule_instanceId :: Lens.Lens' UpdateRule Prelude.Text
updateRule_instanceId = Lens.lens (\UpdateRule' {instanceId} -> instanceId) (\s@UpdateRule' {} a -> s {instanceId = a} :: UpdateRule)

-- | The name of the rule. You can change the name only if
-- @TriggerEventSource@ is one of the following values:
-- @OnZendeskTicketCreate@ | @OnZendeskTicketStatusUpdate@ |
-- @OnSalesforceCaseCreate@
updateRule_name :: Lens.Lens' UpdateRule Prelude.Text
updateRule_name = Lens.lens (\UpdateRule' {name} -> name) (\s@UpdateRule' {} a -> s {name = a} :: UpdateRule)

-- | The conditions of the rule.
updateRule_function :: Lens.Lens' UpdateRule Prelude.Text
updateRule_function = Lens.lens (\UpdateRule' {function} -> function) (\s@UpdateRule' {} a -> s {function = a} :: UpdateRule)

-- | A list of actions to be run when the rule is triggered.
updateRule_actions :: Lens.Lens' UpdateRule [RuleAction]
updateRule_actions = Lens.lens (\UpdateRule' {actions} -> actions) (\s@UpdateRule' {} a -> s {actions = a} :: UpdateRule) Prelude.. Lens.coerced

-- | The publish status of the rule.
updateRule_publishStatus :: Lens.Lens' UpdateRule RulePublishStatus
updateRule_publishStatus = Lens.lens (\UpdateRule' {publishStatus} -> publishStatus) (\s@UpdateRule' {} a -> s {publishStatus = a} :: UpdateRule)

instance Core.AWSRequest UpdateRule where
  type AWSResponse UpdateRule = UpdateRuleResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response = Response.receiveNull UpdateRuleResponse'

instance Prelude.Hashable UpdateRule where
  hashWithSalt _salt UpdateRule' {..} =
    _salt `Prelude.hashWithSalt` ruleId
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` function
      `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` publishStatus

instance Prelude.NFData UpdateRule where
  rnf UpdateRule' {..} =
    Prelude.rnf ruleId
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf function
      `Prelude.seq` Prelude.rnf actions
      `Prelude.seq` Prelude.rnf publishStatus

instance Data.ToHeaders UpdateRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRule where
  toJSON UpdateRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Function" Data..= function),
            Prelude.Just ("Actions" Data..= actions),
            Prelude.Just
              ("PublishStatus" Data..= publishStatus)
          ]
      )

instance Data.ToPath UpdateRule where
  toPath UpdateRule' {..} =
    Prelude.mconcat
      [ "/rules/",
        Data.toBS instanceId,
        "/",
        Data.toBS ruleId
      ]

instance Data.ToQuery UpdateRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRuleResponse' smart constructor.
data UpdateRuleResponse = UpdateRuleResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateRuleResponse ::
  UpdateRuleResponse
newUpdateRuleResponse = UpdateRuleResponse'

instance Prelude.NFData UpdateRuleResponse where
  rnf _ = ()
