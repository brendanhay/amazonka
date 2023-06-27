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
-- Module      : Amazonka.VPCLattice.UpdateRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a rule for the listener. You can\'t modify a default listener
-- rule. To modify a default listener rule, use @UpdateListener@.
module Amazonka.VPCLattice.UpdateRule
  ( -- * Creating a Request
    UpdateRule (..),
    newUpdateRule,

    -- * Request Lenses
    updateRule_action,
    updateRule_match,
    updateRule_priority,
    updateRule_listenerIdentifier,
    updateRule_ruleIdentifier,
    updateRule_serviceIdentifier,

    -- * Destructuring the Response
    UpdateRuleResponse (..),
    newUpdateRuleResponse,

    -- * Response Lenses
    updateRuleResponse_action,
    updateRuleResponse_arn,
    updateRuleResponse_id,
    updateRuleResponse_isDefault,
    updateRuleResponse_match,
    updateRuleResponse_name,
    updateRuleResponse_priority,
    updateRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newUpdateRule' smart constructor.
data UpdateRule = UpdateRule'
  { -- | Information about the action for the specified listener rule.
    action :: Prelude.Maybe RuleAction,
    -- | The rule match.
    match :: Prelude.Maybe RuleMatch,
    -- | The rule priority. A listener can\'t have multiple rules with the same
    -- priority.
    priority :: Prelude.Maybe Prelude.Natural,
    -- | The ID or Amazon Resource Name (ARN) of the listener.
    listenerIdentifier :: Prelude.Text,
    -- | The ID or Amazon Resource Name (ARN) of the rule.
    ruleIdentifier :: Prelude.Text,
    -- | The ID or Amazon Resource Name (ARN) of the service.
    serviceIdentifier :: Prelude.Text
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
-- 'action', 'updateRule_action' - Information about the action for the specified listener rule.
--
-- 'match', 'updateRule_match' - The rule match.
--
-- 'priority', 'updateRule_priority' - The rule priority. A listener can\'t have multiple rules with the same
-- priority.
--
-- 'listenerIdentifier', 'updateRule_listenerIdentifier' - The ID or Amazon Resource Name (ARN) of the listener.
--
-- 'ruleIdentifier', 'updateRule_ruleIdentifier' - The ID or Amazon Resource Name (ARN) of the rule.
--
-- 'serviceIdentifier', 'updateRule_serviceIdentifier' - The ID or Amazon Resource Name (ARN) of the service.
newUpdateRule ::
  -- | 'listenerIdentifier'
  Prelude.Text ->
  -- | 'ruleIdentifier'
  Prelude.Text ->
  -- | 'serviceIdentifier'
  Prelude.Text ->
  UpdateRule
newUpdateRule
  pListenerIdentifier_
  pRuleIdentifier_
  pServiceIdentifier_ =
    UpdateRule'
      { action = Prelude.Nothing,
        match = Prelude.Nothing,
        priority = Prelude.Nothing,
        listenerIdentifier = pListenerIdentifier_,
        ruleIdentifier = pRuleIdentifier_,
        serviceIdentifier = pServiceIdentifier_
      }

-- | Information about the action for the specified listener rule.
updateRule_action :: Lens.Lens' UpdateRule (Prelude.Maybe RuleAction)
updateRule_action = Lens.lens (\UpdateRule' {action} -> action) (\s@UpdateRule' {} a -> s {action = a} :: UpdateRule)

-- | The rule match.
updateRule_match :: Lens.Lens' UpdateRule (Prelude.Maybe RuleMatch)
updateRule_match = Lens.lens (\UpdateRule' {match} -> match) (\s@UpdateRule' {} a -> s {match = a} :: UpdateRule)

-- | The rule priority. A listener can\'t have multiple rules with the same
-- priority.
updateRule_priority :: Lens.Lens' UpdateRule (Prelude.Maybe Prelude.Natural)
updateRule_priority = Lens.lens (\UpdateRule' {priority} -> priority) (\s@UpdateRule' {} a -> s {priority = a} :: UpdateRule)

-- | The ID or Amazon Resource Name (ARN) of the listener.
updateRule_listenerIdentifier :: Lens.Lens' UpdateRule Prelude.Text
updateRule_listenerIdentifier = Lens.lens (\UpdateRule' {listenerIdentifier} -> listenerIdentifier) (\s@UpdateRule' {} a -> s {listenerIdentifier = a} :: UpdateRule)

-- | The ID or Amazon Resource Name (ARN) of the rule.
updateRule_ruleIdentifier :: Lens.Lens' UpdateRule Prelude.Text
updateRule_ruleIdentifier = Lens.lens (\UpdateRule' {ruleIdentifier} -> ruleIdentifier) (\s@UpdateRule' {} a -> s {ruleIdentifier = a} :: UpdateRule)

-- | The ID or Amazon Resource Name (ARN) of the service.
updateRule_serviceIdentifier :: Lens.Lens' UpdateRule Prelude.Text
updateRule_serviceIdentifier = Lens.lens (\UpdateRule' {serviceIdentifier} -> serviceIdentifier) (\s@UpdateRule' {} a -> s {serviceIdentifier = a} :: UpdateRule)

instance Core.AWSRequest UpdateRule where
  type AWSResponse UpdateRule = UpdateRuleResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRuleResponse'
            Prelude.<$> (x Data..?> "action")
            Prelude.<*> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "isDefault")
            Prelude.<*> (x Data..?> "match")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "priority")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRule where
  hashWithSalt _salt UpdateRule' {..} =
    _salt
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` match
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` listenerIdentifier
      `Prelude.hashWithSalt` ruleIdentifier
      `Prelude.hashWithSalt` serviceIdentifier

instance Prelude.NFData UpdateRule where
  rnf UpdateRule' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf match
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf listenerIdentifier
      `Prelude.seq` Prelude.rnf ruleIdentifier
      `Prelude.seq` Prelude.rnf serviceIdentifier

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
          [ ("action" Data..=) Prelude.<$> action,
            ("match" Data..=) Prelude.<$> match,
            ("priority" Data..=) Prelude.<$> priority
          ]
      )

instance Data.ToPath UpdateRule where
  toPath UpdateRule' {..} =
    Prelude.mconcat
      [ "/services/",
        Data.toBS serviceIdentifier,
        "/listeners/",
        Data.toBS listenerIdentifier,
        "/rules/",
        Data.toBS ruleIdentifier
      ]

instance Data.ToQuery UpdateRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRuleResponse' smart constructor.
data UpdateRuleResponse = UpdateRuleResponse'
  { -- | Information about the action for the specified listener rule.
    action :: Prelude.Maybe RuleAction,
    -- | The Amazon Resource Name (ARN) of the listener.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the listener.
    id :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether this is the default rule.
    isDefault :: Prelude.Maybe Prelude.Bool,
    -- | The rule match.
    match :: Prelude.Maybe RuleMatch,
    -- | The name of the listener.
    name :: Prelude.Maybe Prelude.Text,
    -- | The rule priority.
    priority :: Prelude.Maybe Prelude.Natural,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'updateRuleResponse_action' - Information about the action for the specified listener rule.
--
-- 'arn', 'updateRuleResponse_arn' - The Amazon Resource Name (ARN) of the listener.
--
-- 'id', 'updateRuleResponse_id' - The ID of the listener.
--
-- 'isDefault', 'updateRuleResponse_isDefault' - Indicates whether this is the default rule.
--
-- 'match', 'updateRuleResponse_match' - The rule match.
--
-- 'name', 'updateRuleResponse_name' - The name of the listener.
--
-- 'priority', 'updateRuleResponse_priority' - The rule priority.
--
-- 'httpStatus', 'updateRuleResponse_httpStatus' - The response's http status code.
newUpdateRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRuleResponse
newUpdateRuleResponse pHttpStatus_ =
  UpdateRuleResponse'
    { action = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      isDefault = Prelude.Nothing,
      match = Prelude.Nothing,
      name = Prelude.Nothing,
      priority = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the action for the specified listener rule.
updateRuleResponse_action :: Lens.Lens' UpdateRuleResponse (Prelude.Maybe RuleAction)
updateRuleResponse_action = Lens.lens (\UpdateRuleResponse' {action} -> action) (\s@UpdateRuleResponse' {} a -> s {action = a} :: UpdateRuleResponse)

-- | The Amazon Resource Name (ARN) of the listener.
updateRuleResponse_arn :: Lens.Lens' UpdateRuleResponse (Prelude.Maybe Prelude.Text)
updateRuleResponse_arn = Lens.lens (\UpdateRuleResponse' {arn} -> arn) (\s@UpdateRuleResponse' {} a -> s {arn = a} :: UpdateRuleResponse)

-- | The ID of the listener.
updateRuleResponse_id :: Lens.Lens' UpdateRuleResponse (Prelude.Maybe Prelude.Text)
updateRuleResponse_id = Lens.lens (\UpdateRuleResponse' {id} -> id) (\s@UpdateRuleResponse' {} a -> s {id = a} :: UpdateRuleResponse)

-- | Indicates whether this is the default rule.
updateRuleResponse_isDefault :: Lens.Lens' UpdateRuleResponse (Prelude.Maybe Prelude.Bool)
updateRuleResponse_isDefault = Lens.lens (\UpdateRuleResponse' {isDefault} -> isDefault) (\s@UpdateRuleResponse' {} a -> s {isDefault = a} :: UpdateRuleResponse)

-- | The rule match.
updateRuleResponse_match :: Lens.Lens' UpdateRuleResponse (Prelude.Maybe RuleMatch)
updateRuleResponse_match = Lens.lens (\UpdateRuleResponse' {match} -> match) (\s@UpdateRuleResponse' {} a -> s {match = a} :: UpdateRuleResponse)

-- | The name of the listener.
updateRuleResponse_name :: Lens.Lens' UpdateRuleResponse (Prelude.Maybe Prelude.Text)
updateRuleResponse_name = Lens.lens (\UpdateRuleResponse' {name} -> name) (\s@UpdateRuleResponse' {} a -> s {name = a} :: UpdateRuleResponse)

-- | The rule priority.
updateRuleResponse_priority :: Lens.Lens' UpdateRuleResponse (Prelude.Maybe Prelude.Natural)
updateRuleResponse_priority = Lens.lens (\UpdateRuleResponse' {priority} -> priority) (\s@UpdateRuleResponse' {} a -> s {priority = a} :: UpdateRuleResponse)

-- | The response's http status code.
updateRuleResponse_httpStatus :: Lens.Lens' UpdateRuleResponse Prelude.Int
updateRuleResponse_httpStatus = Lens.lens (\UpdateRuleResponse' {httpStatus} -> httpStatus) (\s@UpdateRuleResponse' {} a -> s {httpStatus = a} :: UpdateRuleResponse)

instance Prelude.NFData UpdateRuleResponse where
  rnf UpdateRuleResponse' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf isDefault
      `Prelude.seq` Prelude.rnf match
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf httpStatus
