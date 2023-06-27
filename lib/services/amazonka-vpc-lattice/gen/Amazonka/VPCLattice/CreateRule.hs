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
-- Module      : Amazonka.VPCLattice.CreateRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a listener rule. Each listener has a default rule for checking
-- connection requests, but you can define additional rules. Each rule
-- consists of a priority, one or more actions, and one or more conditions.
-- For more information, see
-- <https://docs.aws.amazon.com/vpc-lattice/latest/ug/listeners.html#listener-rules Listener rules>
-- in the /Amazon VPC Lattice User Guide/.
module Amazonka.VPCLattice.CreateRule
  ( -- * Creating a Request
    CreateRule (..),
    newCreateRule,

    -- * Request Lenses
    createRule_clientToken,
    createRule_tags,
    createRule_action,
    createRule_listenerIdentifier,
    createRule_match,
    createRule_name,
    createRule_priority,
    createRule_serviceIdentifier,

    -- * Destructuring the Response
    CreateRuleResponse (..),
    newCreateRuleResponse,

    -- * Response Lenses
    createRuleResponse_action,
    createRuleResponse_arn,
    createRuleResponse_id,
    createRuleResponse_match,
    createRuleResponse_name,
    createRuleResponse_priority,
    createRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newCreateRule' smart constructor.
data CreateRule = CreateRule'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If you retry a request that completed
    -- successfully using the same client token and parameters, the retry
    -- succeeds without performing any actions. If the parameters aren\'t
    -- identical, the retry fails.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The tags for the rule.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The action for the default rule.
    action :: RuleAction,
    -- | The ID or Amazon Resource Name (ARN) of the listener.
    listenerIdentifier :: Prelude.Text,
    -- | The rule match.
    match :: RuleMatch,
    -- | The name of the rule. The name must be unique within the listener. The
    -- valid characters are a-z, 0-9, and hyphens (-). You can\'t use a hyphen
    -- as the first or last character, or immediately after another hyphen.
    name :: Prelude.Text,
    -- | The priority assigned to the rule. Each rule for a specific listener
    -- must have a unique priority. The lower the priority number the higher
    -- the priority.
    priority :: Prelude.Natural,
    -- | The ID or Amazon Resource Name (ARN) of the service.
    serviceIdentifier :: Prelude.Text
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
-- idempotency of the request. If you retry a request that completed
-- successfully using the same client token and parameters, the retry
-- succeeds without performing any actions. If the parameters aren\'t
-- identical, the retry fails.
--
-- 'tags', 'createRule_tags' - The tags for the rule.
--
-- 'action', 'createRule_action' - The action for the default rule.
--
-- 'listenerIdentifier', 'createRule_listenerIdentifier' - The ID or Amazon Resource Name (ARN) of the listener.
--
-- 'match', 'createRule_match' - The rule match.
--
-- 'name', 'createRule_name' - The name of the rule. The name must be unique within the listener. The
-- valid characters are a-z, 0-9, and hyphens (-). You can\'t use a hyphen
-- as the first or last character, or immediately after another hyphen.
--
-- 'priority', 'createRule_priority' - The priority assigned to the rule. Each rule for a specific listener
-- must have a unique priority. The lower the priority number the higher
-- the priority.
--
-- 'serviceIdentifier', 'createRule_serviceIdentifier' - The ID or Amazon Resource Name (ARN) of the service.
newCreateRule ::
  -- | 'action'
  RuleAction ->
  -- | 'listenerIdentifier'
  Prelude.Text ->
  -- | 'match'
  RuleMatch ->
  -- | 'name'
  Prelude.Text ->
  -- | 'priority'
  Prelude.Natural ->
  -- | 'serviceIdentifier'
  Prelude.Text ->
  CreateRule
newCreateRule
  pAction_
  pListenerIdentifier_
  pMatch_
  pName_
  pPriority_
  pServiceIdentifier_ =
    CreateRule'
      { clientToken = Prelude.Nothing,
        tags = Prelude.Nothing,
        action = pAction_,
        listenerIdentifier = pListenerIdentifier_,
        match = pMatch_,
        name = pName_,
        priority = pPriority_,
        serviceIdentifier = pServiceIdentifier_
      }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you retry a request that completed
-- successfully using the same client token and parameters, the retry
-- succeeds without performing any actions. If the parameters aren\'t
-- identical, the retry fails.
createRule_clientToken :: Lens.Lens' CreateRule (Prelude.Maybe Prelude.Text)
createRule_clientToken = Lens.lens (\CreateRule' {clientToken} -> clientToken) (\s@CreateRule' {} a -> s {clientToken = a} :: CreateRule)

-- | The tags for the rule.
createRule_tags :: Lens.Lens' CreateRule (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createRule_tags = Lens.lens (\CreateRule' {tags} -> tags) (\s@CreateRule' {} a -> s {tags = a} :: CreateRule) Prelude.. Lens.mapping Lens.coerced

-- | The action for the default rule.
createRule_action :: Lens.Lens' CreateRule RuleAction
createRule_action = Lens.lens (\CreateRule' {action} -> action) (\s@CreateRule' {} a -> s {action = a} :: CreateRule)

-- | The ID or Amazon Resource Name (ARN) of the listener.
createRule_listenerIdentifier :: Lens.Lens' CreateRule Prelude.Text
createRule_listenerIdentifier = Lens.lens (\CreateRule' {listenerIdentifier} -> listenerIdentifier) (\s@CreateRule' {} a -> s {listenerIdentifier = a} :: CreateRule)

-- | The rule match.
createRule_match :: Lens.Lens' CreateRule RuleMatch
createRule_match = Lens.lens (\CreateRule' {match} -> match) (\s@CreateRule' {} a -> s {match = a} :: CreateRule)

-- | The name of the rule. The name must be unique within the listener. The
-- valid characters are a-z, 0-9, and hyphens (-). You can\'t use a hyphen
-- as the first or last character, or immediately after another hyphen.
createRule_name :: Lens.Lens' CreateRule Prelude.Text
createRule_name = Lens.lens (\CreateRule' {name} -> name) (\s@CreateRule' {} a -> s {name = a} :: CreateRule)

-- | The priority assigned to the rule. Each rule for a specific listener
-- must have a unique priority. The lower the priority number the higher
-- the priority.
createRule_priority :: Lens.Lens' CreateRule Prelude.Natural
createRule_priority = Lens.lens (\CreateRule' {priority} -> priority) (\s@CreateRule' {} a -> s {priority = a} :: CreateRule)

-- | The ID or Amazon Resource Name (ARN) of the service.
createRule_serviceIdentifier :: Lens.Lens' CreateRule Prelude.Text
createRule_serviceIdentifier = Lens.lens (\CreateRule' {serviceIdentifier} -> serviceIdentifier) (\s@CreateRule' {} a -> s {serviceIdentifier = a} :: CreateRule)

instance Core.AWSRequest CreateRule where
  type AWSResponse CreateRule = CreateRuleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRuleResponse'
            Prelude.<$> (x Data..?> "action")
            Prelude.<*> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "match")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "priority")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateRule where
  hashWithSalt _salt CreateRule' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` listenerIdentifier
      `Prelude.hashWithSalt` match
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` serviceIdentifier

instance Prelude.NFData CreateRule where
  rnf CreateRule' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf action
      `Prelude.seq` Prelude.rnf listenerIdentifier
      `Prelude.seq` Prelude.rnf match
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf serviceIdentifier

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
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("action" Data..= action),
            Prelude.Just ("match" Data..= match),
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("priority" Data..= priority)
          ]
      )

instance Data.ToPath CreateRule where
  toPath CreateRule' {..} =
    Prelude.mconcat
      [ "/services/",
        Data.toBS serviceIdentifier,
        "/listeners/",
        Data.toBS listenerIdentifier,
        "/rules"
      ]

instance Data.ToQuery CreateRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateRuleResponse' smart constructor.
data CreateRuleResponse = CreateRuleResponse'
  { -- | The rule action. Each rule must include exactly one of the following
    -- types of actions: @forward @or @fixed-response@, and it must be the last
    -- action to be performed.
    action :: Prelude.Maybe RuleAction,
    -- | The Amazon Resource Name (ARN) of the rule.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the rule.
    id :: Prelude.Maybe Prelude.Text,
    -- | The rule match. The @RuleMatch@ must be an @HttpMatch@. This means that
    -- the rule should be an exact match on HTTP constraints which are made up
    -- of the HTTP method, path, and header.
    match :: Prelude.Maybe RuleMatch,
    -- | The name of the rule.
    name :: Prelude.Maybe Prelude.Text,
    -- | The priority assigned to the rule. The lower the priority number the
    -- higher the priority.
    priority :: Prelude.Maybe Prelude.Natural,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'action', 'createRuleResponse_action' - The rule action. Each rule must include exactly one of the following
-- types of actions: @forward @or @fixed-response@, and it must be the last
-- action to be performed.
--
-- 'arn', 'createRuleResponse_arn' - The Amazon Resource Name (ARN) of the rule.
--
-- 'id', 'createRuleResponse_id' - The ID of the rule.
--
-- 'match', 'createRuleResponse_match' - The rule match. The @RuleMatch@ must be an @HttpMatch@. This means that
-- the rule should be an exact match on HTTP constraints which are made up
-- of the HTTP method, path, and header.
--
-- 'name', 'createRuleResponse_name' - The name of the rule.
--
-- 'priority', 'createRuleResponse_priority' - The priority assigned to the rule. The lower the priority number the
-- higher the priority.
--
-- 'httpStatus', 'createRuleResponse_httpStatus' - The response's http status code.
newCreateRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateRuleResponse
newCreateRuleResponse pHttpStatus_ =
  CreateRuleResponse'
    { action = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      match = Prelude.Nothing,
      name = Prelude.Nothing,
      priority = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The rule action. Each rule must include exactly one of the following
-- types of actions: @forward @or @fixed-response@, and it must be the last
-- action to be performed.
createRuleResponse_action :: Lens.Lens' CreateRuleResponse (Prelude.Maybe RuleAction)
createRuleResponse_action = Lens.lens (\CreateRuleResponse' {action} -> action) (\s@CreateRuleResponse' {} a -> s {action = a} :: CreateRuleResponse)

-- | The Amazon Resource Name (ARN) of the rule.
createRuleResponse_arn :: Lens.Lens' CreateRuleResponse (Prelude.Maybe Prelude.Text)
createRuleResponse_arn = Lens.lens (\CreateRuleResponse' {arn} -> arn) (\s@CreateRuleResponse' {} a -> s {arn = a} :: CreateRuleResponse)

-- | The ID of the rule.
createRuleResponse_id :: Lens.Lens' CreateRuleResponse (Prelude.Maybe Prelude.Text)
createRuleResponse_id = Lens.lens (\CreateRuleResponse' {id} -> id) (\s@CreateRuleResponse' {} a -> s {id = a} :: CreateRuleResponse)

-- | The rule match. The @RuleMatch@ must be an @HttpMatch@. This means that
-- the rule should be an exact match on HTTP constraints which are made up
-- of the HTTP method, path, and header.
createRuleResponse_match :: Lens.Lens' CreateRuleResponse (Prelude.Maybe RuleMatch)
createRuleResponse_match = Lens.lens (\CreateRuleResponse' {match} -> match) (\s@CreateRuleResponse' {} a -> s {match = a} :: CreateRuleResponse)

-- | The name of the rule.
createRuleResponse_name :: Lens.Lens' CreateRuleResponse (Prelude.Maybe Prelude.Text)
createRuleResponse_name = Lens.lens (\CreateRuleResponse' {name} -> name) (\s@CreateRuleResponse' {} a -> s {name = a} :: CreateRuleResponse)

-- | The priority assigned to the rule. The lower the priority number the
-- higher the priority.
createRuleResponse_priority :: Lens.Lens' CreateRuleResponse (Prelude.Maybe Prelude.Natural)
createRuleResponse_priority = Lens.lens (\CreateRuleResponse' {priority} -> priority) (\s@CreateRuleResponse' {} a -> s {priority = a} :: CreateRuleResponse)

-- | The response's http status code.
createRuleResponse_httpStatus :: Lens.Lens' CreateRuleResponse Prelude.Int
createRuleResponse_httpStatus = Lens.lens (\CreateRuleResponse' {httpStatus} -> httpStatus) (\s@CreateRuleResponse' {} a -> s {httpStatus = a} :: CreateRuleResponse)

instance Prelude.NFData CreateRuleResponse where
  rnf CreateRuleResponse' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf match
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf httpStatus
