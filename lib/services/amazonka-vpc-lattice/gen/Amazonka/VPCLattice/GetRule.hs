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
-- Module      : Amazonka.VPCLattice.GetRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about listener rules. You can also retrieve
-- information about the default listener rule. For more information, see
-- <https://docs.aws.amazon.com/vpc-lattice/latest/ug/listeners.html#listener-rules Listener rules>
-- in the /Amazon VPC Lattice User Guide/.
module Amazonka.VPCLattice.GetRule
  ( -- * Creating a Request
    GetRule (..),
    newGetRule,

    -- * Request Lenses
    getRule_listenerIdentifier,
    getRule_ruleIdentifier,
    getRule_serviceIdentifier,

    -- * Destructuring the Response
    GetRuleResponse (..),
    newGetRuleResponse,

    -- * Response Lenses
    getRuleResponse_action,
    getRuleResponse_arn,
    getRuleResponse_createdAt,
    getRuleResponse_id,
    getRuleResponse_isDefault,
    getRuleResponse_lastUpdatedAt,
    getRuleResponse_match,
    getRuleResponse_name,
    getRuleResponse_priority,
    getRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newGetRule' smart constructor.
data GetRule = GetRule'
  { -- | The ID or Amazon Resource Name (ARN) of the listener.
    listenerIdentifier :: Prelude.Text,
    -- | The ID or Amazon Resource Name (ARN) of the listener rule.
    ruleIdentifier :: Prelude.Text,
    -- | The ID or Amazon Resource Name (ARN) of the service.
    serviceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listenerIdentifier', 'getRule_listenerIdentifier' - The ID or Amazon Resource Name (ARN) of the listener.
--
-- 'ruleIdentifier', 'getRule_ruleIdentifier' - The ID or Amazon Resource Name (ARN) of the listener rule.
--
-- 'serviceIdentifier', 'getRule_serviceIdentifier' - The ID or Amazon Resource Name (ARN) of the service.
newGetRule ::
  -- | 'listenerIdentifier'
  Prelude.Text ->
  -- | 'ruleIdentifier'
  Prelude.Text ->
  -- | 'serviceIdentifier'
  Prelude.Text ->
  GetRule
newGetRule
  pListenerIdentifier_
  pRuleIdentifier_
  pServiceIdentifier_ =
    GetRule'
      { listenerIdentifier = pListenerIdentifier_,
        ruleIdentifier = pRuleIdentifier_,
        serviceIdentifier = pServiceIdentifier_
      }

-- | The ID or Amazon Resource Name (ARN) of the listener.
getRule_listenerIdentifier :: Lens.Lens' GetRule Prelude.Text
getRule_listenerIdentifier = Lens.lens (\GetRule' {listenerIdentifier} -> listenerIdentifier) (\s@GetRule' {} a -> s {listenerIdentifier = a} :: GetRule)

-- | The ID or Amazon Resource Name (ARN) of the listener rule.
getRule_ruleIdentifier :: Lens.Lens' GetRule Prelude.Text
getRule_ruleIdentifier = Lens.lens (\GetRule' {ruleIdentifier} -> ruleIdentifier) (\s@GetRule' {} a -> s {ruleIdentifier = a} :: GetRule)

-- | The ID or Amazon Resource Name (ARN) of the service.
getRule_serviceIdentifier :: Lens.Lens' GetRule Prelude.Text
getRule_serviceIdentifier = Lens.lens (\GetRule' {serviceIdentifier} -> serviceIdentifier) (\s@GetRule' {} a -> s {serviceIdentifier = a} :: GetRule)

instance Core.AWSRequest GetRule where
  type AWSResponse GetRule = GetRuleResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRuleResponse'
            Prelude.<$> (x Data..?> "action")
            Prelude.<*> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "createdAt")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "isDefault")
            Prelude.<*> (x Data..?> "lastUpdatedAt")
            Prelude.<*> (x Data..?> "match")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "priority")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRule where
  hashWithSalt _salt GetRule' {..} =
    _salt
      `Prelude.hashWithSalt` listenerIdentifier
      `Prelude.hashWithSalt` ruleIdentifier
      `Prelude.hashWithSalt` serviceIdentifier

instance Prelude.NFData GetRule where
  rnf GetRule' {..} =
    Prelude.rnf listenerIdentifier
      `Prelude.seq` Prelude.rnf ruleIdentifier
      `Prelude.seq` Prelude.rnf serviceIdentifier

instance Data.ToHeaders GetRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetRule where
  toPath GetRule' {..} =
    Prelude.mconcat
      [ "/services/",
        Data.toBS serviceIdentifier,
        "/listeners/",
        Data.toBS listenerIdentifier,
        "/rules/",
        Data.toBS ruleIdentifier
      ]

instance Data.ToQuery GetRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRuleResponse' smart constructor.
data GetRuleResponse = GetRuleResponse'
  { -- | The action for the default rule.
    action :: Prelude.Maybe RuleAction,
    -- | The Amazon Resource Name (ARN) of the listener.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the listener rule was created, specified in
    -- ISO-8601 format.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | The ID of the listener.
    id :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether this is the default rule.
    isDefault :: Prelude.Maybe Prelude.Bool,
    -- | The date and time that the listener rule was last updated, specified in
    -- ISO-8601 format.
    lastUpdatedAt :: Prelude.Maybe Data.ISO8601,
    -- | The rule match.
    match :: Prelude.Maybe RuleMatch,
    -- | The name of the listener.
    name :: Prelude.Maybe Prelude.Text,
    -- | The priority level for the specified rule.
    priority :: Prelude.Maybe Prelude.Natural,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'getRuleResponse_action' - The action for the default rule.
--
-- 'arn', 'getRuleResponse_arn' - The Amazon Resource Name (ARN) of the listener.
--
-- 'createdAt', 'getRuleResponse_createdAt' - The date and time that the listener rule was created, specified in
-- ISO-8601 format.
--
-- 'id', 'getRuleResponse_id' - The ID of the listener.
--
-- 'isDefault', 'getRuleResponse_isDefault' - Indicates whether this is the default rule.
--
-- 'lastUpdatedAt', 'getRuleResponse_lastUpdatedAt' - The date and time that the listener rule was last updated, specified in
-- ISO-8601 format.
--
-- 'match', 'getRuleResponse_match' - The rule match.
--
-- 'name', 'getRuleResponse_name' - The name of the listener.
--
-- 'priority', 'getRuleResponse_priority' - The priority level for the specified rule.
--
-- 'httpStatus', 'getRuleResponse_httpStatus' - The response's http status code.
newGetRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRuleResponse
newGetRuleResponse pHttpStatus_ =
  GetRuleResponse'
    { action = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      id = Prelude.Nothing,
      isDefault = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      match = Prelude.Nothing,
      name = Prelude.Nothing,
      priority = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The action for the default rule.
getRuleResponse_action :: Lens.Lens' GetRuleResponse (Prelude.Maybe RuleAction)
getRuleResponse_action = Lens.lens (\GetRuleResponse' {action} -> action) (\s@GetRuleResponse' {} a -> s {action = a} :: GetRuleResponse)

-- | The Amazon Resource Name (ARN) of the listener.
getRuleResponse_arn :: Lens.Lens' GetRuleResponse (Prelude.Maybe Prelude.Text)
getRuleResponse_arn = Lens.lens (\GetRuleResponse' {arn} -> arn) (\s@GetRuleResponse' {} a -> s {arn = a} :: GetRuleResponse)

-- | The date and time that the listener rule was created, specified in
-- ISO-8601 format.
getRuleResponse_createdAt :: Lens.Lens' GetRuleResponse (Prelude.Maybe Prelude.UTCTime)
getRuleResponse_createdAt = Lens.lens (\GetRuleResponse' {createdAt} -> createdAt) (\s@GetRuleResponse' {} a -> s {createdAt = a} :: GetRuleResponse) Prelude.. Lens.mapping Data._Time

-- | The ID of the listener.
getRuleResponse_id :: Lens.Lens' GetRuleResponse (Prelude.Maybe Prelude.Text)
getRuleResponse_id = Lens.lens (\GetRuleResponse' {id} -> id) (\s@GetRuleResponse' {} a -> s {id = a} :: GetRuleResponse)

-- | Indicates whether this is the default rule.
getRuleResponse_isDefault :: Lens.Lens' GetRuleResponse (Prelude.Maybe Prelude.Bool)
getRuleResponse_isDefault = Lens.lens (\GetRuleResponse' {isDefault} -> isDefault) (\s@GetRuleResponse' {} a -> s {isDefault = a} :: GetRuleResponse)

-- | The date and time that the listener rule was last updated, specified in
-- ISO-8601 format.
getRuleResponse_lastUpdatedAt :: Lens.Lens' GetRuleResponse (Prelude.Maybe Prelude.UTCTime)
getRuleResponse_lastUpdatedAt = Lens.lens (\GetRuleResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@GetRuleResponse' {} a -> s {lastUpdatedAt = a} :: GetRuleResponse) Prelude.. Lens.mapping Data._Time

-- | The rule match.
getRuleResponse_match :: Lens.Lens' GetRuleResponse (Prelude.Maybe RuleMatch)
getRuleResponse_match = Lens.lens (\GetRuleResponse' {match} -> match) (\s@GetRuleResponse' {} a -> s {match = a} :: GetRuleResponse)

-- | The name of the listener.
getRuleResponse_name :: Lens.Lens' GetRuleResponse (Prelude.Maybe Prelude.Text)
getRuleResponse_name = Lens.lens (\GetRuleResponse' {name} -> name) (\s@GetRuleResponse' {} a -> s {name = a} :: GetRuleResponse)

-- | The priority level for the specified rule.
getRuleResponse_priority :: Lens.Lens' GetRuleResponse (Prelude.Maybe Prelude.Natural)
getRuleResponse_priority = Lens.lens (\GetRuleResponse' {priority} -> priority) (\s@GetRuleResponse' {} a -> s {priority = a} :: GetRuleResponse)

-- | The response's http status code.
getRuleResponse_httpStatus :: Lens.Lens' GetRuleResponse Prelude.Int
getRuleResponse_httpStatus = Lens.lens (\GetRuleResponse' {httpStatus} -> httpStatus) (\s@GetRuleResponse' {} a -> s {httpStatus = a} :: GetRuleResponse)

instance Prelude.NFData GetRuleResponse where
  rnf GetRuleResponse' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf isDefault
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf match
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf httpStatus
