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
-- Module      : Amazonka.VPCLattice.DeleteRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a listener rule. Each listener has a default rule for checking
-- connection requests, but you can define additional rules. Each rule
-- consists of a priority, one or more actions, and one or more conditions.
-- You can delete additional listener rules, but you cannot delete the
-- default rule.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpc-lattice/latest/ug/listeners.html#listener-rules Listener rules>
-- in the /Amazon VPC Lattice User Guide/.
module Amazonka.VPCLattice.DeleteRule
  ( -- * Creating a Request
    DeleteRule (..),
    newDeleteRule,

    -- * Request Lenses
    deleteRule_listenerIdentifier,
    deleteRule_ruleIdentifier,
    deleteRule_serviceIdentifier,

    -- * Destructuring the Response
    DeleteRuleResponse (..),
    newDeleteRuleResponse,

    -- * Response Lenses
    deleteRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newDeleteRule' smart constructor.
data DeleteRule = DeleteRule'
  { -- | The ID or Amazon Resource Name (ARN) of the listener.
    listenerIdentifier :: Prelude.Text,
    -- | The ID or Amazon Resource Name (ARN) of the rule.
    ruleIdentifier :: Prelude.Text,
    -- | The ID or Amazon Resource Name (ARN) of the service.
    serviceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listenerIdentifier', 'deleteRule_listenerIdentifier' - The ID or Amazon Resource Name (ARN) of the listener.
--
-- 'ruleIdentifier', 'deleteRule_ruleIdentifier' - The ID or Amazon Resource Name (ARN) of the rule.
--
-- 'serviceIdentifier', 'deleteRule_serviceIdentifier' - The ID or Amazon Resource Name (ARN) of the service.
newDeleteRule ::
  -- | 'listenerIdentifier'
  Prelude.Text ->
  -- | 'ruleIdentifier'
  Prelude.Text ->
  -- | 'serviceIdentifier'
  Prelude.Text ->
  DeleteRule
newDeleteRule
  pListenerIdentifier_
  pRuleIdentifier_
  pServiceIdentifier_ =
    DeleteRule'
      { listenerIdentifier =
          pListenerIdentifier_,
        ruleIdentifier = pRuleIdentifier_,
        serviceIdentifier = pServiceIdentifier_
      }

-- | The ID or Amazon Resource Name (ARN) of the listener.
deleteRule_listenerIdentifier :: Lens.Lens' DeleteRule Prelude.Text
deleteRule_listenerIdentifier = Lens.lens (\DeleteRule' {listenerIdentifier} -> listenerIdentifier) (\s@DeleteRule' {} a -> s {listenerIdentifier = a} :: DeleteRule)

-- | The ID or Amazon Resource Name (ARN) of the rule.
deleteRule_ruleIdentifier :: Lens.Lens' DeleteRule Prelude.Text
deleteRule_ruleIdentifier = Lens.lens (\DeleteRule' {ruleIdentifier} -> ruleIdentifier) (\s@DeleteRule' {} a -> s {ruleIdentifier = a} :: DeleteRule)

-- | The ID or Amazon Resource Name (ARN) of the service.
deleteRule_serviceIdentifier :: Lens.Lens' DeleteRule Prelude.Text
deleteRule_serviceIdentifier = Lens.lens (\DeleteRule' {serviceIdentifier} -> serviceIdentifier) (\s@DeleteRule' {} a -> s {serviceIdentifier = a} :: DeleteRule)

instance Core.AWSRequest DeleteRule where
  type AWSResponse DeleteRule = DeleteRuleResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteRuleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRule where
  hashWithSalt _salt DeleteRule' {..} =
    _salt
      `Prelude.hashWithSalt` listenerIdentifier
      `Prelude.hashWithSalt` ruleIdentifier
      `Prelude.hashWithSalt` serviceIdentifier

instance Prelude.NFData DeleteRule where
  rnf DeleteRule' {..} =
    Prelude.rnf listenerIdentifier
      `Prelude.seq` Prelude.rnf ruleIdentifier
      `Prelude.seq` Prelude.rnf serviceIdentifier

instance Data.ToHeaders DeleteRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteRule where
  toPath DeleteRule' {..} =
    Prelude.mconcat
      [ "/services/",
        Data.toBS serviceIdentifier,
        "/listeners/",
        Data.toBS listenerIdentifier,
        "/rules/",
        Data.toBS ruleIdentifier
      ]

instance Data.ToQuery DeleteRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRuleResponse' smart constructor.
data DeleteRuleResponse = DeleteRuleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteRuleResponse_httpStatus' - The response's http status code.
newDeleteRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteRuleResponse
newDeleteRuleResponse pHttpStatus_ =
  DeleteRuleResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteRuleResponse_httpStatus :: Lens.Lens' DeleteRuleResponse Prelude.Int
deleteRuleResponse_httpStatus = Lens.lens (\DeleteRuleResponse' {httpStatus} -> httpStatus) (\s@DeleteRuleResponse' {} a -> s {httpStatus = a} :: DeleteRuleResponse)

instance Prelude.NFData DeleteRuleResponse where
  rnf DeleteRuleResponse' {..} = Prelude.rnf httpStatus
