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
-- Module      : Amazonka.VPCLattice.BatchUpdateRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the listener rules in a batch. You can use this operation to
-- change the priority of listener rules. This can be useful when bulk
-- updating or swapping rule priority.
module Amazonka.VPCLattice.BatchUpdateRule
  ( -- * Creating a Request
    BatchUpdateRule (..),
    newBatchUpdateRule,

    -- * Request Lenses
    batchUpdateRule_listenerIdentifier,
    batchUpdateRule_rules,
    batchUpdateRule_serviceIdentifier,

    -- * Destructuring the Response
    BatchUpdateRuleResponse (..),
    newBatchUpdateRuleResponse,

    -- * Response Lenses
    batchUpdateRuleResponse_successful,
    batchUpdateRuleResponse_unsuccessful,
    batchUpdateRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newBatchUpdateRule' smart constructor.
data BatchUpdateRule = BatchUpdateRule'
  { -- | The ID or Amazon Resource Name (ARN) of the listener.
    listenerIdentifier :: Prelude.Text,
    -- | The rules for the specified listener.
    rules :: Prelude.NonEmpty RuleUpdate,
    -- | The ID or Amazon Resource Name (ARN) of the service.
    serviceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchUpdateRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listenerIdentifier', 'batchUpdateRule_listenerIdentifier' - The ID or Amazon Resource Name (ARN) of the listener.
--
-- 'rules', 'batchUpdateRule_rules' - The rules for the specified listener.
--
-- 'serviceIdentifier', 'batchUpdateRule_serviceIdentifier' - The ID or Amazon Resource Name (ARN) of the service.
newBatchUpdateRule ::
  -- | 'listenerIdentifier'
  Prelude.Text ->
  -- | 'rules'
  Prelude.NonEmpty RuleUpdate ->
  -- | 'serviceIdentifier'
  Prelude.Text ->
  BatchUpdateRule
newBatchUpdateRule
  pListenerIdentifier_
  pRules_
  pServiceIdentifier_ =
    BatchUpdateRule'
      { listenerIdentifier =
          pListenerIdentifier_,
        rules = Lens.coerced Lens.# pRules_,
        serviceIdentifier = pServiceIdentifier_
      }

-- | The ID or Amazon Resource Name (ARN) of the listener.
batchUpdateRule_listenerIdentifier :: Lens.Lens' BatchUpdateRule Prelude.Text
batchUpdateRule_listenerIdentifier = Lens.lens (\BatchUpdateRule' {listenerIdentifier} -> listenerIdentifier) (\s@BatchUpdateRule' {} a -> s {listenerIdentifier = a} :: BatchUpdateRule)

-- | The rules for the specified listener.
batchUpdateRule_rules :: Lens.Lens' BatchUpdateRule (Prelude.NonEmpty RuleUpdate)
batchUpdateRule_rules = Lens.lens (\BatchUpdateRule' {rules} -> rules) (\s@BatchUpdateRule' {} a -> s {rules = a} :: BatchUpdateRule) Prelude.. Lens.coerced

-- | The ID or Amazon Resource Name (ARN) of the service.
batchUpdateRule_serviceIdentifier :: Lens.Lens' BatchUpdateRule Prelude.Text
batchUpdateRule_serviceIdentifier = Lens.lens (\BatchUpdateRule' {serviceIdentifier} -> serviceIdentifier) (\s@BatchUpdateRule' {} a -> s {serviceIdentifier = a} :: BatchUpdateRule)

instance Core.AWSRequest BatchUpdateRule where
  type
    AWSResponse BatchUpdateRule =
      BatchUpdateRuleResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchUpdateRuleResponse'
            Prelude.<$> (x Data..?> "successful" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "unsuccessful" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchUpdateRule where
  hashWithSalt _salt BatchUpdateRule' {..} =
    _salt
      `Prelude.hashWithSalt` listenerIdentifier
      `Prelude.hashWithSalt` rules
      `Prelude.hashWithSalt` serviceIdentifier

instance Prelude.NFData BatchUpdateRule where
  rnf BatchUpdateRule' {..} =
    Prelude.rnf listenerIdentifier
      `Prelude.seq` Prelude.rnf rules
      `Prelude.seq` Prelude.rnf serviceIdentifier

instance Data.ToHeaders BatchUpdateRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchUpdateRule where
  toJSON BatchUpdateRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("rules" Data..= rules)]
      )

instance Data.ToPath BatchUpdateRule where
  toPath BatchUpdateRule' {..} =
    Prelude.mconcat
      [ "/services/",
        Data.toBS serviceIdentifier,
        "/listeners/",
        Data.toBS listenerIdentifier,
        "/rules"
      ]

instance Data.ToQuery BatchUpdateRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchUpdateRuleResponse' smart constructor.
data BatchUpdateRuleResponse = BatchUpdateRuleResponse'
  { -- | The rules that were successfully updated.
    successful :: Prelude.Maybe [RuleUpdateSuccess],
    -- | The rules that the operation couldn\'t update.
    unsuccessful :: Prelude.Maybe [RuleUpdateFailure],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchUpdateRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'successful', 'batchUpdateRuleResponse_successful' - The rules that were successfully updated.
--
-- 'unsuccessful', 'batchUpdateRuleResponse_unsuccessful' - The rules that the operation couldn\'t update.
--
-- 'httpStatus', 'batchUpdateRuleResponse_httpStatus' - The response's http status code.
newBatchUpdateRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchUpdateRuleResponse
newBatchUpdateRuleResponse pHttpStatus_ =
  BatchUpdateRuleResponse'
    { successful =
        Prelude.Nothing,
      unsuccessful = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The rules that were successfully updated.
batchUpdateRuleResponse_successful :: Lens.Lens' BatchUpdateRuleResponse (Prelude.Maybe [RuleUpdateSuccess])
batchUpdateRuleResponse_successful = Lens.lens (\BatchUpdateRuleResponse' {successful} -> successful) (\s@BatchUpdateRuleResponse' {} a -> s {successful = a} :: BatchUpdateRuleResponse) Prelude.. Lens.mapping Lens.coerced

-- | The rules that the operation couldn\'t update.
batchUpdateRuleResponse_unsuccessful :: Lens.Lens' BatchUpdateRuleResponse (Prelude.Maybe [RuleUpdateFailure])
batchUpdateRuleResponse_unsuccessful = Lens.lens (\BatchUpdateRuleResponse' {unsuccessful} -> unsuccessful) (\s@BatchUpdateRuleResponse' {} a -> s {unsuccessful = a} :: BatchUpdateRuleResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchUpdateRuleResponse_httpStatus :: Lens.Lens' BatchUpdateRuleResponse Prelude.Int
batchUpdateRuleResponse_httpStatus = Lens.lens (\BatchUpdateRuleResponse' {httpStatus} -> httpStatus) (\s@BatchUpdateRuleResponse' {} a -> s {httpStatus = a} :: BatchUpdateRuleResponse)

instance Prelude.NFData BatchUpdateRuleResponse where
  rnf BatchUpdateRuleResponse' {..} =
    Prelude.rnf successful
      `Prelude.seq` Prelude.rnf unsuccessful
      `Prelude.seq` Prelude.rnf httpStatus
