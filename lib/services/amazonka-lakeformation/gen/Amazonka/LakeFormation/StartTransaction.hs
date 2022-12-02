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
-- Module      : Amazonka.LakeFormation.StartTransaction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a new transaction and returns its transaction ID. Transaction IDs
-- are opaque objects that you can use to identify a transaction.
module Amazonka.LakeFormation.StartTransaction
  ( -- * Creating a Request
    StartTransaction (..),
    newStartTransaction,

    -- * Request Lenses
    startTransaction_transactionType,

    -- * Destructuring the Response
    StartTransactionResponse (..),
    newStartTransactionResponse,

    -- * Response Lenses
    startTransactionResponse_transactionId,
    startTransactionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartTransaction' smart constructor.
data StartTransaction = StartTransaction'
  { -- | Indicates whether this transaction should be read only or read and
    -- write. Writes made using a read-only transaction ID will be rejected.
    -- Read-only transactions do not need to be committed.
    transactionType :: Prelude.Maybe TransactionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartTransaction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transactionType', 'startTransaction_transactionType' - Indicates whether this transaction should be read only or read and
-- write. Writes made using a read-only transaction ID will be rejected.
-- Read-only transactions do not need to be committed.
newStartTransaction ::
  StartTransaction
newStartTransaction =
  StartTransaction'
    { transactionType =
        Prelude.Nothing
    }

-- | Indicates whether this transaction should be read only or read and
-- write. Writes made using a read-only transaction ID will be rejected.
-- Read-only transactions do not need to be committed.
startTransaction_transactionType :: Lens.Lens' StartTransaction (Prelude.Maybe TransactionType)
startTransaction_transactionType = Lens.lens (\StartTransaction' {transactionType} -> transactionType) (\s@StartTransaction' {} a -> s {transactionType = a} :: StartTransaction)

instance Core.AWSRequest StartTransaction where
  type
    AWSResponse StartTransaction =
      StartTransactionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartTransactionResponse'
            Prelude.<$> (x Data..?> "TransactionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartTransaction where
  hashWithSalt _salt StartTransaction' {..} =
    _salt `Prelude.hashWithSalt` transactionType

instance Prelude.NFData StartTransaction where
  rnf StartTransaction' {..} =
    Prelude.rnf transactionType

instance Data.ToHeaders StartTransaction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartTransaction where
  toJSON StartTransaction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TransactionType" Data..=)
              Prelude.<$> transactionType
          ]
      )

instance Data.ToPath StartTransaction where
  toPath = Prelude.const "/StartTransaction"

instance Data.ToQuery StartTransaction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartTransactionResponse' smart constructor.
data StartTransactionResponse = StartTransactionResponse'
  { -- | An opaque identifier for the transaction.
    transactionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartTransactionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transactionId', 'startTransactionResponse_transactionId' - An opaque identifier for the transaction.
--
-- 'httpStatus', 'startTransactionResponse_httpStatus' - The response's http status code.
newStartTransactionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartTransactionResponse
newStartTransactionResponse pHttpStatus_ =
  StartTransactionResponse'
    { transactionId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An opaque identifier for the transaction.
startTransactionResponse_transactionId :: Lens.Lens' StartTransactionResponse (Prelude.Maybe Prelude.Text)
startTransactionResponse_transactionId = Lens.lens (\StartTransactionResponse' {transactionId} -> transactionId) (\s@StartTransactionResponse' {} a -> s {transactionId = a} :: StartTransactionResponse)

-- | The response's http status code.
startTransactionResponse_httpStatus :: Lens.Lens' StartTransactionResponse Prelude.Int
startTransactionResponse_httpStatus = Lens.lens (\StartTransactionResponse' {httpStatus} -> httpStatus) (\s@StartTransactionResponse' {} a -> s {httpStatus = a} :: StartTransactionResponse)

instance Prelude.NFData StartTransactionResponse where
  rnf StartTransactionResponse' {..} =
    Prelude.rnf transactionId
      `Prelude.seq` Prelude.rnf httpStatus
