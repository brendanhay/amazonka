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
-- Module      : Amazonka.LakeFormation.ExtendTransaction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Indicates to the service that the specified transaction is still active
-- and should not be treated as idle and aborted.
--
-- Write transactions that remain idle for a long period are automatically
-- aborted unless explicitly extended.
module Amazonka.LakeFormation.ExtendTransaction
  ( -- * Creating a Request
    ExtendTransaction (..),
    newExtendTransaction,

    -- * Request Lenses
    extendTransaction_transactionId,

    -- * Destructuring the Response
    ExtendTransactionResponse (..),
    newExtendTransactionResponse,

    -- * Response Lenses
    extendTransactionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newExtendTransaction' smart constructor.
data ExtendTransaction = ExtendTransaction'
  { -- | The transaction to extend.
    transactionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExtendTransaction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transactionId', 'extendTransaction_transactionId' - The transaction to extend.
newExtendTransaction ::
  ExtendTransaction
newExtendTransaction =
  ExtendTransaction' {transactionId = Prelude.Nothing}

-- | The transaction to extend.
extendTransaction_transactionId :: Lens.Lens' ExtendTransaction (Prelude.Maybe Prelude.Text)
extendTransaction_transactionId = Lens.lens (\ExtendTransaction' {transactionId} -> transactionId) (\s@ExtendTransaction' {} a -> s {transactionId = a} :: ExtendTransaction)

instance Core.AWSRequest ExtendTransaction where
  type
    AWSResponse ExtendTransaction =
      ExtendTransactionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          ExtendTransactionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ExtendTransaction where
  hashWithSalt _salt ExtendTransaction' {..} =
    _salt `Prelude.hashWithSalt` transactionId

instance Prelude.NFData ExtendTransaction where
  rnf ExtendTransaction' {..} =
    Prelude.rnf transactionId

instance Data.ToHeaders ExtendTransaction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ExtendTransaction where
  toJSON ExtendTransaction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TransactionId" Data..=)
              Prelude.<$> transactionId
          ]
      )

instance Data.ToPath ExtendTransaction where
  toPath = Prelude.const "/ExtendTransaction"

instance Data.ToQuery ExtendTransaction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newExtendTransactionResponse' smart constructor.
data ExtendTransactionResponse = ExtendTransactionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExtendTransactionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'extendTransactionResponse_httpStatus' - The response's http status code.
newExtendTransactionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ExtendTransactionResponse
newExtendTransactionResponse pHttpStatus_ =
  ExtendTransactionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
extendTransactionResponse_httpStatus :: Lens.Lens' ExtendTransactionResponse Prelude.Int
extendTransactionResponse_httpStatus = Lens.lens (\ExtendTransactionResponse' {httpStatus} -> httpStatus) (\s@ExtendTransactionResponse' {} a -> s {httpStatus = a} :: ExtendTransactionResponse)

instance Prelude.NFData ExtendTransactionResponse where
  rnf ExtendTransactionResponse' {..} =
    Prelude.rnf httpStatus
