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
-- Module      : Amazonka.LakeFormation.ListTransactions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata about transactions and their status. To prevent the
-- response from growing indefinitely, only uncommitted transactions and
-- those available for time-travel queries are returned.
--
-- This operation can help you identify uncommitted transactions or to get
-- information about transactions.
module Amazonka.LakeFormation.ListTransactions
  ( -- * Creating a Request
    ListTransactions (..),
    newListTransactions,

    -- * Request Lenses
    listTransactions_catalogId,
    listTransactions_maxResults,
    listTransactions_nextToken,
    listTransactions_statusFilter,

    -- * Destructuring the Response
    ListTransactionsResponse (..),
    newListTransactionsResponse,

    -- * Response Lenses
    listTransactionsResponse_nextToken,
    listTransactionsResponse_transactions,
    listTransactionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTransactions' smart constructor.
data ListTransactions = ListTransactions'
  { -- | The catalog for which to list transactions. Defaults to the account ID
    -- of the caller.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of transactions to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A continuation token if this is not the first call to retrieve
    -- transactions.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A filter indicating the status of transactions to return. Options are
    -- ALL | COMPLETED | COMMITTED | ABORTED | ACTIVE. The default is @ALL@.
    statusFilter :: Prelude.Maybe TransactionStatusFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTransactions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'listTransactions_catalogId' - The catalog for which to list transactions. Defaults to the account ID
-- of the caller.
--
-- 'maxResults', 'listTransactions_maxResults' - The maximum number of transactions to return in a single call.
--
-- 'nextToken', 'listTransactions_nextToken' - A continuation token if this is not the first call to retrieve
-- transactions.
--
-- 'statusFilter', 'listTransactions_statusFilter' - A filter indicating the status of transactions to return. Options are
-- ALL | COMPLETED | COMMITTED | ABORTED | ACTIVE. The default is @ALL@.
newListTransactions ::
  ListTransactions
newListTransactions =
  ListTransactions'
    { catalogId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      statusFilter = Prelude.Nothing
    }

-- | The catalog for which to list transactions. Defaults to the account ID
-- of the caller.
listTransactions_catalogId :: Lens.Lens' ListTransactions (Prelude.Maybe Prelude.Text)
listTransactions_catalogId = Lens.lens (\ListTransactions' {catalogId} -> catalogId) (\s@ListTransactions' {} a -> s {catalogId = a} :: ListTransactions)

-- | The maximum number of transactions to return in a single call.
listTransactions_maxResults :: Lens.Lens' ListTransactions (Prelude.Maybe Prelude.Natural)
listTransactions_maxResults = Lens.lens (\ListTransactions' {maxResults} -> maxResults) (\s@ListTransactions' {} a -> s {maxResults = a} :: ListTransactions)

-- | A continuation token if this is not the first call to retrieve
-- transactions.
listTransactions_nextToken :: Lens.Lens' ListTransactions (Prelude.Maybe Prelude.Text)
listTransactions_nextToken = Lens.lens (\ListTransactions' {nextToken} -> nextToken) (\s@ListTransactions' {} a -> s {nextToken = a} :: ListTransactions)

-- | A filter indicating the status of transactions to return. Options are
-- ALL | COMPLETED | COMMITTED | ABORTED | ACTIVE. The default is @ALL@.
listTransactions_statusFilter :: Lens.Lens' ListTransactions (Prelude.Maybe TransactionStatusFilter)
listTransactions_statusFilter = Lens.lens (\ListTransactions' {statusFilter} -> statusFilter) (\s@ListTransactions' {} a -> s {statusFilter = a} :: ListTransactions)

instance Core.AWSRequest ListTransactions where
  type
    AWSResponse ListTransactions =
      ListTransactionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTransactionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Transactions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTransactions where
  hashWithSalt _salt ListTransactions' {..} =
    _salt
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` statusFilter

instance Prelude.NFData ListTransactions where
  rnf ListTransactions' {..} =
    Prelude.rnf catalogId `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf statusFilter

instance Data.ToHeaders ListTransactions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTransactions where
  toJSON ListTransactions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("StatusFilter" Data..=) Prelude.<$> statusFilter
          ]
      )

instance Data.ToPath ListTransactions where
  toPath = Prelude.const "/ListTransactions"

instance Data.ToQuery ListTransactions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTransactionsResponse' smart constructor.
data ListTransactionsResponse = ListTransactionsResponse'
  { -- | A continuation token indicating whether additional data is available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of transactions. The record for each transaction is a
    -- @TransactionDescription@ object.
    transactions :: Prelude.Maybe [TransactionDescription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTransactionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTransactionsResponse_nextToken' - A continuation token indicating whether additional data is available.
--
-- 'transactions', 'listTransactionsResponse_transactions' - A list of transactions. The record for each transaction is a
-- @TransactionDescription@ object.
--
-- 'httpStatus', 'listTransactionsResponse_httpStatus' - The response's http status code.
newListTransactionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTransactionsResponse
newListTransactionsResponse pHttpStatus_ =
  ListTransactionsResponse'
    { nextToken =
        Prelude.Nothing,
      transactions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token indicating whether additional data is available.
listTransactionsResponse_nextToken :: Lens.Lens' ListTransactionsResponse (Prelude.Maybe Prelude.Text)
listTransactionsResponse_nextToken = Lens.lens (\ListTransactionsResponse' {nextToken} -> nextToken) (\s@ListTransactionsResponse' {} a -> s {nextToken = a} :: ListTransactionsResponse)

-- | A list of transactions. The record for each transaction is a
-- @TransactionDescription@ object.
listTransactionsResponse_transactions :: Lens.Lens' ListTransactionsResponse (Prelude.Maybe [TransactionDescription])
listTransactionsResponse_transactions = Lens.lens (\ListTransactionsResponse' {transactions} -> transactions) (\s@ListTransactionsResponse' {} a -> s {transactions = a} :: ListTransactionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTransactionsResponse_httpStatus :: Lens.Lens' ListTransactionsResponse Prelude.Int
listTransactionsResponse_httpStatus = Lens.lens (\ListTransactionsResponse' {httpStatus} -> httpStatus) (\s@ListTransactionsResponse' {} a -> s {httpStatus = a} :: ListTransactionsResponse)

instance Prelude.NFData ListTransactionsResponse where
  rnf ListTransactionsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf transactions `Prelude.seq`
        Prelude.rnf httpStatus
