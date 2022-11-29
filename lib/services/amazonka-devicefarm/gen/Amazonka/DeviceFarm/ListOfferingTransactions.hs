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
-- Module      : Amazonka.DeviceFarm.ListOfferingTransactions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all historical purchases, renewals, and system renewal
-- transactions for an AWS account. The list is paginated and ordered by a
-- descending timestamp (most recent transactions are first). The API
-- returns a @NotEligible@ error if the user is not permitted to invoke the
-- operation. If you must be able to invoke this operation, contact
-- <mailto:aws-devicefarm-support@amazon.com aws-devicefarm-support\@amazon.com>.
--
-- This operation returns paginated results.
module Amazonka.DeviceFarm.ListOfferingTransactions
  ( -- * Creating a Request
    ListOfferingTransactions (..),
    newListOfferingTransactions,

    -- * Request Lenses
    listOfferingTransactions_nextToken,

    -- * Destructuring the Response
    ListOfferingTransactionsResponse (..),
    newListOfferingTransactionsResponse,

    -- * Response Lenses
    listOfferingTransactionsResponse_nextToken,
    listOfferingTransactionsResponse_offeringTransactions,
    listOfferingTransactionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to list the offering transaction history.
--
-- /See:/ 'newListOfferingTransactions' smart constructor.
data ListOfferingTransactions = ListOfferingTransactions'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOfferingTransactions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOfferingTransactions_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
newListOfferingTransactions ::
  ListOfferingTransactions
newListOfferingTransactions =
  ListOfferingTransactions'
    { nextToken =
        Prelude.Nothing
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listOfferingTransactions_nextToken :: Lens.Lens' ListOfferingTransactions (Prelude.Maybe Prelude.Text)
listOfferingTransactions_nextToken = Lens.lens (\ListOfferingTransactions' {nextToken} -> nextToken) (\s@ListOfferingTransactions' {} a -> s {nextToken = a} :: ListOfferingTransactions)

instance Core.AWSPager ListOfferingTransactions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listOfferingTransactionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listOfferingTransactionsResponse_offeringTransactions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listOfferingTransactions_nextToken
          Lens..~ rs
          Lens.^? listOfferingTransactionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListOfferingTransactions where
  type
    AWSResponse ListOfferingTransactions =
      ListOfferingTransactionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOfferingTransactionsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "offeringTransactions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListOfferingTransactions where
  hashWithSalt _salt ListOfferingTransactions' {..} =
    _salt `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListOfferingTransactions where
  rnf ListOfferingTransactions' {..} =
    Prelude.rnf nextToken

instance Core.ToHeaders ListOfferingTransactions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.ListOfferingTransactions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListOfferingTransactions where
  toJSON ListOfferingTransactions' {..} =
    Core.object
      ( Prelude.catMaybes
          [("nextToken" Core..=) Prelude.<$> nextToken]
      )

instance Core.ToPath ListOfferingTransactions where
  toPath = Prelude.const "/"

instance Core.ToQuery ListOfferingTransactions where
  toQuery = Prelude.const Prelude.mempty

-- | Returns the transaction log of the specified offerings.
--
-- /See:/ 'newListOfferingTransactionsResponse' smart constructor.
data ListOfferingTransactionsResponse = ListOfferingTransactionsResponse'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The audit log of subscriptions you have purchased and modified through
    -- AWS Device Farm.
    offeringTransactions :: Prelude.Maybe [OfferingTransaction],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOfferingTransactionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOfferingTransactionsResponse_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'offeringTransactions', 'listOfferingTransactionsResponse_offeringTransactions' - The audit log of subscriptions you have purchased and modified through
-- AWS Device Farm.
--
-- 'httpStatus', 'listOfferingTransactionsResponse_httpStatus' - The response's http status code.
newListOfferingTransactionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListOfferingTransactionsResponse
newListOfferingTransactionsResponse pHttpStatus_ =
  ListOfferingTransactionsResponse'
    { nextToken =
        Prelude.Nothing,
      offeringTransactions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listOfferingTransactionsResponse_nextToken :: Lens.Lens' ListOfferingTransactionsResponse (Prelude.Maybe Prelude.Text)
listOfferingTransactionsResponse_nextToken = Lens.lens (\ListOfferingTransactionsResponse' {nextToken} -> nextToken) (\s@ListOfferingTransactionsResponse' {} a -> s {nextToken = a} :: ListOfferingTransactionsResponse)

-- | The audit log of subscriptions you have purchased and modified through
-- AWS Device Farm.
listOfferingTransactionsResponse_offeringTransactions :: Lens.Lens' ListOfferingTransactionsResponse (Prelude.Maybe [OfferingTransaction])
listOfferingTransactionsResponse_offeringTransactions = Lens.lens (\ListOfferingTransactionsResponse' {offeringTransactions} -> offeringTransactions) (\s@ListOfferingTransactionsResponse' {} a -> s {offeringTransactions = a} :: ListOfferingTransactionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listOfferingTransactionsResponse_httpStatus :: Lens.Lens' ListOfferingTransactionsResponse Prelude.Int
listOfferingTransactionsResponse_httpStatus = Lens.lens (\ListOfferingTransactionsResponse' {httpStatus} -> httpStatus) (\s@ListOfferingTransactionsResponse' {} a -> s {httpStatus = a} :: ListOfferingTransactionsResponse)

instance
  Prelude.NFData
    ListOfferingTransactionsResponse
  where
  rnf ListOfferingTransactionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf offeringTransactions
      `Prelude.seq` Prelude.rnf httpStatus
