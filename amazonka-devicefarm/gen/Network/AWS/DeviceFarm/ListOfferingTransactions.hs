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
-- Module      : Network.AWS.DeviceFarm.ListOfferingTransactions
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.DeviceFarm.ListOfferingTransactions
  ( -- * Creating a Request
    ListOfferingTransactions (..),
    newListOfferingTransactions,

    -- * Request Lenses
    listOfferingTransactions_nextToken,

    -- * Destructuring the Response
    ListOfferingTransactionsResponse (..),
    newListOfferingTransactionsResponse,

    -- * Response Lenses
    listOfferingTransactionsResponse_offeringTransactions,
    listOfferingTransactionsResponse_nextToken,
    listOfferingTransactionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to list the offering transaction history.
--
-- /See:/ 'newListOfferingTransactions' smart constructor.
data ListOfferingTransactions = ListOfferingTransactions'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  ListOfferingTransactions' {nextToken = Core.Nothing}

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listOfferingTransactions_nextToken :: Lens.Lens' ListOfferingTransactions (Core.Maybe Core.Text)
listOfferingTransactions_nextToken = Lens.lens (\ListOfferingTransactions' {nextToken} -> nextToken) (\s@ListOfferingTransactions' {} a -> s {nextToken = a} :: ListOfferingTransactions)

instance Core.AWSPager ListOfferingTransactions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listOfferingTransactionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listOfferingTransactionsResponse_offeringTransactions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listOfferingTransactions_nextToken
          Lens..~ rs
          Lens.^? listOfferingTransactionsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListOfferingTransactions where
  type
    AWSResponse ListOfferingTransactions =
      ListOfferingTransactionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOfferingTransactionsResponse'
            Core.<$> ( x Core..?> "offeringTransactions"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListOfferingTransactions

instance Core.NFData ListOfferingTransactions

instance Core.ToHeaders ListOfferingTransactions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.ListOfferingTransactions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListOfferingTransactions where
  toJSON ListOfferingTransactions' {..} =
    Core.object
      ( Core.catMaybes
          [("nextToken" Core..=) Core.<$> nextToken]
      )

instance Core.ToPath ListOfferingTransactions where
  toPath = Core.const "/"

instance Core.ToQuery ListOfferingTransactions where
  toQuery = Core.const Core.mempty

-- | Returns the transaction log of the specified offerings.
--
-- /See:/ 'newListOfferingTransactionsResponse' smart constructor.
data ListOfferingTransactionsResponse = ListOfferingTransactionsResponse'
  { -- | The audit log of subscriptions you have purchased and modified through
    -- AWS Device Farm.
    offeringTransactions :: Core.Maybe [OfferingTransaction],
    -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListOfferingTransactionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'offeringTransactions', 'listOfferingTransactionsResponse_offeringTransactions' - The audit log of subscriptions you have purchased and modified through
-- AWS Device Farm.
--
-- 'nextToken', 'listOfferingTransactionsResponse_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'httpStatus', 'listOfferingTransactionsResponse_httpStatus' - The response's http status code.
newListOfferingTransactionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListOfferingTransactionsResponse
newListOfferingTransactionsResponse pHttpStatus_ =
  ListOfferingTransactionsResponse'
    { offeringTransactions =
        Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The audit log of subscriptions you have purchased and modified through
-- AWS Device Farm.
listOfferingTransactionsResponse_offeringTransactions :: Lens.Lens' ListOfferingTransactionsResponse (Core.Maybe [OfferingTransaction])
listOfferingTransactionsResponse_offeringTransactions = Lens.lens (\ListOfferingTransactionsResponse' {offeringTransactions} -> offeringTransactions) (\s@ListOfferingTransactionsResponse' {} a -> s {offeringTransactions = a} :: ListOfferingTransactionsResponse) Core.. Lens.mapping Lens._Coerce

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listOfferingTransactionsResponse_nextToken :: Lens.Lens' ListOfferingTransactionsResponse (Core.Maybe Core.Text)
listOfferingTransactionsResponse_nextToken = Lens.lens (\ListOfferingTransactionsResponse' {nextToken} -> nextToken) (\s@ListOfferingTransactionsResponse' {} a -> s {nextToken = a} :: ListOfferingTransactionsResponse)

-- | The response's http status code.
listOfferingTransactionsResponse_httpStatus :: Lens.Lens' ListOfferingTransactionsResponse Core.Int
listOfferingTransactionsResponse_httpStatus = Lens.lens (\ListOfferingTransactionsResponse' {httpStatus} -> httpStatus) (\s@ListOfferingTransactionsResponse' {} a -> s {httpStatus = a} :: ListOfferingTransactionsResponse)

instance Core.NFData ListOfferingTransactionsResponse
