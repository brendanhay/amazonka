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
-- Module      : Amazonka.Backup.ListRecoveryPointsByLegalHold
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action returns recovery point ARNs (Amazon Resource Names) of the
-- specified legal hold.
--
-- This operation returns paginated results.
module Amazonka.Backup.ListRecoveryPointsByLegalHold
  ( -- * Creating a Request
    ListRecoveryPointsByLegalHold (..),
    newListRecoveryPointsByLegalHold,

    -- * Request Lenses
    listRecoveryPointsByLegalHold_maxResults,
    listRecoveryPointsByLegalHold_nextToken,
    listRecoveryPointsByLegalHold_legalHoldId,

    -- * Destructuring the Response
    ListRecoveryPointsByLegalHoldResponse (..),
    newListRecoveryPointsByLegalHoldResponse,

    -- * Response Lenses
    listRecoveryPointsByLegalHoldResponse_nextToken,
    listRecoveryPointsByLegalHoldResponse_recoveryPoints,
    listRecoveryPointsByLegalHoldResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRecoveryPointsByLegalHold' smart constructor.
data ListRecoveryPointsByLegalHold = ListRecoveryPointsByLegalHold'
  { -- | This is the maximum number of resource list items to be returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | This is the next item following a partial list of returned resources.
    -- For example, if a request is made to return @maxResults@ number of
    -- resources, @NextToken@ allows you to return more items in your list
    -- starting at the location pointed to by the next token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | This is the ID of the legal hold.
    legalHoldId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRecoveryPointsByLegalHold' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listRecoveryPointsByLegalHold_maxResults' - This is the maximum number of resource list items to be returned.
--
-- 'nextToken', 'listRecoveryPointsByLegalHold_nextToken' - This is the next item following a partial list of returned resources.
-- For example, if a request is made to return @maxResults@ number of
-- resources, @NextToken@ allows you to return more items in your list
-- starting at the location pointed to by the next token.
--
-- 'legalHoldId', 'listRecoveryPointsByLegalHold_legalHoldId' - This is the ID of the legal hold.
newListRecoveryPointsByLegalHold ::
  -- | 'legalHoldId'
  Prelude.Text ->
  ListRecoveryPointsByLegalHold
newListRecoveryPointsByLegalHold pLegalHoldId_ =
  ListRecoveryPointsByLegalHold'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      legalHoldId = pLegalHoldId_
    }

-- | This is the maximum number of resource list items to be returned.
listRecoveryPointsByLegalHold_maxResults :: Lens.Lens' ListRecoveryPointsByLegalHold (Prelude.Maybe Prelude.Natural)
listRecoveryPointsByLegalHold_maxResults = Lens.lens (\ListRecoveryPointsByLegalHold' {maxResults} -> maxResults) (\s@ListRecoveryPointsByLegalHold' {} a -> s {maxResults = a} :: ListRecoveryPointsByLegalHold)

-- | This is the next item following a partial list of returned resources.
-- For example, if a request is made to return @maxResults@ number of
-- resources, @NextToken@ allows you to return more items in your list
-- starting at the location pointed to by the next token.
listRecoveryPointsByLegalHold_nextToken :: Lens.Lens' ListRecoveryPointsByLegalHold (Prelude.Maybe Prelude.Text)
listRecoveryPointsByLegalHold_nextToken = Lens.lens (\ListRecoveryPointsByLegalHold' {nextToken} -> nextToken) (\s@ListRecoveryPointsByLegalHold' {} a -> s {nextToken = a} :: ListRecoveryPointsByLegalHold)

-- | This is the ID of the legal hold.
listRecoveryPointsByLegalHold_legalHoldId :: Lens.Lens' ListRecoveryPointsByLegalHold Prelude.Text
listRecoveryPointsByLegalHold_legalHoldId = Lens.lens (\ListRecoveryPointsByLegalHold' {legalHoldId} -> legalHoldId) (\s@ListRecoveryPointsByLegalHold' {} a -> s {legalHoldId = a} :: ListRecoveryPointsByLegalHold)

instance Core.AWSPager ListRecoveryPointsByLegalHold where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRecoveryPointsByLegalHoldResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRecoveryPointsByLegalHoldResponse_recoveryPoints
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listRecoveryPointsByLegalHold_nextToken
          Lens..~ rs
          Lens.^? listRecoveryPointsByLegalHoldResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListRecoveryPointsByLegalHold
  where
  type
    AWSResponse ListRecoveryPointsByLegalHold =
      ListRecoveryPointsByLegalHoldResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRecoveryPointsByLegalHoldResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "RecoveryPoints" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListRecoveryPointsByLegalHold
  where
  hashWithSalt _salt ListRecoveryPointsByLegalHold' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` legalHoldId

instance Prelude.NFData ListRecoveryPointsByLegalHold where
  rnf ListRecoveryPointsByLegalHold' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf legalHoldId

instance Data.ToHeaders ListRecoveryPointsByLegalHold where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListRecoveryPointsByLegalHold where
  toPath ListRecoveryPointsByLegalHold' {..} =
    Prelude.mconcat
      [ "/legal-holds/",
        Data.toBS legalHoldId,
        "/recovery-points"
      ]

instance Data.ToQuery ListRecoveryPointsByLegalHold where
  toQuery ListRecoveryPointsByLegalHold' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListRecoveryPointsByLegalHoldResponse' smart constructor.
data ListRecoveryPointsByLegalHoldResponse = ListRecoveryPointsByLegalHoldResponse'
  { -- | This return is the next item following a partial list of returned
    -- resources.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | This is a list of the recovery points returned by
    -- @ListRecoveryPointsByLegalHold@.
    recoveryPoints :: Prelude.Maybe [RecoveryPointMember],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRecoveryPointsByLegalHoldResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRecoveryPointsByLegalHoldResponse_nextToken' - This return is the next item following a partial list of returned
-- resources.
--
-- 'recoveryPoints', 'listRecoveryPointsByLegalHoldResponse_recoveryPoints' - This is a list of the recovery points returned by
-- @ListRecoveryPointsByLegalHold@.
--
-- 'httpStatus', 'listRecoveryPointsByLegalHoldResponse_httpStatus' - The response's http status code.
newListRecoveryPointsByLegalHoldResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRecoveryPointsByLegalHoldResponse
newListRecoveryPointsByLegalHoldResponse pHttpStatus_ =
  ListRecoveryPointsByLegalHoldResponse'
    { nextToken =
        Prelude.Nothing,
      recoveryPoints = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | This return is the next item following a partial list of returned
-- resources.
listRecoveryPointsByLegalHoldResponse_nextToken :: Lens.Lens' ListRecoveryPointsByLegalHoldResponse (Prelude.Maybe Prelude.Text)
listRecoveryPointsByLegalHoldResponse_nextToken = Lens.lens (\ListRecoveryPointsByLegalHoldResponse' {nextToken} -> nextToken) (\s@ListRecoveryPointsByLegalHoldResponse' {} a -> s {nextToken = a} :: ListRecoveryPointsByLegalHoldResponse)

-- | This is a list of the recovery points returned by
-- @ListRecoveryPointsByLegalHold@.
listRecoveryPointsByLegalHoldResponse_recoveryPoints :: Lens.Lens' ListRecoveryPointsByLegalHoldResponse (Prelude.Maybe [RecoveryPointMember])
listRecoveryPointsByLegalHoldResponse_recoveryPoints = Lens.lens (\ListRecoveryPointsByLegalHoldResponse' {recoveryPoints} -> recoveryPoints) (\s@ListRecoveryPointsByLegalHoldResponse' {} a -> s {recoveryPoints = a} :: ListRecoveryPointsByLegalHoldResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRecoveryPointsByLegalHoldResponse_httpStatus :: Lens.Lens' ListRecoveryPointsByLegalHoldResponse Prelude.Int
listRecoveryPointsByLegalHoldResponse_httpStatus = Lens.lens (\ListRecoveryPointsByLegalHoldResponse' {httpStatus} -> httpStatus) (\s@ListRecoveryPointsByLegalHoldResponse' {} a -> s {httpStatus = a} :: ListRecoveryPointsByLegalHoldResponse)

instance
  Prelude.NFData
    ListRecoveryPointsByLegalHoldResponse
  where
  rnf ListRecoveryPointsByLegalHoldResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf recoveryPoints
      `Prelude.seq` Prelude.rnf httpStatus
