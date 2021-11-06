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
-- Module      : Amazonka.Route53RecoveryReadiness.ListReadinessChecks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a collection of Readiness Checks.
--
-- This operation returns paginated results.
module Amazonka.Route53RecoveryReadiness.ListReadinessChecks
  ( -- * Creating a Request
    ListReadinessChecks (..),
    newListReadinessChecks,

    -- * Request Lenses
    listReadinessChecks_nextToken,
    listReadinessChecks_maxResults,

    -- * Destructuring the Response
    ListReadinessChecksResponse (..),
    newListReadinessChecksResponse,

    -- * Response Lenses
    listReadinessChecksResponse_readinessChecks,
    listReadinessChecksResponse_nextToken,
    listReadinessChecksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | /See:/ 'newListReadinessChecks' smart constructor.
data ListReadinessChecks = ListReadinessChecks'
  { -- | A token used to resume pagination from the end of a previous request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Upper bound on number of records to return.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReadinessChecks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listReadinessChecks_nextToken' - A token used to resume pagination from the end of a previous request.
--
-- 'maxResults', 'listReadinessChecks_maxResults' - Upper bound on number of records to return.
newListReadinessChecks ::
  ListReadinessChecks
newListReadinessChecks =
  ListReadinessChecks'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A token used to resume pagination from the end of a previous request.
listReadinessChecks_nextToken :: Lens.Lens' ListReadinessChecks (Prelude.Maybe Prelude.Text)
listReadinessChecks_nextToken = Lens.lens (\ListReadinessChecks' {nextToken} -> nextToken) (\s@ListReadinessChecks' {} a -> s {nextToken = a} :: ListReadinessChecks)

-- | Upper bound on number of records to return.
listReadinessChecks_maxResults :: Lens.Lens' ListReadinessChecks (Prelude.Maybe Prelude.Natural)
listReadinessChecks_maxResults = Lens.lens (\ListReadinessChecks' {maxResults} -> maxResults) (\s@ListReadinessChecks' {} a -> s {maxResults = a} :: ListReadinessChecks)

instance Core.AWSPager ListReadinessChecks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listReadinessChecksResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listReadinessChecksResponse_readinessChecks
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listReadinessChecks_nextToken
          Lens..~ rs
          Lens.^? listReadinessChecksResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListReadinessChecks where
  type
    AWSResponse ListReadinessChecks =
      ListReadinessChecksResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReadinessChecksResponse'
            Prelude.<$> ( x Core..?> "readinessChecks"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListReadinessChecks

instance Prelude.NFData ListReadinessChecks

instance Core.ToHeaders ListReadinessChecks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListReadinessChecks where
  toPath = Prelude.const "/readinesschecks"

instance Core.ToQuery ListReadinessChecks where
  toQuery ListReadinessChecks' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListReadinessChecksResponse' smart constructor.
data ListReadinessChecksResponse = ListReadinessChecksResponse'
  { -- | A list of ReadinessCheck associated with the account
    readinessChecks :: Prelude.Maybe [ReadinessCheckOutput],
    -- | A token that can be used to resume pagination from the end of the
    -- collection.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReadinessChecksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'readinessChecks', 'listReadinessChecksResponse_readinessChecks' - A list of ReadinessCheck associated with the account
--
-- 'nextToken', 'listReadinessChecksResponse_nextToken' - A token that can be used to resume pagination from the end of the
-- collection.
--
-- 'httpStatus', 'listReadinessChecksResponse_httpStatus' - The response's http status code.
newListReadinessChecksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListReadinessChecksResponse
newListReadinessChecksResponse pHttpStatus_ =
  ListReadinessChecksResponse'
    { readinessChecks =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of ReadinessCheck associated with the account
listReadinessChecksResponse_readinessChecks :: Lens.Lens' ListReadinessChecksResponse (Prelude.Maybe [ReadinessCheckOutput])
listReadinessChecksResponse_readinessChecks = Lens.lens (\ListReadinessChecksResponse' {readinessChecks} -> readinessChecks) (\s@ListReadinessChecksResponse' {} a -> s {readinessChecks = a} :: ListReadinessChecksResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that can be used to resume pagination from the end of the
-- collection.
listReadinessChecksResponse_nextToken :: Lens.Lens' ListReadinessChecksResponse (Prelude.Maybe Prelude.Text)
listReadinessChecksResponse_nextToken = Lens.lens (\ListReadinessChecksResponse' {nextToken} -> nextToken) (\s@ListReadinessChecksResponse' {} a -> s {nextToken = a} :: ListReadinessChecksResponse)

-- | The response's http status code.
listReadinessChecksResponse_httpStatus :: Lens.Lens' ListReadinessChecksResponse Prelude.Int
listReadinessChecksResponse_httpStatus = Lens.lens (\ListReadinessChecksResponse' {httpStatus} -> httpStatus) (\s@ListReadinessChecksResponse' {} a -> s {httpStatus = a} :: ListReadinessChecksResponse)

instance Prelude.NFData ListReadinessChecksResponse
