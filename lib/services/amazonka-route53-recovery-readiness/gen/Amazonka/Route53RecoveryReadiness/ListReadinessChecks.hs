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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the readiness checks for an account.
--
-- This operation returns paginated results.
module Amazonka.Route53RecoveryReadiness.ListReadinessChecks
  ( -- * Creating a Request
    ListReadinessChecks (..),
    newListReadinessChecks,

    -- * Request Lenses
    listReadinessChecks_maxResults,
    listReadinessChecks_nextToken,

    -- * Destructuring the Response
    ListReadinessChecksResponse (..),
    newListReadinessChecksResponse,

    -- * Response Lenses
    listReadinessChecksResponse_nextToken,
    listReadinessChecksResponse_readinessChecks,
    listReadinessChecksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | /See:/ 'newListReadinessChecks' smart constructor.
data ListReadinessChecks = ListReadinessChecks'
  { -- | The number of objects that you want to return with this call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token that identifies which batch of results you want to see.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'maxResults', 'listReadinessChecks_maxResults' - The number of objects that you want to return with this call.
--
-- 'nextToken', 'listReadinessChecks_nextToken' - The token that identifies which batch of results you want to see.
newListReadinessChecks ::
  ListReadinessChecks
newListReadinessChecks =
  ListReadinessChecks'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The number of objects that you want to return with this call.
listReadinessChecks_maxResults :: Lens.Lens' ListReadinessChecks (Prelude.Maybe Prelude.Natural)
listReadinessChecks_maxResults = Lens.lens (\ListReadinessChecks' {maxResults} -> maxResults) (\s@ListReadinessChecks' {} a -> s {maxResults = a} :: ListReadinessChecks)

-- | The token that identifies which batch of results you want to see.
listReadinessChecks_nextToken :: Lens.Lens' ListReadinessChecks (Prelude.Maybe Prelude.Text)
listReadinessChecks_nextToken = Lens.lens (\ListReadinessChecks' {nextToken} -> nextToken) (\s@ListReadinessChecks' {} a -> s {nextToken = a} :: ListReadinessChecks)

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReadinessChecksResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x
                            Data..?> "readinessChecks"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListReadinessChecks where
  hashWithSalt _salt ListReadinessChecks' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListReadinessChecks where
  rnf ListReadinessChecks' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken

instance Data.ToHeaders ListReadinessChecks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListReadinessChecks where
  toPath = Prelude.const "/readinesschecks"

instance Data.ToQuery ListReadinessChecks where
  toQuery ListReadinessChecks' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListReadinessChecksResponse' smart constructor.
data ListReadinessChecksResponse = ListReadinessChecksResponse'
  { -- | The token that identifies which batch of results you want to see.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of readiness checks associated with the account.
    readinessChecks :: Prelude.Maybe [ReadinessCheckOutput],
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
-- 'nextToken', 'listReadinessChecksResponse_nextToken' - The token that identifies which batch of results you want to see.
--
-- 'readinessChecks', 'listReadinessChecksResponse_readinessChecks' - A list of readiness checks associated with the account.
--
-- 'httpStatus', 'listReadinessChecksResponse_httpStatus' - The response's http status code.
newListReadinessChecksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListReadinessChecksResponse
newListReadinessChecksResponse pHttpStatus_ =
  ListReadinessChecksResponse'
    { nextToken =
        Prelude.Nothing,
      readinessChecks = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token that identifies which batch of results you want to see.
listReadinessChecksResponse_nextToken :: Lens.Lens' ListReadinessChecksResponse (Prelude.Maybe Prelude.Text)
listReadinessChecksResponse_nextToken = Lens.lens (\ListReadinessChecksResponse' {nextToken} -> nextToken) (\s@ListReadinessChecksResponse' {} a -> s {nextToken = a} :: ListReadinessChecksResponse)

-- | A list of readiness checks associated with the account.
listReadinessChecksResponse_readinessChecks :: Lens.Lens' ListReadinessChecksResponse (Prelude.Maybe [ReadinessCheckOutput])
listReadinessChecksResponse_readinessChecks = Lens.lens (\ListReadinessChecksResponse' {readinessChecks} -> readinessChecks) (\s@ListReadinessChecksResponse' {} a -> s {readinessChecks = a} :: ListReadinessChecksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listReadinessChecksResponse_httpStatus :: Lens.Lens' ListReadinessChecksResponse Prelude.Int
listReadinessChecksResponse_httpStatus = Lens.lens (\ListReadinessChecksResponse' {httpStatus} -> httpStatus) (\s@ListReadinessChecksResponse' {} a -> s {httpStatus = a} :: ListReadinessChecksResponse)

instance Prelude.NFData ListReadinessChecksResponse where
  rnf ListReadinessChecksResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf readinessChecks `Prelude.seq`
        Prelude.rnf httpStatus
