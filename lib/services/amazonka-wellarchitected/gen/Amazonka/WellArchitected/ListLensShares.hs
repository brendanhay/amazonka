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
-- Module      : Amazonka.WellArchitected.ListLensShares
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the lens shares associated with the lens.
module Amazonka.WellArchitected.ListLensShares
  ( -- * Creating a Request
    ListLensShares (..),
    newListLensShares,

    -- * Request Lenses
    listLensShares_maxResults,
    listLensShares_nextToken,
    listLensShares_sharedWithPrefix,
    listLensShares_status,
    listLensShares_lensAlias,

    -- * Destructuring the Response
    ListLensSharesResponse (..),
    newListLensSharesResponse,

    -- * Response Lenses
    listLensSharesResponse_lensShareSummaries,
    listLensSharesResponse_nextToken,
    listLensSharesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | /See:/ 'newListLensShares' smart constructor.
data ListLensShares = ListLensShares'
  { -- | The maximum number of results to return for this request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID, IAM role, organization ID, or
    -- organizational unit (OU) ID with which the lens is shared.
    sharedWithPrefix :: Prelude.Maybe Prelude.Text,
    status :: Prelude.Maybe ShareStatus,
    lensAlias :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLensShares' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listLensShares_maxResults' - The maximum number of results to return for this request.
--
-- 'nextToken', 'listLensShares_nextToken' - Undocumented member.
--
-- 'sharedWithPrefix', 'listLensShares_sharedWithPrefix' - The Amazon Web Services account ID, IAM role, organization ID, or
-- organizational unit (OU) ID with which the lens is shared.
--
-- 'status', 'listLensShares_status' - Undocumented member.
--
-- 'lensAlias', 'listLensShares_lensAlias' - Undocumented member.
newListLensShares ::
  -- | 'lensAlias'
  Prelude.Text ->
  ListLensShares
newListLensShares pLensAlias_ =
  ListLensShares'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sharedWithPrefix = Prelude.Nothing,
      status = Prelude.Nothing,
      lensAlias = pLensAlias_
    }

-- | The maximum number of results to return for this request.
listLensShares_maxResults :: Lens.Lens' ListLensShares (Prelude.Maybe Prelude.Natural)
listLensShares_maxResults = Lens.lens (\ListLensShares' {maxResults} -> maxResults) (\s@ListLensShares' {} a -> s {maxResults = a} :: ListLensShares)

-- | Undocumented member.
listLensShares_nextToken :: Lens.Lens' ListLensShares (Prelude.Maybe Prelude.Text)
listLensShares_nextToken = Lens.lens (\ListLensShares' {nextToken} -> nextToken) (\s@ListLensShares' {} a -> s {nextToken = a} :: ListLensShares)

-- | The Amazon Web Services account ID, IAM role, organization ID, or
-- organizational unit (OU) ID with which the lens is shared.
listLensShares_sharedWithPrefix :: Lens.Lens' ListLensShares (Prelude.Maybe Prelude.Text)
listLensShares_sharedWithPrefix = Lens.lens (\ListLensShares' {sharedWithPrefix} -> sharedWithPrefix) (\s@ListLensShares' {} a -> s {sharedWithPrefix = a} :: ListLensShares)

-- | Undocumented member.
listLensShares_status :: Lens.Lens' ListLensShares (Prelude.Maybe ShareStatus)
listLensShares_status = Lens.lens (\ListLensShares' {status} -> status) (\s@ListLensShares' {} a -> s {status = a} :: ListLensShares)

-- | Undocumented member.
listLensShares_lensAlias :: Lens.Lens' ListLensShares Prelude.Text
listLensShares_lensAlias = Lens.lens (\ListLensShares' {lensAlias} -> lensAlias) (\s@ListLensShares' {} a -> s {lensAlias = a} :: ListLensShares)

instance Core.AWSRequest ListLensShares where
  type
    AWSResponse ListLensShares =
      ListLensSharesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLensSharesResponse'
            Prelude.<$> ( x
                            Data..?> "LensShareSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLensShares where
  hashWithSalt _salt ListLensShares' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sharedWithPrefix
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` lensAlias

instance Prelude.NFData ListLensShares where
  rnf ListLensShares' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf sharedWithPrefix `Prelude.seq`
          Prelude.rnf status `Prelude.seq`
            Prelude.rnf lensAlias

instance Data.ToHeaders ListLensShares where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListLensShares where
  toPath ListLensShares' {..} =
    Prelude.mconcat
      ["/lenses/", Data.toBS lensAlias, "/shares"]

instance Data.ToQuery ListLensShares where
  toQuery ListLensShares' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "SharedWithPrefix" Data.=: sharedWithPrefix,
        "Status" Data.=: status
      ]

-- | /See:/ 'newListLensSharesResponse' smart constructor.
data ListLensSharesResponse = ListLensSharesResponse'
  { -- | A list of lens share summaries.
    lensShareSummaries :: Prelude.Maybe [LensShareSummary],
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLensSharesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lensShareSummaries', 'listLensSharesResponse_lensShareSummaries' - A list of lens share summaries.
--
-- 'nextToken', 'listLensSharesResponse_nextToken' - Undocumented member.
--
-- 'httpStatus', 'listLensSharesResponse_httpStatus' - The response's http status code.
newListLensSharesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLensSharesResponse
newListLensSharesResponse pHttpStatus_ =
  ListLensSharesResponse'
    { lensShareSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of lens share summaries.
listLensSharesResponse_lensShareSummaries :: Lens.Lens' ListLensSharesResponse (Prelude.Maybe [LensShareSummary])
listLensSharesResponse_lensShareSummaries = Lens.lens (\ListLensSharesResponse' {lensShareSummaries} -> lensShareSummaries) (\s@ListLensSharesResponse' {} a -> s {lensShareSummaries = a} :: ListLensSharesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
listLensSharesResponse_nextToken :: Lens.Lens' ListLensSharesResponse (Prelude.Maybe Prelude.Text)
listLensSharesResponse_nextToken = Lens.lens (\ListLensSharesResponse' {nextToken} -> nextToken) (\s@ListLensSharesResponse' {} a -> s {nextToken = a} :: ListLensSharesResponse)

-- | The response's http status code.
listLensSharesResponse_httpStatus :: Lens.Lens' ListLensSharesResponse Prelude.Int
listLensSharesResponse_httpStatus = Lens.lens (\ListLensSharesResponse' {httpStatus} -> httpStatus) (\s@ListLensSharesResponse' {} a -> s {httpStatus = a} :: ListLensSharesResponse)

instance Prelude.NFData ListLensSharesResponse where
  rnf ListLensSharesResponse' {..} =
    Prelude.rnf lensShareSummaries `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
