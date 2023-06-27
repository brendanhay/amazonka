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
-- Module      : Amazonka.WellArchitected.ListProfileShares
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List profile shares.
module Amazonka.WellArchitected.ListProfileShares
  ( -- * Creating a Request
    ListProfileShares (..),
    newListProfileShares,

    -- * Request Lenses
    listProfileShares_maxResults,
    listProfileShares_nextToken,
    listProfileShares_sharedWithPrefix,
    listProfileShares_status,
    listProfileShares_profileArn,

    -- * Destructuring the Response
    ListProfileSharesResponse (..),
    newListProfileSharesResponse,

    -- * Response Lenses
    listProfileSharesResponse_nextToken,
    listProfileSharesResponse_profileShareSummaries,
    listProfileSharesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | /See:/ 'newListProfileShares' smart constructor.
data ListProfileShares = ListProfileShares'
  { -- | The maximum number of results to return for this request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID, IAM role, organization ID, or
    -- organizational unit (OU) ID with which the profile is shared.
    sharedWithPrefix :: Prelude.Maybe Prelude.Text,
    status :: Prelude.Maybe ShareStatus,
    -- | The profile ARN.
    profileArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProfileShares' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listProfileShares_maxResults' - The maximum number of results to return for this request.
--
-- 'nextToken', 'listProfileShares_nextToken' - Undocumented member.
--
-- 'sharedWithPrefix', 'listProfileShares_sharedWithPrefix' - The Amazon Web Services account ID, IAM role, organization ID, or
-- organizational unit (OU) ID with which the profile is shared.
--
-- 'status', 'listProfileShares_status' - Undocumented member.
--
-- 'profileArn', 'listProfileShares_profileArn' - The profile ARN.
newListProfileShares ::
  -- | 'profileArn'
  Prelude.Text ->
  ListProfileShares
newListProfileShares pProfileArn_ =
  ListProfileShares'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sharedWithPrefix = Prelude.Nothing,
      status = Prelude.Nothing,
      profileArn = pProfileArn_
    }

-- | The maximum number of results to return for this request.
listProfileShares_maxResults :: Lens.Lens' ListProfileShares (Prelude.Maybe Prelude.Natural)
listProfileShares_maxResults = Lens.lens (\ListProfileShares' {maxResults} -> maxResults) (\s@ListProfileShares' {} a -> s {maxResults = a} :: ListProfileShares)

-- | Undocumented member.
listProfileShares_nextToken :: Lens.Lens' ListProfileShares (Prelude.Maybe Prelude.Text)
listProfileShares_nextToken = Lens.lens (\ListProfileShares' {nextToken} -> nextToken) (\s@ListProfileShares' {} a -> s {nextToken = a} :: ListProfileShares)

-- | The Amazon Web Services account ID, IAM role, organization ID, or
-- organizational unit (OU) ID with which the profile is shared.
listProfileShares_sharedWithPrefix :: Lens.Lens' ListProfileShares (Prelude.Maybe Prelude.Text)
listProfileShares_sharedWithPrefix = Lens.lens (\ListProfileShares' {sharedWithPrefix} -> sharedWithPrefix) (\s@ListProfileShares' {} a -> s {sharedWithPrefix = a} :: ListProfileShares)

-- | Undocumented member.
listProfileShares_status :: Lens.Lens' ListProfileShares (Prelude.Maybe ShareStatus)
listProfileShares_status = Lens.lens (\ListProfileShares' {status} -> status) (\s@ListProfileShares' {} a -> s {status = a} :: ListProfileShares)

-- | The profile ARN.
listProfileShares_profileArn :: Lens.Lens' ListProfileShares Prelude.Text
listProfileShares_profileArn = Lens.lens (\ListProfileShares' {profileArn} -> profileArn) (\s@ListProfileShares' {} a -> s {profileArn = a} :: ListProfileShares)

instance Core.AWSRequest ListProfileShares where
  type
    AWSResponse ListProfileShares =
      ListProfileSharesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProfileSharesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "ProfileShareSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListProfileShares where
  hashWithSalt _salt ListProfileShares' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sharedWithPrefix
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` profileArn

instance Prelude.NFData ListProfileShares where
  rnf ListProfileShares' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sharedWithPrefix
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf profileArn

instance Data.ToHeaders ListProfileShares where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListProfileShares where
  toPath ListProfileShares' {..} =
    Prelude.mconcat
      ["/profiles/", Data.toBS profileArn, "/shares"]

instance Data.ToQuery ListProfileShares where
  toQuery ListProfileShares' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "SharedWithPrefix" Data.=: sharedWithPrefix,
        "Status" Data.=: status
      ]

-- | /See:/ 'newListProfileSharesResponse' smart constructor.
data ListProfileSharesResponse = ListProfileSharesResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | Profile share summaries.
    profileShareSummaries :: Prelude.Maybe [ProfileShareSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProfileSharesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProfileSharesResponse_nextToken' - Undocumented member.
--
-- 'profileShareSummaries', 'listProfileSharesResponse_profileShareSummaries' - Profile share summaries.
--
-- 'httpStatus', 'listProfileSharesResponse_httpStatus' - The response's http status code.
newListProfileSharesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProfileSharesResponse
newListProfileSharesResponse pHttpStatus_ =
  ListProfileSharesResponse'
    { nextToken =
        Prelude.Nothing,
      profileShareSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listProfileSharesResponse_nextToken :: Lens.Lens' ListProfileSharesResponse (Prelude.Maybe Prelude.Text)
listProfileSharesResponse_nextToken = Lens.lens (\ListProfileSharesResponse' {nextToken} -> nextToken) (\s@ListProfileSharesResponse' {} a -> s {nextToken = a} :: ListProfileSharesResponse)

-- | Profile share summaries.
listProfileSharesResponse_profileShareSummaries :: Lens.Lens' ListProfileSharesResponse (Prelude.Maybe [ProfileShareSummary])
listProfileSharesResponse_profileShareSummaries = Lens.lens (\ListProfileSharesResponse' {profileShareSummaries} -> profileShareSummaries) (\s@ListProfileSharesResponse' {} a -> s {profileShareSummaries = a} :: ListProfileSharesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listProfileSharesResponse_httpStatus :: Lens.Lens' ListProfileSharesResponse Prelude.Int
listProfileSharesResponse_httpStatus = Lens.lens (\ListProfileSharesResponse' {httpStatus} -> httpStatus) (\s@ListProfileSharesResponse' {} a -> s {httpStatus = a} :: ListProfileSharesResponse)

instance Prelude.NFData ListProfileSharesResponse where
  rnf ListProfileSharesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf profileShareSummaries
      `Prelude.seq` Prelude.rnf httpStatus
