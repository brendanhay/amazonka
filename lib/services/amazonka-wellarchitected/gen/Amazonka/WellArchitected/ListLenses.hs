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
-- Module      : Amazonka.WellArchitected.ListLenses
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the available lenses.
module Amazonka.WellArchitected.ListLenses
  ( -- * Creating a Request
    ListLenses (..),
    newListLenses,

    -- * Request Lenses
    listLenses_nextToken,
    listLenses_lensName,
    listLenses_maxResults,
    listLenses_lensType,
    listLenses_lensStatus,

    -- * Destructuring the Response
    ListLensesResponse (..),
    newListLensesResponse,

    -- * Response Lenses
    listLensesResponse_lensSummaries,
    listLensesResponse_nextToken,
    listLensesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | Input to list lenses.
--
-- /See:/ 'newListLenses' smart constructor.
data ListLenses = ListLenses'
  { nextToken :: Prelude.Maybe Prelude.Text,
    lensName :: Prelude.Maybe Prelude.Text,
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The type of lenses to be returned.
    lensType :: Prelude.Maybe LensType,
    -- | The status of lenses to be returned.
    lensStatus :: Prelude.Maybe LensStatusType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLenses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listLenses_nextToken' - Undocumented member.
--
-- 'lensName', 'listLenses_lensName' - Undocumented member.
--
-- 'maxResults', 'listLenses_maxResults' - Undocumented member.
--
-- 'lensType', 'listLenses_lensType' - The type of lenses to be returned.
--
-- 'lensStatus', 'listLenses_lensStatus' - The status of lenses to be returned.
newListLenses ::
  ListLenses
newListLenses =
  ListLenses'
    { nextToken = Prelude.Nothing,
      lensName = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      lensType = Prelude.Nothing,
      lensStatus = Prelude.Nothing
    }

-- | Undocumented member.
listLenses_nextToken :: Lens.Lens' ListLenses (Prelude.Maybe Prelude.Text)
listLenses_nextToken = Lens.lens (\ListLenses' {nextToken} -> nextToken) (\s@ListLenses' {} a -> s {nextToken = a} :: ListLenses)

-- | Undocumented member.
listLenses_lensName :: Lens.Lens' ListLenses (Prelude.Maybe Prelude.Text)
listLenses_lensName = Lens.lens (\ListLenses' {lensName} -> lensName) (\s@ListLenses' {} a -> s {lensName = a} :: ListLenses)

-- | Undocumented member.
listLenses_maxResults :: Lens.Lens' ListLenses (Prelude.Maybe Prelude.Natural)
listLenses_maxResults = Lens.lens (\ListLenses' {maxResults} -> maxResults) (\s@ListLenses' {} a -> s {maxResults = a} :: ListLenses)

-- | The type of lenses to be returned.
listLenses_lensType :: Lens.Lens' ListLenses (Prelude.Maybe LensType)
listLenses_lensType = Lens.lens (\ListLenses' {lensType} -> lensType) (\s@ListLenses' {} a -> s {lensType = a} :: ListLenses)

-- | The status of lenses to be returned.
listLenses_lensStatus :: Lens.Lens' ListLenses (Prelude.Maybe LensStatusType)
listLenses_lensStatus = Lens.lens (\ListLenses' {lensStatus} -> lensStatus) (\s@ListLenses' {} a -> s {lensStatus = a} :: ListLenses)

instance Core.AWSRequest ListLenses where
  type AWSResponse ListLenses = ListLensesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLensesResponse'
            Prelude.<$> (x Core..?> "LensSummaries" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLenses where
  hashWithSalt _salt ListLenses' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` lensName
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` lensType
      `Prelude.hashWithSalt` lensStatus

instance Prelude.NFData ListLenses where
  rnf ListLenses' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf lensName
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf lensType
      `Prelude.seq` Prelude.rnf lensStatus

instance Core.ToHeaders ListLenses where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListLenses where
  toPath = Prelude.const "/lenses"

instance Core.ToQuery ListLenses where
  toQuery ListLenses' {..} =
    Prelude.mconcat
      [ "NextToken" Core.=: nextToken,
        "LensName" Core.=: lensName,
        "MaxResults" Core.=: maxResults,
        "LensType" Core.=: lensType,
        "LensStatus" Core.=: lensStatus
      ]

-- | Output of a list lenses call.
--
-- /See:/ 'newListLensesResponse' smart constructor.
data ListLensesResponse = ListLensesResponse'
  { lensSummaries :: Prelude.Maybe [LensSummary],
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLensesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lensSummaries', 'listLensesResponse_lensSummaries' - Undocumented member.
--
-- 'nextToken', 'listLensesResponse_nextToken' - Undocumented member.
--
-- 'httpStatus', 'listLensesResponse_httpStatus' - The response's http status code.
newListLensesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLensesResponse
newListLensesResponse pHttpStatus_ =
  ListLensesResponse'
    { lensSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listLensesResponse_lensSummaries :: Lens.Lens' ListLensesResponse (Prelude.Maybe [LensSummary])
listLensesResponse_lensSummaries = Lens.lens (\ListLensesResponse' {lensSummaries} -> lensSummaries) (\s@ListLensesResponse' {} a -> s {lensSummaries = a} :: ListLensesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
listLensesResponse_nextToken :: Lens.Lens' ListLensesResponse (Prelude.Maybe Prelude.Text)
listLensesResponse_nextToken = Lens.lens (\ListLensesResponse' {nextToken} -> nextToken) (\s@ListLensesResponse' {} a -> s {nextToken = a} :: ListLensesResponse)

-- | The response's http status code.
listLensesResponse_httpStatus :: Lens.Lens' ListLensesResponse Prelude.Int
listLensesResponse_httpStatus = Lens.lens (\ListLensesResponse' {httpStatus} -> httpStatus) (\s@ListLensesResponse' {} a -> s {httpStatus = a} :: ListLensesResponse)

instance Prelude.NFData ListLensesResponse where
  rnf ListLensesResponse' {..} =
    Prelude.rnf lensSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
