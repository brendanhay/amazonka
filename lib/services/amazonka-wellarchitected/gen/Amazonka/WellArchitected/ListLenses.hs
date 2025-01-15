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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the available lenses.
module Amazonka.WellArchitected.ListLenses
  ( -- * Creating a Request
    ListLenses (..),
    newListLenses,

    -- * Request Lenses
    listLenses_lensName,
    listLenses_lensStatus,
    listLenses_lensType,
    listLenses_maxResults,
    listLenses_nextToken,

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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | Input to list lenses.
--
-- /See:/ 'newListLenses' smart constructor.
data ListLenses = ListLenses'
  { lensName :: Prelude.Maybe Prelude.Text,
    -- | The status of lenses to be returned.
    lensStatus :: Prelude.Maybe LensStatusType,
    -- | The type of lenses to be returned.
    lensType :: Prelude.Maybe LensType,
    maxResults :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'lensName', 'listLenses_lensName' - Undocumented member.
--
-- 'lensStatus', 'listLenses_lensStatus' - The status of lenses to be returned.
--
-- 'lensType', 'listLenses_lensType' - The type of lenses to be returned.
--
-- 'maxResults', 'listLenses_maxResults' - Undocumented member.
--
-- 'nextToken', 'listLenses_nextToken' - Undocumented member.
newListLenses ::
  ListLenses
newListLenses =
  ListLenses'
    { lensName = Prelude.Nothing,
      lensStatus = Prelude.Nothing,
      lensType = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Undocumented member.
listLenses_lensName :: Lens.Lens' ListLenses (Prelude.Maybe Prelude.Text)
listLenses_lensName = Lens.lens (\ListLenses' {lensName} -> lensName) (\s@ListLenses' {} a -> s {lensName = a} :: ListLenses)

-- | The status of lenses to be returned.
listLenses_lensStatus :: Lens.Lens' ListLenses (Prelude.Maybe LensStatusType)
listLenses_lensStatus = Lens.lens (\ListLenses' {lensStatus} -> lensStatus) (\s@ListLenses' {} a -> s {lensStatus = a} :: ListLenses)

-- | The type of lenses to be returned.
listLenses_lensType :: Lens.Lens' ListLenses (Prelude.Maybe LensType)
listLenses_lensType = Lens.lens (\ListLenses' {lensType} -> lensType) (\s@ListLenses' {} a -> s {lensType = a} :: ListLenses)

-- | Undocumented member.
listLenses_maxResults :: Lens.Lens' ListLenses (Prelude.Maybe Prelude.Natural)
listLenses_maxResults = Lens.lens (\ListLenses' {maxResults} -> maxResults) (\s@ListLenses' {} a -> s {maxResults = a} :: ListLenses)

-- | Undocumented member.
listLenses_nextToken :: Lens.Lens' ListLenses (Prelude.Maybe Prelude.Text)
listLenses_nextToken = Lens.lens (\ListLenses' {nextToken} -> nextToken) (\s@ListLenses' {} a -> s {nextToken = a} :: ListLenses)

instance Core.AWSRequest ListLenses where
  type AWSResponse ListLenses = ListLensesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLensesResponse'
            Prelude.<$> (x Data..?> "LensSummaries" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLenses where
  hashWithSalt _salt ListLenses' {..} =
    _salt
      `Prelude.hashWithSalt` lensName
      `Prelude.hashWithSalt` lensStatus
      `Prelude.hashWithSalt` lensType
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListLenses where
  rnf ListLenses' {..} =
    Prelude.rnf lensName `Prelude.seq`
      Prelude.rnf lensStatus `Prelude.seq`
        Prelude.rnf lensType `Prelude.seq`
          Prelude.rnf maxResults `Prelude.seq`
            Prelude.rnf nextToken

instance Data.ToHeaders ListLenses where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListLenses where
  toPath = Prelude.const "/lenses"

instance Data.ToQuery ListLenses where
  toQuery ListLenses' {..} =
    Prelude.mconcat
      [ "LensName" Data.=: lensName,
        "LensStatus" Data.=: lensStatus,
        "LensType" Data.=: lensType,
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
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
    Prelude.rnf lensSummaries `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
