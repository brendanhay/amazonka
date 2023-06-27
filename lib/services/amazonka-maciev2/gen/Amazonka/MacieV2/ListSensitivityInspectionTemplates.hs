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
-- Module      : Amazonka.MacieV2.ListSensitivityInspectionTemplates
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a subset of information about the sensitivity inspection
-- template for an account.
--
-- This operation returns paginated results.
module Amazonka.MacieV2.ListSensitivityInspectionTemplates
  ( -- * Creating a Request
    ListSensitivityInspectionTemplates (..),
    newListSensitivityInspectionTemplates,

    -- * Request Lenses
    listSensitivityInspectionTemplates_maxResults,
    listSensitivityInspectionTemplates_nextToken,

    -- * Destructuring the Response
    ListSensitivityInspectionTemplatesResponse (..),
    newListSensitivityInspectionTemplatesResponse,

    -- * Response Lenses
    listSensitivityInspectionTemplatesResponse_nextToken,
    listSensitivityInspectionTemplatesResponse_sensitivityInspectionTemplates,
    listSensitivityInspectionTemplatesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSensitivityInspectionTemplates' smart constructor.
data ListSensitivityInspectionTemplates = ListSensitivityInspectionTemplates'
  { -- | The maximum number of items to include in each page of a paginated
    -- response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The nextToken string that specifies which page of results to return in a
    -- paginated response.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSensitivityInspectionTemplates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSensitivityInspectionTemplates_maxResults' - The maximum number of items to include in each page of a paginated
-- response.
--
-- 'nextToken', 'listSensitivityInspectionTemplates_nextToken' - The nextToken string that specifies which page of results to return in a
-- paginated response.
newListSensitivityInspectionTemplates ::
  ListSensitivityInspectionTemplates
newListSensitivityInspectionTemplates =
  ListSensitivityInspectionTemplates'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of items to include in each page of a paginated
-- response.
listSensitivityInspectionTemplates_maxResults :: Lens.Lens' ListSensitivityInspectionTemplates (Prelude.Maybe Prelude.Natural)
listSensitivityInspectionTemplates_maxResults = Lens.lens (\ListSensitivityInspectionTemplates' {maxResults} -> maxResults) (\s@ListSensitivityInspectionTemplates' {} a -> s {maxResults = a} :: ListSensitivityInspectionTemplates)

-- | The nextToken string that specifies which page of results to return in a
-- paginated response.
listSensitivityInspectionTemplates_nextToken :: Lens.Lens' ListSensitivityInspectionTemplates (Prelude.Maybe Prelude.Text)
listSensitivityInspectionTemplates_nextToken = Lens.lens (\ListSensitivityInspectionTemplates' {nextToken} -> nextToken) (\s@ListSensitivityInspectionTemplates' {} a -> s {nextToken = a} :: ListSensitivityInspectionTemplates)

instance
  Core.AWSPager
    ListSensitivityInspectionTemplates
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSensitivityInspectionTemplatesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSensitivityInspectionTemplatesResponse_sensitivityInspectionTemplates
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listSensitivityInspectionTemplates_nextToken
          Lens..~ rs
          Lens.^? listSensitivityInspectionTemplatesResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListSensitivityInspectionTemplates
  where
  type
    AWSResponse ListSensitivityInspectionTemplates =
      ListSensitivityInspectionTemplatesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSensitivityInspectionTemplatesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x
                            Data..?> "sensitivityInspectionTemplates"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListSensitivityInspectionTemplates
  where
  hashWithSalt
    _salt
    ListSensitivityInspectionTemplates' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    ListSensitivityInspectionTemplates
  where
  rnf ListSensitivityInspectionTemplates' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance
  Data.ToHeaders
    ListSensitivityInspectionTemplates
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToPath
    ListSensitivityInspectionTemplates
  where
  toPath =
    Prelude.const "/templates/sensitivity-inspections"

instance
  Data.ToQuery
    ListSensitivityInspectionTemplates
  where
  toQuery ListSensitivityInspectionTemplates' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListSensitivityInspectionTemplatesResponse' smart constructor.
data ListSensitivityInspectionTemplatesResponse = ListSensitivityInspectionTemplatesResponse'
  { -- | The string to use in a subsequent request to get the next page of
    -- results in a paginated response. This value is null if there are no
    -- additional pages.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array that specifies the unique identifier and name of the
    -- sensitivity inspection template for the account.
    sensitivityInspectionTemplates :: Prelude.Maybe [SensitivityInspectionTemplatesEntry],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSensitivityInspectionTemplatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSensitivityInspectionTemplatesResponse_nextToken' - The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
--
-- 'sensitivityInspectionTemplates', 'listSensitivityInspectionTemplatesResponse_sensitivityInspectionTemplates' - An array that specifies the unique identifier and name of the
-- sensitivity inspection template for the account.
--
-- 'httpStatus', 'listSensitivityInspectionTemplatesResponse_httpStatus' - The response's http status code.
newListSensitivityInspectionTemplatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSensitivityInspectionTemplatesResponse
newListSensitivityInspectionTemplatesResponse
  pHttpStatus_ =
    ListSensitivityInspectionTemplatesResponse'
      { nextToken =
          Prelude.Nothing,
        sensitivityInspectionTemplates =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
listSensitivityInspectionTemplatesResponse_nextToken :: Lens.Lens' ListSensitivityInspectionTemplatesResponse (Prelude.Maybe Prelude.Text)
listSensitivityInspectionTemplatesResponse_nextToken = Lens.lens (\ListSensitivityInspectionTemplatesResponse' {nextToken} -> nextToken) (\s@ListSensitivityInspectionTemplatesResponse' {} a -> s {nextToken = a} :: ListSensitivityInspectionTemplatesResponse)

-- | An array that specifies the unique identifier and name of the
-- sensitivity inspection template for the account.
listSensitivityInspectionTemplatesResponse_sensitivityInspectionTemplates :: Lens.Lens' ListSensitivityInspectionTemplatesResponse (Prelude.Maybe [SensitivityInspectionTemplatesEntry])
listSensitivityInspectionTemplatesResponse_sensitivityInspectionTemplates = Lens.lens (\ListSensitivityInspectionTemplatesResponse' {sensitivityInspectionTemplates} -> sensitivityInspectionTemplates) (\s@ListSensitivityInspectionTemplatesResponse' {} a -> s {sensitivityInspectionTemplates = a} :: ListSensitivityInspectionTemplatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSensitivityInspectionTemplatesResponse_httpStatus :: Lens.Lens' ListSensitivityInspectionTemplatesResponse Prelude.Int
listSensitivityInspectionTemplatesResponse_httpStatus = Lens.lens (\ListSensitivityInspectionTemplatesResponse' {httpStatus} -> httpStatus) (\s@ListSensitivityInspectionTemplatesResponse' {} a -> s {httpStatus = a} :: ListSensitivityInspectionTemplatesResponse)

instance
  Prelude.NFData
    ListSensitivityInspectionTemplatesResponse
  where
  rnf ListSensitivityInspectionTemplatesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sensitivityInspectionTemplates
      `Prelude.seq` Prelude.rnf httpStatus
