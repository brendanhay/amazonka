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
-- Module      : Amazonka.CloudTrail.ListImportFailures
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of failures for the specified import.
--
-- This operation returns paginated results.
module Amazonka.CloudTrail.ListImportFailures
  ( -- * Creating a Request
    ListImportFailures (..),
    newListImportFailures,

    -- * Request Lenses
    listImportFailures_maxResults,
    listImportFailures_nextToken,
    listImportFailures_importId,

    -- * Destructuring the Response
    ListImportFailuresResponse (..),
    newListImportFailuresResponse,

    -- * Response Lenses
    listImportFailuresResponse_failures,
    listImportFailuresResponse_nextToken,
    listImportFailuresResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListImportFailures' smart constructor.
data ListImportFailures = ListImportFailures'
  { -- | The maximum number of failures to display on a single page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token you can use to get the next page of import failures.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the import.
    importId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListImportFailures' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listImportFailures_maxResults' - The maximum number of failures to display on a single page.
--
-- 'nextToken', 'listImportFailures_nextToken' - A token you can use to get the next page of import failures.
--
-- 'importId', 'listImportFailures_importId' - The ID of the import.
newListImportFailures ::
  -- | 'importId'
  Prelude.Text ->
  ListImportFailures
newListImportFailures pImportId_ =
  ListImportFailures'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      importId = pImportId_
    }

-- | The maximum number of failures to display on a single page.
listImportFailures_maxResults :: Lens.Lens' ListImportFailures (Prelude.Maybe Prelude.Natural)
listImportFailures_maxResults = Lens.lens (\ListImportFailures' {maxResults} -> maxResults) (\s@ListImportFailures' {} a -> s {maxResults = a} :: ListImportFailures)

-- | A token you can use to get the next page of import failures.
listImportFailures_nextToken :: Lens.Lens' ListImportFailures (Prelude.Maybe Prelude.Text)
listImportFailures_nextToken = Lens.lens (\ListImportFailures' {nextToken} -> nextToken) (\s@ListImportFailures' {} a -> s {nextToken = a} :: ListImportFailures)

-- | The ID of the import.
listImportFailures_importId :: Lens.Lens' ListImportFailures Prelude.Text
listImportFailures_importId = Lens.lens (\ListImportFailures' {importId} -> importId) (\s@ListImportFailures' {} a -> s {importId = a} :: ListImportFailures)

instance Core.AWSPager ListImportFailures where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listImportFailuresResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listImportFailuresResponse_failures
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listImportFailures_nextToken
          Lens..~ rs
          Lens.^? listImportFailuresResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListImportFailures where
  type
    AWSResponse ListImportFailures =
      ListImportFailuresResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListImportFailuresResponse'
            Prelude.<$> (x Data..?> "Failures" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListImportFailures where
  hashWithSalt _salt ListImportFailures' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` importId

instance Prelude.NFData ListImportFailures where
  rnf ListImportFailures' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf importId

instance Data.ToHeaders ListImportFailures where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.ListImportFailures" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListImportFailures where
  toJSON ListImportFailures' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("ImportId" Data..= importId)
          ]
      )

instance Data.ToPath ListImportFailures where
  toPath = Prelude.const "/"

instance Data.ToQuery ListImportFailures where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListImportFailuresResponse' smart constructor.
data ListImportFailuresResponse = ListImportFailuresResponse'
  { -- | Contains information about the import failures.
    failures :: Prelude.Maybe [ImportFailureListItem],
    -- | A token you can use to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListImportFailuresResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failures', 'listImportFailuresResponse_failures' - Contains information about the import failures.
--
-- 'nextToken', 'listImportFailuresResponse_nextToken' - A token you can use to get the next page of results.
--
-- 'httpStatus', 'listImportFailuresResponse_httpStatus' - The response's http status code.
newListImportFailuresResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListImportFailuresResponse
newListImportFailuresResponse pHttpStatus_ =
  ListImportFailuresResponse'
    { failures =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains information about the import failures.
listImportFailuresResponse_failures :: Lens.Lens' ListImportFailuresResponse (Prelude.Maybe [ImportFailureListItem])
listImportFailuresResponse_failures = Lens.lens (\ListImportFailuresResponse' {failures} -> failures) (\s@ListImportFailuresResponse' {} a -> s {failures = a} :: ListImportFailuresResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token you can use to get the next page of results.
listImportFailuresResponse_nextToken :: Lens.Lens' ListImportFailuresResponse (Prelude.Maybe Prelude.Text)
listImportFailuresResponse_nextToken = Lens.lens (\ListImportFailuresResponse' {nextToken} -> nextToken) (\s@ListImportFailuresResponse' {} a -> s {nextToken = a} :: ListImportFailuresResponse)

-- | The response's http status code.
listImportFailuresResponse_httpStatus :: Lens.Lens' ListImportFailuresResponse Prelude.Int
listImportFailuresResponse_httpStatus = Lens.lens (\ListImportFailuresResponse' {httpStatus} -> httpStatus) (\s@ListImportFailuresResponse' {} a -> s {httpStatus = a} :: ListImportFailuresResponse)

instance Prelude.NFData ListImportFailuresResponse where
  rnf ListImportFailuresResponse' {..} =
    Prelude.rnf failures
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
