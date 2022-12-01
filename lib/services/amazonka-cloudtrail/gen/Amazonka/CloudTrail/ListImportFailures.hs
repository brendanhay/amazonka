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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    listImportFailures_nextToken,
    listImportFailures_maxResults,
    listImportFailures_importId,

    -- * Destructuring the Response
    ListImportFailuresResponse (..),
    newListImportFailuresResponse,

    -- * Response Lenses
    listImportFailuresResponse_nextToken,
    listImportFailuresResponse_failures,
    listImportFailuresResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListImportFailures' smart constructor.
data ListImportFailures = ListImportFailures'
  { -- | A token you can use to get the next page of import failures.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of failures to display on a single page.
    maxResults :: Prelude.Maybe Prelude.Natural,
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
-- 'nextToken', 'listImportFailures_nextToken' - A token you can use to get the next page of import failures.
--
-- 'maxResults', 'listImportFailures_maxResults' - The maximum number of failures to display on a single page.
--
-- 'importId', 'listImportFailures_importId' - The ID of the import.
newListImportFailures ::
  -- | 'importId'
  Prelude.Text ->
  ListImportFailures
newListImportFailures pImportId_ =
  ListImportFailures'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      importId = pImportId_
    }

-- | A token you can use to get the next page of import failures.
listImportFailures_nextToken :: Lens.Lens' ListImportFailures (Prelude.Maybe Prelude.Text)
listImportFailures_nextToken = Lens.lens (\ListImportFailures' {nextToken} -> nextToken) (\s@ListImportFailures' {} a -> s {nextToken = a} :: ListImportFailures)

-- | The maximum number of failures to display on a single page.
listImportFailures_maxResults :: Lens.Lens' ListImportFailures (Prelude.Maybe Prelude.Natural)
listImportFailures_maxResults = Lens.lens (\ListImportFailures' {maxResults} -> maxResults) (\s@ListImportFailures' {} a -> s {maxResults = a} :: ListImportFailures)

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
      Prelude.Just Prelude.$
        rq
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
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Failures" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListImportFailures where
  hashWithSalt _salt ListImportFailures' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` importId

instance Prelude.NFData ListImportFailures where
  rnf ListImportFailures' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf importId

instance Core.ToHeaders ListImportFailures where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.ListImportFailures" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListImportFailures where
  toJSON ListImportFailures' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("ImportId" Core..= importId)
          ]
      )

instance Core.ToPath ListImportFailures where
  toPath = Prelude.const "/"

instance Core.ToQuery ListImportFailures where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListImportFailuresResponse' smart constructor.
data ListImportFailuresResponse = ListImportFailuresResponse'
  { -- | A token you can use to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Contains information about the import failures.
    failures :: Prelude.Maybe [ImportFailureListItem],
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
-- 'nextToken', 'listImportFailuresResponse_nextToken' - A token you can use to get the next page of results.
--
-- 'failures', 'listImportFailuresResponse_failures' - Contains information about the import failures.
--
-- 'httpStatus', 'listImportFailuresResponse_httpStatus' - The response's http status code.
newListImportFailuresResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListImportFailuresResponse
newListImportFailuresResponse pHttpStatus_ =
  ListImportFailuresResponse'
    { nextToken =
        Prelude.Nothing,
      failures = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token you can use to get the next page of results.
listImportFailuresResponse_nextToken :: Lens.Lens' ListImportFailuresResponse (Prelude.Maybe Prelude.Text)
listImportFailuresResponse_nextToken = Lens.lens (\ListImportFailuresResponse' {nextToken} -> nextToken) (\s@ListImportFailuresResponse' {} a -> s {nextToken = a} :: ListImportFailuresResponse)

-- | Contains information about the import failures.
listImportFailuresResponse_failures :: Lens.Lens' ListImportFailuresResponse (Prelude.Maybe [ImportFailureListItem])
listImportFailuresResponse_failures = Lens.lens (\ListImportFailuresResponse' {failures} -> failures) (\s@ListImportFailuresResponse' {} a -> s {failures = a} :: ListImportFailuresResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listImportFailuresResponse_httpStatus :: Lens.Lens' ListImportFailuresResponse Prelude.Int
listImportFailuresResponse_httpStatus = Lens.lens (\ListImportFailuresResponse' {httpStatus} -> httpStatus) (\s@ListImportFailuresResponse' {} a -> s {httpStatus = a} :: ListImportFailuresResponse)

instance Prelude.NFData ListImportFailuresResponse where
  rnf ListImportFailuresResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf failures
      `Prelude.seq` Prelude.rnf httpStatus
