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
-- Module      : Amazonka.CloudTrail.ListImports
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information on all imports, or a select set of imports by
-- @ImportStatus@ or @Destination@.
--
-- This operation returns paginated results.
module Amazonka.CloudTrail.ListImports
  ( -- * Creating a Request
    ListImports (..),
    newListImports,

    -- * Request Lenses
    listImports_destination,
    listImports_nextToken,
    listImports_maxResults,
    listImports_importStatus,

    -- * Destructuring the Response
    ListImportsResponse (..),
    newListImportsResponse,

    -- * Response Lenses
    listImportsResponse_imports,
    listImportsResponse_nextToken,
    listImportsResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListImports' smart constructor.
data ListImports = ListImports'
  { -- | The ARN of the destination event data store.
    destination :: Prelude.Maybe Prelude.Text,
    -- | A token you can use to get the next page of import results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of imports to display on a single page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The status of the import.
    importStatus :: Prelude.Maybe ImportStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListImports' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'listImports_destination' - The ARN of the destination event data store.
--
-- 'nextToken', 'listImports_nextToken' - A token you can use to get the next page of import results.
--
-- 'maxResults', 'listImports_maxResults' - The maximum number of imports to display on a single page.
--
-- 'importStatus', 'listImports_importStatus' - The status of the import.
newListImports ::
  ListImports
newListImports =
  ListImports'
    { destination = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      importStatus = Prelude.Nothing
    }

-- | The ARN of the destination event data store.
listImports_destination :: Lens.Lens' ListImports (Prelude.Maybe Prelude.Text)
listImports_destination = Lens.lens (\ListImports' {destination} -> destination) (\s@ListImports' {} a -> s {destination = a} :: ListImports)

-- | A token you can use to get the next page of import results.
listImports_nextToken :: Lens.Lens' ListImports (Prelude.Maybe Prelude.Text)
listImports_nextToken = Lens.lens (\ListImports' {nextToken} -> nextToken) (\s@ListImports' {} a -> s {nextToken = a} :: ListImports)

-- | The maximum number of imports to display on a single page.
listImports_maxResults :: Lens.Lens' ListImports (Prelude.Maybe Prelude.Natural)
listImports_maxResults = Lens.lens (\ListImports' {maxResults} -> maxResults) (\s@ListImports' {} a -> s {maxResults = a} :: ListImports)

-- | The status of the import.
listImports_importStatus :: Lens.Lens' ListImports (Prelude.Maybe ImportStatus)
listImports_importStatus = Lens.lens (\ListImports' {importStatus} -> importStatus) (\s@ListImports' {} a -> s {importStatus = a} :: ListImports)

instance Core.AWSPager ListImports where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listImportsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listImportsResponse_imports Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listImports_nextToken
          Lens..~ rs
          Lens.^? listImportsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListImports where
  type AWSResponse ListImports = ListImportsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListImportsResponse'
            Prelude.<$> (x Core..?> "Imports" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListImports where
  hashWithSalt _salt ListImports' {..} =
    _salt `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` importStatus

instance Prelude.NFData ListImports where
  rnf ListImports' {..} =
    Prelude.rnf destination
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf importStatus

instance Core.ToHeaders ListImports where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.ListImports" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListImports where
  toJSON ListImports' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Destination" Core..=) Prelude.<$> destination,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("ImportStatus" Core..=) Prelude.<$> importStatus
          ]
      )

instance Core.ToPath ListImports where
  toPath = Prelude.const "/"

instance Core.ToQuery ListImports where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListImportsResponse' smart constructor.
data ListImportsResponse = ListImportsResponse'
  { -- | The list of returned imports.
    imports :: Prelude.Maybe [ImportsListItem],
    -- | A token you can use to get the next page of import results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListImportsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imports', 'listImportsResponse_imports' - The list of returned imports.
--
-- 'nextToken', 'listImportsResponse_nextToken' - A token you can use to get the next page of import results.
--
-- 'httpStatus', 'listImportsResponse_httpStatus' - The response's http status code.
newListImportsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListImportsResponse
newListImportsResponse pHttpStatus_ =
  ListImportsResponse'
    { imports = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of returned imports.
listImportsResponse_imports :: Lens.Lens' ListImportsResponse (Prelude.Maybe [ImportsListItem])
listImportsResponse_imports = Lens.lens (\ListImportsResponse' {imports} -> imports) (\s@ListImportsResponse' {} a -> s {imports = a} :: ListImportsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token you can use to get the next page of import results.
listImportsResponse_nextToken :: Lens.Lens' ListImportsResponse (Prelude.Maybe Prelude.Text)
listImportsResponse_nextToken = Lens.lens (\ListImportsResponse' {nextToken} -> nextToken) (\s@ListImportsResponse' {} a -> s {nextToken = a} :: ListImportsResponse)

-- | The response's http status code.
listImportsResponse_httpStatus :: Lens.Lens' ListImportsResponse Prelude.Int
listImportsResponse_httpStatus = Lens.lens (\ListImportsResponse' {httpStatus} -> httpStatus) (\s@ListImportsResponse' {} a -> s {httpStatus = a} :: ListImportsResponse)

instance Prelude.NFData ListImportsResponse where
  rnf ListImportsResponse' {..} =
    Prelude.rnf imports
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
