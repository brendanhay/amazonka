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
-- Module      : Amazonka.DynamoDB.ListImports
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists completed imports within the past 90 days.
module Amazonka.DynamoDB.ListImports
  ( -- * Creating a Request
    ListImports (..),
    newListImports,

    -- * Request Lenses
    listImports_tableArn,
    listImports_nextToken,
    listImports_pageSize,

    -- * Destructuring the Response
    ListImportsResponse (..),
    newListImportsResponse,

    -- * Response Lenses
    listImportsResponse_nextToken,
    listImportsResponse_importSummaryList,
    listImportsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListImports' smart constructor.
data ListImports = ListImports'
  { -- | The Amazon Resource Name (ARN) associated with the table that was
    -- imported to.
    tableArn :: Prelude.Maybe Prelude.Text,
    -- | An optional string that, if supplied, must be copied from the output of
    -- a previous call to @ListImports@. When provided in this manner, the API
    -- fetches the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of @ImportSummary @objects returned in a single page.
    pageSize :: Prelude.Maybe Prelude.Natural
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
-- 'tableArn', 'listImports_tableArn' - The Amazon Resource Name (ARN) associated with the table that was
-- imported to.
--
-- 'nextToken', 'listImports_nextToken' - An optional string that, if supplied, must be copied from the output of
-- a previous call to @ListImports@. When provided in this manner, the API
-- fetches the next page of results.
--
-- 'pageSize', 'listImports_pageSize' - The number of @ImportSummary @objects returned in a single page.
newListImports ::
  ListImports
newListImports =
  ListImports'
    { tableArn = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      pageSize = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) associated with the table that was
-- imported to.
listImports_tableArn :: Lens.Lens' ListImports (Prelude.Maybe Prelude.Text)
listImports_tableArn = Lens.lens (\ListImports' {tableArn} -> tableArn) (\s@ListImports' {} a -> s {tableArn = a} :: ListImports)

-- | An optional string that, if supplied, must be copied from the output of
-- a previous call to @ListImports@. When provided in this manner, the API
-- fetches the next page of results.
listImports_nextToken :: Lens.Lens' ListImports (Prelude.Maybe Prelude.Text)
listImports_nextToken = Lens.lens (\ListImports' {nextToken} -> nextToken) (\s@ListImports' {} a -> s {nextToken = a} :: ListImports)

-- | The number of @ImportSummary @objects returned in a single page.
listImports_pageSize :: Lens.Lens' ListImports (Prelude.Maybe Prelude.Natural)
listImports_pageSize = Lens.lens (\ListImports' {pageSize} -> pageSize) (\s@ListImports' {} a -> s {pageSize = a} :: ListImports)

instance Core.AWSRequest ListImports where
  type AWSResponse ListImports = ListImportsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListImportsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "ImportSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListImports where
  hashWithSalt _salt ListImports' {..} =
    _salt `Prelude.hashWithSalt` tableArn
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` pageSize

instance Prelude.NFData ListImports where
  rnf ListImports' {..} =
    Prelude.rnf tableArn
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf pageSize

instance Core.ToHeaders ListImports where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DynamoDB_20120810.ListImports" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListImports where
  toJSON ListImports' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TableArn" Core..=) Prelude.<$> tableArn,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("PageSize" Core..=) Prelude.<$> pageSize
          ]
      )

instance Core.ToPath ListImports where
  toPath = Prelude.const "/"

instance Core.ToQuery ListImports where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListImportsResponse' smart constructor.
data ListImportsResponse = ListImportsResponse'
  { -- | If this value is returned, there are additional results to be displayed.
    -- To retrieve them, call @ListImports@ again, with @NextToken@ set to this
    -- value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of @ImportSummary@ objects.
    importSummaryList :: Prelude.Maybe [ImportSummary],
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
-- 'nextToken', 'listImportsResponse_nextToken' - If this value is returned, there are additional results to be displayed.
-- To retrieve them, call @ListImports@ again, with @NextToken@ set to this
-- value.
--
-- 'importSummaryList', 'listImportsResponse_importSummaryList' - A list of @ImportSummary@ objects.
--
-- 'httpStatus', 'listImportsResponse_httpStatus' - The response's http status code.
newListImportsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListImportsResponse
newListImportsResponse pHttpStatus_ =
  ListImportsResponse'
    { nextToken = Prelude.Nothing,
      importSummaryList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If this value is returned, there are additional results to be displayed.
-- To retrieve them, call @ListImports@ again, with @NextToken@ set to this
-- value.
listImportsResponse_nextToken :: Lens.Lens' ListImportsResponse (Prelude.Maybe Prelude.Text)
listImportsResponse_nextToken = Lens.lens (\ListImportsResponse' {nextToken} -> nextToken) (\s@ListImportsResponse' {} a -> s {nextToken = a} :: ListImportsResponse)

-- | A list of @ImportSummary@ objects.
listImportsResponse_importSummaryList :: Lens.Lens' ListImportsResponse (Prelude.Maybe [ImportSummary])
listImportsResponse_importSummaryList = Lens.lens (\ListImportsResponse' {importSummaryList} -> importSummaryList) (\s@ListImportsResponse' {} a -> s {importSummaryList = a} :: ListImportsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listImportsResponse_httpStatus :: Lens.Lens' ListImportsResponse Prelude.Int
listImportsResponse_httpStatus = Lens.lens (\ListImportsResponse' {httpStatus} -> httpStatus) (\s@ListImportsResponse' {} a -> s {httpStatus = a} :: ListImportsResponse)

instance Prelude.NFData ListImportsResponse where
  rnf ListImportsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf importSummaryList
      `Prelude.seq` Prelude.rnf httpStatus
