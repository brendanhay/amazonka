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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists completed imports within the past 90 days.
module Amazonka.DynamoDB.ListImports
  ( -- * Creating a Request
    ListImports (..),
    newListImports,

    -- * Request Lenses
    listImports_nextToken,
    listImports_pageSize,
    listImports_tableArn,

    -- * Destructuring the Response
    ListImportsResponse (..),
    newListImportsResponse,

    -- * Response Lenses
    listImportsResponse_importSummaryList,
    listImportsResponse_nextToken,
    listImportsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListImports' smart constructor.
data ListImports = ListImports'
  { -- | An optional string that, if supplied, must be copied from the output of
    -- a previous call to @ListImports@. When provided in this manner, the API
    -- fetches the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of @ImportSummary @objects returned in a single page.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) associated with the table that was
    -- imported to.
    tableArn :: Prelude.Maybe Prelude.Text
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
-- 'nextToken', 'listImports_nextToken' - An optional string that, if supplied, must be copied from the output of
-- a previous call to @ListImports@. When provided in this manner, the API
-- fetches the next page of results.
--
-- 'pageSize', 'listImports_pageSize' - The number of @ImportSummary @objects returned in a single page.
--
-- 'tableArn', 'listImports_tableArn' - The Amazon Resource Name (ARN) associated with the table that was
-- imported to.
newListImports ::
  ListImports
newListImports =
  ListImports'
    { nextToken = Prelude.Nothing,
      pageSize = Prelude.Nothing,
      tableArn = Prelude.Nothing
    }

-- | An optional string that, if supplied, must be copied from the output of
-- a previous call to @ListImports@. When provided in this manner, the API
-- fetches the next page of results.
listImports_nextToken :: Lens.Lens' ListImports (Prelude.Maybe Prelude.Text)
listImports_nextToken = Lens.lens (\ListImports' {nextToken} -> nextToken) (\s@ListImports' {} a -> s {nextToken = a} :: ListImports)

-- | The number of @ImportSummary @objects returned in a single page.
listImports_pageSize :: Lens.Lens' ListImports (Prelude.Maybe Prelude.Natural)
listImports_pageSize = Lens.lens (\ListImports' {pageSize} -> pageSize) (\s@ListImports' {} a -> s {pageSize = a} :: ListImports)

-- | The Amazon Resource Name (ARN) associated with the table that was
-- imported to.
listImports_tableArn :: Lens.Lens' ListImports (Prelude.Maybe Prelude.Text)
listImports_tableArn = Lens.lens (\ListImports' {tableArn} -> tableArn) (\s@ListImports' {} a -> s {tableArn = a} :: ListImports)

instance Core.AWSRequest ListImports where
  type AWSResponse ListImports = ListImportsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListImportsResponse'
            Prelude.<$> ( x
                            Data..?> "ImportSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListImports where
  hashWithSalt _salt ListImports' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` tableArn

instance Prelude.NFData ListImports where
  rnf ListImports' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf pageSize `Prelude.seq`
        Prelude.rnf tableArn

instance Data.ToHeaders ListImports where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DynamoDB_20120810.ListImports" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListImports where
  toJSON ListImports' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("PageSize" Data..=) Prelude.<$> pageSize,
            ("TableArn" Data..=) Prelude.<$> tableArn
          ]
      )

instance Data.ToPath ListImports where
  toPath = Prelude.const "/"

instance Data.ToQuery ListImports where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListImportsResponse' smart constructor.
data ListImportsResponse = ListImportsResponse'
  { -- | A list of @ImportSummary@ objects.
    importSummaryList :: Prelude.Maybe [ImportSummary],
    -- | If this value is returned, there are additional results to be displayed.
    -- To retrieve them, call @ListImports@ again, with @NextToken@ set to this
    -- value.
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
-- 'importSummaryList', 'listImportsResponse_importSummaryList' - A list of @ImportSummary@ objects.
--
-- 'nextToken', 'listImportsResponse_nextToken' - If this value is returned, there are additional results to be displayed.
-- To retrieve them, call @ListImports@ again, with @NextToken@ set to this
-- value.
--
-- 'httpStatus', 'listImportsResponse_httpStatus' - The response's http status code.
newListImportsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListImportsResponse
newListImportsResponse pHttpStatus_ =
  ListImportsResponse'
    { importSummaryList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @ImportSummary@ objects.
listImportsResponse_importSummaryList :: Lens.Lens' ListImportsResponse (Prelude.Maybe [ImportSummary])
listImportsResponse_importSummaryList = Lens.lens (\ListImportsResponse' {importSummaryList} -> importSummaryList) (\s@ListImportsResponse' {} a -> s {importSummaryList = a} :: ListImportsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If this value is returned, there are additional results to be displayed.
-- To retrieve them, call @ListImports@ again, with @NextToken@ set to this
-- value.
listImportsResponse_nextToken :: Lens.Lens' ListImportsResponse (Prelude.Maybe Prelude.Text)
listImportsResponse_nextToken = Lens.lens (\ListImportsResponse' {nextToken} -> nextToken) (\s@ListImportsResponse' {} a -> s {nextToken = a} :: ListImportsResponse)

-- | The response's http status code.
listImportsResponse_httpStatus :: Lens.Lens' ListImportsResponse Prelude.Int
listImportsResponse_httpStatus = Lens.lens (\ListImportsResponse' {httpStatus} -> httpStatus) (\s@ListImportsResponse' {} a -> s {httpStatus = a} :: ListImportsResponse)

instance Prelude.NFData ListImportsResponse where
  rnf ListImportsResponse' {..} =
    Prelude.rnf importSummaryList `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
