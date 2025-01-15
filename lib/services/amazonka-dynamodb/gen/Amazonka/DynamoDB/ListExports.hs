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
-- Module      : Amazonka.DynamoDB.ListExports
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists completed exports within the past 90 days.
module Amazonka.DynamoDB.ListExports
  ( -- * Creating a Request
    ListExports (..),
    newListExports,

    -- * Request Lenses
    listExports_maxResults,
    listExports_nextToken,
    listExports_tableArn,

    -- * Destructuring the Response
    ListExportsResponse (..),
    newListExportsResponse,

    -- * Response Lenses
    listExportsResponse_exportSummaries,
    listExportsResponse_nextToken,
    listExportsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListExports' smart constructor.
data ListExports = ListExports'
  { -- | Maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | An optional string that, if supplied, must be copied from the output of
    -- a previous call to @ListExports@. When provided in this manner, the API
    -- fetches the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) associated with the exported table.
    tableArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListExports' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listExports_maxResults' - Maximum number of results to return per page.
--
-- 'nextToken', 'listExports_nextToken' - An optional string that, if supplied, must be copied from the output of
-- a previous call to @ListExports@. When provided in this manner, the API
-- fetches the next page of results.
--
-- 'tableArn', 'listExports_tableArn' - The Amazon Resource Name (ARN) associated with the exported table.
newListExports ::
  ListExports
newListExports =
  ListExports'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      tableArn = Prelude.Nothing
    }

-- | Maximum number of results to return per page.
listExports_maxResults :: Lens.Lens' ListExports (Prelude.Maybe Prelude.Natural)
listExports_maxResults = Lens.lens (\ListExports' {maxResults} -> maxResults) (\s@ListExports' {} a -> s {maxResults = a} :: ListExports)

-- | An optional string that, if supplied, must be copied from the output of
-- a previous call to @ListExports@. When provided in this manner, the API
-- fetches the next page of results.
listExports_nextToken :: Lens.Lens' ListExports (Prelude.Maybe Prelude.Text)
listExports_nextToken = Lens.lens (\ListExports' {nextToken} -> nextToken) (\s@ListExports' {} a -> s {nextToken = a} :: ListExports)

-- | The Amazon Resource Name (ARN) associated with the exported table.
listExports_tableArn :: Lens.Lens' ListExports (Prelude.Maybe Prelude.Text)
listExports_tableArn = Lens.lens (\ListExports' {tableArn} -> tableArn) (\s@ListExports' {} a -> s {tableArn = a} :: ListExports)

instance Core.AWSRequest ListExports where
  type AWSResponse ListExports = ListExportsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListExportsResponse'
            Prelude.<$> ( x
                            Data..?> "ExportSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListExports where
  hashWithSalt _salt ListExports' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` tableArn

instance Prelude.NFData ListExports where
  rnf ListExports' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf tableArn

instance Data.ToHeaders ListExports where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DynamoDB_20120810.ListExports" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListExports where
  toJSON ListExports' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("TableArn" Data..=) Prelude.<$> tableArn
          ]
      )

instance Data.ToPath ListExports where
  toPath = Prelude.const "/"

instance Data.ToQuery ListExports where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListExportsResponse' smart constructor.
data ListExportsResponse = ListExportsResponse'
  { -- | A list of @ExportSummary@ objects.
    exportSummaries :: Prelude.Maybe [ExportSummary],
    -- | If this value is returned, there are additional results to be displayed.
    -- To retrieve them, call @ListExports@ again, with @NextToken@ set to this
    -- value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListExportsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportSummaries', 'listExportsResponse_exportSummaries' - A list of @ExportSummary@ objects.
--
-- 'nextToken', 'listExportsResponse_nextToken' - If this value is returned, there are additional results to be displayed.
-- To retrieve them, call @ListExports@ again, with @NextToken@ set to this
-- value.
--
-- 'httpStatus', 'listExportsResponse_httpStatus' - The response's http status code.
newListExportsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListExportsResponse
newListExportsResponse pHttpStatus_ =
  ListExportsResponse'
    { exportSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @ExportSummary@ objects.
listExportsResponse_exportSummaries :: Lens.Lens' ListExportsResponse (Prelude.Maybe [ExportSummary])
listExportsResponse_exportSummaries = Lens.lens (\ListExportsResponse' {exportSummaries} -> exportSummaries) (\s@ListExportsResponse' {} a -> s {exportSummaries = a} :: ListExportsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If this value is returned, there are additional results to be displayed.
-- To retrieve them, call @ListExports@ again, with @NextToken@ set to this
-- value.
listExportsResponse_nextToken :: Lens.Lens' ListExportsResponse (Prelude.Maybe Prelude.Text)
listExportsResponse_nextToken = Lens.lens (\ListExportsResponse' {nextToken} -> nextToken) (\s@ListExportsResponse' {} a -> s {nextToken = a} :: ListExportsResponse)

-- | The response's http status code.
listExportsResponse_httpStatus :: Lens.Lens' ListExportsResponse Prelude.Int
listExportsResponse_httpStatus = Lens.lens (\ListExportsResponse' {httpStatus} -> httpStatus) (\s@ListExportsResponse' {} a -> s {httpStatus = a} :: ListExportsResponse)

instance Prelude.NFData ListExportsResponse where
  rnf ListExportsResponse' {..} =
    Prelude.rnf exportSummaries `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
