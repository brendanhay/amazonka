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
-- Module      : Amazonka.MGN.ListExportErrors
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List export errors.
--
-- This operation returns paginated results.
module Amazonka.MGN.ListExportErrors
  ( -- * Creating a Request
    ListExportErrors (..),
    newListExportErrors,

    -- * Request Lenses
    listExportErrors_maxResults,
    listExportErrors_nextToken,
    listExportErrors_exportID,

    -- * Destructuring the Response
    ListExportErrorsResponse (..),
    newListExportErrorsResponse,

    -- * Response Lenses
    listExportErrorsResponse_items,
    listExportErrorsResponse_nextToken,
    listExportErrorsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | List export errors request.
--
-- /See:/ 'newListExportErrors' smart constructor.
data ListExportErrors = ListExportErrors'
  { -- | List export errors request max results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | List export errors request next token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List export errors request export id.
    exportID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListExportErrors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listExportErrors_maxResults' - List export errors request max results.
--
-- 'nextToken', 'listExportErrors_nextToken' - List export errors request next token.
--
-- 'exportID', 'listExportErrors_exportID' - List export errors request export id.
newListExportErrors ::
  -- | 'exportID'
  Prelude.Text ->
  ListExportErrors
newListExportErrors pExportID_ =
  ListExportErrors'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      exportID = pExportID_
    }

-- | List export errors request max results.
listExportErrors_maxResults :: Lens.Lens' ListExportErrors (Prelude.Maybe Prelude.Natural)
listExportErrors_maxResults = Lens.lens (\ListExportErrors' {maxResults} -> maxResults) (\s@ListExportErrors' {} a -> s {maxResults = a} :: ListExportErrors)

-- | List export errors request next token.
listExportErrors_nextToken :: Lens.Lens' ListExportErrors (Prelude.Maybe Prelude.Text)
listExportErrors_nextToken = Lens.lens (\ListExportErrors' {nextToken} -> nextToken) (\s@ListExportErrors' {} a -> s {nextToken = a} :: ListExportErrors)

-- | List export errors request export id.
listExportErrors_exportID :: Lens.Lens' ListExportErrors Prelude.Text
listExportErrors_exportID = Lens.lens (\ListExportErrors' {exportID} -> exportID) (\s@ListExportErrors' {} a -> s {exportID = a} :: ListExportErrors)

instance Core.AWSPager ListExportErrors where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listExportErrorsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listExportErrorsResponse_items
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listExportErrors_nextToken
          Lens..~ rs
          Lens.^? listExportErrorsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListExportErrors where
  type
    AWSResponse ListExportErrors =
      ListExportErrorsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListExportErrorsResponse'
            Prelude.<$> (x Data..?> "items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListExportErrors where
  hashWithSalt _salt ListExportErrors' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` exportID

instance Prelude.NFData ListExportErrors where
  rnf ListExportErrors' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf exportID

instance Data.ToHeaders ListExportErrors where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListExportErrors where
  toJSON ListExportErrors' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("exportID" Data..= exportID)
          ]
      )

instance Data.ToPath ListExportErrors where
  toPath = Prelude.const "/ListExportErrors"

instance Data.ToQuery ListExportErrors where
  toQuery = Prelude.const Prelude.mempty

-- | List export errors response.
--
-- /See:/ 'newListExportErrorsResponse' smart constructor.
data ListExportErrorsResponse = ListExportErrorsResponse'
  { -- | List export errors response items.
    items :: Prelude.Maybe [ExportTaskError],
    -- | List export errors response next token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListExportErrorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'listExportErrorsResponse_items' - List export errors response items.
--
-- 'nextToken', 'listExportErrorsResponse_nextToken' - List export errors response next token.
--
-- 'httpStatus', 'listExportErrorsResponse_httpStatus' - The response's http status code.
newListExportErrorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListExportErrorsResponse
newListExportErrorsResponse pHttpStatus_ =
  ListExportErrorsResponse'
    { items = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List export errors response items.
listExportErrorsResponse_items :: Lens.Lens' ListExportErrorsResponse (Prelude.Maybe [ExportTaskError])
listExportErrorsResponse_items = Lens.lens (\ListExportErrorsResponse' {items} -> items) (\s@ListExportErrorsResponse' {} a -> s {items = a} :: ListExportErrorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | List export errors response next token.
listExportErrorsResponse_nextToken :: Lens.Lens' ListExportErrorsResponse (Prelude.Maybe Prelude.Text)
listExportErrorsResponse_nextToken = Lens.lens (\ListExportErrorsResponse' {nextToken} -> nextToken) (\s@ListExportErrorsResponse' {} a -> s {nextToken = a} :: ListExportErrorsResponse)

-- | The response's http status code.
listExportErrorsResponse_httpStatus :: Lens.Lens' ListExportErrorsResponse Prelude.Int
listExportErrorsResponse_httpStatus = Lens.lens (\ListExportErrorsResponse' {httpStatus} -> httpStatus) (\s@ListExportErrorsResponse' {} a -> s {httpStatus = a} :: ListExportErrorsResponse)

instance Prelude.NFData ListExportErrorsResponse where
  rnf ListExportErrorsResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
