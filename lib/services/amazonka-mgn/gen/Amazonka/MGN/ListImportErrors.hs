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
-- Module      : Amazonka.MGN.ListImportErrors
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List import errors.
--
-- This operation returns paginated results.
module Amazonka.MGN.ListImportErrors
  ( -- * Creating a Request
    ListImportErrors (..),
    newListImportErrors,

    -- * Request Lenses
    listImportErrors_maxResults,
    listImportErrors_nextToken,
    listImportErrors_importID,

    -- * Destructuring the Response
    ListImportErrorsResponse (..),
    newListImportErrorsResponse,

    -- * Response Lenses
    listImportErrorsResponse_items,
    listImportErrorsResponse_nextToken,
    listImportErrorsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | List import errors request.
--
-- /See:/ 'newListImportErrors' smart constructor.
data ListImportErrors = ListImportErrors'
  { -- | List import errors request max results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | List import errors request next token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List import errors request import id.
    importID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListImportErrors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listImportErrors_maxResults' - List import errors request max results.
--
-- 'nextToken', 'listImportErrors_nextToken' - List import errors request next token.
--
-- 'importID', 'listImportErrors_importID' - List import errors request import id.
newListImportErrors ::
  -- | 'importID'
  Prelude.Text ->
  ListImportErrors
newListImportErrors pImportID_ =
  ListImportErrors'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      importID = pImportID_
    }

-- | List import errors request max results.
listImportErrors_maxResults :: Lens.Lens' ListImportErrors (Prelude.Maybe Prelude.Natural)
listImportErrors_maxResults = Lens.lens (\ListImportErrors' {maxResults} -> maxResults) (\s@ListImportErrors' {} a -> s {maxResults = a} :: ListImportErrors)

-- | List import errors request next token.
listImportErrors_nextToken :: Lens.Lens' ListImportErrors (Prelude.Maybe Prelude.Text)
listImportErrors_nextToken = Lens.lens (\ListImportErrors' {nextToken} -> nextToken) (\s@ListImportErrors' {} a -> s {nextToken = a} :: ListImportErrors)

-- | List import errors request import id.
listImportErrors_importID :: Lens.Lens' ListImportErrors Prelude.Text
listImportErrors_importID = Lens.lens (\ListImportErrors' {importID} -> importID) (\s@ListImportErrors' {} a -> s {importID = a} :: ListImportErrors)

instance Core.AWSPager ListImportErrors where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listImportErrorsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listImportErrorsResponse_items
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listImportErrors_nextToken
          Lens..~ rs
          Lens.^? listImportErrorsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListImportErrors where
  type
    AWSResponse ListImportErrors =
      ListImportErrorsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListImportErrorsResponse'
            Prelude.<$> (x Data..?> "items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListImportErrors where
  hashWithSalt _salt ListImportErrors' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` importID

instance Prelude.NFData ListImportErrors where
  rnf ListImportErrors' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf importID

instance Data.ToHeaders ListImportErrors where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListImportErrors where
  toJSON ListImportErrors' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("importID" Data..= importID)
          ]
      )

instance Data.ToPath ListImportErrors where
  toPath = Prelude.const "/ListImportErrors"

instance Data.ToQuery ListImportErrors where
  toQuery = Prelude.const Prelude.mempty

-- | List imports errors response.
--
-- /See:/ 'newListImportErrorsResponse' smart constructor.
data ListImportErrorsResponse = ListImportErrorsResponse'
  { -- | List imports errors response items.
    items :: Prelude.Maybe [ImportTaskError],
    -- | List imports errors response next token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListImportErrorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'listImportErrorsResponse_items' - List imports errors response items.
--
-- 'nextToken', 'listImportErrorsResponse_nextToken' - List imports errors response next token.
--
-- 'httpStatus', 'listImportErrorsResponse_httpStatus' - The response's http status code.
newListImportErrorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListImportErrorsResponse
newListImportErrorsResponse pHttpStatus_ =
  ListImportErrorsResponse'
    { items = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List imports errors response items.
listImportErrorsResponse_items :: Lens.Lens' ListImportErrorsResponse (Prelude.Maybe [ImportTaskError])
listImportErrorsResponse_items = Lens.lens (\ListImportErrorsResponse' {items} -> items) (\s@ListImportErrorsResponse' {} a -> s {items = a} :: ListImportErrorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | List imports errors response next token.
listImportErrorsResponse_nextToken :: Lens.Lens' ListImportErrorsResponse (Prelude.Maybe Prelude.Text)
listImportErrorsResponse_nextToken = Lens.lens (\ListImportErrorsResponse' {nextToken} -> nextToken) (\s@ListImportErrorsResponse' {} a -> s {nextToken = a} :: ListImportErrorsResponse)

-- | The response's http status code.
listImportErrorsResponse_httpStatus :: Lens.Lens' ListImportErrorsResponse Prelude.Int
listImportErrorsResponse_httpStatus = Lens.lens (\ListImportErrorsResponse' {httpStatus} -> httpStatus) (\s@ListImportErrorsResponse' {} a -> s {httpStatus = a} :: ListImportErrorsResponse)

instance Prelude.NFData ListImportErrorsResponse where
  rnf ListImportErrorsResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
