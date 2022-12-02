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
-- Module      : Amazonka.SSM.ListDocumentVersions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all versions for a document.
--
-- This operation returns paginated results.
module Amazonka.SSM.ListDocumentVersions
  ( -- * Creating a Request
    ListDocumentVersions (..),
    newListDocumentVersions,

    -- * Request Lenses
    listDocumentVersions_nextToken,
    listDocumentVersions_maxResults,
    listDocumentVersions_name,

    -- * Destructuring the Response
    ListDocumentVersionsResponse (..),
    newListDocumentVersionsResponse,

    -- * Response Lenses
    listDocumentVersionsResponse_nextToken,
    listDocumentVersionsResponse_documentVersions,
    listDocumentVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newListDocumentVersions' smart constructor.
data ListDocumentVersions = ListDocumentVersions'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the document. You can specify an Amazon Resource Name (ARN).
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDocumentVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDocumentVersions_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'maxResults', 'listDocumentVersions_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'name', 'listDocumentVersions_name' - The name of the document. You can specify an Amazon Resource Name (ARN).
newListDocumentVersions ::
  -- | 'name'
  Prelude.Text ->
  ListDocumentVersions
newListDocumentVersions pName_ =
  ListDocumentVersions'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      name = pName_
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
listDocumentVersions_nextToken :: Lens.Lens' ListDocumentVersions (Prelude.Maybe Prelude.Text)
listDocumentVersions_nextToken = Lens.lens (\ListDocumentVersions' {nextToken} -> nextToken) (\s@ListDocumentVersions' {} a -> s {nextToken = a} :: ListDocumentVersions)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
listDocumentVersions_maxResults :: Lens.Lens' ListDocumentVersions (Prelude.Maybe Prelude.Natural)
listDocumentVersions_maxResults = Lens.lens (\ListDocumentVersions' {maxResults} -> maxResults) (\s@ListDocumentVersions' {} a -> s {maxResults = a} :: ListDocumentVersions)

-- | The name of the document. You can specify an Amazon Resource Name (ARN).
listDocumentVersions_name :: Lens.Lens' ListDocumentVersions Prelude.Text
listDocumentVersions_name = Lens.lens (\ListDocumentVersions' {name} -> name) (\s@ListDocumentVersions' {} a -> s {name = a} :: ListDocumentVersions)

instance Core.AWSPager ListDocumentVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDocumentVersionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDocumentVersionsResponse_documentVersions
              Prelude.. Lens._Just
              Prelude.. Lens.to Prelude.toList
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDocumentVersions_nextToken
          Lens..~ rs
          Lens.^? listDocumentVersionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListDocumentVersions where
  type
    AWSResponse ListDocumentVersions =
      ListDocumentVersionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDocumentVersionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "DocumentVersions")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDocumentVersions where
  hashWithSalt _salt ListDocumentVersions' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` name

instance Prelude.NFData ListDocumentVersions where
  rnf ListDocumentVersions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders ListDocumentVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.ListDocumentVersions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDocumentVersions where
  toJSON ListDocumentVersions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath ListDocumentVersions where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDocumentVersions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDocumentVersionsResponse' smart constructor.
data ListDocumentVersionsResponse = ListDocumentVersionsResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The document versions.
    documentVersions :: Prelude.Maybe (Prelude.NonEmpty DocumentVersionInfo),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDocumentVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDocumentVersionsResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'documentVersions', 'listDocumentVersionsResponse_documentVersions' - The document versions.
--
-- 'httpStatus', 'listDocumentVersionsResponse_httpStatus' - The response's http status code.
newListDocumentVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDocumentVersionsResponse
newListDocumentVersionsResponse pHttpStatus_ =
  ListDocumentVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      documentVersions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
listDocumentVersionsResponse_nextToken :: Lens.Lens' ListDocumentVersionsResponse (Prelude.Maybe Prelude.Text)
listDocumentVersionsResponse_nextToken = Lens.lens (\ListDocumentVersionsResponse' {nextToken} -> nextToken) (\s@ListDocumentVersionsResponse' {} a -> s {nextToken = a} :: ListDocumentVersionsResponse)

-- | The document versions.
listDocumentVersionsResponse_documentVersions :: Lens.Lens' ListDocumentVersionsResponse (Prelude.Maybe (Prelude.NonEmpty DocumentVersionInfo))
listDocumentVersionsResponse_documentVersions = Lens.lens (\ListDocumentVersionsResponse' {documentVersions} -> documentVersions) (\s@ListDocumentVersionsResponse' {} a -> s {documentVersions = a} :: ListDocumentVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDocumentVersionsResponse_httpStatus :: Lens.Lens' ListDocumentVersionsResponse Prelude.Int
listDocumentVersionsResponse_httpStatus = Lens.lens (\ListDocumentVersionsResponse' {httpStatus} -> httpStatus) (\s@ListDocumentVersionsResponse' {} a -> s {httpStatus = a} :: ListDocumentVersionsResponse)

instance Prelude.NFData ListDocumentVersionsResponse where
  rnf ListDocumentVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf documentVersions
      `Prelude.seq` Prelude.rnf httpStatus
