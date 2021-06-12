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
-- Module      : Network.AWS.SSM.ListDocumentVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all versions for a document.
--
-- This operation returns paginated results.
module Network.AWS.SSM.ListDocumentVersions
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newListDocumentVersions' smart constructor.
data ListDocumentVersions = ListDocumentVersions'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The name of the document. You can specify an Amazon Resource Name (ARN).
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  ListDocumentVersions
newListDocumentVersions pName_ =
  ListDocumentVersions'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      name = pName_
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
listDocumentVersions_nextToken :: Lens.Lens' ListDocumentVersions (Core.Maybe Core.Text)
listDocumentVersions_nextToken = Lens.lens (\ListDocumentVersions' {nextToken} -> nextToken) (\s@ListDocumentVersions' {} a -> s {nextToken = a} :: ListDocumentVersions)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
listDocumentVersions_maxResults :: Lens.Lens' ListDocumentVersions (Core.Maybe Core.Natural)
listDocumentVersions_maxResults = Lens.lens (\ListDocumentVersions' {maxResults} -> maxResults) (\s@ListDocumentVersions' {} a -> s {maxResults = a} :: ListDocumentVersions)

-- | The name of the document. You can specify an Amazon Resource Name (ARN).
listDocumentVersions_name :: Lens.Lens' ListDocumentVersions Core.Text
listDocumentVersions_name = Lens.lens (\ListDocumentVersions' {name} -> name) (\s@ListDocumentVersions' {} a -> s {name = a} :: ListDocumentVersions)

instance Core.AWSPager ListDocumentVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDocumentVersionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listDocumentVersionsResponse_documentVersions
              Core.. Lens._Just
              Core.. Lens.to Core.toList
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listDocumentVersions_nextToken
          Lens..~ rs
          Lens.^? listDocumentVersionsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListDocumentVersions where
  type
    AWSResponse ListDocumentVersions =
      ListDocumentVersionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDocumentVersionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "DocumentVersions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListDocumentVersions

instance Core.NFData ListDocumentVersions

instance Core.ToHeaders ListDocumentVersions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.ListDocumentVersions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListDocumentVersions where
  toJSON ListDocumentVersions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath ListDocumentVersions where
  toPath = Core.const "/"

instance Core.ToQuery ListDocumentVersions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListDocumentVersionsResponse' smart constructor.
data ListDocumentVersionsResponse = ListDocumentVersionsResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Core.Maybe Core.Text,
    -- | The document versions.
    documentVersions :: Core.Maybe (Core.NonEmpty DocumentVersionInfo),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListDocumentVersionsResponse
newListDocumentVersionsResponse pHttpStatus_ =
  ListDocumentVersionsResponse'
    { nextToken =
        Core.Nothing,
      documentVersions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
listDocumentVersionsResponse_nextToken :: Lens.Lens' ListDocumentVersionsResponse (Core.Maybe Core.Text)
listDocumentVersionsResponse_nextToken = Lens.lens (\ListDocumentVersionsResponse' {nextToken} -> nextToken) (\s@ListDocumentVersionsResponse' {} a -> s {nextToken = a} :: ListDocumentVersionsResponse)

-- | The document versions.
listDocumentVersionsResponse_documentVersions :: Lens.Lens' ListDocumentVersionsResponse (Core.Maybe (Core.NonEmpty DocumentVersionInfo))
listDocumentVersionsResponse_documentVersions = Lens.lens (\ListDocumentVersionsResponse' {documentVersions} -> documentVersions) (\s@ListDocumentVersionsResponse' {} a -> s {documentVersions = a} :: ListDocumentVersionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listDocumentVersionsResponse_httpStatus :: Lens.Lens' ListDocumentVersionsResponse Core.Int
listDocumentVersionsResponse_httpStatus = Lens.lens (\ListDocumentVersionsResponse' {httpStatus} -> httpStatus) (\s@ListDocumentVersionsResponse' {} a -> s {httpStatus = a} :: ListDocumentVersionsResponse)

instance Core.NFData ListDocumentVersionsResponse
