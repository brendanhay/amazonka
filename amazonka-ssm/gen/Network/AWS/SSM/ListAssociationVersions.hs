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
-- Module      : Network.AWS.SSM.ListAssociationVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all versions of an association for a specific association ID.
--
-- This operation returns paginated results.
module Network.AWS.SSM.ListAssociationVersions
  ( -- * Creating a Request
    ListAssociationVersions (..),
    newListAssociationVersions,

    -- * Request Lenses
    listAssociationVersions_nextToken,
    listAssociationVersions_maxResults,
    listAssociationVersions_associationId,

    -- * Destructuring the Response
    ListAssociationVersionsResponse (..),
    newListAssociationVersionsResponse,

    -- * Response Lenses
    listAssociationVersionsResponse_nextToken,
    listAssociationVersionsResponse_associationVersions,
    listAssociationVersionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newListAssociationVersions' smart constructor.
data ListAssociationVersions = ListAssociationVersions'
  { -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The association ID for which you want to view all versions.
    associationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAssociationVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssociationVersions_nextToken' - A token to start the list. Use this token to get the next set of
-- results.
--
-- 'maxResults', 'listAssociationVersions_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'associationId', 'listAssociationVersions_associationId' - The association ID for which you want to view all versions.
newListAssociationVersions ::
  -- | 'associationId'
  Core.Text ->
  ListAssociationVersions
newListAssociationVersions pAssociationId_ =
  ListAssociationVersions'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      associationId = pAssociationId_
    }

-- | A token to start the list. Use this token to get the next set of
-- results.
listAssociationVersions_nextToken :: Lens.Lens' ListAssociationVersions (Core.Maybe Core.Text)
listAssociationVersions_nextToken = Lens.lens (\ListAssociationVersions' {nextToken} -> nextToken) (\s@ListAssociationVersions' {} a -> s {nextToken = a} :: ListAssociationVersions)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
listAssociationVersions_maxResults :: Lens.Lens' ListAssociationVersions (Core.Maybe Core.Natural)
listAssociationVersions_maxResults = Lens.lens (\ListAssociationVersions' {maxResults} -> maxResults) (\s@ListAssociationVersions' {} a -> s {maxResults = a} :: ListAssociationVersions)

-- | The association ID for which you want to view all versions.
listAssociationVersions_associationId :: Lens.Lens' ListAssociationVersions Core.Text
listAssociationVersions_associationId = Lens.lens (\ListAssociationVersions' {associationId} -> associationId) (\s@ListAssociationVersions' {} a -> s {associationId = a} :: ListAssociationVersions)

instance Core.AWSPager ListAssociationVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAssociationVersionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listAssociationVersionsResponse_associationVersions
              Core.. Lens._Just
              Core.. Lens.to Core.toList
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listAssociationVersions_nextToken
          Lens..~ rs
          Lens.^? listAssociationVersionsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListAssociationVersions where
  type
    AWSResponse ListAssociationVersions =
      ListAssociationVersionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssociationVersionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "AssociationVersions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListAssociationVersions

instance Core.NFData ListAssociationVersions

instance Core.ToHeaders ListAssociationVersions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.ListAssociationVersions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListAssociationVersions where
  toJSON ListAssociationVersions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("AssociationId" Core..= associationId)
          ]
      )

instance Core.ToPath ListAssociationVersions where
  toPath = Core.const "/"

instance Core.ToQuery ListAssociationVersions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListAssociationVersionsResponse' smart constructor.
data ListAssociationVersionsResponse = ListAssociationVersionsResponse'
  { -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about all versions of the association for the specified
    -- association ID.
    associationVersions :: Core.Maybe (Core.NonEmpty AssociationVersionInfo),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAssociationVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssociationVersionsResponse_nextToken' - The token for the next set of items to return. Use this token to get the
-- next set of results.
--
-- 'associationVersions', 'listAssociationVersionsResponse_associationVersions' - Information about all versions of the association for the specified
-- association ID.
--
-- 'httpStatus', 'listAssociationVersionsResponse_httpStatus' - The response's http status code.
newListAssociationVersionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListAssociationVersionsResponse
newListAssociationVersionsResponse pHttpStatus_ =
  ListAssociationVersionsResponse'
    { nextToken =
        Core.Nothing,
      associationVersions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
listAssociationVersionsResponse_nextToken :: Lens.Lens' ListAssociationVersionsResponse (Core.Maybe Core.Text)
listAssociationVersionsResponse_nextToken = Lens.lens (\ListAssociationVersionsResponse' {nextToken} -> nextToken) (\s@ListAssociationVersionsResponse' {} a -> s {nextToken = a} :: ListAssociationVersionsResponse)

-- | Information about all versions of the association for the specified
-- association ID.
listAssociationVersionsResponse_associationVersions :: Lens.Lens' ListAssociationVersionsResponse (Core.Maybe (Core.NonEmpty AssociationVersionInfo))
listAssociationVersionsResponse_associationVersions = Lens.lens (\ListAssociationVersionsResponse' {associationVersions} -> associationVersions) (\s@ListAssociationVersionsResponse' {} a -> s {associationVersions = a} :: ListAssociationVersionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listAssociationVersionsResponse_httpStatus :: Lens.Lens' ListAssociationVersionsResponse Core.Int
listAssociationVersionsResponse_httpStatus = Lens.lens (\ListAssociationVersionsResponse' {httpStatus} -> httpStatus) (\s@ListAssociationVersionsResponse' {} a -> s {httpStatus = a} :: ListAssociationVersionsResponse)

instance Core.NFData ListAssociationVersionsResponse
