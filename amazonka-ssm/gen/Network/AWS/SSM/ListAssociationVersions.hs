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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newListAssociationVersions' smart constructor.
data ListAssociationVersions = ListAssociationVersions'
  { -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The association ID for which you want to view all versions.
    associationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ListAssociationVersions
newListAssociationVersions pAssociationId_ =
  ListAssociationVersions'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      associationId = pAssociationId_
    }

-- | A token to start the list. Use this token to get the next set of
-- results.
listAssociationVersions_nextToken :: Lens.Lens' ListAssociationVersions (Prelude.Maybe Prelude.Text)
listAssociationVersions_nextToken = Lens.lens (\ListAssociationVersions' {nextToken} -> nextToken) (\s@ListAssociationVersions' {} a -> s {nextToken = a} :: ListAssociationVersions)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
listAssociationVersions_maxResults :: Lens.Lens' ListAssociationVersions (Prelude.Maybe Prelude.Natural)
listAssociationVersions_maxResults = Lens.lens (\ListAssociationVersions' {maxResults} -> maxResults) (\s@ListAssociationVersions' {} a -> s {maxResults = a} :: ListAssociationVersions)

-- | The association ID for which you want to view all versions.
listAssociationVersions_associationId :: Lens.Lens' ListAssociationVersions Prelude.Text
listAssociationVersions_associationId = Lens.lens (\ListAssociationVersions' {associationId} -> associationId) (\s@ListAssociationVersions' {} a -> s {associationId = a} :: ListAssociationVersions)

instance Core.AWSPager ListAssociationVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAssociationVersionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAssociationVersionsResponse_associationVersions
              Prelude.. Lens._Just
              Prelude.. Lens.to Prelude.toList
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAssociationVersions_nextToken
          Lens..~ rs
          Lens.^? listAssociationVersionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListAssociationVersions where
  type
    AWSResponse ListAssociationVersions =
      ListAssociationVersionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssociationVersionsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "AssociationVersions")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAssociationVersions

instance Prelude.NFData ListAssociationVersions

instance Core.ToHeaders ListAssociationVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.ListAssociationVersions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListAssociationVersions where
  toJSON ListAssociationVersions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just
              ("AssociationId" Core..= associationId)
          ]
      )

instance Core.ToPath ListAssociationVersions where
  toPath = Prelude.const "/"

instance Core.ToQuery ListAssociationVersions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAssociationVersionsResponse' smart constructor.
data ListAssociationVersionsResponse = ListAssociationVersionsResponse'
  { -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about all versions of the association for the specified
    -- association ID.
    associationVersions :: Prelude.Maybe (Prelude.NonEmpty AssociationVersionInfo),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListAssociationVersionsResponse
newListAssociationVersionsResponse pHttpStatus_ =
  ListAssociationVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      associationVersions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
listAssociationVersionsResponse_nextToken :: Lens.Lens' ListAssociationVersionsResponse (Prelude.Maybe Prelude.Text)
listAssociationVersionsResponse_nextToken = Lens.lens (\ListAssociationVersionsResponse' {nextToken} -> nextToken) (\s@ListAssociationVersionsResponse' {} a -> s {nextToken = a} :: ListAssociationVersionsResponse)

-- | Information about all versions of the association for the specified
-- association ID.
listAssociationVersionsResponse_associationVersions :: Lens.Lens' ListAssociationVersionsResponse (Prelude.Maybe (Prelude.NonEmpty AssociationVersionInfo))
listAssociationVersionsResponse_associationVersions = Lens.lens (\ListAssociationVersionsResponse' {associationVersions} -> associationVersions) (\s@ListAssociationVersionsResponse' {} a -> s {associationVersions = a} :: ListAssociationVersionsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listAssociationVersionsResponse_httpStatus :: Lens.Lens' ListAssociationVersionsResponse Prelude.Int
listAssociationVersionsResponse_httpStatus = Lens.lens (\ListAssociationVersionsResponse' {httpStatus} -> httpStatus) (\s@ListAssociationVersionsResponse' {} a -> s {httpStatus = a} :: ListAssociationVersionsResponse)

instance
  Prelude.NFData
    ListAssociationVersionsResponse
