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
-- Module      : Network.AWS.SSM.ListAssociations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all State Manager associations in the current AWS account and
-- Region. You can limit the results to a specific State Manager
-- association document or instance by specifying a filter.
--
-- This operation returns paginated results.
module Network.AWS.SSM.ListAssociations
  ( -- * Creating a Request
    ListAssociations (..),
    newListAssociations,

    -- * Request Lenses
    listAssociations_nextToken,
    listAssociations_maxResults,
    listAssociations_associationFilterList,

    -- * Destructuring the Response
    ListAssociationsResponse (..),
    newListAssociationsResponse,

    -- * Response Lenses
    listAssociationsResponse_nextToken,
    listAssociationsResponse_associations,
    listAssociationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newListAssociations' smart constructor.
data ListAssociations = ListAssociations'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | One or more filters. Use a filter to return a more specific list of
    -- results.
    --
    -- Filtering associations using the @InstanceID@ attribute only returns
    -- legacy associations created using the @InstanceID@ attribute.
    -- Associations targeting the instance that are part of the Target
    -- Attributes @ResourceGroup@ or @Tags@ are not returned.
    associationFilterList :: Core.Maybe (Core.NonEmpty AssociationFilter)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssociations_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'maxResults', 'listAssociations_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'associationFilterList', 'listAssociations_associationFilterList' - One or more filters. Use a filter to return a more specific list of
-- results.
--
-- Filtering associations using the @InstanceID@ attribute only returns
-- legacy associations created using the @InstanceID@ attribute.
-- Associations targeting the instance that are part of the Target
-- Attributes @ResourceGroup@ or @Tags@ are not returned.
newListAssociations ::
  ListAssociations
newListAssociations =
  ListAssociations'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      associationFilterList = Core.Nothing
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
listAssociations_nextToken :: Lens.Lens' ListAssociations (Core.Maybe Core.Text)
listAssociations_nextToken = Lens.lens (\ListAssociations' {nextToken} -> nextToken) (\s@ListAssociations' {} a -> s {nextToken = a} :: ListAssociations)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
listAssociations_maxResults :: Lens.Lens' ListAssociations (Core.Maybe Core.Natural)
listAssociations_maxResults = Lens.lens (\ListAssociations' {maxResults} -> maxResults) (\s@ListAssociations' {} a -> s {maxResults = a} :: ListAssociations)

-- | One or more filters. Use a filter to return a more specific list of
-- results.
--
-- Filtering associations using the @InstanceID@ attribute only returns
-- legacy associations created using the @InstanceID@ attribute.
-- Associations targeting the instance that are part of the Target
-- Attributes @ResourceGroup@ or @Tags@ are not returned.
listAssociations_associationFilterList :: Lens.Lens' ListAssociations (Core.Maybe (Core.NonEmpty AssociationFilter))
listAssociations_associationFilterList = Lens.lens (\ListAssociations' {associationFilterList} -> associationFilterList) (\s@ListAssociations' {} a -> s {associationFilterList = a} :: ListAssociations) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager ListAssociations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAssociationsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listAssociationsResponse_associations
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listAssociations_nextToken
          Lens..~ rs
          Lens.^? listAssociationsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListAssociations where
  type
    AWSResponse ListAssociations =
      ListAssociationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssociationsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Associations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListAssociations

instance Core.NFData ListAssociations

instance Core.ToHeaders ListAssociations where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.ListAssociations" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListAssociations where
  toJSON ListAssociations' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("AssociationFilterList" Core..=)
              Core.<$> associationFilterList
          ]
      )

instance Core.ToPath ListAssociations where
  toPath = Core.const "/"

instance Core.ToQuery ListAssociations where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListAssociationsResponse' smart constructor.
data ListAssociationsResponse = ListAssociationsResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Core.Maybe Core.Text,
    -- | The associations.
    associations :: Core.Maybe [Association],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssociationsResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'associations', 'listAssociationsResponse_associations' - The associations.
--
-- 'httpStatus', 'listAssociationsResponse_httpStatus' - The response's http status code.
newListAssociationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListAssociationsResponse
newListAssociationsResponse pHttpStatus_ =
  ListAssociationsResponse'
    { nextToken = Core.Nothing,
      associations = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
listAssociationsResponse_nextToken :: Lens.Lens' ListAssociationsResponse (Core.Maybe Core.Text)
listAssociationsResponse_nextToken = Lens.lens (\ListAssociationsResponse' {nextToken} -> nextToken) (\s@ListAssociationsResponse' {} a -> s {nextToken = a} :: ListAssociationsResponse)

-- | The associations.
listAssociationsResponse_associations :: Lens.Lens' ListAssociationsResponse (Core.Maybe [Association])
listAssociationsResponse_associations = Lens.lens (\ListAssociationsResponse' {associations} -> associations) (\s@ListAssociationsResponse' {} a -> s {associations = a} :: ListAssociationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listAssociationsResponse_httpStatus :: Lens.Lens' ListAssociationsResponse Core.Int
listAssociationsResponse_httpStatus = Lens.lens (\ListAssociationsResponse' {httpStatus} -> httpStatus) (\s@ListAssociationsResponse' {} a -> s {httpStatus = a} :: ListAssociationsResponse)

instance Core.NFData ListAssociationsResponse
