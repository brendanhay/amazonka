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
-- Module      : Network.AWS.SageMaker.ListAssociations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the associations in your account and their properties.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListAssociations
  ( -- * Creating a Request
    ListAssociations (..),
    newListAssociations,

    -- * Request Lenses
    listAssociations_createdAfter,
    listAssociations_sortOrder,
    listAssociations_nextToken,
    listAssociations_destinationType,
    listAssociations_createdBefore,
    listAssociations_destinationArn,
    listAssociations_maxResults,
    listAssociations_sortBy,
    listAssociations_associationType,
    listAssociations_sourceArn,
    listAssociations_sourceType,

    -- * Destructuring the Response
    ListAssociationsResponse (..),
    newListAssociationsResponse,

    -- * Response Lenses
    listAssociationsResponse_nextToken,
    listAssociationsResponse_associationSummaries,
    listAssociationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListAssociations' smart constructor.
data ListAssociations = ListAssociations'
  { -- | A filter that returns only associations created on or after the
    -- specified time.
    createdAfter :: Core.Maybe Core.POSIX,
    -- | The sort order. The default value is @Descending@.
    sortOrder :: Core.Maybe SortOrder,
    -- | If the previous call to @ListAssociations@ didn\'t return the full set
    -- of associations, the call returns a token for getting the next set of
    -- associations.
    nextToken :: Core.Maybe Core.Text,
    -- | A filter that returns only associations with the specified destination
    -- type.
    destinationType :: Core.Maybe Core.Text,
    -- | A filter that returns only associations created on or before the
    -- specified time.
    createdBefore :: Core.Maybe Core.POSIX,
    -- | A filter that returns only associations with the specified destination
    -- Amazon Resource Name (ARN).
    destinationArn :: Core.Maybe Core.Text,
    -- | The maximum number of associations to return in the response. The
    -- default value is 10.
    maxResults :: Core.Maybe Core.Natural,
    -- | The property used to sort results. The default value is @CreationTime@.
    sortBy :: Core.Maybe SortAssociationsBy,
    -- | A filter that returns only associations of the specified type.
    associationType :: Core.Maybe AssociationEdgeType,
    -- | A filter that returns only associations with the specified source ARN.
    sourceArn :: Core.Maybe Core.Text,
    -- | A filter that returns only associations with the specified source type.
    sourceType :: Core.Maybe Core.Text
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
-- 'createdAfter', 'listAssociations_createdAfter' - A filter that returns only associations created on or after the
-- specified time.
--
-- 'sortOrder', 'listAssociations_sortOrder' - The sort order. The default value is @Descending@.
--
-- 'nextToken', 'listAssociations_nextToken' - If the previous call to @ListAssociations@ didn\'t return the full set
-- of associations, the call returns a token for getting the next set of
-- associations.
--
-- 'destinationType', 'listAssociations_destinationType' - A filter that returns only associations with the specified destination
-- type.
--
-- 'createdBefore', 'listAssociations_createdBefore' - A filter that returns only associations created on or before the
-- specified time.
--
-- 'destinationArn', 'listAssociations_destinationArn' - A filter that returns only associations with the specified destination
-- Amazon Resource Name (ARN).
--
-- 'maxResults', 'listAssociations_maxResults' - The maximum number of associations to return in the response. The
-- default value is 10.
--
-- 'sortBy', 'listAssociations_sortBy' - The property used to sort results. The default value is @CreationTime@.
--
-- 'associationType', 'listAssociations_associationType' - A filter that returns only associations of the specified type.
--
-- 'sourceArn', 'listAssociations_sourceArn' - A filter that returns only associations with the specified source ARN.
--
-- 'sourceType', 'listAssociations_sourceType' - A filter that returns only associations with the specified source type.
newListAssociations ::
  ListAssociations
newListAssociations =
  ListAssociations'
    { createdAfter = Core.Nothing,
      sortOrder = Core.Nothing,
      nextToken = Core.Nothing,
      destinationType = Core.Nothing,
      createdBefore = Core.Nothing,
      destinationArn = Core.Nothing,
      maxResults = Core.Nothing,
      sortBy = Core.Nothing,
      associationType = Core.Nothing,
      sourceArn = Core.Nothing,
      sourceType = Core.Nothing
    }

-- | A filter that returns only associations created on or after the
-- specified time.
listAssociations_createdAfter :: Lens.Lens' ListAssociations (Core.Maybe Core.UTCTime)
listAssociations_createdAfter = Lens.lens (\ListAssociations' {createdAfter} -> createdAfter) (\s@ListAssociations' {} a -> s {createdAfter = a} :: ListAssociations) Core.. Lens.mapping Core._Time

-- | The sort order. The default value is @Descending@.
listAssociations_sortOrder :: Lens.Lens' ListAssociations (Core.Maybe SortOrder)
listAssociations_sortOrder = Lens.lens (\ListAssociations' {sortOrder} -> sortOrder) (\s@ListAssociations' {} a -> s {sortOrder = a} :: ListAssociations)

-- | If the previous call to @ListAssociations@ didn\'t return the full set
-- of associations, the call returns a token for getting the next set of
-- associations.
listAssociations_nextToken :: Lens.Lens' ListAssociations (Core.Maybe Core.Text)
listAssociations_nextToken = Lens.lens (\ListAssociations' {nextToken} -> nextToken) (\s@ListAssociations' {} a -> s {nextToken = a} :: ListAssociations)

-- | A filter that returns only associations with the specified destination
-- type.
listAssociations_destinationType :: Lens.Lens' ListAssociations (Core.Maybe Core.Text)
listAssociations_destinationType = Lens.lens (\ListAssociations' {destinationType} -> destinationType) (\s@ListAssociations' {} a -> s {destinationType = a} :: ListAssociations)

-- | A filter that returns only associations created on or before the
-- specified time.
listAssociations_createdBefore :: Lens.Lens' ListAssociations (Core.Maybe Core.UTCTime)
listAssociations_createdBefore = Lens.lens (\ListAssociations' {createdBefore} -> createdBefore) (\s@ListAssociations' {} a -> s {createdBefore = a} :: ListAssociations) Core.. Lens.mapping Core._Time

-- | A filter that returns only associations with the specified destination
-- Amazon Resource Name (ARN).
listAssociations_destinationArn :: Lens.Lens' ListAssociations (Core.Maybe Core.Text)
listAssociations_destinationArn = Lens.lens (\ListAssociations' {destinationArn} -> destinationArn) (\s@ListAssociations' {} a -> s {destinationArn = a} :: ListAssociations)

-- | The maximum number of associations to return in the response. The
-- default value is 10.
listAssociations_maxResults :: Lens.Lens' ListAssociations (Core.Maybe Core.Natural)
listAssociations_maxResults = Lens.lens (\ListAssociations' {maxResults} -> maxResults) (\s@ListAssociations' {} a -> s {maxResults = a} :: ListAssociations)

-- | The property used to sort results. The default value is @CreationTime@.
listAssociations_sortBy :: Lens.Lens' ListAssociations (Core.Maybe SortAssociationsBy)
listAssociations_sortBy = Lens.lens (\ListAssociations' {sortBy} -> sortBy) (\s@ListAssociations' {} a -> s {sortBy = a} :: ListAssociations)

-- | A filter that returns only associations of the specified type.
listAssociations_associationType :: Lens.Lens' ListAssociations (Core.Maybe AssociationEdgeType)
listAssociations_associationType = Lens.lens (\ListAssociations' {associationType} -> associationType) (\s@ListAssociations' {} a -> s {associationType = a} :: ListAssociations)

-- | A filter that returns only associations with the specified source ARN.
listAssociations_sourceArn :: Lens.Lens' ListAssociations (Core.Maybe Core.Text)
listAssociations_sourceArn = Lens.lens (\ListAssociations' {sourceArn} -> sourceArn) (\s@ListAssociations' {} a -> s {sourceArn = a} :: ListAssociations)

-- | A filter that returns only associations with the specified source type.
listAssociations_sourceType :: Lens.Lens' ListAssociations (Core.Maybe Core.Text)
listAssociations_sourceType = Lens.lens (\ListAssociations' {sourceType} -> sourceType) (\s@ListAssociations' {} a -> s {sourceType = a} :: ListAssociations)

instance Core.AWSPager ListAssociations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAssociationsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listAssociationsResponse_associationSummaries
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
            Core.<*> ( x Core..?> "AssociationSummaries"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListAssociations

instance Core.NFData ListAssociations

instance Core.ToHeaders ListAssociations where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListAssociations" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListAssociations where
  toJSON ListAssociations' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CreatedAfter" Core..=) Core.<$> createdAfter,
            ("SortOrder" Core..=) Core.<$> sortOrder,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("DestinationType" Core..=) Core.<$> destinationType,
            ("CreatedBefore" Core..=) Core.<$> createdBefore,
            ("DestinationArn" Core..=) Core.<$> destinationArn,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("AssociationType" Core..=) Core.<$> associationType,
            ("SourceArn" Core..=) Core.<$> sourceArn,
            ("SourceType" Core..=) Core.<$> sourceType
          ]
      )

instance Core.ToPath ListAssociations where
  toPath = Core.const "/"

instance Core.ToQuery ListAssociations where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListAssociationsResponse' smart constructor.
data ListAssociationsResponse = ListAssociationsResponse'
  { -- | A token for getting the next set of associations, if there are any.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of associations and their properties.
    associationSummaries :: Core.Maybe [AssociationSummary],
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
-- 'nextToken', 'listAssociationsResponse_nextToken' - A token for getting the next set of associations, if there are any.
--
-- 'associationSummaries', 'listAssociationsResponse_associationSummaries' - A list of associations and their properties.
--
-- 'httpStatus', 'listAssociationsResponse_httpStatus' - The response's http status code.
newListAssociationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListAssociationsResponse
newListAssociationsResponse pHttpStatus_ =
  ListAssociationsResponse'
    { nextToken = Core.Nothing,
      associationSummaries = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token for getting the next set of associations, if there are any.
listAssociationsResponse_nextToken :: Lens.Lens' ListAssociationsResponse (Core.Maybe Core.Text)
listAssociationsResponse_nextToken = Lens.lens (\ListAssociationsResponse' {nextToken} -> nextToken) (\s@ListAssociationsResponse' {} a -> s {nextToken = a} :: ListAssociationsResponse)

-- | A list of associations and their properties.
listAssociationsResponse_associationSummaries :: Lens.Lens' ListAssociationsResponse (Core.Maybe [AssociationSummary])
listAssociationsResponse_associationSummaries = Lens.lens (\ListAssociationsResponse' {associationSummaries} -> associationSummaries) (\s@ListAssociationsResponse' {} a -> s {associationSummaries = a} :: ListAssociationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listAssociationsResponse_httpStatus :: Lens.Lens' ListAssociationsResponse Core.Int
listAssociationsResponse_httpStatus = Lens.lens (\ListAssociationsResponse' {httpStatus} -> httpStatus) (\s@ListAssociationsResponse' {} a -> s {httpStatus = a} :: ListAssociationsResponse)

instance Core.NFData ListAssociationsResponse
