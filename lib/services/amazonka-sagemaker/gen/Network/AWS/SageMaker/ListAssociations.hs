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
    listAssociations_sourceType,
    listAssociations_sourceArn,
    listAssociations_associationType,
    listAssociations_destinationArn,
    listAssociations_nextToken,
    listAssociations_destinationType,
    listAssociations_sortOrder,
    listAssociations_maxResults,
    listAssociations_createdBefore,
    listAssociations_sortBy,

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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListAssociations' smart constructor.
data ListAssociations = ListAssociations'
  { -- | A filter that returns only associations created on or after the
    -- specified time.
    createdAfter :: Prelude.Maybe Core.POSIX,
    -- | A filter that returns only associations with the specified source type.
    sourceType :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only associations with the specified source ARN.
    sourceArn :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only associations of the specified type.
    associationType :: Prelude.Maybe AssociationEdgeType,
    -- | A filter that returns only associations with the specified destination
    -- Amazon Resource Name (ARN).
    destinationArn :: Prelude.Maybe Prelude.Text,
    -- | If the previous call to @ListAssociations@ didn\'t return the full set
    -- of associations, the call returns a token for getting the next set of
    -- associations.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only associations with the specified destination
    -- type.
    destinationType :: Prelude.Maybe Prelude.Text,
    -- | The sort order. The default value is @Descending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | The maximum number of associations to return in the response. The
    -- default value is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A filter that returns only associations created on or before the
    -- specified time.
    createdBefore :: Prelude.Maybe Core.POSIX,
    -- | The property used to sort results. The default value is @CreationTime@.
    sortBy :: Prelude.Maybe SortAssociationsBy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'sourceType', 'listAssociations_sourceType' - A filter that returns only associations with the specified source type.
--
-- 'sourceArn', 'listAssociations_sourceArn' - A filter that returns only associations with the specified source ARN.
--
-- 'associationType', 'listAssociations_associationType' - A filter that returns only associations of the specified type.
--
-- 'destinationArn', 'listAssociations_destinationArn' - A filter that returns only associations with the specified destination
-- Amazon Resource Name (ARN).
--
-- 'nextToken', 'listAssociations_nextToken' - If the previous call to @ListAssociations@ didn\'t return the full set
-- of associations, the call returns a token for getting the next set of
-- associations.
--
-- 'destinationType', 'listAssociations_destinationType' - A filter that returns only associations with the specified destination
-- type.
--
-- 'sortOrder', 'listAssociations_sortOrder' - The sort order. The default value is @Descending@.
--
-- 'maxResults', 'listAssociations_maxResults' - The maximum number of associations to return in the response. The
-- default value is 10.
--
-- 'createdBefore', 'listAssociations_createdBefore' - A filter that returns only associations created on or before the
-- specified time.
--
-- 'sortBy', 'listAssociations_sortBy' - The property used to sort results. The default value is @CreationTime@.
newListAssociations ::
  ListAssociations
newListAssociations =
  ListAssociations'
    { createdAfter = Prelude.Nothing,
      sourceType = Prelude.Nothing,
      sourceArn = Prelude.Nothing,
      associationType = Prelude.Nothing,
      destinationArn = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      destinationType = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      sortBy = Prelude.Nothing
    }

-- | A filter that returns only associations created on or after the
-- specified time.
listAssociations_createdAfter :: Lens.Lens' ListAssociations (Prelude.Maybe Prelude.UTCTime)
listAssociations_createdAfter = Lens.lens (\ListAssociations' {createdAfter} -> createdAfter) (\s@ListAssociations' {} a -> s {createdAfter = a} :: ListAssociations) Prelude.. Lens.mapping Core._Time

-- | A filter that returns only associations with the specified source type.
listAssociations_sourceType :: Lens.Lens' ListAssociations (Prelude.Maybe Prelude.Text)
listAssociations_sourceType = Lens.lens (\ListAssociations' {sourceType} -> sourceType) (\s@ListAssociations' {} a -> s {sourceType = a} :: ListAssociations)

-- | A filter that returns only associations with the specified source ARN.
listAssociations_sourceArn :: Lens.Lens' ListAssociations (Prelude.Maybe Prelude.Text)
listAssociations_sourceArn = Lens.lens (\ListAssociations' {sourceArn} -> sourceArn) (\s@ListAssociations' {} a -> s {sourceArn = a} :: ListAssociations)

-- | A filter that returns only associations of the specified type.
listAssociations_associationType :: Lens.Lens' ListAssociations (Prelude.Maybe AssociationEdgeType)
listAssociations_associationType = Lens.lens (\ListAssociations' {associationType} -> associationType) (\s@ListAssociations' {} a -> s {associationType = a} :: ListAssociations)

-- | A filter that returns only associations with the specified destination
-- Amazon Resource Name (ARN).
listAssociations_destinationArn :: Lens.Lens' ListAssociations (Prelude.Maybe Prelude.Text)
listAssociations_destinationArn = Lens.lens (\ListAssociations' {destinationArn} -> destinationArn) (\s@ListAssociations' {} a -> s {destinationArn = a} :: ListAssociations)

-- | If the previous call to @ListAssociations@ didn\'t return the full set
-- of associations, the call returns a token for getting the next set of
-- associations.
listAssociations_nextToken :: Lens.Lens' ListAssociations (Prelude.Maybe Prelude.Text)
listAssociations_nextToken = Lens.lens (\ListAssociations' {nextToken} -> nextToken) (\s@ListAssociations' {} a -> s {nextToken = a} :: ListAssociations)

-- | A filter that returns only associations with the specified destination
-- type.
listAssociations_destinationType :: Lens.Lens' ListAssociations (Prelude.Maybe Prelude.Text)
listAssociations_destinationType = Lens.lens (\ListAssociations' {destinationType} -> destinationType) (\s@ListAssociations' {} a -> s {destinationType = a} :: ListAssociations)

-- | The sort order. The default value is @Descending@.
listAssociations_sortOrder :: Lens.Lens' ListAssociations (Prelude.Maybe SortOrder)
listAssociations_sortOrder = Lens.lens (\ListAssociations' {sortOrder} -> sortOrder) (\s@ListAssociations' {} a -> s {sortOrder = a} :: ListAssociations)

-- | The maximum number of associations to return in the response. The
-- default value is 10.
listAssociations_maxResults :: Lens.Lens' ListAssociations (Prelude.Maybe Prelude.Natural)
listAssociations_maxResults = Lens.lens (\ListAssociations' {maxResults} -> maxResults) (\s@ListAssociations' {} a -> s {maxResults = a} :: ListAssociations)

-- | A filter that returns only associations created on or before the
-- specified time.
listAssociations_createdBefore :: Lens.Lens' ListAssociations (Prelude.Maybe Prelude.UTCTime)
listAssociations_createdBefore = Lens.lens (\ListAssociations' {createdBefore} -> createdBefore) (\s@ListAssociations' {} a -> s {createdBefore = a} :: ListAssociations) Prelude.. Lens.mapping Core._Time

-- | The property used to sort results. The default value is @CreationTime@.
listAssociations_sortBy :: Lens.Lens' ListAssociations (Prelude.Maybe SortAssociationsBy)
listAssociations_sortBy = Lens.lens (\ListAssociations' {sortBy} -> sortBy) (\s@ListAssociations' {} a -> s {sortBy = a} :: ListAssociations)

instance Core.AWSPager ListAssociations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAssociationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAssociationsResponse_associationSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAssociations_nextToken
          Lens..~ rs
          Lens.^? listAssociationsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListAssociations where
  type
    AWSResponse ListAssociations =
      ListAssociationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssociationsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "AssociationSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAssociations

instance Prelude.NFData ListAssociations

instance Core.ToHeaders ListAssociations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListAssociations" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListAssociations where
  toJSON ListAssociations' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CreatedAfter" Core..=) Prelude.<$> createdAfter,
            ("SourceType" Core..=) Prelude.<$> sourceType,
            ("SourceArn" Core..=) Prelude.<$> sourceArn,
            ("AssociationType" Core..=)
              Prelude.<$> associationType,
            ("DestinationArn" Core..=)
              Prelude.<$> destinationArn,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("DestinationType" Core..=)
              Prelude.<$> destinationType,
            ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("CreatedBefore" Core..=) Prelude.<$> createdBefore,
            ("SortBy" Core..=) Prelude.<$> sortBy
          ]
      )

instance Core.ToPath ListAssociations where
  toPath = Prelude.const "/"

instance Core.ToQuery ListAssociations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAssociationsResponse' smart constructor.
data ListAssociationsResponse = ListAssociationsResponse'
  { -- | A token for getting the next set of associations, if there are any.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of associations and their properties.
    associationSummaries :: Prelude.Maybe [AssociationSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListAssociationsResponse
newListAssociationsResponse pHttpStatus_ =
  ListAssociationsResponse'
    { nextToken =
        Prelude.Nothing,
      associationSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token for getting the next set of associations, if there are any.
listAssociationsResponse_nextToken :: Lens.Lens' ListAssociationsResponse (Prelude.Maybe Prelude.Text)
listAssociationsResponse_nextToken = Lens.lens (\ListAssociationsResponse' {nextToken} -> nextToken) (\s@ListAssociationsResponse' {} a -> s {nextToken = a} :: ListAssociationsResponse)

-- | A list of associations and their properties.
listAssociationsResponse_associationSummaries :: Lens.Lens' ListAssociationsResponse (Prelude.Maybe [AssociationSummary])
listAssociationsResponse_associationSummaries = Lens.lens (\ListAssociationsResponse' {associationSummaries} -> associationSummaries) (\s@ListAssociationsResponse' {} a -> s {associationSummaries = a} :: ListAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAssociationsResponse_httpStatus :: Lens.Lens' ListAssociationsResponse Prelude.Int
listAssociationsResponse_httpStatus = Lens.lens (\ListAssociationsResponse' {httpStatus} -> httpStatus) (\s@ListAssociationsResponse' {} a -> s {httpStatus = a} :: ListAssociationsResponse)

instance Prelude.NFData ListAssociationsResponse
