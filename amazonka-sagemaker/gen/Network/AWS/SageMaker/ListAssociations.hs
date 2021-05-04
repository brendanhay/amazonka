{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListAssociations' smart constructor.
data ListAssociations = ListAssociations'
  { -- | A filter that returns only associations created on or after the
    -- specified time.
    createdAfter :: Prelude.Maybe Prelude.POSIX,
    -- | The sort order. The default value is @Descending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | If the previous call to @ListAssociations@ didn\'t return the full set
    -- of associations, the call returns a token for getting the next set of
    -- associations.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only associations with the specified destination
    -- type.
    destinationType :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only associations created on or before the
    -- specified time.
    createdBefore :: Prelude.Maybe Prelude.POSIX,
    -- | A filter that returns only associations with the specified destination
    -- Amazon Resource Name (ARN).
    destinationArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of associations to return in the response. The
    -- default value is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The property used to sort results. The default value is @CreationTime@.
    sortBy :: Prelude.Maybe SortAssociationsBy,
    -- | A filter that returns only associations of the specified type.
    associationType :: Prelude.Maybe AssociationEdgeType,
    -- | A filter that returns only associations with the specified source ARN.
    sourceArn :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only associations with the specified source type.
    sourceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { createdAfter = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      destinationType = Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      destinationArn = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      associationType = Prelude.Nothing,
      sourceArn = Prelude.Nothing,
      sourceType = Prelude.Nothing
    }

-- | A filter that returns only associations created on or after the
-- specified time.
listAssociations_createdAfter :: Lens.Lens' ListAssociations (Prelude.Maybe Prelude.UTCTime)
listAssociations_createdAfter = Lens.lens (\ListAssociations' {createdAfter} -> createdAfter) (\s@ListAssociations' {} a -> s {createdAfter = a} :: ListAssociations) Prelude.. Lens.mapping Prelude._Time

-- | The sort order. The default value is @Descending@.
listAssociations_sortOrder :: Lens.Lens' ListAssociations (Prelude.Maybe SortOrder)
listAssociations_sortOrder = Lens.lens (\ListAssociations' {sortOrder} -> sortOrder) (\s@ListAssociations' {} a -> s {sortOrder = a} :: ListAssociations)

-- | If the previous call to @ListAssociations@ didn\'t return the full set
-- of associations, the call returns a token for getting the next set of
-- associations.
listAssociations_nextToken :: Lens.Lens' ListAssociations (Prelude.Maybe Prelude.Text)
listAssociations_nextToken = Lens.lens (\ListAssociations' {nextToken} -> nextToken) (\s@ListAssociations' {} a -> s {nextToken = a} :: ListAssociations)

-- | A filter that returns only associations with the specified destination
-- type.
listAssociations_destinationType :: Lens.Lens' ListAssociations (Prelude.Maybe Prelude.Text)
listAssociations_destinationType = Lens.lens (\ListAssociations' {destinationType} -> destinationType) (\s@ListAssociations' {} a -> s {destinationType = a} :: ListAssociations)

-- | A filter that returns only associations created on or before the
-- specified time.
listAssociations_createdBefore :: Lens.Lens' ListAssociations (Prelude.Maybe Prelude.UTCTime)
listAssociations_createdBefore = Lens.lens (\ListAssociations' {createdBefore} -> createdBefore) (\s@ListAssociations' {} a -> s {createdBefore = a} :: ListAssociations) Prelude.. Lens.mapping Prelude._Time

-- | A filter that returns only associations with the specified destination
-- Amazon Resource Name (ARN).
listAssociations_destinationArn :: Lens.Lens' ListAssociations (Prelude.Maybe Prelude.Text)
listAssociations_destinationArn = Lens.lens (\ListAssociations' {destinationArn} -> destinationArn) (\s@ListAssociations' {} a -> s {destinationArn = a} :: ListAssociations)

-- | The maximum number of associations to return in the response. The
-- default value is 10.
listAssociations_maxResults :: Lens.Lens' ListAssociations (Prelude.Maybe Prelude.Natural)
listAssociations_maxResults = Lens.lens (\ListAssociations' {maxResults} -> maxResults) (\s@ListAssociations' {} a -> s {maxResults = a} :: ListAssociations)

-- | The property used to sort results. The default value is @CreationTime@.
listAssociations_sortBy :: Lens.Lens' ListAssociations (Prelude.Maybe SortAssociationsBy)
listAssociations_sortBy = Lens.lens (\ListAssociations' {sortBy} -> sortBy) (\s@ListAssociations' {} a -> s {sortBy = a} :: ListAssociations)

-- | A filter that returns only associations of the specified type.
listAssociations_associationType :: Lens.Lens' ListAssociations (Prelude.Maybe AssociationEdgeType)
listAssociations_associationType = Lens.lens (\ListAssociations' {associationType} -> associationType) (\s@ListAssociations' {} a -> s {associationType = a} :: ListAssociations)

-- | A filter that returns only associations with the specified source ARN.
listAssociations_sourceArn :: Lens.Lens' ListAssociations (Prelude.Maybe Prelude.Text)
listAssociations_sourceArn = Lens.lens (\ListAssociations' {sourceArn} -> sourceArn) (\s@ListAssociations' {} a -> s {sourceArn = a} :: ListAssociations)

-- | A filter that returns only associations with the specified source type.
listAssociations_sourceType :: Lens.Lens' ListAssociations (Prelude.Maybe Prelude.Text)
listAssociations_sourceType = Lens.lens (\ListAssociations' {sourceType} -> sourceType) (\s@ListAssociations' {} a -> s {sourceType = a} :: ListAssociations)

instance Pager.AWSPager ListAssociations where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listAssociationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listAssociationsResponse_associationSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listAssociations_nextToken
          Lens..~ rs
          Lens.^? listAssociationsResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListAssociations where
  type Rs ListAssociations = ListAssociationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssociationsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "AssociationSummaries"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAssociations

instance Prelude.NFData ListAssociations

instance Prelude.ToHeaders ListAssociations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.ListAssociations" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListAssociations where
  toJSON ListAssociations' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CreatedAfter" Prelude..=)
              Prelude.<$> createdAfter,
            ("SortOrder" Prelude..=) Prelude.<$> sortOrder,
            ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("DestinationType" Prelude..=)
              Prelude.<$> destinationType,
            ("CreatedBefore" Prelude..=)
              Prelude.<$> createdBefore,
            ("DestinationArn" Prelude..=)
              Prelude.<$> destinationArn,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            ("SortBy" Prelude..=) Prelude.<$> sortBy,
            ("AssociationType" Prelude..=)
              Prelude.<$> associationType,
            ("SourceArn" Prelude..=) Prelude.<$> sourceArn,
            ("SourceType" Prelude..=) Prelude.<$> sourceType
          ]
      )

instance Prelude.ToPath ListAssociations where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListAssociations where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
listAssociationsResponse_associationSummaries = Lens.lens (\ListAssociationsResponse' {associationSummaries} -> associationSummaries) (\s@ListAssociationsResponse' {} a -> s {associationSummaries = a} :: ListAssociationsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listAssociationsResponse_httpStatus :: Lens.Lens' ListAssociationsResponse Prelude.Int
listAssociationsResponse_httpStatus = Lens.lens (\ListAssociationsResponse' {httpStatus} -> httpStatus) (\s@ListAssociationsResponse' {} a -> s {httpStatus = a} :: ListAssociationsResponse)

instance Prelude.NFData ListAssociationsResponse
