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
-- Module      : Amazonka.SageMaker.ListAssociations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the associations in your account and their properties.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListAssociations
  ( -- * Creating a Request
    ListAssociations (..),
    newListAssociations,

    -- * Request Lenses
    listAssociations_associationType,
    listAssociations_createdAfter,
    listAssociations_createdBefore,
    listAssociations_destinationArn,
    listAssociations_destinationType,
    listAssociations_maxResults,
    listAssociations_nextToken,
    listAssociations_sortBy,
    listAssociations_sortOrder,
    listAssociations_sourceArn,
    listAssociations_sourceType,

    -- * Destructuring the Response
    ListAssociationsResponse (..),
    newListAssociationsResponse,

    -- * Response Lenses
    listAssociationsResponse_associationSummaries,
    listAssociationsResponse_nextToken,
    listAssociationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListAssociations' smart constructor.
data ListAssociations = ListAssociations'
  { -- | A filter that returns only associations of the specified type.
    associationType :: Prelude.Maybe AssociationEdgeType,
    -- | A filter that returns only associations created on or after the
    -- specified time.
    createdAfter :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only associations created on or before the
    -- specified time.
    createdBefore :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only associations with the specified destination
    -- Amazon Resource Name (ARN).
    destinationArn :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only associations with the specified destination
    -- type.
    destinationType :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of associations to return in the response. The
    -- default value is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the previous call to @ListAssociations@ didn\'t return the full set
    -- of associations, the call returns a token for getting the next set of
    -- associations.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The property used to sort results. The default value is @CreationTime@.
    sortBy :: Prelude.Maybe SortAssociationsBy,
    -- | The sort order. The default value is @Descending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | A filter that returns only associations with the specified source ARN.
    sourceArn :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only associations with the specified source type.
    sourceType :: Prelude.Maybe Prelude.Text
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
-- 'associationType', 'listAssociations_associationType' - A filter that returns only associations of the specified type.
--
-- 'createdAfter', 'listAssociations_createdAfter' - A filter that returns only associations created on or after the
-- specified time.
--
-- 'createdBefore', 'listAssociations_createdBefore' - A filter that returns only associations created on or before the
-- specified time.
--
-- 'destinationArn', 'listAssociations_destinationArn' - A filter that returns only associations with the specified destination
-- Amazon Resource Name (ARN).
--
-- 'destinationType', 'listAssociations_destinationType' - A filter that returns only associations with the specified destination
-- type.
--
-- 'maxResults', 'listAssociations_maxResults' - The maximum number of associations to return in the response. The
-- default value is 10.
--
-- 'nextToken', 'listAssociations_nextToken' - If the previous call to @ListAssociations@ didn\'t return the full set
-- of associations, the call returns a token for getting the next set of
-- associations.
--
-- 'sortBy', 'listAssociations_sortBy' - The property used to sort results. The default value is @CreationTime@.
--
-- 'sortOrder', 'listAssociations_sortOrder' - The sort order. The default value is @Descending@.
--
-- 'sourceArn', 'listAssociations_sourceArn' - A filter that returns only associations with the specified source ARN.
--
-- 'sourceType', 'listAssociations_sourceType' - A filter that returns only associations with the specified source type.
newListAssociations ::
  ListAssociations
newListAssociations =
  ListAssociations'
    { associationType =
        Prelude.Nothing,
      createdAfter = Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      destinationArn = Prelude.Nothing,
      destinationType = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      sourceArn = Prelude.Nothing,
      sourceType = Prelude.Nothing
    }

-- | A filter that returns only associations of the specified type.
listAssociations_associationType :: Lens.Lens' ListAssociations (Prelude.Maybe AssociationEdgeType)
listAssociations_associationType = Lens.lens (\ListAssociations' {associationType} -> associationType) (\s@ListAssociations' {} a -> s {associationType = a} :: ListAssociations)

-- | A filter that returns only associations created on or after the
-- specified time.
listAssociations_createdAfter :: Lens.Lens' ListAssociations (Prelude.Maybe Prelude.UTCTime)
listAssociations_createdAfter = Lens.lens (\ListAssociations' {createdAfter} -> createdAfter) (\s@ListAssociations' {} a -> s {createdAfter = a} :: ListAssociations) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only associations created on or before the
-- specified time.
listAssociations_createdBefore :: Lens.Lens' ListAssociations (Prelude.Maybe Prelude.UTCTime)
listAssociations_createdBefore = Lens.lens (\ListAssociations' {createdBefore} -> createdBefore) (\s@ListAssociations' {} a -> s {createdBefore = a} :: ListAssociations) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only associations with the specified destination
-- Amazon Resource Name (ARN).
listAssociations_destinationArn :: Lens.Lens' ListAssociations (Prelude.Maybe Prelude.Text)
listAssociations_destinationArn = Lens.lens (\ListAssociations' {destinationArn} -> destinationArn) (\s@ListAssociations' {} a -> s {destinationArn = a} :: ListAssociations)

-- | A filter that returns only associations with the specified destination
-- type.
listAssociations_destinationType :: Lens.Lens' ListAssociations (Prelude.Maybe Prelude.Text)
listAssociations_destinationType = Lens.lens (\ListAssociations' {destinationType} -> destinationType) (\s@ListAssociations' {} a -> s {destinationType = a} :: ListAssociations)

-- | The maximum number of associations to return in the response. The
-- default value is 10.
listAssociations_maxResults :: Lens.Lens' ListAssociations (Prelude.Maybe Prelude.Natural)
listAssociations_maxResults = Lens.lens (\ListAssociations' {maxResults} -> maxResults) (\s@ListAssociations' {} a -> s {maxResults = a} :: ListAssociations)

-- | If the previous call to @ListAssociations@ didn\'t return the full set
-- of associations, the call returns a token for getting the next set of
-- associations.
listAssociations_nextToken :: Lens.Lens' ListAssociations (Prelude.Maybe Prelude.Text)
listAssociations_nextToken = Lens.lens (\ListAssociations' {nextToken} -> nextToken) (\s@ListAssociations' {} a -> s {nextToken = a} :: ListAssociations)

-- | The property used to sort results. The default value is @CreationTime@.
listAssociations_sortBy :: Lens.Lens' ListAssociations (Prelude.Maybe SortAssociationsBy)
listAssociations_sortBy = Lens.lens (\ListAssociations' {sortBy} -> sortBy) (\s@ListAssociations' {} a -> s {sortBy = a} :: ListAssociations)

-- | The sort order. The default value is @Descending@.
listAssociations_sortOrder :: Lens.Lens' ListAssociations (Prelude.Maybe SortOrder)
listAssociations_sortOrder = Lens.lens (\ListAssociations' {sortOrder} -> sortOrder) (\s@ListAssociations' {} a -> s {sortOrder = a} :: ListAssociations)

-- | A filter that returns only associations with the specified source ARN.
listAssociations_sourceArn :: Lens.Lens' ListAssociations (Prelude.Maybe Prelude.Text)
listAssociations_sourceArn = Lens.lens (\ListAssociations' {sourceArn} -> sourceArn) (\s@ListAssociations' {} a -> s {sourceArn = a} :: ListAssociations)

-- | A filter that returns only associations with the specified source type.
listAssociations_sourceType :: Lens.Lens' ListAssociations (Prelude.Maybe Prelude.Text)
listAssociations_sourceType = Lens.lens (\ListAssociations' {sourceType} -> sourceType) (\s@ListAssociations' {} a -> s {sourceType = a} :: ListAssociations)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssociationsResponse'
            Prelude.<$> ( x
                            Data..?> "AssociationSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAssociations where
  hashWithSalt _salt ListAssociations' {..} =
    _salt
      `Prelude.hashWithSalt` associationType
      `Prelude.hashWithSalt` createdAfter
      `Prelude.hashWithSalt` createdBefore
      `Prelude.hashWithSalt` destinationArn
      `Prelude.hashWithSalt` destinationType
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` sourceArn
      `Prelude.hashWithSalt` sourceType

instance Prelude.NFData ListAssociations where
  rnf ListAssociations' {..} =
    Prelude.rnf associationType `Prelude.seq`
      Prelude.rnf createdAfter `Prelude.seq`
        Prelude.rnf createdBefore `Prelude.seq`
          Prelude.rnf destinationArn `Prelude.seq`
            Prelude.rnf destinationType `Prelude.seq`
              Prelude.rnf maxResults `Prelude.seq`
                Prelude.rnf nextToken `Prelude.seq`
                  Prelude.rnf sortBy `Prelude.seq`
                    Prelude.rnf sortOrder `Prelude.seq`
                      Prelude.rnf sourceArn `Prelude.seq`
                        Prelude.rnf sourceType

instance Data.ToHeaders ListAssociations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.ListAssociations" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAssociations where
  toJSON ListAssociations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AssociationType" Data..=)
              Prelude.<$> associationType,
            ("CreatedAfter" Data..=) Prelude.<$> createdAfter,
            ("CreatedBefore" Data..=) Prelude.<$> createdBefore,
            ("DestinationArn" Data..=)
              Prelude.<$> destinationArn,
            ("DestinationType" Data..=)
              Prelude.<$> destinationType,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder,
            ("SourceArn" Data..=) Prelude.<$> sourceArn,
            ("SourceType" Data..=) Prelude.<$> sourceType
          ]
      )

instance Data.ToPath ListAssociations where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAssociations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAssociationsResponse' smart constructor.
data ListAssociationsResponse = ListAssociationsResponse'
  { -- | A list of associations and their properties.
    associationSummaries :: Prelude.Maybe [AssociationSummary],
    -- | A token for getting the next set of associations, if there are any.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'associationSummaries', 'listAssociationsResponse_associationSummaries' - A list of associations and their properties.
--
-- 'nextToken', 'listAssociationsResponse_nextToken' - A token for getting the next set of associations, if there are any.
--
-- 'httpStatus', 'listAssociationsResponse_httpStatus' - The response's http status code.
newListAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAssociationsResponse
newListAssociationsResponse pHttpStatus_ =
  ListAssociationsResponse'
    { associationSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of associations and their properties.
listAssociationsResponse_associationSummaries :: Lens.Lens' ListAssociationsResponse (Prelude.Maybe [AssociationSummary])
listAssociationsResponse_associationSummaries = Lens.lens (\ListAssociationsResponse' {associationSummaries} -> associationSummaries) (\s@ListAssociationsResponse' {} a -> s {associationSummaries = a} :: ListAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token for getting the next set of associations, if there are any.
listAssociationsResponse_nextToken :: Lens.Lens' ListAssociationsResponse (Prelude.Maybe Prelude.Text)
listAssociationsResponse_nextToken = Lens.lens (\ListAssociationsResponse' {nextToken} -> nextToken) (\s@ListAssociationsResponse' {} a -> s {nextToken = a} :: ListAssociationsResponse)

-- | The response's http status code.
listAssociationsResponse_httpStatus :: Lens.Lens' ListAssociationsResponse Prelude.Int
listAssociationsResponse_httpStatus = Lens.lens (\ListAssociationsResponse' {httpStatus} -> httpStatus) (\s@ListAssociationsResponse' {} a -> s {httpStatus = a} :: ListAssociationsResponse)

instance Prelude.NFData ListAssociationsResponse where
  rnf ListAssociationsResponse' {..} =
    Prelude.rnf associationSummaries `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
