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
-- Module      : Amazonka.CloudFront.ListInvalidations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists invalidation batches.
--
-- This operation returns paginated results.
module Amazonka.CloudFront.ListInvalidations
  ( -- * Creating a Request
    ListInvalidations (..),
    newListInvalidations,

    -- * Request Lenses
    listInvalidations_marker,
    listInvalidations_maxItems,
    listInvalidations_distributionId,

    -- * Destructuring the Response
    ListInvalidationsResponse (..),
    newListInvalidationsResponse,

    -- * Response Lenses
    listInvalidationsResponse_httpStatus,
    listInvalidationsResponse_invalidationList,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request to list invalidations.
--
-- /See:/ 'newListInvalidations' smart constructor.
data ListInvalidations = ListInvalidations'
  { -- | Use this parameter when paginating results to indicate where to begin in
    -- your list of invalidation batches. Because the results are returned in
    -- decreasing order from most recent to oldest, the most recent results are
    -- on the first page, the second page will contain earlier results, and so
    -- on. To get the next page of results, set @Marker@ to the value of the
    -- @NextMarker@ from the current page\'s response. This value is the same
    -- as the ID of the last invalidation batch on that page.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of invalidation batches that you want in the response
    -- body.
    maxItems :: Prelude.Maybe Prelude.Text,
    -- | The distribution\'s ID.
    distributionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInvalidations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listInvalidations_marker' - Use this parameter when paginating results to indicate where to begin in
-- your list of invalidation batches. Because the results are returned in
-- decreasing order from most recent to oldest, the most recent results are
-- on the first page, the second page will contain earlier results, and so
-- on. To get the next page of results, set @Marker@ to the value of the
-- @NextMarker@ from the current page\'s response. This value is the same
-- as the ID of the last invalidation batch on that page.
--
-- 'maxItems', 'listInvalidations_maxItems' - The maximum number of invalidation batches that you want in the response
-- body.
--
-- 'distributionId', 'listInvalidations_distributionId' - The distribution\'s ID.
newListInvalidations ::
  -- | 'distributionId'
  Prelude.Text ->
  ListInvalidations
newListInvalidations pDistributionId_ =
  ListInvalidations'
    { marker = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      distributionId = pDistributionId_
    }

-- | Use this parameter when paginating results to indicate where to begin in
-- your list of invalidation batches. Because the results are returned in
-- decreasing order from most recent to oldest, the most recent results are
-- on the first page, the second page will contain earlier results, and so
-- on. To get the next page of results, set @Marker@ to the value of the
-- @NextMarker@ from the current page\'s response. This value is the same
-- as the ID of the last invalidation batch on that page.
listInvalidations_marker :: Lens.Lens' ListInvalidations (Prelude.Maybe Prelude.Text)
listInvalidations_marker = Lens.lens (\ListInvalidations' {marker} -> marker) (\s@ListInvalidations' {} a -> s {marker = a} :: ListInvalidations)

-- | The maximum number of invalidation batches that you want in the response
-- body.
listInvalidations_maxItems :: Lens.Lens' ListInvalidations (Prelude.Maybe Prelude.Text)
listInvalidations_maxItems = Lens.lens (\ListInvalidations' {maxItems} -> maxItems) (\s@ListInvalidations' {} a -> s {maxItems = a} :: ListInvalidations)

-- | The distribution\'s ID.
listInvalidations_distributionId :: Lens.Lens' ListInvalidations Prelude.Text
listInvalidations_distributionId = Lens.lens (\ListInvalidations' {distributionId} -> distributionId) (\s@ListInvalidations' {} a -> s {distributionId = a} :: ListInvalidations)

instance Core.AWSPager ListInvalidations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^. listInvalidationsResponse_invalidationList
              Prelude.. invalidationList_isTruncated
        ) =
      Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listInvalidationsResponse_invalidationList
              Prelude.. invalidationList_nextMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listInvalidations_marker
          Lens..~ rs
          Lens.^? listInvalidationsResponse_invalidationList
            Prelude.. invalidationList_nextMarker
            Prelude.. Lens._Just

instance Core.AWSRequest ListInvalidations where
  type
    AWSResponse ListInvalidations =
      ListInvalidationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListInvalidationsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.parseXML x)
      )

instance Prelude.Hashable ListInvalidations where
  hashWithSalt _salt ListInvalidations' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` distributionId

instance Prelude.NFData ListInvalidations where
  rnf ListInvalidations' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf distributionId

instance Core.ToHeaders ListInvalidations where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListInvalidations where
  toPath ListInvalidations' {..} =
    Prelude.mconcat
      [ "/2020-05-31/distribution/",
        Core.toBS distributionId,
        "/invalidation"
      ]

instance Core.ToQuery ListInvalidations where
  toQuery ListInvalidations' {..} =
    Prelude.mconcat
      [ "Marker" Core.=: marker,
        "MaxItems" Core.=: maxItems
      ]

-- | The returned result of the corresponding request.
--
-- /See:/ 'newListInvalidationsResponse' smart constructor.
data ListInvalidationsResponse = ListInvalidationsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about invalidation batches.
    invalidationList :: InvalidationList
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInvalidationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'listInvalidationsResponse_httpStatus' - The response's http status code.
--
-- 'invalidationList', 'listInvalidationsResponse_invalidationList' - Information about invalidation batches.
newListInvalidationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'invalidationList'
  InvalidationList ->
  ListInvalidationsResponse
newListInvalidationsResponse
  pHttpStatus_
  pInvalidationList_ =
    ListInvalidationsResponse'
      { httpStatus =
          pHttpStatus_,
        invalidationList = pInvalidationList_
      }

-- | The response's http status code.
listInvalidationsResponse_httpStatus :: Lens.Lens' ListInvalidationsResponse Prelude.Int
listInvalidationsResponse_httpStatus = Lens.lens (\ListInvalidationsResponse' {httpStatus} -> httpStatus) (\s@ListInvalidationsResponse' {} a -> s {httpStatus = a} :: ListInvalidationsResponse)

-- | Information about invalidation batches.
listInvalidationsResponse_invalidationList :: Lens.Lens' ListInvalidationsResponse InvalidationList
listInvalidationsResponse_invalidationList = Lens.lens (\ListInvalidationsResponse' {invalidationList} -> invalidationList) (\s@ListInvalidationsResponse' {} a -> s {invalidationList = a} :: ListInvalidationsResponse)

instance Prelude.NFData ListInvalidationsResponse where
  rnf ListInvalidationsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf invalidationList
