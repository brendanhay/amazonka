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
-- Module      : Network.AWS.CloudFront.ListInvalidations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists invalidation batches.
--
-- This operation returns paginated results.
module Network.AWS.CloudFront.ListInvalidations
  ( -- * Creating a Request
    ListInvalidations (..),
    newListInvalidations,

    -- * Request Lenses
    listInvalidations_maxItems,
    listInvalidations_marker,
    listInvalidations_distributionId,

    -- * Destructuring the Response
    ListInvalidationsResponse (..),
    newListInvalidationsResponse,

    -- * Response Lenses
    listInvalidationsResponse_httpStatus,
    listInvalidationsResponse_invalidationList,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to list invalidations.
--
-- /See:/ 'newListInvalidations' smart constructor.
data ListInvalidations = ListInvalidations'
  { -- | The maximum number of invalidation batches that you want in the response
    -- body.
    maxItems :: Prelude.Maybe Prelude.Text,
    -- | Use this parameter when paginating results to indicate where to begin in
    -- your list of invalidation batches. Because the results are returned in
    -- decreasing order from most recent to oldest, the most recent results are
    -- on the first page, the second page will contain earlier results, and so
    -- on. To get the next page of results, set @Marker@ to the value of the
    -- @NextMarker@ from the current page\'s response. This value is the same
    -- as the ID of the last invalidation batch on that page.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The distribution\'s ID.
    distributionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListInvalidations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxItems', 'listInvalidations_maxItems' - The maximum number of invalidation batches that you want in the response
-- body.
--
-- 'marker', 'listInvalidations_marker' - Use this parameter when paginating results to indicate where to begin in
-- your list of invalidation batches. Because the results are returned in
-- decreasing order from most recent to oldest, the most recent results are
-- on the first page, the second page will contain earlier results, and so
-- on. To get the next page of results, set @Marker@ to the value of the
-- @NextMarker@ from the current page\'s response. This value is the same
-- as the ID of the last invalidation batch on that page.
--
-- 'distributionId', 'listInvalidations_distributionId' - The distribution\'s ID.
newListInvalidations ::
  -- | 'distributionId'
  Prelude.Text ->
  ListInvalidations
newListInvalidations pDistributionId_ =
  ListInvalidations'
    { maxItems = Prelude.Nothing,
      marker = Prelude.Nothing,
      distributionId = pDistributionId_
    }

-- | The maximum number of invalidation batches that you want in the response
-- body.
listInvalidations_maxItems :: Lens.Lens' ListInvalidations (Prelude.Maybe Prelude.Text)
listInvalidations_maxItems = Lens.lens (\ListInvalidations' {maxItems} -> maxItems) (\s@ListInvalidations' {} a -> s {maxItems = a} :: ListInvalidations)

-- | Use this parameter when paginating results to indicate where to begin in
-- your list of invalidation batches. Because the results are returned in
-- decreasing order from most recent to oldest, the most recent results are
-- on the first page, the second page will contain earlier results, and so
-- on. To get the next page of results, set @Marker@ to the value of the
-- @NextMarker@ from the current page\'s response. This value is the same
-- as the ID of the last invalidation batch on that page.
listInvalidations_marker :: Lens.Lens' ListInvalidations (Prelude.Maybe Prelude.Text)
listInvalidations_marker = Lens.lens (\ListInvalidations' {marker} -> marker) (\s@ListInvalidations' {} a -> s {marker = a} :: ListInvalidations)

-- | The distribution\'s ID.
listInvalidations_distributionId :: Lens.Lens' ListInvalidations Prelude.Text
listInvalidations_distributionId = Lens.lens (\ListInvalidations' {distributionId} -> distributionId) (\s@ListInvalidations' {} a -> s {distributionId = a} :: ListInvalidations)

instance Pager.AWSPager ListInvalidations where
  page rq rs
    | Pager.stop
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
          Lens.& listInvalidations_marker
          Lens..~ rs
          Lens.^? listInvalidationsResponse_invalidationList
            Prelude.. invalidationList_nextMarker
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListInvalidations where
  type Rs ListInvalidations = ListInvalidationsResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ListInvalidationsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.parseXML x)
      )

instance Prelude.Hashable ListInvalidations

instance Prelude.NFData ListInvalidations

instance Prelude.ToHeaders ListInvalidations where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ListInvalidations where
  toPath ListInvalidations' {..} =
    Prelude.mconcat
      [ "/2020-05-31/distribution/",
        Prelude.toBS distributionId,
        "/invalidation"
      ]

instance Prelude.ToQuery ListInvalidations where
  toQuery ListInvalidations' {..} =
    Prelude.mconcat
      [ "MaxItems" Prelude.=: maxItems,
        "Marker" Prelude.=: marker
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData ListInvalidationsResponse
