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
-- Module      : Network.AWS.CloudFront.ListDistributions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List CloudFront distributions.
--
-- This operation returns paginated results.
module Network.AWS.CloudFront.ListDistributions
  ( -- * Creating a Request
    ListDistributions (..),
    newListDistributions,

    -- * Request Lenses
    listDistributions_maxItems,
    listDistributions_marker,

    -- * Destructuring the Response
    ListDistributionsResponse (..),
    newListDistributionsResponse,

    -- * Response Lenses
    listDistributionsResponse_httpStatus,
    listDistributionsResponse_distributionList,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to list your distributions.
--
-- /See:/ 'newListDistributions' smart constructor.
data ListDistributions = ListDistributions'
  { -- | The maximum number of distributions you want in the response body.
    maxItems :: Prelude.Maybe Prelude.Text,
    -- | Use this when paginating results to indicate where to begin in your list
    -- of distributions. The results include distributions in the list that
    -- occur after the marker. To get the next page of results, set the
    -- @Marker@ to the value of the @NextMarker@ from the current page\'s
    -- response (which is also the ID of the last distribution on that page).
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDistributions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxItems', 'listDistributions_maxItems' - The maximum number of distributions you want in the response body.
--
-- 'marker', 'listDistributions_marker' - Use this when paginating results to indicate where to begin in your list
-- of distributions. The results include distributions in the list that
-- occur after the marker. To get the next page of results, set the
-- @Marker@ to the value of the @NextMarker@ from the current page\'s
-- response (which is also the ID of the last distribution on that page).
newListDistributions ::
  ListDistributions
newListDistributions =
  ListDistributions'
    { maxItems = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | The maximum number of distributions you want in the response body.
listDistributions_maxItems :: Lens.Lens' ListDistributions (Prelude.Maybe Prelude.Text)
listDistributions_maxItems = Lens.lens (\ListDistributions' {maxItems} -> maxItems) (\s@ListDistributions' {} a -> s {maxItems = a} :: ListDistributions)

-- | Use this when paginating results to indicate where to begin in your list
-- of distributions. The results include distributions in the list that
-- occur after the marker. To get the next page of results, set the
-- @Marker@ to the value of the @NextMarker@ from the current page\'s
-- response (which is also the ID of the last distribution on that page).
listDistributions_marker :: Lens.Lens' ListDistributions (Prelude.Maybe Prelude.Text)
listDistributions_marker = Lens.lens (\ListDistributions' {marker} -> marker) (\s@ListDistributions' {} a -> s {marker = a} :: ListDistributions)

instance Core.AWSPager ListDistributions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^. listDistributionsResponse_distributionList
              Prelude.. distributionList_isTruncated
        ) =
      Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listDistributionsResponse_distributionList
              Prelude.. distributionList_nextMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDistributions_marker
          Lens..~ rs
          Lens.^? listDistributionsResponse_distributionList
            Prelude.. distributionList_nextMarker
            Prelude.. Lens._Just

instance Core.AWSRequest ListDistributions where
  type
    AWSResponse ListDistributions =
      ListDistributionsResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ListDistributionsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.parseXML x)
      )

instance Prelude.Hashable ListDistributions

instance Prelude.NFData ListDistributions

instance Core.ToHeaders ListDistributions where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListDistributions where
  toPath = Prelude.const "/2020-05-31/distribution"

instance Core.ToQuery ListDistributions where
  toQuery ListDistributions' {..} =
    Prelude.mconcat
      [ "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker
      ]

-- | The returned result of the corresponding request.
--
-- /See:/ 'newListDistributionsResponse' smart constructor.
data ListDistributionsResponse = ListDistributionsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The @DistributionList@ type.
    distributionList :: DistributionList
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDistributionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'listDistributionsResponse_httpStatus' - The response's http status code.
--
-- 'distributionList', 'listDistributionsResponse_distributionList' - The @DistributionList@ type.
newListDistributionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'distributionList'
  DistributionList ->
  ListDistributionsResponse
newListDistributionsResponse
  pHttpStatus_
  pDistributionList_ =
    ListDistributionsResponse'
      { httpStatus =
          pHttpStatus_,
        distributionList = pDistributionList_
      }

-- | The response's http status code.
listDistributionsResponse_httpStatus :: Lens.Lens' ListDistributionsResponse Prelude.Int
listDistributionsResponse_httpStatus = Lens.lens (\ListDistributionsResponse' {httpStatus} -> httpStatus) (\s@ListDistributionsResponse' {} a -> s {httpStatus = a} :: ListDistributionsResponse)

-- | The @DistributionList@ type.
listDistributionsResponse_distributionList :: Lens.Lens' ListDistributionsResponse DistributionList
listDistributionsResponse_distributionList = Lens.lens (\ListDistributionsResponse' {distributionList} -> distributionList) (\s@ListDistributionsResponse' {} a -> s {distributionList = a} :: ListDistributionsResponse)

instance Prelude.NFData ListDistributionsResponse
