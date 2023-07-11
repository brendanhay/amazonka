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
-- Module      : Amazonka.CloudFront.ListDistributions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List CloudFront distributions.
--
-- This operation returns paginated results.
module Amazonka.CloudFront.ListDistributions
  ( -- * Creating a Request
    ListDistributions (..),
    newListDistributions,

    -- * Request Lenses
    listDistributions_marker,
    listDistributions_maxItems,

    -- * Destructuring the Response
    ListDistributionsResponse (..),
    newListDistributionsResponse,

    -- * Response Lenses
    listDistributionsResponse_httpStatus,
    listDistributionsResponse_distributionList,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request to list your distributions.
--
-- /See:/ 'newListDistributions' smart constructor.
data ListDistributions = ListDistributions'
  { -- | Use this when paginating results to indicate where to begin in your list
    -- of distributions. The results include distributions in the list that
    -- occur after the marker. To get the next page of results, set the
    -- @Marker@ to the value of the @NextMarker@ from the current page\'s
    -- response (which is also the ID of the last distribution on that page).
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of distributions you want in the response body.
    maxItems :: Prelude.Maybe Prelude.Text
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
-- 'marker', 'listDistributions_marker' - Use this when paginating results to indicate where to begin in your list
-- of distributions. The results include distributions in the list that
-- occur after the marker. To get the next page of results, set the
-- @Marker@ to the value of the @NextMarker@ from the current page\'s
-- response (which is also the ID of the last distribution on that page).
--
-- 'maxItems', 'listDistributions_maxItems' - The maximum number of distributions you want in the response body.
newListDistributions ::
  ListDistributions
newListDistributions =
  ListDistributions'
    { marker = Prelude.Nothing,
      maxItems = Prelude.Nothing
    }

-- | Use this when paginating results to indicate where to begin in your list
-- of distributions. The results include distributions in the list that
-- occur after the marker. To get the next page of results, set the
-- @Marker@ to the value of the @NextMarker@ from the current page\'s
-- response (which is also the ID of the last distribution on that page).
listDistributions_marker :: Lens.Lens' ListDistributions (Prelude.Maybe Prelude.Text)
listDistributions_marker = Lens.lens (\ListDistributions' {marker} -> marker) (\s@ListDistributions' {} a -> s {marker = a} :: ListDistributions)

-- | The maximum number of distributions you want in the response body.
listDistributions_maxItems :: Lens.Lens' ListDistributions (Prelude.Maybe Prelude.Text)
listDistributions_maxItems = Lens.lens (\ListDistributions' {maxItems} -> maxItems) (\s@ListDistributions' {} a -> s {maxItems = a} :: ListDistributions)

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
        Prelude.Just
          Prelude.$ rq
          Prelude.& listDistributions_marker
          Lens..~ rs
          Lens.^? listDistributionsResponse_distributionList
          Prelude.. distributionList_nextMarker
          Prelude.. Lens._Just

instance Core.AWSRequest ListDistributions where
  type
    AWSResponse ListDistributions =
      ListDistributionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListDistributionsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.parseXML x)
      )

instance Prelude.Hashable ListDistributions where
  hashWithSalt _salt ListDistributions' {..} =
    _salt
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems

instance Prelude.NFData ListDistributions where
  rnf ListDistributions' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems

instance Data.ToHeaders ListDistributions where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListDistributions where
  toPath = Prelude.const "/2020-05-31/distribution"

instance Data.ToQuery ListDistributions where
  toQuery ListDistributions' {..} =
    Prelude.mconcat
      [ "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems
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
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData ListDistributionsResponse where
  rnf ListDistributionsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf distributionList
