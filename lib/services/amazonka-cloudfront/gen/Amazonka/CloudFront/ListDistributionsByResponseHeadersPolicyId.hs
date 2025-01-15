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
-- Module      : Amazonka.CloudFront.ListDistributionsByResponseHeadersPolicyId
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of distribution IDs for distributions that have a cache
-- behavior that\'s associated with the specified response headers policy.
--
-- You can optionally specify the maximum number of items to receive in the
-- response. If the total number of items in the list exceeds the maximum
-- that you specify, or the default maximum, the response is paginated. To
-- get the next page of items, send a subsequent request that specifies the
-- @NextMarker@ value from the current response as the @Marker@ value in
-- the subsequent request.
module Amazonka.CloudFront.ListDistributionsByResponseHeadersPolicyId
  ( -- * Creating a Request
    ListDistributionsByResponseHeadersPolicyId (..),
    newListDistributionsByResponseHeadersPolicyId,

    -- * Request Lenses
    listDistributionsByResponseHeadersPolicyId_marker,
    listDistributionsByResponseHeadersPolicyId_maxItems,
    listDistributionsByResponseHeadersPolicyId_responseHeadersPolicyId,

    -- * Destructuring the Response
    ListDistributionsByResponseHeadersPolicyIdResponse (..),
    newListDistributionsByResponseHeadersPolicyIdResponse,

    -- * Response Lenses
    listDistributionsByResponseHeadersPolicyIdResponse_distributionIdList,
    listDistributionsByResponseHeadersPolicyIdResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDistributionsByResponseHeadersPolicyId' smart constructor.
data ListDistributionsByResponseHeadersPolicyId = ListDistributionsByResponseHeadersPolicyId'
  { -- | Use this field when paginating results to indicate where to begin in
    -- your list of distribution IDs. The response includes distribution IDs in
    -- the list that occur after the marker. To get the next page of the list,
    -- set this field\'s value to the value of @NextMarker@ from the current
    -- page\'s response.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of distribution IDs that you want to get in the
    -- response.
    maxItems :: Prelude.Maybe Prelude.Text,
    -- | The ID of the response headers policy whose associated distribution IDs
    -- you want to list.
    responseHeadersPolicyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDistributionsByResponseHeadersPolicyId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listDistributionsByResponseHeadersPolicyId_marker' - Use this field when paginating results to indicate where to begin in
-- your list of distribution IDs. The response includes distribution IDs in
-- the list that occur after the marker. To get the next page of the list,
-- set this field\'s value to the value of @NextMarker@ from the current
-- page\'s response.
--
-- 'maxItems', 'listDistributionsByResponseHeadersPolicyId_maxItems' - The maximum number of distribution IDs that you want to get in the
-- response.
--
-- 'responseHeadersPolicyId', 'listDistributionsByResponseHeadersPolicyId_responseHeadersPolicyId' - The ID of the response headers policy whose associated distribution IDs
-- you want to list.
newListDistributionsByResponseHeadersPolicyId ::
  -- | 'responseHeadersPolicyId'
  Prelude.Text ->
  ListDistributionsByResponseHeadersPolicyId
newListDistributionsByResponseHeadersPolicyId
  pResponseHeadersPolicyId_ =
    ListDistributionsByResponseHeadersPolicyId'
      { marker =
          Prelude.Nothing,
        maxItems = Prelude.Nothing,
        responseHeadersPolicyId =
          pResponseHeadersPolicyId_
      }

-- | Use this field when paginating results to indicate where to begin in
-- your list of distribution IDs. The response includes distribution IDs in
-- the list that occur after the marker. To get the next page of the list,
-- set this field\'s value to the value of @NextMarker@ from the current
-- page\'s response.
listDistributionsByResponseHeadersPolicyId_marker :: Lens.Lens' ListDistributionsByResponseHeadersPolicyId (Prelude.Maybe Prelude.Text)
listDistributionsByResponseHeadersPolicyId_marker = Lens.lens (\ListDistributionsByResponseHeadersPolicyId' {marker} -> marker) (\s@ListDistributionsByResponseHeadersPolicyId' {} a -> s {marker = a} :: ListDistributionsByResponseHeadersPolicyId)

-- | The maximum number of distribution IDs that you want to get in the
-- response.
listDistributionsByResponseHeadersPolicyId_maxItems :: Lens.Lens' ListDistributionsByResponseHeadersPolicyId (Prelude.Maybe Prelude.Text)
listDistributionsByResponseHeadersPolicyId_maxItems = Lens.lens (\ListDistributionsByResponseHeadersPolicyId' {maxItems} -> maxItems) (\s@ListDistributionsByResponseHeadersPolicyId' {} a -> s {maxItems = a} :: ListDistributionsByResponseHeadersPolicyId)

-- | The ID of the response headers policy whose associated distribution IDs
-- you want to list.
listDistributionsByResponseHeadersPolicyId_responseHeadersPolicyId :: Lens.Lens' ListDistributionsByResponseHeadersPolicyId Prelude.Text
listDistributionsByResponseHeadersPolicyId_responseHeadersPolicyId = Lens.lens (\ListDistributionsByResponseHeadersPolicyId' {responseHeadersPolicyId} -> responseHeadersPolicyId) (\s@ListDistributionsByResponseHeadersPolicyId' {} a -> s {responseHeadersPolicyId = a} :: ListDistributionsByResponseHeadersPolicyId)

instance
  Core.AWSRequest
    ListDistributionsByResponseHeadersPolicyId
  where
  type
    AWSResponse
      ListDistributionsByResponseHeadersPolicyId =
      ListDistributionsByResponseHeadersPolicyIdResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListDistributionsByResponseHeadersPolicyIdResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListDistributionsByResponseHeadersPolicyId
  where
  hashWithSalt
    _salt
    ListDistributionsByResponseHeadersPolicyId' {..} =
      _salt
        `Prelude.hashWithSalt` marker
        `Prelude.hashWithSalt` maxItems
        `Prelude.hashWithSalt` responseHeadersPolicyId

instance
  Prelude.NFData
    ListDistributionsByResponseHeadersPolicyId
  where
  rnf ListDistributionsByResponseHeadersPolicyId' {..} =
    Prelude.rnf marker `Prelude.seq`
      Prelude.rnf maxItems `Prelude.seq`
        Prelude.rnf responseHeadersPolicyId

instance
  Data.ToHeaders
    ListDistributionsByResponseHeadersPolicyId
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    ListDistributionsByResponseHeadersPolicyId
  where
  toPath
    ListDistributionsByResponseHeadersPolicyId' {..} =
      Prelude.mconcat
        [ "/2020-05-31/distributionsByResponseHeadersPolicyId/",
          Data.toBS responseHeadersPolicyId
        ]

instance
  Data.ToQuery
    ListDistributionsByResponseHeadersPolicyId
  where
  toQuery
    ListDistributionsByResponseHeadersPolicyId' {..} =
      Prelude.mconcat
        [ "Marker" Data.=: marker,
          "MaxItems" Data.=: maxItems
        ]

-- | /See:/ 'newListDistributionsByResponseHeadersPolicyIdResponse' smart constructor.
data ListDistributionsByResponseHeadersPolicyIdResponse = ListDistributionsByResponseHeadersPolicyIdResponse'
  { distributionIdList :: Prelude.Maybe DistributionIdList,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDistributionsByResponseHeadersPolicyIdResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distributionIdList', 'listDistributionsByResponseHeadersPolicyIdResponse_distributionIdList' - Undocumented member.
--
-- 'httpStatus', 'listDistributionsByResponseHeadersPolicyIdResponse_httpStatus' - The response's http status code.
newListDistributionsByResponseHeadersPolicyIdResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDistributionsByResponseHeadersPolicyIdResponse
newListDistributionsByResponseHeadersPolicyIdResponse
  pHttpStatus_ =
    ListDistributionsByResponseHeadersPolicyIdResponse'
      { distributionIdList =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Undocumented member.
listDistributionsByResponseHeadersPolicyIdResponse_distributionIdList :: Lens.Lens' ListDistributionsByResponseHeadersPolicyIdResponse (Prelude.Maybe DistributionIdList)
listDistributionsByResponseHeadersPolicyIdResponse_distributionIdList = Lens.lens (\ListDistributionsByResponseHeadersPolicyIdResponse' {distributionIdList} -> distributionIdList) (\s@ListDistributionsByResponseHeadersPolicyIdResponse' {} a -> s {distributionIdList = a} :: ListDistributionsByResponseHeadersPolicyIdResponse)

-- | The response's http status code.
listDistributionsByResponseHeadersPolicyIdResponse_httpStatus :: Lens.Lens' ListDistributionsByResponseHeadersPolicyIdResponse Prelude.Int
listDistributionsByResponseHeadersPolicyIdResponse_httpStatus = Lens.lens (\ListDistributionsByResponseHeadersPolicyIdResponse' {httpStatus} -> httpStatus) (\s@ListDistributionsByResponseHeadersPolicyIdResponse' {} a -> s {httpStatus = a} :: ListDistributionsByResponseHeadersPolicyIdResponse)

instance
  Prelude.NFData
    ListDistributionsByResponseHeadersPolicyIdResponse
  where
  rnf
    ListDistributionsByResponseHeadersPolicyIdResponse' {..} =
      Prelude.rnf distributionIdList `Prelude.seq`
        Prelude.rnf httpStatus
