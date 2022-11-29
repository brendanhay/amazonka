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
-- Module      : Amazonka.CloudFront.ListDistributionsByOriginRequestPolicyId
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of distribution IDs for distributions that have a cache
-- behavior that’s associated with the specified origin request policy.
--
-- You can optionally specify the maximum number of items to receive in the
-- response. If the total number of items in the list exceeds the maximum
-- that you specify, or the default maximum, the response is paginated. To
-- get the next page of items, send a subsequent request that specifies the
-- @NextMarker@ value from the current response as the @Marker@ value in
-- the subsequent request.
module Amazonka.CloudFront.ListDistributionsByOriginRequestPolicyId
  ( -- * Creating a Request
    ListDistributionsByOriginRequestPolicyId (..),
    newListDistributionsByOriginRequestPolicyId,

    -- * Request Lenses
    listDistributionsByOriginRequestPolicyId_marker,
    listDistributionsByOriginRequestPolicyId_maxItems,
    listDistributionsByOriginRequestPolicyId_originRequestPolicyId,

    -- * Destructuring the Response
    ListDistributionsByOriginRequestPolicyIdResponse (..),
    newListDistributionsByOriginRequestPolicyIdResponse,

    -- * Response Lenses
    listDistributionsByOriginRequestPolicyIdResponse_distributionIdList,
    listDistributionsByOriginRequestPolicyIdResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDistributionsByOriginRequestPolicyId' smart constructor.
data ListDistributionsByOriginRequestPolicyId = ListDistributionsByOriginRequestPolicyId'
  { -- | Use this field when paginating results to indicate where to begin in
    -- your list of distribution IDs. The response includes distribution IDs in
    -- the list that occur after the marker. To get the next page of the list,
    -- set this field’s value to the value of @NextMarker@ from the current
    -- page’s response.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of distribution IDs that you want in the response.
    maxItems :: Prelude.Maybe Prelude.Text,
    -- | The ID of the origin request policy whose associated distribution IDs
    -- you want to list.
    originRequestPolicyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDistributionsByOriginRequestPolicyId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listDistributionsByOriginRequestPolicyId_marker' - Use this field when paginating results to indicate where to begin in
-- your list of distribution IDs. The response includes distribution IDs in
-- the list that occur after the marker. To get the next page of the list,
-- set this field’s value to the value of @NextMarker@ from the current
-- page’s response.
--
-- 'maxItems', 'listDistributionsByOriginRequestPolicyId_maxItems' - The maximum number of distribution IDs that you want in the response.
--
-- 'originRequestPolicyId', 'listDistributionsByOriginRequestPolicyId_originRequestPolicyId' - The ID of the origin request policy whose associated distribution IDs
-- you want to list.
newListDistributionsByOriginRequestPolicyId ::
  -- | 'originRequestPolicyId'
  Prelude.Text ->
  ListDistributionsByOriginRequestPolicyId
newListDistributionsByOriginRequestPolicyId
  pOriginRequestPolicyId_ =
    ListDistributionsByOriginRequestPolicyId'
      { marker =
          Prelude.Nothing,
        maxItems = Prelude.Nothing,
        originRequestPolicyId =
          pOriginRequestPolicyId_
      }

-- | Use this field when paginating results to indicate where to begin in
-- your list of distribution IDs. The response includes distribution IDs in
-- the list that occur after the marker. To get the next page of the list,
-- set this field’s value to the value of @NextMarker@ from the current
-- page’s response.
listDistributionsByOriginRequestPolicyId_marker :: Lens.Lens' ListDistributionsByOriginRequestPolicyId (Prelude.Maybe Prelude.Text)
listDistributionsByOriginRequestPolicyId_marker = Lens.lens (\ListDistributionsByOriginRequestPolicyId' {marker} -> marker) (\s@ListDistributionsByOriginRequestPolicyId' {} a -> s {marker = a} :: ListDistributionsByOriginRequestPolicyId)

-- | The maximum number of distribution IDs that you want in the response.
listDistributionsByOriginRequestPolicyId_maxItems :: Lens.Lens' ListDistributionsByOriginRequestPolicyId (Prelude.Maybe Prelude.Text)
listDistributionsByOriginRequestPolicyId_maxItems = Lens.lens (\ListDistributionsByOriginRequestPolicyId' {maxItems} -> maxItems) (\s@ListDistributionsByOriginRequestPolicyId' {} a -> s {maxItems = a} :: ListDistributionsByOriginRequestPolicyId)

-- | The ID of the origin request policy whose associated distribution IDs
-- you want to list.
listDistributionsByOriginRequestPolicyId_originRequestPolicyId :: Lens.Lens' ListDistributionsByOriginRequestPolicyId Prelude.Text
listDistributionsByOriginRequestPolicyId_originRequestPolicyId = Lens.lens (\ListDistributionsByOriginRequestPolicyId' {originRequestPolicyId} -> originRequestPolicyId) (\s@ListDistributionsByOriginRequestPolicyId' {} a -> s {originRequestPolicyId = a} :: ListDistributionsByOriginRequestPolicyId)

instance
  Core.AWSRequest
    ListDistributionsByOriginRequestPolicyId
  where
  type
    AWSResponse
      ListDistributionsByOriginRequestPolicyId =
      ListDistributionsByOriginRequestPolicyIdResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListDistributionsByOriginRequestPolicyIdResponse'
            Prelude.<$> (Core.parseXML x)
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListDistributionsByOriginRequestPolicyId
  where
  hashWithSalt
    _salt
    ListDistributionsByOriginRequestPolicyId' {..} =
      _salt `Prelude.hashWithSalt` marker
        `Prelude.hashWithSalt` maxItems
        `Prelude.hashWithSalt` originRequestPolicyId

instance
  Prelude.NFData
    ListDistributionsByOriginRequestPolicyId
  where
  rnf ListDistributionsByOriginRequestPolicyId' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf originRequestPolicyId

instance
  Core.ToHeaders
    ListDistributionsByOriginRequestPolicyId
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    ListDistributionsByOriginRequestPolicyId
  where
  toPath ListDistributionsByOriginRequestPolicyId' {..} =
    Prelude.mconcat
      [ "/2020-05-31/distributionsByOriginRequestPolicyId/",
        Core.toBS originRequestPolicyId
      ]

instance
  Core.ToQuery
    ListDistributionsByOriginRequestPolicyId
  where
  toQuery ListDistributionsByOriginRequestPolicyId' {..} =
    Prelude.mconcat
      [ "Marker" Core.=: marker,
        "MaxItems" Core.=: maxItems
      ]

-- | /See:/ 'newListDistributionsByOriginRequestPolicyIdResponse' smart constructor.
data ListDistributionsByOriginRequestPolicyIdResponse = ListDistributionsByOriginRequestPolicyIdResponse'
  { -- | A list of distribution IDs.
    distributionIdList :: Prelude.Maybe DistributionIdList,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDistributionsByOriginRequestPolicyIdResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distributionIdList', 'listDistributionsByOriginRequestPolicyIdResponse_distributionIdList' - A list of distribution IDs.
--
-- 'httpStatus', 'listDistributionsByOriginRequestPolicyIdResponse_httpStatus' - The response's http status code.
newListDistributionsByOriginRequestPolicyIdResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDistributionsByOriginRequestPolicyIdResponse
newListDistributionsByOriginRequestPolicyIdResponse
  pHttpStatus_ =
    ListDistributionsByOriginRequestPolicyIdResponse'
      { distributionIdList =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of distribution IDs.
listDistributionsByOriginRequestPolicyIdResponse_distributionIdList :: Lens.Lens' ListDistributionsByOriginRequestPolicyIdResponse (Prelude.Maybe DistributionIdList)
listDistributionsByOriginRequestPolicyIdResponse_distributionIdList = Lens.lens (\ListDistributionsByOriginRequestPolicyIdResponse' {distributionIdList} -> distributionIdList) (\s@ListDistributionsByOriginRequestPolicyIdResponse' {} a -> s {distributionIdList = a} :: ListDistributionsByOriginRequestPolicyIdResponse)

-- | The response's http status code.
listDistributionsByOriginRequestPolicyIdResponse_httpStatus :: Lens.Lens' ListDistributionsByOriginRequestPolicyIdResponse Prelude.Int
listDistributionsByOriginRequestPolicyIdResponse_httpStatus = Lens.lens (\ListDistributionsByOriginRequestPolicyIdResponse' {httpStatus} -> httpStatus) (\s@ListDistributionsByOriginRequestPolicyIdResponse' {} a -> s {httpStatus = a} :: ListDistributionsByOriginRequestPolicyIdResponse)

instance
  Prelude.NFData
    ListDistributionsByOriginRequestPolicyIdResponse
  where
  rnf
    ListDistributionsByOriginRequestPolicyIdResponse' {..} =
      Prelude.rnf distributionIdList
        `Prelude.seq` Prelude.rnf httpStatus
