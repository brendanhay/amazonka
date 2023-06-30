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
-- Module      : Amazonka.CloudFront.ListDistributionsByKeyGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of distribution IDs for distributions that have a cache
-- behavior that references the specified key group.
--
-- You can optionally specify the maximum number of items to receive in the
-- response. If the total number of items in the list exceeds the maximum
-- that you specify, or the default maximum, the response is paginated. To
-- get the next page of items, send a subsequent request that specifies the
-- @NextMarker@ value from the current response as the @Marker@ value in
-- the subsequent request.
module Amazonka.CloudFront.ListDistributionsByKeyGroup
  ( -- * Creating a Request
    ListDistributionsByKeyGroup (..),
    newListDistributionsByKeyGroup,

    -- * Request Lenses
    listDistributionsByKeyGroup_marker,
    listDistributionsByKeyGroup_maxItems,
    listDistributionsByKeyGroup_keyGroupId,

    -- * Destructuring the Response
    ListDistributionsByKeyGroupResponse (..),
    newListDistributionsByKeyGroupResponse,

    -- * Response Lenses
    listDistributionsByKeyGroupResponse_distributionIdList,
    listDistributionsByKeyGroupResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDistributionsByKeyGroup' smart constructor.
data ListDistributionsByKeyGroup = ListDistributionsByKeyGroup'
  { -- | Use this field when paginating results to indicate where to begin in
    -- your list of distribution IDs. The response includes distribution IDs in
    -- the list that occur after the marker. To get the next page of the list,
    -- set this field\'s value to the value of @NextMarker@ from the current
    -- page\'s response.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of distribution IDs that you want in the response.
    maxItems :: Prelude.Maybe Prelude.Text,
    -- | The ID of the key group whose associated distribution IDs you are
    -- listing.
    keyGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDistributionsByKeyGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listDistributionsByKeyGroup_marker' - Use this field when paginating results to indicate where to begin in
-- your list of distribution IDs. The response includes distribution IDs in
-- the list that occur after the marker. To get the next page of the list,
-- set this field\'s value to the value of @NextMarker@ from the current
-- page\'s response.
--
-- 'maxItems', 'listDistributionsByKeyGroup_maxItems' - The maximum number of distribution IDs that you want in the response.
--
-- 'keyGroupId', 'listDistributionsByKeyGroup_keyGroupId' - The ID of the key group whose associated distribution IDs you are
-- listing.
newListDistributionsByKeyGroup ::
  -- | 'keyGroupId'
  Prelude.Text ->
  ListDistributionsByKeyGroup
newListDistributionsByKeyGroup pKeyGroupId_ =
  ListDistributionsByKeyGroup'
    { marker =
        Prelude.Nothing,
      maxItems = Prelude.Nothing,
      keyGroupId = pKeyGroupId_
    }

-- | Use this field when paginating results to indicate where to begin in
-- your list of distribution IDs. The response includes distribution IDs in
-- the list that occur after the marker. To get the next page of the list,
-- set this field\'s value to the value of @NextMarker@ from the current
-- page\'s response.
listDistributionsByKeyGroup_marker :: Lens.Lens' ListDistributionsByKeyGroup (Prelude.Maybe Prelude.Text)
listDistributionsByKeyGroup_marker = Lens.lens (\ListDistributionsByKeyGroup' {marker} -> marker) (\s@ListDistributionsByKeyGroup' {} a -> s {marker = a} :: ListDistributionsByKeyGroup)

-- | The maximum number of distribution IDs that you want in the response.
listDistributionsByKeyGroup_maxItems :: Lens.Lens' ListDistributionsByKeyGroup (Prelude.Maybe Prelude.Text)
listDistributionsByKeyGroup_maxItems = Lens.lens (\ListDistributionsByKeyGroup' {maxItems} -> maxItems) (\s@ListDistributionsByKeyGroup' {} a -> s {maxItems = a} :: ListDistributionsByKeyGroup)

-- | The ID of the key group whose associated distribution IDs you are
-- listing.
listDistributionsByKeyGroup_keyGroupId :: Lens.Lens' ListDistributionsByKeyGroup Prelude.Text
listDistributionsByKeyGroup_keyGroupId = Lens.lens (\ListDistributionsByKeyGroup' {keyGroupId} -> keyGroupId) (\s@ListDistributionsByKeyGroup' {} a -> s {keyGroupId = a} :: ListDistributionsByKeyGroup)

instance Core.AWSRequest ListDistributionsByKeyGroup where
  type
    AWSResponse ListDistributionsByKeyGroup =
      ListDistributionsByKeyGroupResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListDistributionsByKeyGroupResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDistributionsByKeyGroup where
  hashWithSalt _salt ListDistributionsByKeyGroup' {..} =
    _salt
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` keyGroupId

instance Prelude.NFData ListDistributionsByKeyGroup where
  rnf ListDistributionsByKeyGroup' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf keyGroupId

instance Data.ToHeaders ListDistributionsByKeyGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListDistributionsByKeyGroup where
  toPath ListDistributionsByKeyGroup' {..} =
    Prelude.mconcat
      [ "/2020-05-31/distributionsByKeyGroupId/",
        Data.toBS keyGroupId
      ]

instance Data.ToQuery ListDistributionsByKeyGroup where
  toQuery ListDistributionsByKeyGroup' {..} =
    Prelude.mconcat
      [ "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems
      ]

-- | /See:/ 'newListDistributionsByKeyGroupResponse' smart constructor.
data ListDistributionsByKeyGroupResponse = ListDistributionsByKeyGroupResponse'
  { distributionIdList :: Prelude.Maybe DistributionIdList,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDistributionsByKeyGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distributionIdList', 'listDistributionsByKeyGroupResponse_distributionIdList' - Undocumented member.
--
-- 'httpStatus', 'listDistributionsByKeyGroupResponse_httpStatus' - The response's http status code.
newListDistributionsByKeyGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDistributionsByKeyGroupResponse
newListDistributionsByKeyGroupResponse pHttpStatus_ =
  ListDistributionsByKeyGroupResponse'
    { distributionIdList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listDistributionsByKeyGroupResponse_distributionIdList :: Lens.Lens' ListDistributionsByKeyGroupResponse (Prelude.Maybe DistributionIdList)
listDistributionsByKeyGroupResponse_distributionIdList = Lens.lens (\ListDistributionsByKeyGroupResponse' {distributionIdList} -> distributionIdList) (\s@ListDistributionsByKeyGroupResponse' {} a -> s {distributionIdList = a} :: ListDistributionsByKeyGroupResponse)

-- | The response's http status code.
listDistributionsByKeyGroupResponse_httpStatus :: Lens.Lens' ListDistributionsByKeyGroupResponse Prelude.Int
listDistributionsByKeyGroupResponse_httpStatus = Lens.lens (\ListDistributionsByKeyGroupResponse' {httpStatus} -> httpStatus) (\s@ListDistributionsByKeyGroupResponse' {} a -> s {httpStatus = a} :: ListDistributionsByKeyGroupResponse)

instance
  Prelude.NFData
    ListDistributionsByKeyGroupResponse
  where
  rnf ListDistributionsByKeyGroupResponse' {..} =
    Prelude.rnf distributionIdList
      `Prelude.seq` Prelude.rnf httpStatus
