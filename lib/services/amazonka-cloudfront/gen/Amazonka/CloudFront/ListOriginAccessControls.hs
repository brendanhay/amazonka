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
-- Module      : Amazonka.CloudFront.ListOriginAccessControls
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the list of CloudFront origin access controls in this Amazon Web
-- Services account.
--
-- You can optionally specify the maximum number of items to receive in the
-- response. If the total number of items in the list exceeds the maximum
-- that you specify, or the default maximum, the response is paginated. To
-- get the next page of items, send another request that specifies the
-- @NextMarker@ value from the current response as the @Marker@ value in
-- the next request.
module Amazonka.CloudFront.ListOriginAccessControls
  ( -- * Creating a Request
    ListOriginAccessControls (..),
    newListOriginAccessControls,

    -- * Request Lenses
    listOriginAccessControls_marker,
    listOriginAccessControls_maxItems,

    -- * Destructuring the Response
    ListOriginAccessControlsResponse (..),
    newListOriginAccessControlsResponse,

    -- * Response Lenses
    listOriginAccessControlsResponse_originAccessControlList,
    listOriginAccessControlsResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListOriginAccessControls' smart constructor.
data ListOriginAccessControls = ListOriginAccessControls'
  { -- | Use this field when paginating results to indicate where to begin in
    -- your list of origin access controls. The response includes the items in
    -- the list that occur after the marker. To get the next page of the list,
    -- set this field\'s value to the value of @NextMarker@ from the current
    -- page\'s response.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of origin access controls that you want in the
    -- response.
    maxItems :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOriginAccessControls' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listOriginAccessControls_marker' - Use this field when paginating results to indicate where to begin in
-- your list of origin access controls. The response includes the items in
-- the list that occur after the marker. To get the next page of the list,
-- set this field\'s value to the value of @NextMarker@ from the current
-- page\'s response.
--
-- 'maxItems', 'listOriginAccessControls_maxItems' - The maximum number of origin access controls that you want in the
-- response.
newListOriginAccessControls ::
  ListOriginAccessControls
newListOriginAccessControls =
  ListOriginAccessControls'
    { marker = Prelude.Nothing,
      maxItems = Prelude.Nothing
    }

-- | Use this field when paginating results to indicate where to begin in
-- your list of origin access controls. The response includes the items in
-- the list that occur after the marker. To get the next page of the list,
-- set this field\'s value to the value of @NextMarker@ from the current
-- page\'s response.
listOriginAccessControls_marker :: Lens.Lens' ListOriginAccessControls (Prelude.Maybe Prelude.Text)
listOriginAccessControls_marker = Lens.lens (\ListOriginAccessControls' {marker} -> marker) (\s@ListOriginAccessControls' {} a -> s {marker = a} :: ListOriginAccessControls)

-- | The maximum number of origin access controls that you want in the
-- response.
listOriginAccessControls_maxItems :: Lens.Lens' ListOriginAccessControls (Prelude.Maybe Prelude.Text)
listOriginAccessControls_maxItems = Lens.lens (\ListOriginAccessControls' {maxItems} -> maxItems) (\s@ListOriginAccessControls' {} a -> s {maxItems = a} :: ListOriginAccessControls)

instance Core.AWSRequest ListOriginAccessControls where
  type
    AWSResponse ListOriginAccessControls =
      ListOriginAccessControlsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListOriginAccessControlsResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListOriginAccessControls where
  hashWithSalt _salt ListOriginAccessControls' {..} =
    _salt
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems

instance Prelude.NFData ListOriginAccessControls where
  rnf ListOriginAccessControls' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems

instance Data.ToHeaders ListOriginAccessControls where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListOriginAccessControls where
  toPath =
    Prelude.const "/2020-05-31/origin-access-control"

instance Data.ToQuery ListOriginAccessControls where
  toQuery ListOriginAccessControls' {..} =
    Prelude.mconcat
      [ "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems
      ]

-- | /See:/ 'newListOriginAccessControlsResponse' smart constructor.
data ListOriginAccessControlsResponse = ListOriginAccessControlsResponse'
  { -- | A list of origin access controls.
    originAccessControlList :: Prelude.Maybe OriginAccessControlList,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOriginAccessControlsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'originAccessControlList', 'listOriginAccessControlsResponse_originAccessControlList' - A list of origin access controls.
--
-- 'httpStatus', 'listOriginAccessControlsResponse_httpStatus' - The response's http status code.
newListOriginAccessControlsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListOriginAccessControlsResponse
newListOriginAccessControlsResponse pHttpStatus_ =
  ListOriginAccessControlsResponse'
    { originAccessControlList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of origin access controls.
listOriginAccessControlsResponse_originAccessControlList :: Lens.Lens' ListOriginAccessControlsResponse (Prelude.Maybe OriginAccessControlList)
listOriginAccessControlsResponse_originAccessControlList = Lens.lens (\ListOriginAccessControlsResponse' {originAccessControlList} -> originAccessControlList) (\s@ListOriginAccessControlsResponse' {} a -> s {originAccessControlList = a} :: ListOriginAccessControlsResponse)

-- | The response's http status code.
listOriginAccessControlsResponse_httpStatus :: Lens.Lens' ListOriginAccessControlsResponse Prelude.Int
listOriginAccessControlsResponse_httpStatus = Lens.lens (\ListOriginAccessControlsResponse' {httpStatus} -> httpStatus) (\s@ListOriginAccessControlsResponse' {} a -> s {httpStatus = a} :: ListOriginAccessControlsResponse)

instance
  Prelude.NFData
    ListOriginAccessControlsResponse
  where
  rnf ListOriginAccessControlsResponse' {..} =
    Prelude.rnf originAccessControlList
      `Prelude.seq` Prelude.rnf httpStatus
