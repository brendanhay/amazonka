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
-- Module      : Amazonka.CloudFront.ListCloudFrontOriginAccessIdentities
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists origin access identities.
--
-- This operation returns paginated results.
module Amazonka.CloudFront.ListCloudFrontOriginAccessIdentities
  ( -- * Creating a Request
    ListCloudFrontOriginAccessIdentities (..),
    newListCloudFrontOriginAccessIdentities,

    -- * Request Lenses
    listCloudFrontOriginAccessIdentities_marker,
    listCloudFrontOriginAccessIdentities_maxItems,

    -- * Destructuring the Response
    ListCloudFrontOriginAccessIdentitiesResponse (..),
    newListCloudFrontOriginAccessIdentitiesResponse,

    -- * Response Lenses
    listCloudFrontOriginAccessIdentitiesResponse_httpStatus,
    listCloudFrontOriginAccessIdentitiesResponse_cloudFrontOriginAccessIdentityList,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request to list origin access identities.
--
-- /See:/ 'newListCloudFrontOriginAccessIdentities' smart constructor.
data ListCloudFrontOriginAccessIdentities = ListCloudFrontOriginAccessIdentities'
  { -- | Use this when paginating results to indicate where to begin in your list
    -- of origin access identities. The results include identities in the list
    -- that occur after the marker. To get the next page of results, set the
    -- @Marker@ to the value of the @NextMarker@ from the current page\'s
    -- response (which is also the ID of the last identity on that page).
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of origin access identities you want in the response
    -- body.
    maxItems :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCloudFrontOriginAccessIdentities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listCloudFrontOriginAccessIdentities_marker' - Use this when paginating results to indicate where to begin in your list
-- of origin access identities. The results include identities in the list
-- that occur after the marker. To get the next page of results, set the
-- @Marker@ to the value of the @NextMarker@ from the current page\'s
-- response (which is also the ID of the last identity on that page).
--
-- 'maxItems', 'listCloudFrontOriginAccessIdentities_maxItems' - The maximum number of origin access identities you want in the response
-- body.
newListCloudFrontOriginAccessIdentities ::
  ListCloudFrontOriginAccessIdentities
newListCloudFrontOriginAccessIdentities =
  ListCloudFrontOriginAccessIdentities'
    { marker =
        Prelude.Nothing,
      maxItems = Prelude.Nothing
    }

-- | Use this when paginating results to indicate where to begin in your list
-- of origin access identities. The results include identities in the list
-- that occur after the marker. To get the next page of results, set the
-- @Marker@ to the value of the @NextMarker@ from the current page\'s
-- response (which is also the ID of the last identity on that page).
listCloudFrontOriginAccessIdentities_marker :: Lens.Lens' ListCloudFrontOriginAccessIdentities (Prelude.Maybe Prelude.Text)
listCloudFrontOriginAccessIdentities_marker = Lens.lens (\ListCloudFrontOriginAccessIdentities' {marker} -> marker) (\s@ListCloudFrontOriginAccessIdentities' {} a -> s {marker = a} :: ListCloudFrontOriginAccessIdentities)

-- | The maximum number of origin access identities you want in the response
-- body.
listCloudFrontOriginAccessIdentities_maxItems :: Lens.Lens' ListCloudFrontOriginAccessIdentities (Prelude.Maybe Prelude.Text)
listCloudFrontOriginAccessIdentities_maxItems = Lens.lens (\ListCloudFrontOriginAccessIdentities' {maxItems} -> maxItems) (\s@ListCloudFrontOriginAccessIdentities' {} a -> s {maxItems = a} :: ListCloudFrontOriginAccessIdentities)

instance
  Core.AWSPager
    ListCloudFrontOriginAccessIdentities
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^. listCloudFrontOriginAccessIdentitiesResponse_cloudFrontOriginAccessIdentityList
            Prelude.. cloudFrontOriginAccessIdentityList_isTruncated
        ) =
        Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listCloudFrontOriginAccessIdentitiesResponse_cloudFrontOriginAccessIdentityList
            Prelude.. cloudFrontOriginAccessIdentityList_nextMarker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listCloudFrontOriginAccessIdentities_marker
          Lens..~ rs
          Lens.^? listCloudFrontOriginAccessIdentitiesResponse_cloudFrontOriginAccessIdentityList
          Prelude.. cloudFrontOriginAccessIdentityList_nextMarker
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListCloudFrontOriginAccessIdentities
  where
  type
    AWSResponse ListCloudFrontOriginAccessIdentities =
      ListCloudFrontOriginAccessIdentitiesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListCloudFrontOriginAccessIdentitiesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.parseXML x)
      )

instance
  Prelude.Hashable
    ListCloudFrontOriginAccessIdentities
  where
  hashWithSalt
    _salt
    ListCloudFrontOriginAccessIdentities' {..} =
      _salt
        `Prelude.hashWithSalt` marker
        `Prelude.hashWithSalt` maxItems

instance
  Prelude.NFData
    ListCloudFrontOriginAccessIdentities
  where
  rnf ListCloudFrontOriginAccessIdentities' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems

instance
  Data.ToHeaders
    ListCloudFrontOriginAccessIdentities
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    ListCloudFrontOriginAccessIdentities
  where
  toPath =
    Prelude.const
      "/2020-05-31/origin-access-identity/cloudfront"

instance
  Data.ToQuery
    ListCloudFrontOriginAccessIdentities
  where
  toQuery ListCloudFrontOriginAccessIdentities' {..} =
    Prelude.mconcat
      [ "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems
      ]

-- | The returned result of the corresponding request.
--
-- /See:/ 'newListCloudFrontOriginAccessIdentitiesResponse' smart constructor.
data ListCloudFrontOriginAccessIdentitiesResponse = ListCloudFrontOriginAccessIdentitiesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The @CloudFrontOriginAccessIdentityList@ type.
    cloudFrontOriginAccessIdentityList :: CloudFrontOriginAccessIdentityList
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCloudFrontOriginAccessIdentitiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'listCloudFrontOriginAccessIdentitiesResponse_httpStatus' - The response's http status code.
--
-- 'cloudFrontOriginAccessIdentityList', 'listCloudFrontOriginAccessIdentitiesResponse_cloudFrontOriginAccessIdentityList' - The @CloudFrontOriginAccessIdentityList@ type.
newListCloudFrontOriginAccessIdentitiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'cloudFrontOriginAccessIdentityList'
  CloudFrontOriginAccessIdentityList ->
  ListCloudFrontOriginAccessIdentitiesResponse
newListCloudFrontOriginAccessIdentitiesResponse
  pHttpStatus_
  pCloudFrontOriginAccessIdentityList_ =
    ListCloudFrontOriginAccessIdentitiesResponse'
      { httpStatus =
          pHttpStatus_,
        cloudFrontOriginAccessIdentityList =
          pCloudFrontOriginAccessIdentityList_
      }

-- | The response's http status code.
listCloudFrontOriginAccessIdentitiesResponse_httpStatus :: Lens.Lens' ListCloudFrontOriginAccessIdentitiesResponse Prelude.Int
listCloudFrontOriginAccessIdentitiesResponse_httpStatus = Lens.lens (\ListCloudFrontOriginAccessIdentitiesResponse' {httpStatus} -> httpStatus) (\s@ListCloudFrontOriginAccessIdentitiesResponse' {} a -> s {httpStatus = a} :: ListCloudFrontOriginAccessIdentitiesResponse)

-- | The @CloudFrontOriginAccessIdentityList@ type.
listCloudFrontOriginAccessIdentitiesResponse_cloudFrontOriginAccessIdentityList :: Lens.Lens' ListCloudFrontOriginAccessIdentitiesResponse CloudFrontOriginAccessIdentityList
listCloudFrontOriginAccessIdentitiesResponse_cloudFrontOriginAccessIdentityList = Lens.lens (\ListCloudFrontOriginAccessIdentitiesResponse' {cloudFrontOriginAccessIdentityList} -> cloudFrontOriginAccessIdentityList) (\s@ListCloudFrontOriginAccessIdentitiesResponse' {} a -> s {cloudFrontOriginAccessIdentityList = a} :: ListCloudFrontOriginAccessIdentitiesResponse)

instance
  Prelude.NFData
    ListCloudFrontOriginAccessIdentitiesResponse
  where
  rnf ListCloudFrontOriginAccessIdentitiesResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf cloudFrontOriginAccessIdentityList
