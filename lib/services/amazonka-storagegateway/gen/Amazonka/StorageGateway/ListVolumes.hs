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
-- Module      : Amazonka.StorageGateway.ListVolumes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the iSCSI stored volumes of a gateway. Results are sorted by
-- volume ARN. The response includes only the volume ARNs. If you want
-- additional volume information, use the DescribeStorediSCSIVolumes or the
-- DescribeCachediSCSIVolumes API.
--
-- The operation supports pagination. By default, the operation returns a
-- maximum of up to 100 volumes. You can optionally specify the @Limit@
-- field in the body to limit the number of volumes in the response. If the
-- number of volumes returned in the response is truncated, the response
-- includes a Marker field. You can use this Marker value in your
-- subsequent request to retrieve the next set of volumes. This operation
-- is only supported in the cached volume and stored volume gateway types.
--
-- This operation returns paginated results.
module Amazonka.StorageGateway.ListVolumes
  ( -- * Creating a Request
    ListVolumes (..),
    newListVolumes,

    -- * Request Lenses
    listVolumes_marker,
    listVolumes_gatewayARN,
    listVolumes_limit,

    -- * Destructuring the Response
    ListVolumesResponse (..),
    newListVolumesResponse,

    -- * Response Lenses
    listVolumesResponse_marker,
    listVolumesResponse_volumeInfos,
    listVolumesResponse_gatewayARN,
    listVolumesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | A JSON object that contains one or more of the following fields:
--
-- -   ListVolumesInput$Limit
--
-- -   ListVolumesInput$Marker
--
-- /See:/ 'newListVolumes' smart constructor.
data ListVolumes = ListVolumes'
  { -- | A string that indicates the position at which to begin the returned list
    -- of volumes. Obtain the marker from the response of a previous List iSCSI
    -- Volumes request.
    marker :: Prelude.Maybe Prelude.Text,
    gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | Specifies that the list of volumes returned be limited to the specified
    -- number of items.
    limit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVolumes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listVolumes_marker' - A string that indicates the position at which to begin the returned list
-- of volumes. Obtain the marker from the response of a previous List iSCSI
-- Volumes request.
--
-- 'gatewayARN', 'listVolumes_gatewayARN' - Undocumented member.
--
-- 'limit', 'listVolumes_limit' - Specifies that the list of volumes returned be limited to the specified
-- number of items.
newListVolumes ::
  ListVolumes
newListVolumes =
  ListVolumes'
    { marker = Prelude.Nothing,
      gatewayARN = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | A string that indicates the position at which to begin the returned list
-- of volumes. Obtain the marker from the response of a previous List iSCSI
-- Volumes request.
listVolumes_marker :: Lens.Lens' ListVolumes (Prelude.Maybe Prelude.Text)
listVolumes_marker = Lens.lens (\ListVolumes' {marker} -> marker) (\s@ListVolumes' {} a -> s {marker = a} :: ListVolumes)

-- | Undocumented member.
listVolumes_gatewayARN :: Lens.Lens' ListVolumes (Prelude.Maybe Prelude.Text)
listVolumes_gatewayARN = Lens.lens (\ListVolumes' {gatewayARN} -> gatewayARN) (\s@ListVolumes' {} a -> s {gatewayARN = a} :: ListVolumes)

-- | Specifies that the list of volumes returned be limited to the specified
-- number of items.
listVolumes_limit :: Lens.Lens' ListVolumes (Prelude.Maybe Prelude.Natural)
listVolumes_limit = Lens.lens (\ListVolumes' {limit} -> limit) (\s@ListVolumes' {} a -> s {limit = a} :: ListVolumes)

instance Core.AWSPager ListVolumes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listVolumesResponse_marker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listVolumesResponse_volumeInfos Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listVolumes_marker
          Lens..~ rs
          Lens.^? listVolumesResponse_marker Prelude.. Lens._Just

instance Core.AWSRequest ListVolumes where
  type AWSResponse ListVolumes = ListVolumesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVolumesResponse'
            Prelude.<$> (x Core..?> "Marker")
            Prelude.<*> (x Core..?> "VolumeInfos" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "GatewayARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListVolumes where
  hashWithSalt _salt ListVolumes' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` gatewayARN
      `Prelude.hashWithSalt` limit

instance Prelude.NFData ListVolumes where
  rnf ListVolumes' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf limit

instance Core.ToHeaders ListVolumes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.ListVolumes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListVolumes where
  toJSON ListVolumes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Marker" Core..=) Prelude.<$> marker,
            ("GatewayARN" Core..=) Prelude.<$> gatewayARN,
            ("Limit" Core..=) Prelude.<$> limit
          ]
      )

instance Core.ToPath ListVolumes where
  toPath = Prelude.const "/"

instance Core.ToQuery ListVolumes where
  toQuery = Prelude.const Prelude.mempty

-- | A JSON object containing the following fields:
--
-- -   ListVolumesOutput$Marker
--
-- -   ListVolumesOutput$VolumeInfos
--
-- /See:/ 'newListVolumesResponse' smart constructor.
data ListVolumesResponse = ListVolumesResponse'
  { -- | Use the marker in your next request to continue pagination of iSCSI
    -- volumes. If there are no more volumes to list, this field does not
    -- appear in the response body.
    marker :: Prelude.Maybe Prelude.Text,
    -- | An array of VolumeInfo objects, where each object describes an iSCSI
    -- volume. If no volumes are defined for the gateway, then @VolumeInfos@ is
    -- an empty array \"[]\".
    volumeInfos :: Prelude.Maybe [VolumeInfo],
    gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVolumesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listVolumesResponse_marker' - Use the marker in your next request to continue pagination of iSCSI
-- volumes. If there are no more volumes to list, this field does not
-- appear in the response body.
--
-- 'volumeInfos', 'listVolumesResponse_volumeInfos' - An array of VolumeInfo objects, where each object describes an iSCSI
-- volume. If no volumes are defined for the gateway, then @VolumeInfos@ is
-- an empty array \"[]\".
--
-- 'gatewayARN', 'listVolumesResponse_gatewayARN' - Undocumented member.
--
-- 'httpStatus', 'listVolumesResponse_httpStatus' - The response's http status code.
newListVolumesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListVolumesResponse
newListVolumesResponse pHttpStatus_ =
  ListVolumesResponse'
    { marker = Prelude.Nothing,
      volumeInfos = Prelude.Nothing,
      gatewayARN = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Use the marker in your next request to continue pagination of iSCSI
-- volumes. If there are no more volumes to list, this field does not
-- appear in the response body.
listVolumesResponse_marker :: Lens.Lens' ListVolumesResponse (Prelude.Maybe Prelude.Text)
listVolumesResponse_marker = Lens.lens (\ListVolumesResponse' {marker} -> marker) (\s@ListVolumesResponse' {} a -> s {marker = a} :: ListVolumesResponse)

-- | An array of VolumeInfo objects, where each object describes an iSCSI
-- volume. If no volumes are defined for the gateway, then @VolumeInfos@ is
-- an empty array \"[]\".
listVolumesResponse_volumeInfos :: Lens.Lens' ListVolumesResponse (Prelude.Maybe [VolumeInfo])
listVolumesResponse_volumeInfos = Lens.lens (\ListVolumesResponse' {volumeInfos} -> volumeInfos) (\s@ListVolumesResponse' {} a -> s {volumeInfos = a} :: ListVolumesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
listVolumesResponse_gatewayARN :: Lens.Lens' ListVolumesResponse (Prelude.Maybe Prelude.Text)
listVolumesResponse_gatewayARN = Lens.lens (\ListVolumesResponse' {gatewayARN} -> gatewayARN) (\s@ListVolumesResponse' {} a -> s {gatewayARN = a} :: ListVolumesResponse)

-- | The response's http status code.
listVolumesResponse_httpStatus :: Lens.Lens' ListVolumesResponse Prelude.Int
listVolumesResponse_httpStatus = Lens.lens (\ListVolumesResponse' {httpStatus} -> httpStatus) (\s@ListVolumesResponse' {} a -> s {httpStatus = a} :: ListVolumesResponse)

instance Prelude.NFData ListVolumesResponse where
  rnf ListVolumesResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf volumeInfos
      `Prelude.seq` Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf httpStatus
