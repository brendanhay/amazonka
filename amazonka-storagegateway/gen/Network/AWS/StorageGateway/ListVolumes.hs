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
-- Module      : Network.AWS.StorageGateway.ListVolumes
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.StorageGateway.ListVolumes
  ( -- * Creating a Request
    ListVolumes (..),
    newListVolumes,

    -- * Request Lenses
    listVolumes_limit,
    listVolumes_gatewayARN,
    listVolumes_marker,

    -- * Destructuring the Response
    ListVolumesResponse (..),
    newListVolumesResponse,

    -- * Response Lenses
    listVolumesResponse_gatewayARN,
    listVolumesResponse_volumeInfos,
    listVolumesResponse_marker,
    listVolumesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | A JSON object that contains one or more of the following fields:
--
-- -   ListVolumesInput$Limit
--
-- -   ListVolumesInput$Marker
--
-- /See:/ 'newListVolumes' smart constructor.
data ListVolumes = ListVolumes'
  { -- | Specifies that the list of volumes returned be limited to the specified
    -- number of items.
    limit :: Prelude.Maybe Prelude.Natural,
    gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | A string that indicates the position at which to begin the returned list
    -- of volumes. Obtain the marker from the response of a previous List iSCSI
    -- Volumes request.
    marker :: Prelude.Maybe Prelude.Text
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
-- 'limit', 'listVolumes_limit' - Specifies that the list of volumes returned be limited to the specified
-- number of items.
--
-- 'gatewayARN', 'listVolumes_gatewayARN' - Undocumented member.
--
-- 'marker', 'listVolumes_marker' - A string that indicates the position at which to begin the returned list
-- of volumes. Obtain the marker from the response of a previous List iSCSI
-- Volumes request.
newListVolumes ::
  ListVolumes
newListVolumes =
  ListVolumes'
    { limit = Prelude.Nothing,
      gatewayARN = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | Specifies that the list of volumes returned be limited to the specified
-- number of items.
listVolumes_limit :: Lens.Lens' ListVolumes (Prelude.Maybe Prelude.Natural)
listVolumes_limit = Lens.lens (\ListVolumes' {limit} -> limit) (\s@ListVolumes' {} a -> s {limit = a} :: ListVolumes)

-- | Undocumented member.
listVolumes_gatewayARN :: Lens.Lens' ListVolumes (Prelude.Maybe Prelude.Text)
listVolumes_gatewayARN = Lens.lens (\ListVolumes' {gatewayARN} -> gatewayARN) (\s@ListVolumes' {} a -> s {gatewayARN = a} :: ListVolumes)

-- | A string that indicates the position at which to begin the returned list
-- of volumes. Obtain the marker from the response of a previous List iSCSI
-- Volumes request.
listVolumes_marker :: Lens.Lens' ListVolumes (Prelude.Maybe Prelude.Text)
listVolumes_marker = Lens.lens (\ListVolumes' {marker} -> marker) (\s@ListVolumes' {} a -> s {marker = a} :: ListVolumes)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVolumesResponse'
            Prelude.<$> (x Core..?> "GatewayARN")
            Prelude.<*> (x Core..?> "VolumeInfos" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListVolumes

instance Prelude.NFData ListVolumes

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
          [ ("Limit" Core..=) Prelude.<$> limit,
            ("GatewayARN" Core..=) Prelude.<$> gatewayARN,
            ("Marker" Core..=) Prelude.<$> marker
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
  { gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | An array of VolumeInfo objects, where each object describes an iSCSI
    -- volume. If no volumes are defined for the gateway, then @VolumeInfos@ is
    -- an empty array \"[]\".
    volumeInfos :: Prelude.Maybe [VolumeInfo],
    -- | Use the marker in your next request to continue pagination of iSCSI
    -- volumes. If there are no more volumes to list, this field does not
    -- appear in the response body.
    marker :: Prelude.Maybe Prelude.Text,
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
-- 'gatewayARN', 'listVolumesResponse_gatewayARN' - Undocumented member.
--
-- 'volumeInfos', 'listVolumesResponse_volumeInfos' - An array of VolumeInfo objects, where each object describes an iSCSI
-- volume. If no volumes are defined for the gateway, then @VolumeInfos@ is
-- an empty array \"[]\".
--
-- 'marker', 'listVolumesResponse_marker' - Use the marker in your next request to continue pagination of iSCSI
-- volumes. If there are no more volumes to list, this field does not
-- appear in the response body.
--
-- 'httpStatus', 'listVolumesResponse_httpStatus' - The response's http status code.
newListVolumesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListVolumesResponse
newListVolumesResponse pHttpStatus_ =
  ListVolumesResponse'
    { gatewayARN = Prelude.Nothing,
      volumeInfos = Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listVolumesResponse_gatewayARN :: Lens.Lens' ListVolumesResponse (Prelude.Maybe Prelude.Text)
listVolumesResponse_gatewayARN = Lens.lens (\ListVolumesResponse' {gatewayARN} -> gatewayARN) (\s@ListVolumesResponse' {} a -> s {gatewayARN = a} :: ListVolumesResponse)

-- | An array of VolumeInfo objects, where each object describes an iSCSI
-- volume. If no volumes are defined for the gateway, then @VolumeInfos@ is
-- an empty array \"[]\".
listVolumesResponse_volumeInfos :: Lens.Lens' ListVolumesResponse (Prelude.Maybe [VolumeInfo])
listVolumesResponse_volumeInfos = Lens.lens (\ListVolumesResponse' {volumeInfos} -> volumeInfos) (\s@ListVolumesResponse' {} a -> s {volumeInfos = a} :: ListVolumesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Use the marker in your next request to continue pagination of iSCSI
-- volumes. If there are no more volumes to list, this field does not
-- appear in the response body.
listVolumesResponse_marker :: Lens.Lens' ListVolumesResponse (Prelude.Maybe Prelude.Text)
listVolumesResponse_marker = Lens.lens (\ListVolumesResponse' {marker} -> marker) (\s@ListVolumesResponse' {} a -> s {marker = a} :: ListVolumesResponse)

-- | The response's http status code.
listVolumesResponse_httpStatus :: Lens.Lens' ListVolumesResponse Prelude.Int
listVolumesResponse_httpStatus = Lens.lens (\ListVolumesResponse' {httpStatus} -> httpStatus) (\s@ListVolumesResponse' {} a -> s {httpStatus = a} :: ListVolumesResponse)

instance Prelude.NFData ListVolumesResponse
