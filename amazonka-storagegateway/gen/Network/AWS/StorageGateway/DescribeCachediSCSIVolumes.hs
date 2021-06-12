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
-- Module      : Network.AWS.StorageGateway.DescribeCachediSCSIVolumes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of the gateway volumes specified in the request.
-- This operation is only supported in the cached volume gateway types.
--
-- The list of gateway volumes in the request must be from one gateway. In
-- the response, AWS Storage Gateway returns volume information sorted by
-- volume Amazon Resource Name (ARN).
module Network.AWS.StorageGateway.DescribeCachediSCSIVolumes
  ( -- * Creating a Request
    DescribeCachediSCSIVolumes (..),
    newDescribeCachediSCSIVolumes,

    -- * Request Lenses
    describeCachediSCSIVolumes_volumeARNs,

    -- * Destructuring the Response
    DescribeCachediSCSIVolumesResponse (..),
    newDescribeCachediSCSIVolumesResponse,

    -- * Response Lenses
    describeCachediSCSIVolumesResponse_cachediSCSIVolumes,
    describeCachediSCSIVolumesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'newDescribeCachediSCSIVolumes' smart constructor.
data DescribeCachediSCSIVolumes = DescribeCachediSCSIVolumes'
  { -- | An array of strings where each string represents the Amazon Resource
    -- Name (ARN) of a cached volume. All of the specified cached volumes must
    -- be from the same gateway. Use ListVolumes to get volume ARNs for a
    -- gateway.
    volumeARNs :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeCachediSCSIVolumes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeARNs', 'describeCachediSCSIVolumes_volumeARNs' - An array of strings where each string represents the Amazon Resource
-- Name (ARN) of a cached volume. All of the specified cached volumes must
-- be from the same gateway. Use ListVolumes to get volume ARNs for a
-- gateway.
newDescribeCachediSCSIVolumes ::
  DescribeCachediSCSIVolumes
newDescribeCachediSCSIVolumes =
  DescribeCachediSCSIVolumes'
    { volumeARNs =
        Core.mempty
    }

-- | An array of strings where each string represents the Amazon Resource
-- Name (ARN) of a cached volume. All of the specified cached volumes must
-- be from the same gateway. Use ListVolumes to get volume ARNs for a
-- gateway.
describeCachediSCSIVolumes_volumeARNs :: Lens.Lens' DescribeCachediSCSIVolumes [Core.Text]
describeCachediSCSIVolumes_volumeARNs = Lens.lens (\DescribeCachediSCSIVolumes' {volumeARNs} -> volumeARNs) (\s@DescribeCachediSCSIVolumes' {} a -> s {volumeARNs = a} :: DescribeCachediSCSIVolumes) Core.. Lens._Coerce

instance Core.AWSRequest DescribeCachediSCSIVolumes where
  type
    AWSResponse DescribeCachediSCSIVolumes =
      DescribeCachediSCSIVolumesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCachediSCSIVolumesResponse'
            Core.<$> ( x Core..?> "CachediSCSIVolumes"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeCachediSCSIVolumes

instance Core.NFData DescribeCachediSCSIVolumes

instance Core.ToHeaders DescribeCachediSCSIVolumes where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.DescribeCachediSCSIVolumes" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeCachediSCSIVolumes where
  toJSON DescribeCachediSCSIVolumes' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("VolumeARNs" Core..= volumeARNs)]
      )

instance Core.ToPath DescribeCachediSCSIVolumes where
  toPath = Core.const "/"

instance Core.ToQuery DescribeCachediSCSIVolumes where
  toQuery = Core.const Core.mempty

-- | A JSON object containing the following fields:
--
-- /See:/ 'newDescribeCachediSCSIVolumesResponse' smart constructor.
data DescribeCachediSCSIVolumesResponse = DescribeCachediSCSIVolumesResponse'
  { -- | An array of objects where each object contains metadata about one cached
    -- volume.
    cachediSCSIVolumes :: Core.Maybe [CachediSCSIVolume],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeCachediSCSIVolumesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cachediSCSIVolumes', 'describeCachediSCSIVolumesResponse_cachediSCSIVolumes' - An array of objects where each object contains metadata about one cached
-- volume.
--
-- 'httpStatus', 'describeCachediSCSIVolumesResponse_httpStatus' - The response's http status code.
newDescribeCachediSCSIVolumesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeCachediSCSIVolumesResponse
newDescribeCachediSCSIVolumesResponse pHttpStatus_ =
  DescribeCachediSCSIVolumesResponse'
    { cachediSCSIVolumes =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects where each object contains metadata about one cached
-- volume.
describeCachediSCSIVolumesResponse_cachediSCSIVolumes :: Lens.Lens' DescribeCachediSCSIVolumesResponse (Core.Maybe [CachediSCSIVolume])
describeCachediSCSIVolumesResponse_cachediSCSIVolumes = Lens.lens (\DescribeCachediSCSIVolumesResponse' {cachediSCSIVolumes} -> cachediSCSIVolumes) (\s@DescribeCachediSCSIVolumesResponse' {} a -> s {cachediSCSIVolumes = a} :: DescribeCachediSCSIVolumesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeCachediSCSIVolumesResponse_httpStatus :: Lens.Lens' DescribeCachediSCSIVolumesResponse Core.Int
describeCachediSCSIVolumesResponse_httpStatus = Lens.lens (\DescribeCachediSCSIVolumesResponse' {httpStatus} -> httpStatus) (\s@DescribeCachediSCSIVolumesResponse' {} a -> s {httpStatus = a} :: DescribeCachediSCSIVolumesResponse)

instance
  Core.NFData
    DescribeCachediSCSIVolumesResponse
