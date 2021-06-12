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
-- Module      : Network.AWS.StorageGateway.DeleteVolume
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified storage volume that you previously created using
-- the CreateCachediSCSIVolume or CreateStorediSCSIVolume API. This
-- operation is only supported in the cached volume and stored volume
-- types. For stored volume gateways, the local disk that was configured as
-- the storage volume is not deleted. You can reuse the local disk to
-- create another storage volume.
--
-- Before you delete a volume, make sure there are no iSCSI connections to
-- the volume you are deleting. You should also make sure there is no
-- snapshot in progress. You can use the Amazon Elastic Compute Cloud
-- (Amazon EC2) API to query snapshots on the volume you are deleting and
-- check the snapshot status. For more information, go to
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshots.html DescribeSnapshots>
-- in the /Amazon Elastic Compute Cloud API Reference/.
--
-- In the request, you must provide the Amazon Resource Name (ARN) of the
-- storage volume you want to delete.
module Network.AWS.StorageGateway.DeleteVolume
  ( -- * Creating a Request
    DeleteVolume (..),
    newDeleteVolume,

    -- * Request Lenses
    deleteVolume_volumeARN,

    -- * Destructuring the Response
    DeleteVolumeResponse (..),
    newDeleteVolumeResponse,

    -- * Response Lenses
    deleteVolumeResponse_volumeARN,
    deleteVolumeResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | A JSON object containing the DeleteVolumeInput$VolumeARN to delete.
--
-- /See:/ 'newDeleteVolume' smart constructor.
data DeleteVolume = DeleteVolume'
  { -- | The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
    -- operation to return a list of gateway volumes.
    volumeARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteVolume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeARN', 'deleteVolume_volumeARN' - The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
-- operation to return a list of gateway volumes.
newDeleteVolume ::
  -- | 'volumeARN'
  Core.Text ->
  DeleteVolume
newDeleteVolume pVolumeARN_ =
  DeleteVolume' {volumeARN = pVolumeARN_}

-- | The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
-- operation to return a list of gateway volumes.
deleteVolume_volumeARN :: Lens.Lens' DeleteVolume Core.Text
deleteVolume_volumeARN = Lens.lens (\DeleteVolume' {volumeARN} -> volumeARN) (\s@DeleteVolume' {} a -> s {volumeARN = a} :: DeleteVolume)

instance Core.AWSRequest DeleteVolume where
  type AWSResponse DeleteVolume = DeleteVolumeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteVolumeResponse'
            Core.<$> (x Core..?> "VolumeARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteVolume

instance Core.NFData DeleteVolume

instance Core.ToHeaders DeleteVolume where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.DeleteVolume" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteVolume where
  toJSON DeleteVolume' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("VolumeARN" Core..= volumeARN)]
      )

instance Core.ToPath DeleteVolume where
  toPath = Core.const "/"

instance Core.ToQuery DeleteVolume where
  toQuery = Core.const Core.mempty

-- | A JSON object containing the Amazon Resource Name (ARN) of the storage
-- volume that was deleted.
--
-- /See:/ 'newDeleteVolumeResponse' smart constructor.
data DeleteVolumeResponse = DeleteVolumeResponse'
  { -- | The Amazon Resource Name (ARN) of the storage volume that was deleted.
    -- It is the same ARN you provided in the request.
    volumeARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteVolumeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeARN', 'deleteVolumeResponse_volumeARN' - The Amazon Resource Name (ARN) of the storage volume that was deleted.
-- It is the same ARN you provided in the request.
--
-- 'httpStatus', 'deleteVolumeResponse_httpStatus' - The response's http status code.
newDeleteVolumeResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteVolumeResponse
newDeleteVolumeResponse pHttpStatus_ =
  DeleteVolumeResponse'
    { volumeARN = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the storage volume that was deleted.
-- It is the same ARN you provided in the request.
deleteVolumeResponse_volumeARN :: Lens.Lens' DeleteVolumeResponse (Core.Maybe Core.Text)
deleteVolumeResponse_volumeARN = Lens.lens (\DeleteVolumeResponse' {volumeARN} -> volumeARN) (\s@DeleteVolumeResponse' {} a -> s {volumeARN = a} :: DeleteVolumeResponse)

-- | The response's http status code.
deleteVolumeResponse_httpStatus :: Lens.Lens' DeleteVolumeResponse Core.Int
deleteVolumeResponse_httpStatus = Lens.lens (\DeleteVolumeResponse' {httpStatus} -> httpStatus) (\s@DeleteVolumeResponse' {} a -> s {httpStatus = a} :: DeleteVolumeResponse)

instance Core.NFData DeleteVolumeResponse
