{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | A JSON object containing the DeleteVolumeInput$VolumeARN to delete.
--
-- /See:/ 'newDeleteVolume' smart constructor.
data DeleteVolume = DeleteVolume'
  { -- | The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
    -- operation to return a list of gateway volumes.
    volumeARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteVolume
newDeleteVolume pVolumeARN_ =
  DeleteVolume' {volumeARN = pVolumeARN_}

-- | The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
-- operation to return a list of gateway volumes.
deleteVolume_volumeARN :: Lens.Lens' DeleteVolume Prelude.Text
deleteVolume_volumeARN = Lens.lens (\DeleteVolume' {volumeARN} -> volumeARN) (\s@DeleteVolume' {} a -> s {volumeARN = a} :: DeleteVolume)

instance Prelude.AWSRequest DeleteVolume where
  type Rs DeleteVolume = DeleteVolumeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteVolumeResponse'
            Prelude.<$> (x Prelude..?> "VolumeARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteVolume

instance Prelude.NFData DeleteVolume

instance Prelude.ToHeaders DeleteVolume where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StorageGateway_20130630.DeleteVolume" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteVolume where
  toJSON DeleteVolume' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("VolumeARN" Prelude..= volumeARN)]
      )

instance Prelude.ToPath DeleteVolume where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteVolume where
  toQuery = Prelude.const Prelude.mempty

-- | A JSON object containing the Amazon Resource Name (ARN) of the storage
-- volume that was deleted.
--
-- /See:/ 'newDeleteVolumeResponse' smart constructor.
data DeleteVolumeResponse = DeleteVolumeResponse'
  { -- | The Amazon Resource Name (ARN) of the storage volume that was deleted.
    -- It is the same ARN you provided in the request.
    volumeARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteVolumeResponse
newDeleteVolumeResponse pHttpStatus_ =
  DeleteVolumeResponse'
    { volumeARN = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the storage volume that was deleted.
-- It is the same ARN you provided in the request.
deleteVolumeResponse_volumeARN :: Lens.Lens' DeleteVolumeResponse (Prelude.Maybe Prelude.Text)
deleteVolumeResponse_volumeARN = Lens.lens (\DeleteVolumeResponse' {volumeARN} -> volumeARN) (\s@DeleteVolumeResponse' {} a -> s {volumeARN = a} :: DeleteVolumeResponse)

-- | The response's http status code.
deleteVolumeResponse_httpStatus :: Lens.Lens' DeleteVolumeResponse Prelude.Int
deleteVolumeResponse_httpStatus = Lens.lens (\DeleteVolumeResponse' {httpStatus} -> httpStatus) (\s@DeleteVolumeResponse' {} a -> s {httpStatus = a} :: DeleteVolumeResponse)

instance Prelude.NFData DeleteVolumeResponse
