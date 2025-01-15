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
-- Module      : Amazonka.FSx.DeleteVolume
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon FSx for NetApp ONTAP or Amazon FSx for OpenZFS volume.
module Amazonka.FSx.DeleteVolume
  ( -- * Creating a Request
    DeleteVolume (..),
    newDeleteVolume,

    -- * Request Lenses
    deleteVolume_clientRequestToken,
    deleteVolume_ontapConfiguration,
    deleteVolume_openZFSConfiguration,
    deleteVolume_volumeId,

    -- * Destructuring the Response
    DeleteVolumeResponse (..),
    newDeleteVolumeResponse,

    -- * Response Lenses
    deleteVolumeResponse_lifecycle,
    deleteVolumeResponse_ontapResponse,
    deleteVolumeResponse_volumeId,
    deleteVolumeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVolume' smart constructor.
data DeleteVolume = DeleteVolume'
  { clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | For Amazon FSx for ONTAP volumes, specify whether to take a final backup
    -- of the volume and apply tags to the backup. To apply tags to the backup,
    -- you must have the @fsx:TagResource@ permission.
    ontapConfiguration :: Prelude.Maybe DeleteVolumeOntapConfiguration,
    -- | For Amazon FSx for OpenZFS volumes, specify whether to delete all child
    -- volumes and snapshots.
    openZFSConfiguration :: Prelude.Maybe DeleteVolumeOpenZFSConfiguration,
    -- | The ID of the volume that you are deleting.
    volumeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVolume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'deleteVolume_clientRequestToken' - Undocumented member.
--
-- 'ontapConfiguration', 'deleteVolume_ontapConfiguration' - For Amazon FSx for ONTAP volumes, specify whether to take a final backup
-- of the volume and apply tags to the backup. To apply tags to the backup,
-- you must have the @fsx:TagResource@ permission.
--
-- 'openZFSConfiguration', 'deleteVolume_openZFSConfiguration' - For Amazon FSx for OpenZFS volumes, specify whether to delete all child
-- volumes and snapshots.
--
-- 'volumeId', 'deleteVolume_volumeId' - The ID of the volume that you are deleting.
newDeleteVolume ::
  -- | 'volumeId'
  Prelude.Text ->
  DeleteVolume
newDeleteVolume pVolumeId_ =
  DeleteVolume'
    { clientRequestToken = Prelude.Nothing,
      ontapConfiguration = Prelude.Nothing,
      openZFSConfiguration = Prelude.Nothing,
      volumeId = pVolumeId_
    }

-- | Undocumented member.
deleteVolume_clientRequestToken :: Lens.Lens' DeleteVolume (Prelude.Maybe Prelude.Text)
deleteVolume_clientRequestToken = Lens.lens (\DeleteVolume' {clientRequestToken} -> clientRequestToken) (\s@DeleteVolume' {} a -> s {clientRequestToken = a} :: DeleteVolume)

-- | For Amazon FSx for ONTAP volumes, specify whether to take a final backup
-- of the volume and apply tags to the backup. To apply tags to the backup,
-- you must have the @fsx:TagResource@ permission.
deleteVolume_ontapConfiguration :: Lens.Lens' DeleteVolume (Prelude.Maybe DeleteVolumeOntapConfiguration)
deleteVolume_ontapConfiguration = Lens.lens (\DeleteVolume' {ontapConfiguration} -> ontapConfiguration) (\s@DeleteVolume' {} a -> s {ontapConfiguration = a} :: DeleteVolume)

-- | For Amazon FSx for OpenZFS volumes, specify whether to delete all child
-- volumes and snapshots.
deleteVolume_openZFSConfiguration :: Lens.Lens' DeleteVolume (Prelude.Maybe DeleteVolumeOpenZFSConfiguration)
deleteVolume_openZFSConfiguration = Lens.lens (\DeleteVolume' {openZFSConfiguration} -> openZFSConfiguration) (\s@DeleteVolume' {} a -> s {openZFSConfiguration = a} :: DeleteVolume)

-- | The ID of the volume that you are deleting.
deleteVolume_volumeId :: Lens.Lens' DeleteVolume Prelude.Text
deleteVolume_volumeId = Lens.lens (\DeleteVolume' {volumeId} -> volumeId) (\s@DeleteVolume' {} a -> s {volumeId = a} :: DeleteVolume)

instance Core.AWSRequest DeleteVolume where
  type AWSResponse DeleteVolume = DeleteVolumeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteVolumeResponse'
            Prelude.<$> (x Data..?> "Lifecycle")
            Prelude.<*> (x Data..?> "OntapResponse")
            Prelude.<*> (x Data..?> "VolumeId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteVolume where
  hashWithSalt _salt DeleteVolume' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` ontapConfiguration
      `Prelude.hashWithSalt` openZFSConfiguration
      `Prelude.hashWithSalt` volumeId

instance Prelude.NFData DeleteVolume where
  rnf DeleteVolume' {..} =
    Prelude.rnf clientRequestToken `Prelude.seq`
      Prelude.rnf ontapConfiguration `Prelude.seq`
        Prelude.rnf openZFSConfiguration `Prelude.seq`
          Prelude.rnf volumeId

instance Data.ToHeaders DeleteVolume where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSimbaAPIService_v20180301.DeleteVolume" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteVolume where
  toJSON DeleteVolume' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("OntapConfiguration" Data..=)
              Prelude.<$> ontapConfiguration,
            ("OpenZFSConfiguration" Data..=)
              Prelude.<$> openZFSConfiguration,
            Prelude.Just ("VolumeId" Data..= volumeId)
          ]
      )

instance Data.ToPath DeleteVolume where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteVolume where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVolumeResponse' smart constructor.
data DeleteVolumeResponse = DeleteVolumeResponse'
  { -- | The lifecycle state of the volume being deleted. If the @DeleteVolume@
    -- operation is successful, this value is @DELETING@.
    lifecycle :: Prelude.Maybe VolumeLifecycle,
    -- | Returned after a @DeleteVolume@ request, showing the status of the
    -- delete request.
    ontapResponse :: Prelude.Maybe DeleteVolumeOntapResponse,
    -- | The ID of the volume that\'s being deleted.
    volumeId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVolumeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lifecycle', 'deleteVolumeResponse_lifecycle' - The lifecycle state of the volume being deleted. If the @DeleteVolume@
-- operation is successful, this value is @DELETING@.
--
-- 'ontapResponse', 'deleteVolumeResponse_ontapResponse' - Returned after a @DeleteVolume@ request, showing the status of the
-- delete request.
--
-- 'volumeId', 'deleteVolumeResponse_volumeId' - The ID of the volume that\'s being deleted.
--
-- 'httpStatus', 'deleteVolumeResponse_httpStatus' - The response's http status code.
newDeleteVolumeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteVolumeResponse
newDeleteVolumeResponse pHttpStatus_ =
  DeleteVolumeResponse'
    { lifecycle = Prelude.Nothing,
      ontapResponse = Prelude.Nothing,
      volumeId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The lifecycle state of the volume being deleted. If the @DeleteVolume@
-- operation is successful, this value is @DELETING@.
deleteVolumeResponse_lifecycle :: Lens.Lens' DeleteVolumeResponse (Prelude.Maybe VolumeLifecycle)
deleteVolumeResponse_lifecycle = Lens.lens (\DeleteVolumeResponse' {lifecycle} -> lifecycle) (\s@DeleteVolumeResponse' {} a -> s {lifecycle = a} :: DeleteVolumeResponse)

-- | Returned after a @DeleteVolume@ request, showing the status of the
-- delete request.
deleteVolumeResponse_ontapResponse :: Lens.Lens' DeleteVolumeResponse (Prelude.Maybe DeleteVolumeOntapResponse)
deleteVolumeResponse_ontapResponse = Lens.lens (\DeleteVolumeResponse' {ontapResponse} -> ontapResponse) (\s@DeleteVolumeResponse' {} a -> s {ontapResponse = a} :: DeleteVolumeResponse)

-- | The ID of the volume that\'s being deleted.
deleteVolumeResponse_volumeId :: Lens.Lens' DeleteVolumeResponse (Prelude.Maybe Prelude.Text)
deleteVolumeResponse_volumeId = Lens.lens (\DeleteVolumeResponse' {volumeId} -> volumeId) (\s@DeleteVolumeResponse' {} a -> s {volumeId = a} :: DeleteVolumeResponse)

-- | The response's http status code.
deleteVolumeResponse_httpStatus :: Lens.Lens' DeleteVolumeResponse Prelude.Int
deleteVolumeResponse_httpStatus = Lens.lens (\DeleteVolumeResponse' {httpStatus} -> httpStatus) (\s@DeleteVolumeResponse' {} a -> s {httpStatus = a} :: DeleteVolumeResponse)

instance Prelude.NFData DeleteVolumeResponse where
  rnf DeleteVolumeResponse' {..} =
    Prelude.rnf lifecycle `Prelude.seq`
      Prelude.rnf ontapResponse `Prelude.seq`
        Prelude.rnf volumeId `Prelude.seq`
          Prelude.rnf httpStatus
