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
-- Module      : Amazonka.FSx.UpdateVolume
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration of an Amazon FSx for NetApp ONTAP or Amazon
-- FSx for OpenZFS volume.
module Amazonka.FSx.UpdateVolume
  ( -- * Creating a Request
    UpdateVolume (..),
    newUpdateVolume,

    -- * Request Lenses
    updateVolume_clientRequestToken,
    updateVolume_name,
    updateVolume_ontapConfiguration,
    updateVolume_openZFSConfiguration,
    updateVolume_volumeId,

    -- * Destructuring the Response
    UpdateVolumeResponse (..),
    newUpdateVolumeResponse,

    -- * Response Lenses
    updateVolumeResponse_volume,
    updateVolumeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateVolume' smart constructor.
data UpdateVolume = UpdateVolume'
  { clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the OpenZFS volume. OpenZFS root volumes are automatically
    -- named @FSX@. Child volume names must be unique among their parent
    -- volume\'s children. The name of the volume is part of the mount string
    -- for the OpenZFS volume.
    name :: Prelude.Maybe Prelude.Text,
    -- | The configuration of the ONTAP volume that you are updating.
    ontapConfiguration :: Prelude.Maybe UpdateOntapVolumeConfiguration,
    -- | The configuration of the OpenZFS volume that you are updating.
    openZFSConfiguration :: Prelude.Maybe UpdateOpenZFSVolumeConfiguration,
    -- | The ID of the volume that you want to update, in the format
    -- @fsvol-0123456789abcdef0@.
    volumeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVolume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'updateVolume_clientRequestToken' - Undocumented member.
--
-- 'name', 'updateVolume_name' - The name of the OpenZFS volume. OpenZFS root volumes are automatically
-- named @FSX@. Child volume names must be unique among their parent
-- volume\'s children. The name of the volume is part of the mount string
-- for the OpenZFS volume.
--
-- 'ontapConfiguration', 'updateVolume_ontapConfiguration' - The configuration of the ONTAP volume that you are updating.
--
-- 'openZFSConfiguration', 'updateVolume_openZFSConfiguration' - The configuration of the OpenZFS volume that you are updating.
--
-- 'volumeId', 'updateVolume_volumeId' - The ID of the volume that you want to update, in the format
-- @fsvol-0123456789abcdef0@.
newUpdateVolume ::
  -- | 'volumeId'
  Prelude.Text ->
  UpdateVolume
newUpdateVolume pVolumeId_ =
  UpdateVolume'
    { clientRequestToken = Prelude.Nothing,
      name = Prelude.Nothing,
      ontapConfiguration = Prelude.Nothing,
      openZFSConfiguration = Prelude.Nothing,
      volumeId = pVolumeId_
    }

-- | Undocumented member.
updateVolume_clientRequestToken :: Lens.Lens' UpdateVolume (Prelude.Maybe Prelude.Text)
updateVolume_clientRequestToken = Lens.lens (\UpdateVolume' {clientRequestToken} -> clientRequestToken) (\s@UpdateVolume' {} a -> s {clientRequestToken = a} :: UpdateVolume)

-- | The name of the OpenZFS volume. OpenZFS root volumes are automatically
-- named @FSX@. Child volume names must be unique among their parent
-- volume\'s children. The name of the volume is part of the mount string
-- for the OpenZFS volume.
updateVolume_name :: Lens.Lens' UpdateVolume (Prelude.Maybe Prelude.Text)
updateVolume_name = Lens.lens (\UpdateVolume' {name} -> name) (\s@UpdateVolume' {} a -> s {name = a} :: UpdateVolume)

-- | The configuration of the ONTAP volume that you are updating.
updateVolume_ontapConfiguration :: Lens.Lens' UpdateVolume (Prelude.Maybe UpdateOntapVolumeConfiguration)
updateVolume_ontapConfiguration = Lens.lens (\UpdateVolume' {ontapConfiguration} -> ontapConfiguration) (\s@UpdateVolume' {} a -> s {ontapConfiguration = a} :: UpdateVolume)

-- | The configuration of the OpenZFS volume that you are updating.
updateVolume_openZFSConfiguration :: Lens.Lens' UpdateVolume (Prelude.Maybe UpdateOpenZFSVolumeConfiguration)
updateVolume_openZFSConfiguration = Lens.lens (\UpdateVolume' {openZFSConfiguration} -> openZFSConfiguration) (\s@UpdateVolume' {} a -> s {openZFSConfiguration = a} :: UpdateVolume)

-- | The ID of the volume that you want to update, in the format
-- @fsvol-0123456789abcdef0@.
updateVolume_volumeId :: Lens.Lens' UpdateVolume Prelude.Text
updateVolume_volumeId = Lens.lens (\UpdateVolume' {volumeId} -> volumeId) (\s@UpdateVolume' {} a -> s {volumeId = a} :: UpdateVolume)

instance Core.AWSRequest UpdateVolume where
  type AWSResponse UpdateVolume = UpdateVolumeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateVolumeResponse'
            Prelude.<$> (x Data..?> "Volume")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateVolume where
  hashWithSalt _salt UpdateVolume' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` ontapConfiguration
      `Prelude.hashWithSalt` openZFSConfiguration
      `Prelude.hashWithSalt` volumeId

instance Prelude.NFData UpdateVolume where
  rnf UpdateVolume' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf ontapConfiguration
      `Prelude.seq` Prelude.rnf openZFSConfiguration
      `Prelude.seq` Prelude.rnf volumeId

instance Data.ToHeaders UpdateVolume where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSimbaAPIService_v20180301.UpdateVolume" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateVolume where
  toJSON UpdateVolume' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("Name" Data..=) Prelude.<$> name,
            ("OntapConfiguration" Data..=)
              Prelude.<$> ontapConfiguration,
            ("OpenZFSConfiguration" Data..=)
              Prelude.<$> openZFSConfiguration,
            Prelude.Just ("VolumeId" Data..= volumeId)
          ]
      )

instance Data.ToPath UpdateVolume where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateVolume where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateVolumeResponse' smart constructor.
data UpdateVolumeResponse = UpdateVolumeResponse'
  { -- | A description of the volume just updated. Returned after a successful
    -- @UpdateVolume@ API operation.
    volume :: Prelude.Maybe Volume,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVolumeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volume', 'updateVolumeResponse_volume' - A description of the volume just updated. Returned after a successful
-- @UpdateVolume@ API operation.
--
-- 'httpStatus', 'updateVolumeResponse_httpStatus' - The response's http status code.
newUpdateVolumeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateVolumeResponse
newUpdateVolumeResponse pHttpStatus_ =
  UpdateVolumeResponse'
    { volume = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A description of the volume just updated. Returned after a successful
-- @UpdateVolume@ API operation.
updateVolumeResponse_volume :: Lens.Lens' UpdateVolumeResponse (Prelude.Maybe Volume)
updateVolumeResponse_volume = Lens.lens (\UpdateVolumeResponse' {volume} -> volume) (\s@UpdateVolumeResponse' {} a -> s {volume = a} :: UpdateVolumeResponse)

-- | The response's http status code.
updateVolumeResponse_httpStatus :: Lens.Lens' UpdateVolumeResponse Prelude.Int
updateVolumeResponse_httpStatus = Lens.lens (\UpdateVolumeResponse' {httpStatus} -> httpStatus) (\s@UpdateVolumeResponse' {} a -> s {httpStatus = a} :: UpdateVolumeResponse)

instance Prelude.NFData UpdateVolumeResponse where
  rnf UpdateVolumeResponse' {..} =
    Prelude.rnf volume
      `Prelude.seq` Prelude.rnf httpStatus
