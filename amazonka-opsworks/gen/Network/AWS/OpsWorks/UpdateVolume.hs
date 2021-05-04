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
-- Module      : Network.AWS.OpsWorks.UpdateVolume
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an Amazon EBS volume\'s name or mount point. For more
-- information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.UpdateVolume
  ( -- * Creating a Request
    UpdateVolume (..),
    newUpdateVolume,

    -- * Request Lenses
    updateVolume_name,
    updateVolume_mountPoint,
    updateVolume_volumeId,

    -- * Destructuring the Response
    UpdateVolumeResponse (..),
    newUpdateVolumeResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateVolume' smart constructor.
data UpdateVolume = UpdateVolume'
  { -- | The new name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The new mount point.
    mountPoint :: Prelude.Maybe Prelude.Text,
    -- | The volume ID.
    volumeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateVolume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateVolume_name' - The new name.
--
-- 'mountPoint', 'updateVolume_mountPoint' - The new mount point.
--
-- 'volumeId', 'updateVolume_volumeId' - The volume ID.
newUpdateVolume ::
  -- | 'volumeId'
  Prelude.Text ->
  UpdateVolume
newUpdateVolume pVolumeId_ =
  UpdateVolume'
    { name = Prelude.Nothing,
      mountPoint = Prelude.Nothing,
      volumeId = pVolumeId_
    }

-- | The new name.
updateVolume_name :: Lens.Lens' UpdateVolume (Prelude.Maybe Prelude.Text)
updateVolume_name = Lens.lens (\UpdateVolume' {name} -> name) (\s@UpdateVolume' {} a -> s {name = a} :: UpdateVolume)

-- | The new mount point.
updateVolume_mountPoint :: Lens.Lens' UpdateVolume (Prelude.Maybe Prelude.Text)
updateVolume_mountPoint = Lens.lens (\UpdateVolume' {mountPoint} -> mountPoint) (\s@UpdateVolume' {} a -> s {mountPoint = a} :: UpdateVolume)

-- | The volume ID.
updateVolume_volumeId :: Lens.Lens' UpdateVolume Prelude.Text
updateVolume_volumeId = Lens.lens (\UpdateVolume' {volumeId} -> volumeId) (\s@UpdateVolume' {} a -> s {volumeId = a} :: UpdateVolume)

instance Prelude.AWSRequest UpdateVolume where
  type Rs UpdateVolume = UpdateVolumeResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull UpdateVolumeResponse'

instance Prelude.Hashable UpdateVolume

instance Prelude.NFData UpdateVolume

instance Prelude.ToHeaders UpdateVolume where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "OpsWorks_20130218.UpdateVolume" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateVolume where
  toJSON UpdateVolume' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Name" Prelude..=) Prelude.<$> name,
            ("MountPoint" Prelude..=) Prelude.<$> mountPoint,
            Prelude.Just ("VolumeId" Prelude..= volumeId)
          ]
      )

instance Prelude.ToPath UpdateVolume where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateVolume where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateVolumeResponse' smart constructor.
data UpdateVolumeResponse = UpdateVolumeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateVolumeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateVolumeResponse ::
  UpdateVolumeResponse
newUpdateVolumeResponse = UpdateVolumeResponse'

instance Prelude.NFData UpdateVolumeResponse
