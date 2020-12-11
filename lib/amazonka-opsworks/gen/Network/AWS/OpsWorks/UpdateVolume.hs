{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.UpdateVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an Amazon EBS volume's name or mount point. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.UpdateVolume
  ( -- * Creating a request
    UpdateVolume (..),
    mkUpdateVolume,

    -- ** Request lenses
    uName,
    uMountPoint,
    uVolumeId,

    -- * Destructuring the response
    UpdateVolumeResponse (..),
    mkUpdateVolumeResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateVolume' smart constructor.
data UpdateVolume = UpdateVolume'
  { name :: Lude.Maybe Lude.Text,
    mountPoint :: Lude.Maybe Lude.Text,
    volumeId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateVolume' with the minimum fields required to make a request.
--
-- * 'mountPoint' - The new mount point.
-- * 'name' - The new name.
-- * 'volumeId' - The volume ID.
mkUpdateVolume ::
  -- | 'volumeId'
  Lude.Text ->
  UpdateVolume
mkUpdateVolume pVolumeId_ =
  UpdateVolume'
    { name = Lude.Nothing,
      mountPoint = Lude.Nothing,
      volumeId = pVolumeId_
    }

-- | The new name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uName :: Lens.Lens' UpdateVolume (Lude.Maybe Lude.Text)
uName = Lens.lens (name :: UpdateVolume -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateVolume)
{-# DEPRECATED uName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The new mount point.
--
-- /Note:/ Consider using 'mountPoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uMountPoint :: Lens.Lens' UpdateVolume (Lude.Maybe Lude.Text)
uMountPoint = Lens.lens (mountPoint :: UpdateVolume -> Lude.Maybe Lude.Text) (\s a -> s {mountPoint = a} :: UpdateVolume)
{-# DEPRECATED uMountPoint "Use generic-lens or generic-optics with 'mountPoint' instead." #-}

-- | The volume ID.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uVolumeId :: Lens.Lens' UpdateVolume Lude.Text
uVolumeId = Lens.lens (volumeId :: UpdateVolume -> Lude.Text) (\s a -> s {volumeId = a} :: UpdateVolume)
{-# DEPRECATED uVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

instance Lude.AWSRequest UpdateVolume where
  type Rs UpdateVolume = UpdateVolumeResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull UpdateVolumeResponse'

instance Lude.ToHeaders UpdateVolume where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.UpdateVolume" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateVolume where
  toJSON UpdateVolume' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Name" Lude..=) Lude.<$> name,
            ("MountPoint" Lude..=) Lude.<$> mountPoint,
            Lude.Just ("VolumeId" Lude..= volumeId)
          ]
      )

instance Lude.ToPath UpdateVolume where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateVolume where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateVolumeResponse' smart constructor.
data UpdateVolumeResponse = UpdateVolumeResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateVolumeResponse' with the minimum fields required to make a request.
mkUpdateVolumeResponse ::
  UpdateVolumeResponse
mkUpdateVolumeResponse = UpdateVolumeResponse'
