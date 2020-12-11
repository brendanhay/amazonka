{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.AssignVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns one of the stack's registered Amazon EBS volumes to a specified instance. The volume must first be registered with the stack by calling 'RegisterVolume' . After you register the volume, you must call 'UpdateVolume' to specify a mount point before calling @AssignVolume@ . For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.AssignVolume
  ( -- * Creating a request
    AssignVolume (..),
    mkAssignVolume,

    -- ** Request lenses
    avInstanceId,
    avVolumeId,

    -- * Destructuring the response
    AssignVolumeResponse (..),
    mkAssignVolumeResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssignVolume' smart constructor.
data AssignVolume = AssignVolume'
  { instanceId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'AssignVolume' with the minimum fields required to make a request.
--
-- * 'instanceId' - The instance ID.
-- * 'volumeId' - The volume ID.
mkAssignVolume ::
  -- | 'volumeId'
  Lude.Text ->
  AssignVolume
mkAssignVolume pVolumeId_ =
  AssignVolume' {instanceId = Lude.Nothing, volumeId = pVolumeId_}

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avInstanceId :: Lens.Lens' AssignVolume (Lude.Maybe Lude.Text)
avInstanceId = Lens.lens (instanceId :: AssignVolume -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: AssignVolume)
{-# DEPRECATED avInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The volume ID.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avVolumeId :: Lens.Lens' AssignVolume Lude.Text
avVolumeId = Lens.lens (volumeId :: AssignVolume -> Lude.Text) (\s a -> s {volumeId = a} :: AssignVolume)
{-# DEPRECATED avVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

instance Lude.AWSRequest AssignVolume where
  type Rs AssignVolume = AssignVolumeResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull AssignVolumeResponse'

instance Lude.ToHeaders AssignVolume where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.AssignVolume" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssignVolume where
  toJSON AssignVolume' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InstanceId" Lude..=) Lude.<$> instanceId,
            Lude.Just ("VolumeId" Lude..= volumeId)
          ]
      )

instance Lude.ToPath AssignVolume where
  toPath = Lude.const "/"

instance Lude.ToQuery AssignVolume where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssignVolumeResponse' smart constructor.
data AssignVolumeResponse = AssignVolumeResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssignVolumeResponse' with the minimum fields required to make a request.
mkAssignVolumeResponse ::
  AssignVolumeResponse
mkAssignVolumeResponse = AssignVolumeResponse'
