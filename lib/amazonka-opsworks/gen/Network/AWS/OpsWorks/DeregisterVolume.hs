{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DeregisterVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters an Amazon EBS volume. The volume can then be registered by another stack. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DeregisterVolume
  ( -- * Creating a request
    DeregisterVolume (..),
    mkDeregisterVolume,

    -- ** Request lenses
    dvVolumeId,

    -- * Destructuring the response
    DeregisterVolumeResponse (..),
    mkDeregisterVolumeResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeregisterVolume' smart constructor.
newtype DeregisterVolume = DeregisterVolume' {volumeId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterVolume' with the minimum fields required to make a request.
--
-- * 'volumeId' - The AWS OpsWorks Stacks volume ID, which is the GUID that AWS OpsWorks Stacks assigned to the instance when you registered the volume with the stack, not the Amazon EC2 volume ID.
mkDeregisterVolume ::
  -- | 'volumeId'
  Lude.Text ->
  DeregisterVolume
mkDeregisterVolume pVolumeId_ =
  DeregisterVolume' {volumeId = pVolumeId_}

-- | The AWS OpsWorks Stacks volume ID, which is the GUID that AWS OpsWorks Stacks assigned to the instance when you registered the volume with the stack, not the Amazon EC2 volume ID.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvVolumeId :: Lens.Lens' DeregisterVolume Lude.Text
dvVolumeId = Lens.lens (volumeId :: DeregisterVolume -> Lude.Text) (\s a -> s {volumeId = a} :: DeregisterVolume)
{-# DEPRECATED dvVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

instance Lude.AWSRequest DeregisterVolume where
  type Rs DeregisterVolume = DeregisterVolumeResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull DeregisterVolumeResponse'

instance Lude.ToHeaders DeregisterVolume where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.DeregisterVolume" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeregisterVolume where
  toJSON DeregisterVolume' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("VolumeId" Lude..= volumeId)])

instance Lude.ToPath DeregisterVolume where
  toPath = Lude.const "/"

instance Lude.ToQuery DeregisterVolume where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeregisterVolumeResponse' smart constructor.
data DeregisterVolumeResponse = DeregisterVolumeResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterVolumeResponse' with the minimum fields required to make a request.
mkDeregisterVolumeResponse ::
  DeregisterVolumeResponse
mkDeregisterVolumeResponse = DeregisterVolumeResponse'
