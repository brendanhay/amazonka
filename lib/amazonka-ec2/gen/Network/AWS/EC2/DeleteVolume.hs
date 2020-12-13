{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified EBS volume. The volume must be in the @available@ state (not attached to an instance).
--
-- The volume can remain in the @deleting@ state for several minutes.
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-deleting-volume.html Deleting an Amazon EBS volume> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.DeleteVolume
  ( -- * Creating a request
    DeleteVolume (..),
    mkDeleteVolume,

    -- ** Request lenses
    dvfVolumeId,
    dvfDryRun,

    -- * Destructuring the response
    DeleteVolumeResponse (..),
    mkDeleteVolumeResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteVolume' smart constructor.
data DeleteVolume = DeleteVolume'
  { -- | The ID of the volume.
    volumeId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVolume' with the minimum fields required to make a request.
--
-- * 'volumeId' - The ID of the volume.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDeleteVolume ::
  -- | 'volumeId'
  Lude.Text ->
  DeleteVolume
mkDeleteVolume pVolumeId_ =
  DeleteVolume' {volumeId = pVolumeId_, dryRun = Lude.Nothing}

-- | The ID of the volume.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvfVolumeId :: Lens.Lens' DeleteVolume Lude.Text
dvfVolumeId = Lens.lens (volumeId :: DeleteVolume -> Lude.Text) (\s a -> s {volumeId = a} :: DeleteVolume)
{-# DEPRECATED dvfVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvfDryRun :: Lens.Lens' DeleteVolume (Lude.Maybe Lude.Bool)
dvfDryRun = Lens.lens (dryRun :: DeleteVolume -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteVolume)
{-# DEPRECATED dvfDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DeleteVolume where
  type Rs DeleteVolume = DeleteVolumeResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull DeleteVolumeResponse'

instance Lude.ToHeaders DeleteVolume where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteVolume where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteVolume where
  toQuery DeleteVolume' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteVolume" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "VolumeId" Lude.=: volumeId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDeleteVolumeResponse' smart constructor.
data DeleteVolumeResponse = DeleteVolumeResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVolumeResponse' with the minimum fields required to make a request.
mkDeleteVolumeResponse ::
  DeleteVolumeResponse
mkDeleteVolumeResponse = DeleteVolumeResponse'
