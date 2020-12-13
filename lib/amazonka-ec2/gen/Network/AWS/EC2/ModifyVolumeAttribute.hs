{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyVolumeAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a volume attribute.
--
-- By default, all I/O operations for the volume are suspended when the data on the volume is determined to be potentially inconsistent, to prevent undetectable, latent data corruption. The I/O access to the volume can be resumed by first enabling I/O access and then checking the data consistency on your volume.
-- You can change the default behavior to resume I/O operations. We recommend that you change this only for boot volumes or for volumes that are stateless or disposable.
module Network.AWS.EC2.ModifyVolumeAttribute
  ( -- * Creating a request
    ModifyVolumeAttribute (..),
    mkModifyVolumeAttribute,

    -- ** Request lenses
    mvaVolumeId,
    mvaAutoEnableIO,
    mvaDryRun,

    -- * Destructuring the response
    ModifyVolumeAttributeResponse (..),
    mkModifyVolumeAttributeResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyVolumeAttribute' smart constructor.
data ModifyVolumeAttribute = ModifyVolumeAttribute'
  { -- | The ID of the volume.
    volumeId :: Lude.Text,
    -- | Indicates whether the volume should be auto-enabled for I/O operations.
    autoEnableIO :: Lude.Maybe AttributeBooleanValue,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyVolumeAttribute' with the minimum fields required to make a request.
--
-- * 'volumeId' - The ID of the volume.
-- * 'autoEnableIO' - Indicates whether the volume should be auto-enabled for I/O operations.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkModifyVolumeAttribute ::
  -- | 'volumeId'
  Lude.Text ->
  ModifyVolumeAttribute
mkModifyVolumeAttribute pVolumeId_ =
  ModifyVolumeAttribute'
    { volumeId = pVolumeId_,
      autoEnableIO = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The ID of the volume.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvaVolumeId :: Lens.Lens' ModifyVolumeAttribute Lude.Text
mvaVolumeId = Lens.lens (volumeId :: ModifyVolumeAttribute -> Lude.Text) (\s a -> s {volumeId = a} :: ModifyVolumeAttribute)
{-# DEPRECATED mvaVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

-- | Indicates whether the volume should be auto-enabled for I/O operations.
--
-- /Note:/ Consider using 'autoEnableIO' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvaAutoEnableIO :: Lens.Lens' ModifyVolumeAttribute (Lude.Maybe AttributeBooleanValue)
mvaAutoEnableIO = Lens.lens (autoEnableIO :: ModifyVolumeAttribute -> Lude.Maybe AttributeBooleanValue) (\s a -> s {autoEnableIO = a} :: ModifyVolumeAttribute)
{-# DEPRECATED mvaAutoEnableIO "Use generic-lens or generic-optics with 'autoEnableIO' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvaDryRun :: Lens.Lens' ModifyVolumeAttribute (Lude.Maybe Lude.Bool)
mvaDryRun = Lens.lens (dryRun :: ModifyVolumeAttribute -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ModifyVolumeAttribute)
{-# DEPRECATED mvaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest ModifyVolumeAttribute where
  type Rs ModifyVolumeAttribute = ModifyVolumeAttributeResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull ModifyVolumeAttributeResponse'

instance Lude.ToHeaders ModifyVolumeAttribute where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyVolumeAttribute where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyVolumeAttribute where
  toQuery ModifyVolumeAttribute' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyVolumeAttribute" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "VolumeId" Lude.=: volumeId,
        "AutoEnableIO" Lude.=: autoEnableIO,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkModifyVolumeAttributeResponse' smart constructor.
data ModifyVolumeAttributeResponse = ModifyVolumeAttributeResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyVolumeAttributeResponse' with the minimum fields required to make a request.
mkModifyVolumeAttributeResponse ::
  ModifyVolumeAttributeResponse
mkModifyVolumeAttributeResponse = ModifyVolumeAttributeResponse'
