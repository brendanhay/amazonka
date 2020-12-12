{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeStatusAttachmentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeStatusAttachmentStatus
  ( VolumeStatusAttachmentStatus (..),

    -- * Smart constructor
    mkVolumeStatusAttachmentStatus,

    -- * Lenses
    vsasInstanceId,
    vsasIOPerformance,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the instances to which the volume is attached.
--
-- /See:/ 'mkVolumeStatusAttachmentStatus' smart constructor.
data VolumeStatusAttachmentStatus = VolumeStatusAttachmentStatus'
  { instanceId ::
      Lude.Maybe Lude.Text,
    iOPerformance ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VolumeStatusAttachmentStatus' with the minimum fields required to make a request.
--
-- * 'iOPerformance' - The maximum IOPS supported by the attached instance.
-- * 'instanceId' - The ID of the attached instance.
mkVolumeStatusAttachmentStatus ::
  VolumeStatusAttachmentStatus
mkVolumeStatusAttachmentStatus =
  VolumeStatusAttachmentStatus'
    { instanceId = Lude.Nothing,
      iOPerformance = Lude.Nothing
    }

-- | The ID of the attached instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsasInstanceId :: Lens.Lens' VolumeStatusAttachmentStatus (Lude.Maybe Lude.Text)
vsasInstanceId = Lens.lens (instanceId :: VolumeStatusAttachmentStatus -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: VolumeStatusAttachmentStatus)
{-# DEPRECATED vsasInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The maximum IOPS supported by the attached instance.
--
-- /Note:/ Consider using 'iOPerformance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsasIOPerformance :: Lens.Lens' VolumeStatusAttachmentStatus (Lude.Maybe Lude.Text)
vsasIOPerformance = Lens.lens (iOPerformance :: VolumeStatusAttachmentStatus -> Lude.Maybe Lude.Text) (\s a -> s {iOPerformance = a} :: VolumeStatusAttachmentStatus)
{-# DEPRECATED vsasIOPerformance "Use generic-lens or generic-optics with 'iOPerformance' instead." #-}

instance Lude.FromXML VolumeStatusAttachmentStatus where
  parseXML x =
    VolumeStatusAttachmentStatus'
      Lude.<$> (x Lude..@? "instanceId") Lude.<*> (x Lude..@? "ioPerformance")
