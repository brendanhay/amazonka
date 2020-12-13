{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeStatusInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeStatusInfo
  ( VolumeStatusInfo (..),

    -- * Smart constructor
    mkVolumeStatusInfo,

    -- * Lenses
    vsiStatus,
    vsiDetails,
  )
where

import Network.AWS.EC2.Types.VolumeStatusDetails
import Network.AWS.EC2.Types.VolumeStatusInfoStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the status of a volume.
--
-- /See:/ 'mkVolumeStatusInfo' smart constructor.
data VolumeStatusInfo = VolumeStatusInfo'
  { -- | The status of the volume.
    status :: Lude.Maybe VolumeStatusInfoStatus,
    -- | The details of the volume status.
    details :: Lude.Maybe [VolumeStatusDetails]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VolumeStatusInfo' with the minimum fields required to make a request.
--
-- * 'status' - The status of the volume.
-- * 'details' - The details of the volume status.
mkVolumeStatusInfo ::
  VolumeStatusInfo
mkVolumeStatusInfo =
  VolumeStatusInfo' {status = Lude.Nothing, details = Lude.Nothing}

-- | The status of the volume.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsiStatus :: Lens.Lens' VolumeStatusInfo (Lude.Maybe VolumeStatusInfoStatus)
vsiStatus = Lens.lens (status :: VolumeStatusInfo -> Lude.Maybe VolumeStatusInfoStatus) (\s a -> s {status = a} :: VolumeStatusInfo)
{-# DEPRECATED vsiStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The details of the volume status.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsiDetails :: Lens.Lens' VolumeStatusInfo (Lude.Maybe [VolumeStatusDetails])
vsiDetails = Lens.lens (details :: VolumeStatusInfo -> Lude.Maybe [VolumeStatusDetails]) (\s a -> s {details = a} :: VolumeStatusInfo)
{-# DEPRECATED vsiDetails "Use generic-lens or generic-optics with 'details' instead." #-}

instance Lude.FromXML VolumeStatusInfo where
  parseXML x =
    VolumeStatusInfo'
      Lude.<$> (x Lude..@? "status")
      Lude.<*> ( x Lude..@? "details" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
