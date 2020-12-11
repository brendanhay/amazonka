-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.VolumeiSCSIAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.VolumeiSCSIAttributes
  ( VolumeiSCSIAttributes (..),

    -- * Smart constructor
    mkVolumeiSCSIAttributes,

    -- * Lenses
    vscsiaLunNumber,
    vscsiaTargetARN,
    vscsiaChapEnabled,
    vscsiaNetworkInterfaceId,
    vscsiaNetworkInterfacePort,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Lists iSCSI information about a volume.
--
-- /See:/ 'mkVolumeiSCSIAttributes' smart constructor.
data VolumeiSCSIAttributes = VolumeiSCSIAttributes'
  { lunNumber ::
      Lude.Maybe Lude.Natural,
    targetARN :: Lude.Maybe Lude.Text,
    chapEnabled :: Lude.Maybe Lude.Bool,
    networkInterfaceId :: Lude.Maybe Lude.Text,
    networkInterfacePort :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VolumeiSCSIAttributes' with the minimum fields required to make a request.
--
-- * 'chapEnabled' - Indicates whether mutual CHAP is enabled for the iSCSI target.
-- * 'lunNumber' - The logical disk number.
-- * 'networkInterfaceId' - The network interface identifier.
-- * 'networkInterfacePort' - The port used to communicate with iSCSI targets.
-- * 'targetARN' - The Amazon Resource Name (ARN) of the volume target.
mkVolumeiSCSIAttributes ::
  VolumeiSCSIAttributes
mkVolumeiSCSIAttributes =
  VolumeiSCSIAttributes'
    { lunNumber = Lude.Nothing,
      targetARN = Lude.Nothing,
      chapEnabled = Lude.Nothing,
      networkInterfaceId = Lude.Nothing,
      networkInterfacePort = Lude.Nothing
    }

-- | The logical disk number.
--
-- /Note:/ Consider using 'lunNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vscsiaLunNumber :: Lens.Lens' VolumeiSCSIAttributes (Lude.Maybe Lude.Natural)
vscsiaLunNumber = Lens.lens (lunNumber :: VolumeiSCSIAttributes -> Lude.Maybe Lude.Natural) (\s a -> s {lunNumber = a} :: VolumeiSCSIAttributes)
{-# DEPRECATED vscsiaLunNumber "Use generic-lens or generic-optics with 'lunNumber' instead." #-}

-- | The Amazon Resource Name (ARN) of the volume target.
--
-- /Note:/ Consider using 'targetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vscsiaTargetARN :: Lens.Lens' VolumeiSCSIAttributes (Lude.Maybe Lude.Text)
vscsiaTargetARN = Lens.lens (targetARN :: VolumeiSCSIAttributes -> Lude.Maybe Lude.Text) (\s a -> s {targetARN = a} :: VolumeiSCSIAttributes)
{-# DEPRECATED vscsiaTargetARN "Use generic-lens or generic-optics with 'targetARN' instead." #-}

-- | Indicates whether mutual CHAP is enabled for the iSCSI target.
--
-- /Note:/ Consider using 'chapEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vscsiaChapEnabled :: Lens.Lens' VolumeiSCSIAttributes (Lude.Maybe Lude.Bool)
vscsiaChapEnabled = Lens.lens (chapEnabled :: VolumeiSCSIAttributes -> Lude.Maybe Lude.Bool) (\s a -> s {chapEnabled = a} :: VolumeiSCSIAttributes)
{-# DEPRECATED vscsiaChapEnabled "Use generic-lens or generic-optics with 'chapEnabled' instead." #-}

-- | The network interface identifier.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vscsiaNetworkInterfaceId :: Lens.Lens' VolumeiSCSIAttributes (Lude.Maybe Lude.Text)
vscsiaNetworkInterfaceId = Lens.lens (networkInterfaceId :: VolumeiSCSIAttributes -> Lude.Maybe Lude.Text) (\s a -> s {networkInterfaceId = a} :: VolumeiSCSIAttributes)
{-# DEPRECATED vscsiaNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The port used to communicate with iSCSI targets.
--
-- /Note:/ Consider using 'networkInterfacePort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vscsiaNetworkInterfacePort :: Lens.Lens' VolumeiSCSIAttributes (Lude.Maybe Lude.Int)
vscsiaNetworkInterfacePort = Lens.lens (networkInterfacePort :: VolumeiSCSIAttributes -> Lude.Maybe Lude.Int) (\s a -> s {networkInterfacePort = a} :: VolumeiSCSIAttributes)
{-# DEPRECATED vscsiaNetworkInterfacePort "Use generic-lens or generic-optics with 'networkInterfacePort' instead." #-}

instance Lude.FromJSON VolumeiSCSIAttributes where
  parseJSON =
    Lude.withObject
      "VolumeiSCSIAttributes"
      ( \x ->
          VolumeiSCSIAttributes'
            Lude.<$> (x Lude..:? "LunNumber")
            Lude.<*> (x Lude..:? "TargetARN")
            Lude.<*> (x Lude..:? "ChapEnabled")
            Lude.<*> (x Lude..:? "NetworkInterfaceId")
            Lude.<*> (x Lude..:? "NetworkInterfacePort")
      )
