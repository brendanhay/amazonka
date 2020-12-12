{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.DeviceiSCSIAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.DeviceiSCSIAttributes
  ( DeviceiSCSIAttributes (..),

    -- * Smart constructor
    mkDeviceiSCSIAttributes,

    -- * Lenses
    dscsiaTargetARN,
    dscsiaChapEnabled,
    dscsiaNetworkInterfaceId,
    dscsiaNetworkInterfacePort,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Lists iSCSI information about a VTL device.
--
-- /See:/ 'mkDeviceiSCSIAttributes' smart constructor.
data DeviceiSCSIAttributes = DeviceiSCSIAttributes'
  { targetARN ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DeviceiSCSIAttributes' with the minimum fields required to make a request.
--
-- * 'chapEnabled' - Indicates whether mutual CHAP is enabled for the iSCSI target.
-- * 'networkInterfaceId' - The network interface identifier of the VTL device.
-- * 'networkInterfacePort' - The port used to communicate with iSCSI VTL device targets.
-- * 'targetARN' - Specifies the unique Amazon Resource Name (ARN) that encodes the iSCSI qualified name(iqn) of a tape drive or media changer target.
mkDeviceiSCSIAttributes ::
  DeviceiSCSIAttributes
mkDeviceiSCSIAttributes =
  DeviceiSCSIAttributes'
    { targetARN = Lude.Nothing,
      chapEnabled = Lude.Nothing,
      networkInterfaceId = Lude.Nothing,
      networkInterfacePort = Lude.Nothing
    }

-- | Specifies the unique Amazon Resource Name (ARN) that encodes the iSCSI qualified name(iqn) of a tape drive or media changer target.
--
-- /Note:/ Consider using 'targetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscsiaTargetARN :: Lens.Lens' DeviceiSCSIAttributes (Lude.Maybe Lude.Text)
dscsiaTargetARN = Lens.lens (targetARN :: DeviceiSCSIAttributes -> Lude.Maybe Lude.Text) (\s a -> s {targetARN = a} :: DeviceiSCSIAttributes)
{-# DEPRECATED dscsiaTargetARN "Use generic-lens or generic-optics with 'targetARN' instead." #-}

-- | Indicates whether mutual CHAP is enabled for the iSCSI target.
--
-- /Note:/ Consider using 'chapEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscsiaChapEnabled :: Lens.Lens' DeviceiSCSIAttributes (Lude.Maybe Lude.Bool)
dscsiaChapEnabled = Lens.lens (chapEnabled :: DeviceiSCSIAttributes -> Lude.Maybe Lude.Bool) (\s a -> s {chapEnabled = a} :: DeviceiSCSIAttributes)
{-# DEPRECATED dscsiaChapEnabled "Use generic-lens or generic-optics with 'chapEnabled' instead." #-}

-- | The network interface identifier of the VTL device.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscsiaNetworkInterfaceId :: Lens.Lens' DeviceiSCSIAttributes (Lude.Maybe Lude.Text)
dscsiaNetworkInterfaceId = Lens.lens (networkInterfaceId :: DeviceiSCSIAttributes -> Lude.Maybe Lude.Text) (\s a -> s {networkInterfaceId = a} :: DeviceiSCSIAttributes)
{-# DEPRECATED dscsiaNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The port used to communicate with iSCSI VTL device targets.
--
-- /Note:/ Consider using 'networkInterfacePort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscsiaNetworkInterfacePort :: Lens.Lens' DeviceiSCSIAttributes (Lude.Maybe Lude.Int)
dscsiaNetworkInterfacePort = Lens.lens (networkInterfacePort :: DeviceiSCSIAttributes -> Lude.Maybe Lude.Int) (\s a -> s {networkInterfacePort = a} :: DeviceiSCSIAttributes)
{-# DEPRECATED dscsiaNetworkInterfacePort "Use generic-lens or generic-optics with 'networkInterfacePort' instead." #-}

instance Lude.FromJSON DeviceiSCSIAttributes where
  parseJSON =
    Lude.withObject
      "DeviceiSCSIAttributes"
      ( \x ->
          DeviceiSCSIAttributes'
            Lude.<$> (x Lude..:? "TargetARN")
            Lude.<*> (x Lude..:? "ChapEnabled")
            Lude.<*> (x Lude..:? "NetworkInterfaceId")
            Lude.<*> (x Lude..:? "NetworkInterfacePort")
      )
