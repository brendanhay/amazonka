{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDestinationVPC
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDestinationVPC
  ( InputDestinationVPC (..),

    -- * Smart constructor
    mkInputDestinationVPC,

    -- * Lenses
    idvNetworkInterfaceId,
    idvAvailabilityZone,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The properties for a VPC type input destination.
--
-- /See:/ 'mkInputDestinationVPC' smart constructor.
data InputDestinationVPC = InputDestinationVPC'
  { -- | The network interface ID of the Input destination in the VPC.
    networkInterfaceId :: Lude.Maybe Lude.Text,
    -- | The availability zone of the Input destination.
    availabilityZone :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputDestinationVPC' with the minimum fields required to make a request.
--
-- * 'networkInterfaceId' - The network interface ID of the Input destination in the VPC.
-- * 'availabilityZone' - The availability zone of the Input destination.
mkInputDestinationVPC ::
  InputDestinationVPC
mkInputDestinationVPC =
  InputDestinationVPC'
    { networkInterfaceId = Lude.Nothing,
      availabilityZone = Lude.Nothing
    }

-- | The network interface ID of the Input destination in the VPC.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idvNetworkInterfaceId :: Lens.Lens' InputDestinationVPC (Lude.Maybe Lude.Text)
idvNetworkInterfaceId = Lens.lens (networkInterfaceId :: InputDestinationVPC -> Lude.Maybe Lude.Text) (\s a -> s {networkInterfaceId = a} :: InputDestinationVPC)
{-# DEPRECATED idvNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The availability zone of the Input destination.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idvAvailabilityZone :: Lens.Lens' InputDestinationVPC (Lude.Maybe Lude.Text)
idvAvailabilityZone = Lens.lens (availabilityZone :: InputDestinationVPC -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: InputDestinationVPC)
{-# DEPRECATED idvAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

instance Lude.FromJSON InputDestinationVPC where
  parseJSON =
    Lude.withObject
      "InputDestinationVPC"
      ( \x ->
          InputDestinationVPC'
            Lude.<$> (x Lude..:? "networkInterfaceId")
            Lude.<*> (x Lude..:? "availabilityZone")
      )
