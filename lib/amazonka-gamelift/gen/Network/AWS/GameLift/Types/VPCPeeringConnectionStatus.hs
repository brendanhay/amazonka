{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.VPCPeeringConnectionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.VPCPeeringConnectionStatus
  ( VPCPeeringConnectionStatus (..),

    -- * Smart constructor
    mkVPCPeeringConnectionStatus,

    -- * Lenses
    vpcsCode,
    vpcsMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents status information for a VPC peering connection. Status is associated with a 'VpcPeeringConnection' object. Status codes and messages are provided from EC2 (see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_VpcPeeringConnectionStateReason.html VpcPeeringConnectionStateReason> ). Connection status information is also communicated as a fleet 'Event' .
--
-- /See:/ 'mkVPCPeeringConnectionStatus' smart constructor.
data VPCPeeringConnectionStatus = VPCPeeringConnectionStatus'
  { -- | Code indicating the status of a VPC peering connection.
    code :: Lude.Maybe Lude.Text,
    -- | Additional messaging associated with the connection status.
    message :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VPCPeeringConnectionStatus' with the minimum fields required to make a request.
--
-- * 'code' - Code indicating the status of a VPC peering connection.
-- * 'message' - Additional messaging associated with the connection status.
mkVPCPeeringConnectionStatus ::
  VPCPeeringConnectionStatus
mkVPCPeeringConnectionStatus =
  VPCPeeringConnectionStatus'
    { code = Lude.Nothing,
      message = Lude.Nothing
    }

-- | Code indicating the status of a VPC peering connection.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcsCode :: Lens.Lens' VPCPeeringConnectionStatus (Lude.Maybe Lude.Text)
vpcsCode = Lens.lens (code :: VPCPeeringConnectionStatus -> Lude.Maybe Lude.Text) (\s a -> s {code = a} :: VPCPeeringConnectionStatus)
{-# DEPRECATED vpcsCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | Additional messaging associated with the connection status.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcsMessage :: Lens.Lens' VPCPeeringConnectionStatus (Lude.Maybe Lude.Text)
vpcsMessage = Lens.lens (message :: VPCPeeringConnectionStatus -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: VPCPeeringConnectionStatus)
{-# DEPRECATED vpcsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON VPCPeeringConnectionStatus where
  parseJSON =
    Lude.withObject
      "VPCPeeringConnectionStatus"
      ( \x ->
          VPCPeeringConnectionStatus'
            Lude.<$> (x Lude..:? "Code") Lude.<*> (x Lude..:? "Message")
      )
