-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPCPeeringConnectionStateReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPCPeeringConnectionStateReason
  ( VPCPeeringConnectionStateReason (..),

    -- * Smart constructor
    mkVPCPeeringConnectionStateReason,

    -- * Lenses
    vpcsrCode,
    vpcsrMessage,
  )
where

import Network.AWS.EC2.Types.VPCPeeringConnectionStateReasonCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the status of a VPC peering connection.
--
-- /See:/ 'mkVPCPeeringConnectionStateReason' smart constructor.
data VPCPeeringConnectionStateReason = VPCPeeringConnectionStateReason'
  { code ::
      Lude.Maybe
        VPCPeeringConnectionStateReasonCode,
    message ::
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

-- | Creates a value of 'VPCPeeringConnectionStateReason' with the minimum fields required to make a request.
--
-- * 'code' - The status of the VPC peering connection.
-- * 'message' - A message that provides more information about the status, if applicable.
mkVPCPeeringConnectionStateReason ::
  VPCPeeringConnectionStateReason
mkVPCPeeringConnectionStateReason =
  VPCPeeringConnectionStateReason'
    { code = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The status of the VPC peering connection.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcsrCode :: Lens.Lens' VPCPeeringConnectionStateReason (Lude.Maybe VPCPeeringConnectionStateReasonCode)
vpcsrCode = Lens.lens (code :: VPCPeeringConnectionStateReason -> Lude.Maybe VPCPeeringConnectionStateReasonCode) (\s a -> s {code = a} :: VPCPeeringConnectionStateReason)
{-# DEPRECATED vpcsrCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | A message that provides more information about the status, if applicable.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcsrMessage :: Lens.Lens' VPCPeeringConnectionStateReason (Lude.Maybe Lude.Text)
vpcsrMessage = Lens.lens (message :: VPCPeeringConnectionStateReason -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: VPCPeeringConnectionStateReason)
{-# DEPRECATED vpcsrMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromXML VPCPeeringConnectionStateReason where
  parseXML x =
    VPCPeeringConnectionStateReason'
      Lude.<$> (x Lude..@? "code") Lude.<*> (x Lude..@? "message")
