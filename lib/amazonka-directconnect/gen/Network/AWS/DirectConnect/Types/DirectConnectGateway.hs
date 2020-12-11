-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.DirectConnectGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.DirectConnectGateway
  ( DirectConnectGateway (..),

    -- * Smart constructor
    mkDirectConnectGateway,

    -- * Lenses
    dcgDirectConnectGatewayId,
    dcgStateChangeError,
    dcgAmazonSideASN,
    dcgDirectConnectGatewayName,
    dcgDirectConnectGatewayState,
    dcgOwnerAccount,
  )
where

import Network.AWS.DirectConnect.Types.DirectConnectGatewayState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a Direct Connect gateway, which enables you to connect virtual interfaces and virtual private gateway or transit gateways.
--
-- /See:/ 'mkDirectConnectGateway' smart constructor.
data DirectConnectGateway = DirectConnectGateway'
  { directConnectGatewayId ::
      Lude.Maybe Lude.Text,
    stateChangeError :: Lude.Maybe Lude.Text,
    amazonSideASN :: Lude.Maybe Lude.Integer,
    directConnectGatewayName :: Lude.Maybe Lude.Text,
    directConnectGatewayState ::
      Lude.Maybe DirectConnectGatewayState,
    ownerAccount :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DirectConnectGateway' with the minimum fields required to make a request.
--
-- * 'amazonSideASN' - The autonomous system number (ASN) for the Amazon side of the connection.
-- * 'directConnectGatewayId' - The ID of the Direct Connect gateway.
-- * 'directConnectGatewayName' - The name of the Direct Connect gateway.
-- * 'directConnectGatewayState' - The state of the Direct Connect gateway. The following are the possible values:
--
--
--     * @pending@ : The initial state after calling 'CreateDirectConnectGateway' .
--
--
--     * @available@ : The Direct Connect gateway is ready for use.
--
--
--     * @deleting@ : The initial state after calling 'DeleteDirectConnectGateway' .
--
--
--     * @deleted@ : The Direct Connect gateway is deleted and cannot pass traffic.
--
--
-- * 'ownerAccount' - The ID of the AWS account that owns the Direct Connect gateway.
-- * 'stateChangeError' - The error message if the state of an object failed to advance.
mkDirectConnectGateway ::
  DirectConnectGateway
mkDirectConnectGateway =
  DirectConnectGateway'
    { directConnectGatewayId = Lude.Nothing,
      stateChangeError = Lude.Nothing,
      amazonSideASN = Lude.Nothing,
      directConnectGatewayName = Lude.Nothing,
      directConnectGatewayState = Lude.Nothing,
      ownerAccount = Lude.Nothing
    }

-- | The ID of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgDirectConnectGatewayId :: Lens.Lens' DirectConnectGateway (Lude.Maybe Lude.Text)
dcgDirectConnectGatewayId = Lens.lens (directConnectGatewayId :: DirectConnectGateway -> Lude.Maybe Lude.Text) (\s a -> s {directConnectGatewayId = a} :: DirectConnectGateway)
{-# DEPRECATED dcgDirectConnectGatewayId "Use generic-lens or generic-optics with 'directConnectGatewayId' instead." #-}

-- | The error message if the state of an object failed to advance.
--
-- /Note:/ Consider using 'stateChangeError' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgStateChangeError :: Lens.Lens' DirectConnectGateway (Lude.Maybe Lude.Text)
dcgStateChangeError = Lens.lens (stateChangeError :: DirectConnectGateway -> Lude.Maybe Lude.Text) (\s a -> s {stateChangeError = a} :: DirectConnectGateway)
{-# DEPRECATED dcgStateChangeError "Use generic-lens or generic-optics with 'stateChangeError' instead." #-}

-- | The autonomous system number (ASN) for the Amazon side of the connection.
--
-- /Note:/ Consider using 'amazonSideASN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgAmazonSideASN :: Lens.Lens' DirectConnectGateway (Lude.Maybe Lude.Integer)
dcgAmazonSideASN = Lens.lens (amazonSideASN :: DirectConnectGateway -> Lude.Maybe Lude.Integer) (\s a -> s {amazonSideASN = a} :: DirectConnectGateway)
{-# DEPRECATED dcgAmazonSideASN "Use generic-lens or generic-optics with 'amazonSideASN' instead." #-}

-- | The name of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgDirectConnectGatewayName :: Lens.Lens' DirectConnectGateway (Lude.Maybe Lude.Text)
dcgDirectConnectGatewayName = Lens.lens (directConnectGatewayName :: DirectConnectGateway -> Lude.Maybe Lude.Text) (\s a -> s {directConnectGatewayName = a} :: DirectConnectGateway)
{-# DEPRECATED dcgDirectConnectGatewayName "Use generic-lens or generic-optics with 'directConnectGatewayName' instead." #-}

-- | The state of the Direct Connect gateway. The following are the possible values:
--
--
--     * @pending@ : The initial state after calling 'CreateDirectConnectGateway' .
--
--
--     * @available@ : The Direct Connect gateway is ready for use.
--
--
--     * @deleting@ : The initial state after calling 'DeleteDirectConnectGateway' .
--
--
--     * @deleted@ : The Direct Connect gateway is deleted and cannot pass traffic.
--
--
--
-- /Note:/ Consider using 'directConnectGatewayState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgDirectConnectGatewayState :: Lens.Lens' DirectConnectGateway (Lude.Maybe DirectConnectGatewayState)
dcgDirectConnectGatewayState = Lens.lens (directConnectGatewayState :: DirectConnectGateway -> Lude.Maybe DirectConnectGatewayState) (\s a -> s {directConnectGatewayState = a} :: DirectConnectGateway)
{-# DEPRECATED dcgDirectConnectGatewayState "Use generic-lens or generic-optics with 'directConnectGatewayState' instead." #-}

-- | The ID of the AWS account that owns the Direct Connect gateway.
--
-- /Note:/ Consider using 'ownerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgOwnerAccount :: Lens.Lens' DirectConnectGateway (Lude.Maybe Lude.Text)
dcgOwnerAccount = Lens.lens (ownerAccount :: DirectConnectGateway -> Lude.Maybe Lude.Text) (\s a -> s {ownerAccount = a} :: DirectConnectGateway)
{-# DEPRECATED dcgOwnerAccount "Use generic-lens or generic-optics with 'ownerAccount' instead." #-}

instance Lude.FromJSON DirectConnectGateway where
  parseJSON =
    Lude.withObject
      "DirectConnectGateway"
      ( \x ->
          DirectConnectGateway'
            Lude.<$> (x Lude..:? "directConnectGatewayId")
            Lude.<*> (x Lude..:? "stateChangeError")
            Lude.<*> (x Lude..:? "amazonSideAsn")
            Lude.<*> (x Lude..:? "directConnectGatewayName")
            Lude.<*> (x Lude..:? "directConnectGatewayState")
            Lude.<*> (x Lude..:? "ownerAccount")
      )
