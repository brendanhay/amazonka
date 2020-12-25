{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    dcgAmazonSideAsn,
    dcgDirectConnectGatewayId,
    dcgDirectConnectGatewayName,
    dcgDirectConnectGatewayState,
    dcgOwnerAccount,
    dcgStateChangeError,
  )
where

import qualified Network.AWS.DirectConnect.Types.DirectConnectGatewayId as Types
import qualified Network.AWS.DirectConnect.Types.DirectConnectGatewayName as Types
import qualified Network.AWS.DirectConnect.Types.DirectConnectGatewayState as Types
import qualified Network.AWS.DirectConnect.Types.OwnerAccount as Types
import qualified Network.AWS.DirectConnect.Types.StateChangeError as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a Direct Connect gateway, which enables you to connect virtual interfaces and virtual private gateway or transit gateways.
--
-- /See:/ 'mkDirectConnectGateway' smart constructor.
data DirectConnectGateway = DirectConnectGateway'
  { -- | The autonomous system number (ASN) for the Amazon side of the connection.
    amazonSideAsn :: Core.Maybe Core.Integer,
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Core.Maybe Types.DirectConnectGatewayId,
    -- | The name of the Direct Connect gateway.
    directConnectGatewayName :: Core.Maybe Types.DirectConnectGatewayName,
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
    directConnectGatewayState :: Core.Maybe Types.DirectConnectGatewayState,
    -- | The ID of the AWS account that owns the Direct Connect gateway.
    ownerAccount :: Core.Maybe Types.OwnerAccount,
    -- | The error message if the state of an object failed to advance.
    stateChangeError :: Core.Maybe Types.StateChangeError
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DirectConnectGateway' value with any optional fields omitted.
mkDirectConnectGateway ::
  DirectConnectGateway
mkDirectConnectGateway =
  DirectConnectGateway'
    { amazonSideAsn = Core.Nothing,
      directConnectGatewayId = Core.Nothing,
      directConnectGatewayName = Core.Nothing,
      directConnectGatewayState = Core.Nothing,
      ownerAccount = Core.Nothing,
      stateChangeError = Core.Nothing
    }

-- | The autonomous system number (ASN) for the Amazon side of the connection.
--
-- /Note:/ Consider using 'amazonSideAsn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgAmazonSideAsn :: Lens.Lens' DirectConnectGateway (Core.Maybe Core.Integer)
dcgAmazonSideAsn = Lens.field @"amazonSideAsn"
{-# DEPRECATED dcgAmazonSideAsn "Use generic-lens or generic-optics with 'amazonSideAsn' instead." #-}

-- | The ID of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgDirectConnectGatewayId :: Lens.Lens' DirectConnectGateway (Core.Maybe Types.DirectConnectGatewayId)
dcgDirectConnectGatewayId = Lens.field @"directConnectGatewayId"
{-# DEPRECATED dcgDirectConnectGatewayId "Use generic-lens or generic-optics with 'directConnectGatewayId' instead." #-}

-- | The name of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgDirectConnectGatewayName :: Lens.Lens' DirectConnectGateway (Core.Maybe Types.DirectConnectGatewayName)
dcgDirectConnectGatewayName = Lens.field @"directConnectGatewayName"
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
dcgDirectConnectGatewayState :: Lens.Lens' DirectConnectGateway (Core.Maybe Types.DirectConnectGatewayState)
dcgDirectConnectGatewayState = Lens.field @"directConnectGatewayState"
{-# DEPRECATED dcgDirectConnectGatewayState "Use generic-lens or generic-optics with 'directConnectGatewayState' instead." #-}

-- | The ID of the AWS account that owns the Direct Connect gateway.
--
-- /Note:/ Consider using 'ownerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgOwnerAccount :: Lens.Lens' DirectConnectGateway (Core.Maybe Types.OwnerAccount)
dcgOwnerAccount = Lens.field @"ownerAccount"
{-# DEPRECATED dcgOwnerAccount "Use generic-lens or generic-optics with 'ownerAccount' instead." #-}

-- | The error message if the state of an object failed to advance.
--
-- /Note:/ Consider using 'stateChangeError' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgStateChangeError :: Lens.Lens' DirectConnectGateway (Core.Maybe Types.StateChangeError)
dcgStateChangeError = Lens.field @"stateChangeError"
{-# DEPRECATED dcgStateChangeError "Use generic-lens or generic-optics with 'stateChangeError' instead." #-}

instance Core.FromJSON DirectConnectGateway where
  parseJSON =
    Core.withObject "DirectConnectGateway" Core.$
      \x ->
        DirectConnectGateway'
          Core.<$> (x Core..:? "amazonSideAsn")
          Core.<*> (x Core..:? "directConnectGatewayId")
          Core.<*> (x Core..:? "directConnectGatewayName")
          Core.<*> (x Core..:? "directConnectGatewayState")
          Core.<*> (x Core..:? "ownerAccount")
          Core.<*> (x Core..:? "stateChangeError")
