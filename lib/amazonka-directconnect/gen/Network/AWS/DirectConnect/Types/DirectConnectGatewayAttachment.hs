{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.DirectConnectGatewayAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types.DirectConnectGatewayAttachment
  ( DirectConnectGatewayAttachment (..)
  -- * Smart constructor
  , mkDirectConnectGatewayAttachment
  -- * Lenses
  , dAttachmentState
  , dAttachmentType
  , dDirectConnectGatewayId
  , dStateChangeError
  , dVirtualInterfaceId
  , dVirtualInterfaceOwnerAccount
  , dVirtualInterfaceRegion
  ) where

import qualified Network.AWS.DirectConnect.Types.DirectConnectGatewayAttachmentState as Types
import qualified Network.AWS.DirectConnect.Types.DirectConnectGatewayAttachmentType as Types
import qualified Network.AWS.DirectConnect.Types.DirectConnectGatewayId as Types
import qualified Network.AWS.DirectConnect.Types.StateChangeError as Types
import qualified Network.AWS.DirectConnect.Types.VirtualInterfaceId as Types
import qualified Network.AWS.DirectConnect.Types.VirtualInterfaceOwnerAccount as Types
import qualified Network.AWS.DirectConnect.Types.VirtualInterfaceRegion as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an attachment between a Direct Connect gateway and a virtual interface.
--
-- /See:/ 'mkDirectConnectGatewayAttachment' smart constructor.
data DirectConnectGatewayAttachment = DirectConnectGatewayAttachment'
  { attachmentState :: Core.Maybe Types.DirectConnectGatewayAttachmentState
    -- ^ The state of the attachment. The following are the possible values:
--
--
--     * @attaching@ : The initial state after a virtual interface is created using the Direct Connect gateway.
--
--
--     * @attached@ : The Direct Connect gateway and virtual interface are attached and ready to pass traffic.
--
--
--     * @detaching@ : The initial state after calling 'DeleteVirtualInterface' .
--
--
--     * @detached@ : The virtual interface is detached from the Direct Connect gateway. Traffic flow between the Direct Connect gateway and virtual interface is stopped.
--
--
  , attachmentType :: Core.Maybe Types.DirectConnectGatewayAttachmentType
    -- ^ The type of attachment.
  , directConnectGatewayId :: Core.Maybe Types.DirectConnectGatewayId
    -- ^ The ID of the Direct Connect gateway.
  , stateChangeError :: Core.Maybe Types.StateChangeError
    -- ^ The error message if the state of an object failed to advance.
  , virtualInterfaceId :: Core.Maybe Types.VirtualInterfaceId
    -- ^ The ID of the virtual interface.
  , virtualInterfaceOwnerAccount :: Core.Maybe Types.VirtualInterfaceOwnerAccount
    -- ^ The ID of the AWS account that owns the virtual interface.
  , virtualInterfaceRegion :: Core.Maybe Types.VirtualInterfaceRegion
    -- ^ The AWS Region where the virtual interface is located.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DirectConnectGatewayAttachment' value with any optional fields omitted.
mkDirectConnectGatewayAttachment
    :: DirectConnectGatewayAttachment
mkDirectConnectGatewayAttachment
  = DirectConnectGatewayAttachment'{attachmentState = Core.Nothing,
                                    attachmentType = Core.Nothing,
                                    directConnectGatewayId = Core.Nothing,
                                    stateChangeError = Core.Nothing,
                                    virtualInterfaceId = Core.Nothing,
                                    virtualInterfaceOwnerAccount = Core.Nothing,
                                    virtualInterfaceRegion = Core.Nothing}

-- | The state of the attachment. The following are the possible values:
--
--
--     * @attaching@ : The initial state after a virtual interface is created using the Direct Connect gateway.
--
--
--     * @attached@ : The Direct Connect gateway and virtual interface are attached and ready to pass traffic.
--
--
--     * @detaching@ : The initial state after calling 'DeleteVirtualInterface' .
--
--
--     * @detached@ : The virtual interface is detached from the Direct Connect gateway. Traffic flow between the Direct Connect gateway and virtual interface is stopped.
--
--
--
-- /Note:/ Consider using 'attachmentState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAttachmentState :: Lens.Lens' DirectConnectGatewayAttachment (Core.Maybe Types.DirectConnectGatewayAttachmentState)
dAttachmentState = Lens.field @"attachmentState"
{-# INLINEABLE dAttachmentState #-}
{-# DEPRECATED attachmentState "Use generic-lens or generic-optics with 'attachmentState' instead"  #-}

-- | The type of attachment.
--
-- /Note:/ Consider using 'attachmentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAttachmentType :: Lens.Lens' DirectConnectGatewayAttachment (Core.Maybe Types.DirectConnectGatewayAttachmentType)
dAttachmentType = Lens.field @"attachmentType"
{-# INLINEABLE dAttachmentType #-}
{-# DEPRECATED attachmentType "Use generic-lens or generic-optics with 'attachmentType' instead"  #-}

-- | The ID of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDirectConnectGatewayId :: Lens.Lens' DirectConnectGatewayAttachment (Core.Maybe Types.DirectConnectGatewayId)
dDirectConnectGatewayId = Lens.field @"directConnectGatewayId"
{-# INLINEABLE dDirectConnectGatewayId #-}
{-# DEPRECATED directConnectGatewayId "Use generic-lens or generic-optics with 'directConnectGatewayId' instead"  #-}

-- | The error message if the state of an object failed to advance.
--
-- /Note:/ Consider using 'stateChangeError' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStateChangeError :: Lens.Lens' DirectConnectGatewayAttachment (Core.Maybe Types.StateChangeError)
dStateChangeError = Lens.field @"stateChangeError"
{-# INLINEABLE dStateChangeError #-}
{-# DEPRECATED stateChangeError "Use generic-lens or generic-optics with 'stateChangeError' instead"  #-}

-- | The ID of the virtual interface.
--
-- /Note:/ Consider using 'virtualInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dVirtualInterfaceId :: Lens.Lens' DirectConnectGatewayAttachment (Core.Maybe Types.VirtualInterfaceId)
dVirtualInterfaceId = Lens.field @"virtualInterfaceId"
{-# INLINEABLE dVirtualInterfaceId #-}
{-# DEPRECATED virtualInterfaceId "Use generic-lens or generic-optics with 'virtualInterfaceId' instead"  #-}

-- | The ID of the AWS account that owns the virtual interface.
--
-- /Note:/ Consider using 'virtualInterfaceOwnerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dVirtualInterfaceOwnerAccount :: Lens.Lens' DirectConnectGatewayAttachment (Core.Maybe Types.VirtualInterfaceOwnerAccount)
dVirtualInterfaceOwnerAccount = Lens.field @"virtualInterfaceOwnerAccount"
{-# INLINEABLE dVirtualInterfaceOwnerAccount #-}
{-# DEPRECATED virtualInterfaceOwnerAccount "Use generic-lens or generic-optics with 'virtualInterfaceOwnerAccount' instead"  #-}

-- | The AWS Region where the virtual interface is located.
--
-- /Note:/ Consider using 'virtualInterfaceRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dVirtualInterfaceRegion :: Lens.Lens' DirectConnectGatewayAttachment (Core.Maybe Types.VirtualInterfaceRegion)
dVirtualInterfaceRegion = Lens.field @"virtualInterfaceRegion"
{-# INLINEABLE dVirtualInterfaceRegion #-}
{-# DEPRECATED virtualInterfaceRegion "Use generic-lens or generic-optics with 'virtualInterfaceRegion' instead"  #-}

instance Core.FromJSON DirectConnectGatewayAttachment where
        parseJSON
          = Core.withObject "DirectConnectGatewayAttachment" Core.$
              \ x ->
                DirectConnectGatewayAttachment' Core.<$>
                  (x Core..:? "attachmentState") Core.<*> x Core..:? "attachmentType"
                    Core.<*> x Core..:? "directConnectGatewayId"
                    Core.<*> x Core..:? "stateChangeError"
                    Core.<*> x Core..:? "virtualInterfaceId"
                    Core.<*> x Core..:? "virtualInterfaceOwnerAccount"
                    Core.<*> x Core..:? "virtualInterfaceRegion"
