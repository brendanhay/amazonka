{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.DirectConnectGatewayAttachment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.DirectConnectGatewayAttachment where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types.DirectConnectGatewayAttachmentState
import Network.AWS.DirectConnect.Types.DirectConnectGatewayAttachmentType
import qualified Network.AWS.Lens as Lens

-- | Information about an attachment between a Direct Connect gateway and a
-- virtual interface.
--
-- /See:/ 'newDirectConnectGatewayAttachment' smart constructor.
data DirectConnectGatewayAttachment = DirectConnectGatewayAttachment'
  { -- | The error message if the state of an object failed to advance.
    stateChangeError :: Core.Maybe Core.Text,
    -- | The state of the attachment. The following are the possible values:
    --
    -- -   @attaching@: The initial state after a virtual interface is created
    --     using the Direct Connect gateway.
    --
    -- -   @attached@: The Direct Connect gateway and virtual interface are
    --     attached and ready to pass traffic.
    --
    -- -   @detaching@: The initial state after calling DeleteVirtualInterface.
    --
    -- -   @detached@: The virtual interface is detached from the Direct
    --     Connect gateway. Traffic flow between the Direct Connect gateway and
    --     virtual interface is stopped.
    attachmentState :: Core.Maybe DirectConnectGatewayAttachmentState,
    -- | The ID of the AWS account that owns the virtual interface.
    virtualInterfaceOwnerAccount :: Core.Maybe Core.Text,
    -- | The AWS Region where the virtual interface is located.
    virtualInterfaceRegion :: Core.Maybe Core.Text,
    -- | The type of attachment.
    attachmentType :: Core.Maybe DirectConnectGatewayAttachmentType,
    -- | The ID of the virtual interface.
    virtualInterfaceId :: Core.Maybe Core.Text,
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DirectConnectGatewayAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stateChangeError', 'directConnectGatewayAttachment_stateChangeError' - The error message if the state of an object failed to advance.
--
-- 'attachmentState', 'directConnectGatewayAttachment_attachmentState' - The state of the attachment. The following are the possible values:
--
-- -   @attaching@: The initial state after a virtual interface is created
--     using the Direct Connect gateway.
--
-- -   @attached@: The Direct Connect gateway and virtual interface are
--     attached and ready to pass traffic.
--
-- -   @detaching@: The initial state after calling DeleteVirtualInterface.
--
-- -   @detached@: The virtual interface is detached from the Direct
--     Connect gateway. Traffic flow between the Direct Connect gateway and
--     virtual interface is stopped.
--
-- 'virtualInterfaceOwnerAccount', 'directConnectGatewayAttachment_virtualInterfaceOwnerAccount' - The ID of the AWS account that owns the virtual interface.
--
-- 'virtualInterfaceRegion', 'directConnectGatewayAttachment_virtualInterfaceRegion' - The AWS Region where the virtual interface is located.
--
-- 'attachmentType', 'directConnectGatewayAttachment_attachmentType' - The type of attachment.
--
-- 'virtualInterfaceId', 'directConnectGatewayAttachment_virtualInterfaceId' - The ID of the virtual interface.
--
-- 'directConnectGatewayId', 'directConnectGatewayAttachment_directConnectGatewayId' - The ID of the Direct Connect gateway.
newDirectConnectGatewayAttachment ::
  DirectConnectGatewayAttachment
newDirectConnectGatewayAttachment =
  DirectConnectGatewayAttachment'
    { stateChangeError =
        Core.Nothing,
      attachmentState = Core.Nothing,
      virtualInterfaceOwnerAccount = Core.Nothing,
      virtualInterfaceRegion = Core.Nothing,
      attachmentType = Core.Nothing,
      virtualInterfaceId = Core.Nothing,
      directConnectGatewayId = Core.Nothing
    }

-- | The error message if the state of an object failed to advance.
directConnectGatewayAttachment_stateChangeError :: Lens.Lens' DirectConnectGatewayAttachment (Core.Maybe Core.Text)
directConnectGatewayAttachment_stateChangeError = Lens.lens (\DirectConnectGatewayAttachment' {stateChangeError} -> stateChangeError) (\s@DirectConnectGatewayAttachment' {} a -> s {stateChangeError = a} :: DirectConnectGatewayAttachment)

-- | The state of the attachment. The following are the possible values:
--
-- -   @attaching@: The initial state after a virtual interface is created
--     using the Direct Connect gateway.
--
-- -   @attached@: The Direct Connect gateway and virtual interface are
--     attached and ready to pass traffic.
--
-- -   @detaching@: The initial state after calling DeleteVirtualInterface.
--
-- -   @detached@: The virtual interface is detached from the Direct
--     Connect gateway. Traffic flow between the Direct Connect gateway and
--     virtual interface is stopped.
directConnectGatewayAttachment_attachmentState :: Lens.Lens' DirectConnectGatewayAttachment (Core.Maybe DirectConnectGatewayAttachmentState)
directConnectGatewayAttachment_attachmentState = Lens.lens (\DirectConnectGatewayAttachment' {attachmentState} -> attachmentState) (\s@DirectConnectGatewayAttachment' {} a -> s {attachmentState = a} :: DirectConnectGatewayAttachment)

-- | The ID of the AWS account that owns the virtual interface.
directConnectGatewayAttachment_virtualInterfaceOwnerAccount :: Lens.Lens' DirectConnectGatewayAttachment (Core.Maybe Core.Text)
directConnectGatewayAttachment_virtualInterfaceOwnerAccount = Lens.lens (\DirectConnectGatewayAttachment' {virtualInterfaceOwnerAccount} -> virtualInterfaceOwnerAccount) (\s@DirectConnectGatewayAttachment' {} a -> s {virtualInterfaceOwnerAccount = a} :: DirectConnectGatewayAttachment)

-- | The AWS Region where the virtual interface is located.
directConnectGatewayAttachment_virtualInterfaceRegion :: Lens.Lens' DirectConnectGatewayAttachment (Core.Maybe Core.Text)
directConnectGatewayAttachment_virtualInterfaceRegion = Lens.lens (\DirectConnectGatewayAttachment' {virtualInterfaceRegion} -> virtualInterfaceRegion) (\s@DirectConnectGatewayAttachment' {} a -> s {virtualInterfaceRegion = a} :: DirectConnectGatewayAttachment)

-- | The type of attachment.
directConnectGatewayAttachment_attachmentType :: Lens.Lens' DirectConnectGatewayAttachment (Core.Maybe DirectConnectGatewayAttachmentType)
directConnectGatewayAttachment_attachmentType = Lens.lens (\DirectConnectGatewayAttachment' {attachmentType} -> attachmentType) (\s@DirectConnectGatewayAttachment' {} a -> s {attachmentType = a} :: DirectConnectGatewayAttachment)

-- | The ID of the virtual interface.
directConnectGatewayAttachment_virtualInterfaceId :: Lens.Lens' DirectConnectGatewayAttachment (Core.Maybe Core.Text)
directConnectGatewayAttachment_virtualInterfaceId = Lens.lens (\DirectConnectGatewayAttachment' {virtualInterfaceId} -> virtualInterfaceId) (\s@DirectConnectGatewayAttachment' {} a -> s {virtualInterfaceId = a} :: DirectConnectGatewayAttachment)

-- | The ID of the Direct Connect gateway.
directConnectGatewayAttachment_directConnectGatewayId :: Lens.Lens' DirectConnectGatewayAttachment (Core.Maybe Core.Text)
directConnectGatewayAttachment_directConnectGatewayId = Lens.lens (\DirectConnectGatewayAttachment' {directConnectGatewayId} -> directConnectGatewayId) (\s@DirectConnectGatewayAttachment' {} a -> s {directConnectGatewayId = a} :: DirectConnectGatewayAttachment)

instance Core.FromJSON DirectConnectGatewayAttachment where
  parseJSON =
    Core.withObject
      "DirectConnectGatewayAttachment"
      ( \x ->
          DirectConnectGatewayAttachment'
            Core.<$> (x Core..:? "stateChangeError")
            Core.<*> (x Core..:? "attachmentState")
            Core.<*> (x Core..:? "virtualInterfaceOwnerAccount")
            Core.<*> (x Core..:? "virtualInterfaceRegion")
            Core.<*> (x Core..:? "attachmentType")
            Core.<*> (x Core..:? "virtualInterfaceId")
            Core.<*> (x Core..:? "directConnectGatewayId")
      )

instance Core.Hashable DirectConnectGatewayAttachment

instance Core.NFData DirectConnectGatewayAttachment
