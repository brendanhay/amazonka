{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.DirectConnect.Types.DirectConnectGatewayAttachmentState
import Network.AWS.DirectConnect.Types.DirectConnectGatewayAttachmentType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an attachment between a Direct Connect gateway and a
-- virtual interface.
--
-- /See:/ 'newDirectConnectGatewayAttachment' smart constructor.
data DirectConnectGatewayAttachment = DirectConnectGatewayAttachment'
  { -- | The error message if the state of an object failed to advance.
    stateChangeError :: Prelude.Maybe Prelude.Text,
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
    attachmentState :: Prelude.Maybe DirectConnectGatewayAttachmentState,
    -- | The ID of the AWS account that owns the virtual interface.
    virtualInterfaceOwnerAccount :: Prelude.Maybe Prelude.Text,
    -- | The AWS Region where the virtual interface is located.
    virtualInterfaceRegion :: Prelude.Maybe Prelude.Text,
    -- | The type of attachment.
    attachmentType :: Prelude.Maybe DirectConnectGatewayAttachmentType,
    -- | The ID of the virtual interface.
    virtualInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      attachmentState = Prelude.Nothing,
      virtualInterfaceOwnerAccount =
        Prelude.Nothing,
      virtualInterfaceRegion = Prelude.Nothing,
      attachmentType = Prelude.Nothing,
      virtualInterfaceId = Prelude.Nothing,
      directConnectGatewayId = Prelude.Nothing
    }

-- | The error message if the state of an object failed to advance.
directConnectGatewayAttachment_stateChangeError :: Lens.Lens' DirectConnectGatewayAttachment (Prelude.Maybe Prelude.Text)
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
directConnectGatewayAttachment_attachmentState :: Lens.Lens' DirectConnectGatewayAttachment (Prelude.Maybe DirectConnectGatewayAttachmentState)
directConnectGatewayAttachment_attachmentState = Lens.lens (\DirectConnectGatewayAttachment' {attachmentState} -> attachmentState) (\s@DirectConnectGatewayAttachment' {} a -> s {attachmentState = a} :: DirectConnectGatewayAttachment)

-- | The ID of the AWS account that owns the virtual interface.
directConnectGatewayAttachment_virtualInterfaceOwnerAccount :: Lens.Lens' DirectConnectGatewayAttachment (Prelude.Maybe Prelude.Text)
directConnectGatewayAttachment_virtualInterfaceOwnerAccount = Lens.lens (\DirectConnectGatewayAttachment' {virtualInterfaceOwnerAccount} -> virtualInterfaceOwnerAccount) (\s@DirectConnectGatewayAttachment' {} a -> s {virtualInterfaceOwnerAccount = a} :: DirectConnectGatewayAttachment)

-- | The AWS Region where the virtual interface is located.
directConnectGatewayAttachment_virtualInterfaceRegion :: Lens.Lens' DirectConnectGatewayAttachment (Prelude.Maybe Prelude.Text)
directConnectGatewayAttachment_virtualInterfaceRegion = Lens.lens (\DirectConnectGatewayAttachment' {virtualInterfaceRegion} -> virtualInterfaceRegion) (\s@DirectConnectGatewayAttachment' {} a -> s {virtualInterfaceRegion = a} :: DirectConnectGatewayAttachment)

-- | The type of attachment.
directConnectGatewayAttachment_attachmentType :: Lens.Lens' DirectConnectGatewayAttachment (Prelude.Maybe DirectConnectGatewayAttachmentType)
directConnectGatewayAttachment_attachmentType = Lens.lens (\DirectConnectGatewayAttachment' {attachmentType} -> attachmentType) (\s@DirectConnectGatewayAttachment' {} a -> s {attachmentType = a} :: DirectConnectGatewayAttachment)

-- | The ID of the virtual interface.
directConnectGatewayAttachment_virtualInterfaceId :: Lens.Lens' DirectConnectGatewayAttachment (Prelude.Maybe Prelude.Text)
directConnectGatewayAttachment_virtualInterfaceId = Lens.lens (\DirectConnectGatewayAttachment' {virtualInterfaceId} -> virtualInterfaceId) (\s@DirectConnectGatewayAttachment' {} a -> s {virtualInterfaceId = a} :: DirectConnectGatewayAttachment)

-- | The ID of the Direct Connect gateway.
directConnectGatewayAttachment_directConnectGatewayId :: Lens.Lens' DirectConnectGatewayAttachment (Prelude.Maybe Prelude.Text)
directConnectGatewayAttachment_directConnectGatewayId = Lens.lens (\DirectConnectGatewayAttachment' {directConnectGatewayId} -> directConnectGatewayId) (\s@DirectConnectGatewayAttachment' {} a -> s {directConnectGatewayId = a} :: DirectConnectGatewayAttachment)

instance
  Prelude.FromJSON
    DirectConnectGatewayAttachment
  where
  parseJSON =
    Prelude.withObject
      "DirectConnectGatewayAttachment"
      ( \x ->
          DirectConnectGatewayAttachment'
            Prelude.<$> (x Prelude..:? "stateChangeError")
            Prelude.<*> (x Prelude..:? "attachmentState")
            Prelude.<*> (x Prelude..:? "virtualInterfaceOwnerAccount")
            Prelude.<*> (x Prelude..:? "virtualInterfaceRegion")
            Prelude.<*> (x Prelude..:? "attachmentType")
            Prelude.<*> (x Prelude..:? "virtualInterfaceId")
            Prelude.<*> (x Prelude..:? "directConnectGatewayId")
      )

instance
  Prelude.Hashable
    DirectConnectGatewayAttachment

instance
  Prelude.NFData
    DirectConnectGatewayAttachment
