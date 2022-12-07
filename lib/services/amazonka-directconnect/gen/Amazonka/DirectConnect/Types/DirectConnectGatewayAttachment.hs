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
-- Module      : Amazonka.DirectConnect.Types.DirectConnectGatewayAttachment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectConnect.Types.DirectConnectGatewayAttachment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types.DirectConnectGatewayAttachmentState
import Amazonka.DirectConnect.Types.DirectConnectGatewayAttachmentType
import qualified Amazonka.Prelude as Prelude

-- | Information about an attachment between a Direct Connect gateway and a
-- virtual interface.
--
-- /See:/ 'newDirectConnectGatewayAttachment' smart constructor.
data DirectConnectGatewayAttachment = DirectConnectGatewayAttachment'
  { -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The error message if the state of an object failed to advance.
    stateChangeError :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the virtual
    -- interface.
    virtualInterfaceOwnerAccount :: Prelude.Maybe Prelude.Text,
    -- | The ID of the virtual interface.
    virtualInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region where the virtual interface is located.
    virtualInterfaceRegion :: Prelude.Maybe Prelude.Text,
    -- | The type of attachment.
    attachmentType :: Prelude.Maybe DirectConnectGatewayAttachmentType,
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
    attachmentState :: Prelude.Maybe DirectConnectGatewayAttachmentState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DirectConnectGatewayAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directConnectGatewayId', 'directConnectGatewayAttachment_directConnectGatewayId' - The ID of the Direct Connect gateway.
--
-- 'stateChangeError', 'directConnectGatewayAttachment_stateChangeError' - The error message if the state of an object failed to advance.
--
-- 'virtualInterfaceOwnerAccount', 'directConnectGatewayAttachment_virtualInterfaceOwnerAccount' - The ID of the Amazon Web Services account that owns the virtual
-- interface.
--
-- 'virtualInterfaceId', 'directConnectGatewayAttachment_virtualInterfaceId' - The ID of the virtual interface.
--
-- 'virtualInterfaceRegion', 'directConnectGatewayAttachment_virtualInterfaceRegion' - The Amazon Web Services Region where the virtual interface is located.
--
-- 'attachmentType', 'directConnectGatewayAttachment_attachmentType' - The type of attachment.
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
newDirectConnectGatewayAttachment ::
  DirectConnectGatewayAttachment
newDirectConnectGatewayAttachment =
  DirectConnectGatewayAttachment'
    { directConnectGatewayId =
        Prelude.Nothing,
      stateChangeError = Prelude.Nothing,
      virtualInterfaceOwnerAccount =
        Prelude.Nothing,
      virtualInterfaceId = Prelude.Nothing,
      virtualInterfaceRegion = Prelude.Nothing,
      attachmentType = Prelude.Nothing,
      attachmentState = Prelude.Nothing
    }

-- | The ID of the Direct Connect gateway.
directConnectGatewayAttachment_directConnectGatewayId :: Lens.Lens' DirectConnectGatewayAttachment (Prelude.Maybe Prelude.Text)
directConnectGatewayAttachment_directConnectGatewayId = Lens.lens (\DirectConnectGatewayAttachment' {directConnectGatewayId} -> directConnectGatewayId) (\s@DirectConnectGatewayAttachment' {} a -> s {directConnectGatewayId = a} :: DirectConnectGatewayAttachment)

-- | The error message if the state of an object failed to advance.
directConnectGatewayAttachment_stateChangeError :: Lens.Lens' DirectConnectGatewayAttachment (Prelude.Maybe Prelude.Text)
directConnectGatewayAttachment_stateChangeError = Lens.lens (\DirectConnectGatewayAttachment' {stateChangeError} -> stateChangeError) (\s@DirectConnectGatewayAttachment' {} a -> s {stateChangeError = a} :: DirectConnectGatewayAttachment)

-- | The ID of the Amazon Web Services account that owns the virtual
-- interface.
directConnectGatewayAttachment_virtualInterfaceOwnerAccount :: Lens.Lens' DirectConnectGatewayAttachment (Prelude.Maybe Prelude.Text)
directConnectGatewayAttachment_virtualInterfaceOwnerAccount = Lens.lens (\DirectConnectGatewayAttachment' {virtualInterfaceOwnerAccount} -> virtualInterfaceOwnerAccount) (\s@DirectConnectGatewayAttachment' {} a -> s {virtualInterfaceOwnerAccount = a} :: DirectConnectGatewayAttachment)

-- | The ID of the virtual interface.
directConnectGatewayAttachment_virtualInterfaceId :: Lens.Lens' DirectConnectGatewayAttachment (Prelude.Maybe Prelude.Text)
directConnectGatewayAttachment_virtualInterfaceId = Lens.lens (\DirectConnectGatewayAttachment' {virtualInterfaceId} -> virtualInterfaceId) (\s@DirectConnectGatewayAttachment' {} a -> s {virtualInterfaceId = a} :: DirectConnectGatewayAttachment)

-- | The Amazon Web Services Region where the virtual interface is located.
directConnectGatewayAttachment_virtualInterfaceRegion :: Lens.Lens' DirectConnectGatewayAttachment (Prelude.Maybe Prelude.Text)
directConnectGatewayAttachment_virtualInterfaceRegion = Lens.lens (\DirectConnectGatewayAttachment' {virtualInterfaceRegion} -> virtualInterfaceRegion) (\s@DirectConnectGatewayAttachment' {} a -> s {virtualInterfaceRegion = a} :: DirectConnectGatewayAttachment)

-- | The type of attachment.
directConnectGatewayAttachment_attachmentType :: Lens.Lens' DirectConnectGatewayAttachment (Prelude.Maybe DirectConnectGatewayAttachmentType)
directConnectGatewayAttachment_attachmentType = Lens.lens (\DirectConnectGatewayAttachment' {attachmentType} -> attachmentType) (\s@DirectConnectGatewayAttachment' {} a -> s {attachmentType = a} :: DirectConnectGatewayAttachment)

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

instance Data.FromJSON DirectConnectGatewayAttachment where
  parseJSON =
    Data.withObject
      "DirectConnectGatewayAttachment"
      ( \x ->
          DirectConnectGatewayAttachment'
            Prelude.<$> (x Data..:? "directConnectGatewayId")
            Prelude.<*> (x Data..:? "stateChangeError")
            Prelude.<*> (x Data..:? "virtualInterfaceOwnerAccount")
            Prelude.<*> (x Data..:? "virtualInterfaceId")
            Prelude.<*> (x Data..:? "virtualInterfaceRegion")
            Prelude.<*> (x Data..:? "attachmentType")
            Prelude.<*> (x Data..:? "attachmentState")
      )

instance
  Prelude.Hashable
    DirectConnectGatewayAttachment
  where
  hashWithSalt
    _salt
    DirectConnectGatewayAttachment' {..} =
      _salt `Prelude.hashWithSalt` directConnectGatewayId
        `Prelude.hashWithSalt` stateChangeError
        `Prelude.hashWithSalt` virtualInterfaceOwnerAccount
        `Prelude.hashWithSalt` virtualInterfaceId
        `Prelude.hashWithSalt` virtualInterfaceRegion
        `Prelude.hashWithSalt` attachmentType
        `Prelude.hashWithSalt` attachmentState

instance
  Prelude.NFData
    DirectConnectGatewayAttachment
  where
  rnf DirectConnectGatewayAttachment' {..} =
    Prelude.rnf directConnectGatewayId
      `Prelude.seq` Prelude.rnf stateChangeError
      `Prelude.seq` Prelude.rnf virtualInterfaceOwnerAccount
      `Prelude.seq` Prelude.rnf virtualInterfaceId
      `Prelude.seq` Prelude.rnf virtualInterfaceRegion
      `Prelude.seq` Prelude.rnf attachmentType
      `Prelude.seq` Prelude.rnf attachmentState
