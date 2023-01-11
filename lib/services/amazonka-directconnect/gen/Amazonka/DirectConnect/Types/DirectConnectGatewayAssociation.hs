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
-- Module      : Amazonka.DirectConnect.Types.DirectConnectGatewayAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectConnect.Types.DirectConnectGatewayAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types.AssociatedGateway
import Amazonka.DirectConnect.Types.DirectConnectGatewayAssociationState
import Amazonka.DirectConnect.Types.RouteFilterPrefix
import qualified Amazonka.Prelude as Prelude

-- | Information about an association between a Direct Connect gateway and a
-- virtual private gateway or transit gateway.
--
-- /See:/ 'newDirectConnectGatewayAssociation' smart constructor.
data DirectConnectGatewayAssociation = DirectConnectGatewayAssociation'
  { -- | The Amazon VPC prefixes to advertise to the Direct Connect gateway.
    allowedPrefixesToDirectConnectGateway :: Prelude.Maybe [RouteFilterPrefix],
    -- | Information about the associated gateway.
    associatedGateway :: Prelude.Maybe AssociatedGateway,
    -- | The ID of the Direct Connect gateway association.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The state of the association. The following are the possible values:
    --
    -- -   @associating@: The initial state after calling
    --     CreateDirectConnectGatewayAssociation.
    --
    -- -   @associated@: The Direct Connect gateway and virtual private gateway
    --     or transit gateway are successfully associated and ready to pass
    --     traffic.
    --
    -- -   @disassociating@: The initial state after calling
    --     DeleteDirectConnectGatewayAssociation.
    --
    -- -   @disassociated@: The virtual private gateway or transit gateway is
    --     disassociated from the Direct Connect gateway. Traffic flow between
    --     the Direct Connect gateway and virtual private gateway or transit
    --     gateway is stopped.
    associationState :: Prelude.Maybe DirectConnectGatewayAssociationState,
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the associated
    -- gateway.
    directConnectGatewayOwnerAccount :: Prelude.Maybe Prelude.Text,
    -- | The error message if the state of an object failed to advance.
    stateChangeError :: Prelude.Maybe Prelude.Text,
    -- | The ID of the virtual private gateway. Applies only to private virtual
    -- interfaces.
    virtualGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the virtual private
    -- gateway.
    virtualGatewayOwnerAccount :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region where the virtual private gateway is
    -- located.
    virtualGatewayRegion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DirectConnectGatewayAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowedPrefixesToDirectConnectGateway', 'directConnectGatewayAssociation_allowedPrefixesToDirectConnectGateway' - The Amazon VPC prefixes to advertise to the Direct Connect gateway.
--
-- 'associatedGateway', 'directConnectGatewayAssociation_associatedGateway' - Information about the associated gateway.
--
-- 'associationId', 'directConnectGatewayAssociation_associationId' - The ID of the Direct Connect gateway association.
--
-- 'associationState', 'directConnectGatewayAssociation_associationState' - The state of the association. The following are the possible values:
--
-- -   @associating@: The initial state after calling
--     CreateDirectConnectGatewayAssociation.
--
-- -   @associated@: The Direct Connect gateway and virtual private gateway
--     or transit gateway are successfully associated and ready to pass
--     traffic.
--
-- -   @disassociating@: The initial state after calling
--     DeleteDirectConnectGatewayAssociation.
--
-- -   @disassociated@: The virtual private gateway or transit gateway is
--     disassociated from the Direct Connect gateway. Traffic flow between
--     the Direct Connect gateway and virtual private gateway or transit
--     gateway is stopped.
--
-- 'directConnectGatewayId', 'directConnectGatewayAssociation_directConnectGatewayId' - The ID of the Direct Connect gateway.
--
-- 'directConnectGatewayOwnerAccount', 'directConnectGatewayAssociation_directConnectGatewayOwnerAccount' - The ID of the Amazon Web Services account that owns the associated
-- gateway.
--
-- 'stateChangeError', 'directConnectGatewayAssociation_stateChangeError' - The error message if the state of an object failed to advance.
--
-- 'virtualGatewayId', 'directConnectGatewayAssociation_virtualGatewayId' - The ID of the virtual private gateway. Applies only to private virtual
-- interfaces.
--
-- 'virtualGatewayOwnerAccount', 'directConnectGatewayAssociation_virtualGatewayOwnerAccount' - The ID of the Amazon Web Services account that owns the virtual private
-- gateway.
--
-- 'virtualGatewayRegion', 'directConnectGatewayAssociation_virtualGatewayRegion' - The Amazon Web Services Region where the virtual private gateway is
-- located.
newDirectConnectGatewayAssociation ::
  DirectConnectGatewayAssociation
newDirectConnectGatewayAssociation =
  DirectConnectGatewayAssociation'
    { allowedPrefixesToDirectConnectGateway =
        Prelude.Nothing,
      associatedGateway = Prelude.Nothing,
      associationId = Prelude.Nothing,
      associationState = Prelude.Nothing,
      directConnectGatewayId = Prelude.Nothing,
      directConnectGatewayOwnerAccount =
        Prelude.Nothing,
      stateChangeError = Prelude.Nothing,
      virtualGatewayId = Prelude.Nothing,
      virtualGatewayOwnerAccount =
        Prelude.Nothing,
      virtualGatewayRegion = Prelude.Nothing
    }

-- | The Amazon VPC prefixes to advertise to the Direct Connect gateway.
directConnectGatewayAssociation_allowedPrefixesToDirectConnectGateway :: Lens.Lens' DirectConnectGatewayAssociation (Prelude.Maybe [RouteFilterPrefix])
directConnectGatewayAssociation_allowedPrefixesToDirectConnectGateway = Lens.lens (\DirectConnectGatewayAssociation' {allowedPrefixesToDirectConnectGateway} -> allowedPrefixesToDirectConnectGateway) (\s@DirectConnectGatewayAssociation' {} a -> s {allowedPrefixesToDirectConnectGateway = a} :: DirectConnectGatewayAssociation) Prelude.. Lens.mapping Lens.coerced

-- | Information about the associated gateway.
directConnectGatewayAssociation_associatedGateway :: Lens.Lens' DirectConnectGatewayAssociation (Prelude.Maybe AssociatedGateway)
directConnectGatewayAssociation_associatedGateway = Lens.lens (\DirectConnectGatewayAssociation' {associatedGateway} -> associatedGateway) (\s@DirectConnectGatewayAssociation' {} a -> s {associatedGateway = a} :: DirectConnectGatewayAssociation)

-- | The ID of the Direct Connect gateway association.
directConnectGatewayAssociation_associationId :: Lens.Lens' DirectConnectGatewayAssociation (Prelude.Maybe Prelude.Text)
directConnectGatewayAssociation_associationId = Lens.lens (\DirectConnectGatewayAssociation' {associationId} -> associationId) (\s@DirectConnectGatewayAssociation' {} a -> s {associationId = a} :: DirectConnectGatewayAssociation)

-- | The state of the association. The following are the possible values:
--
-- -   @associating@: The initial state after calling
--     CreateDirectConnectGatewayAssociation.
--
-- -   @associated@: The Direct Connect gateway and virtual private gateway
--     or transit gateway are successfully associated and ready to pass
--     traffic.
--
-- -   @disassociating@: The initial state after calling
--     DeleteDirectConnectGatewayAssociation.
--
-- -   @disassociated@: The virtual private gateway or transit gateway is
--     disassociated from the Direct Connect gateway. Traffic flow between
--     the Direct Connect gateway and virtual private gateway or transit
--     gateway is stopped.
directConnectGatewayAssociation_associationState :: Lens.Lens' DirectConnectGatewayAssociation (Prelude.Maybe DirectConnectGatewayAssociationState)
directConnectGatewayAssociation_associationState = Lens.lens (\DirectConnectGatewayAssociation' {associationState} -> associationState) (\s@DirectConnectGatewayAssociation' {} a -> s {associationState = a} :: DirectConnectGatewayAssociation)

-- | The ID of the Direct Connect gateway.
directConnectGatewayAssociation_directConnectGatewayId :: Lens.Lens' DirectConnectGatewayAssociation (Prelude.Maybe Prelude.Text)
directConnectGatewayAssociation_directConnectGatewayId = Lens.lens (\DirectConnectGatewayAssociation' {directConnectGatewayId} -> directConnectGatewayId) (\s@DirectConnectGatewayAssociation' {} a -> s {directConnectGatewayId = a} :: DirectConnectGatewayAssociation)

-- | The ID of the Amazon Web Services account that owns the associated
-- gateway.
directConnectGatewayAssociation_directConnectGatewayOwnerAccount :: Lens.Lens' DirectConnectGatewayAssociation (Prelude.Maybe Prelude.Text)
directConnectGatewayAssociation_directConnectGatewayOwnerAccount = Lens.lens (\DirectConnectGatewayAssociation' {directConnectGatewayOwnerAccount} -> directConnectGatewayOwnerAccount) (\s@DirectConnectGatewayAssociation' {} a -> s {directConnectGatewayOwnerAccount = a} :: DirectConnectGatewayAssociation)

-- | The error message if the state of an object failed to advance.
directConnectGatewayAssociation_stateChangeError :: Lens.Lens' DirectConnectGatewayAssociation (Prelude.Maybe Prelude.Text)
directConnectGatewayAssociation_stateChangeError = Lens.lens (\DirectConnectGatewayAssociation' {stateChangeError} -> stateChangeError) (\s@DirectConnectGatewayAssociation' {} a -> s {stateChangeError = a} :: DirectConnectGatewayAssociation)

-- | The ID of the virtual private gateway. Applies only to private virtual
-- interfaces.
directConnectGatewayAssociation_virtualGatewayId :: Lens.Lens' DirectConnectGatewayAssociation (Prelude.Maybe Prelude.Text)
directConnectGatewayAssociation_virtualGatewayId = Lens.lens (\DirectConnectGatewayAssociation' {virtualGatewayId} -> virtualGatewayId) (\s@DirectConnectGatewayAssociation' {} a -> s {virtualGatewayId = a} :: DirectConnectGatewayAssociation)

-- | The ID of the Amazon Web Services account that owns the virtual private
-- gateway.
directConnectGatewayAssociation_virtualGatewayOwnerAccount :: Lens.Lens' DirectConnectGatewayAssociation (Prelude.Maybe Prelude.Text)
directConnectGatewayAssociation_virtualGatewayOwnerAccount = Lens.lens (\DirectConnectGatewayAssociation' {virtualGatewayOwnerAccount} -> virtualGatewayOwnerAccount) (\s@DirectConnectGatewayAssociation' {} a -> s {virtualGatewayOwnerAccount = a} :: DirectConnectGatewayAssociation)

-- | The Amazon Web Services Region where the virtual private gateway is
-- located.
directConnectGatewayAssociation_virtualGatewayRegion :: Lens.Lens' DirectConnectGatewayAssociation (Prelude.Maybe Prelude.Text)
directConnectGatewayAssociation_virtualGatewayRegion = Lens.lens (\DirectConnectGatewayAssociation' {virtualGatewayRegion} -> virtualGatewayRegion) (\s@DirectConnectGatewayAssociation' {} a -> s {virtualGatewayRegion = a} :: DirectConnectGatewayAssociation)

instance
  Data.FromJSON
    DirectConnectGatewayAssociation
  where
  parseJSON =
    Data.withObject
      "DirectConnectGatewayAssociation"
      ( \x ->
          DirectConnectGatewayAssociation'
            Prelude.<$> ( x Data..:? "allowedPrefixesToDirectConnectGateway"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "associatedGateway")
            Prelude.<*> (x Data..:? "associationId")
            Prelude.<*> (x Data..:? "associationState")
            Prelude.<*> (x Data..:? "directConnectGatewayId")
            Prelude.<*> (x Data..:? "directConnectGatewayOwnerAccount")
            Prelude.<*> (x Data..:? "stateChangeError")
            Prelude.<*> (x Data..:? "virtualGatewayId")
            Prelude.<*> (x Data..:? "virtualGatewayOwnerAccount")
            Prelude.<*> (x Data..:? "virtualGatewayRegion")
      )

instance
  Prelude.Hashable
    DirectConnectGatewayAssociation
  where
  hashWithSalt
    _salt
    DirectConnectGatewayAssociation' {..} =
      _salt
        `Prelude.hashWithSalt` allowedPrefixesToDirectConnectGateway
        `Prelude.hashWithSalt` associatedGateway
        `Prelude.hashWithSalt` associationId
        `Prelude.hashWithSalt` associationState
        `Prelude.hashWithSalt` directConnectGatewayId
        `Prelude.hashWithSalt` directConnectGatewayOwnerAccount
        `Prelude.hashWithSalt` stateChangeError
        `Prelude.hashWithSalt` virtualGatewayId
        `Prelude.hashWithSalt` virtualGatewayOwnerAccount
        `Prelude.hashWithSalt` virtualGatewayRegion

instance
  Prelude.NFData
    DirectConnectGatewayAssociation
  where
  rnf DirectConnectGatewayAssociation' {..} =
    Prelude.rnf allowedPrefixesToDirectConnectGateway
      `Prelude.seq` Prelude.rnf associatedGateway
      `Prelude.seq` Prelude.rnf associationId
      `Prelude.seq` Prelude.rnf associationState
      `Prelude.seq` Prelude.rnf directConnectGatewayId
      `Prelude.seq` Prelude.rnf directConnectGatewayOwnerAccount
      `Prelude.seq` Prelude.rnf stateChangeError
      `Prelude.seq` Prelude.rnf virtualGatewayId
      `Prelude.seq` Prelude.rnf virtualGatewayOwnerAccount
      `Prelude.seq` Prelude.rnf virtualGatewayRegion
