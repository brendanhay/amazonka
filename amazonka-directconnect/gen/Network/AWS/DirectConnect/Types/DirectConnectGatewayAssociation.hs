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
-- Module      : Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociation where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types.AssociatedGateway
import Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationState
import Network.AWS.DirectConnect.Types.RouteFilterPrefix
import qualified Network.AWS.Lens as Lens

-- | Information about an association between a Direct Connect gateway and a
-- virtual private gateway or transit gateway.
--
-- /See:/ 'newDirectConnectGatewayAssociation' smart constructor.
data DirectConnectGatewayAssociation = DirectConnectGatewayAssociation'
  { -- | The ID of the virtual private gateway. Applies only to private virtual
    -- interfaces.
    virtualGatewayId :: Core.Maybe Core.Text,
    -- | The ID of the AWS account that owns the virtual private gateway.
    virtualGatewayOwnerAccount :: Core.Maybe Core.Text,
    -- | The error message if the state of an object failed to advance.
    stateChangeError :: Core.Maybe Core.Text,
    -- | The AWS Region where the virtual private gateway is located.
    virtualGatewayRegion :: Core.Maybe Core.Text,
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
    associationState :: Core.Maybe DirectConnectGatewayAssociationState,
    -- | Information about the associated gateway.
    associatedGateway :: Core.Maybe AssociatedGateway,
    -- | The ID of the Direct Connect gateway association.
    associationId :: Core.Maybe Core.Text,
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Core.Maybe Core.Text,
    -- | The Amazon VPC prefixes to advertise to the Direct Connect gateway.
    allowedPrefixesToDirectConnectGateway :: Core.Maybe [RouteFilterPrefix],
    -- | The ID of the AWS account that owns the associated gateway.
    directConnectGatewayOwnerAccount :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DirectConnectGatewayAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'virtualGatewayId', 'directConnectGatewayAssociation_virtualGatewayId' - The ID of the virtual private gateway. Applies only to private virtual
-- interfaces.
--
-- 'virtualGatewayOwnerAccount', 'directConnectGatewayAssociation_virtualGatewayOwnerAccount' - The ID of the AWS account that owns the virtual private gateway.
--
-- 'stateChangeError', 'directConnectGatewayAssociation_stateChangeError' - The error message if the state of an object failed to advance.
--
-- 'virtualGatewayRegion', 'directConnectGatewayAssociation_virtualGatewayRegion' - The AWS Region where the virtual private gateway is located.
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
-- 'associatedGateway', 'directConnectGatewayAssociation_associatedGateway' - Information about the associated gateway.
--
-- 'associationId', 'directConnectGatewayAssociation_associationId' - The ID of the Direct Connect gateway association.
--
-- 'directConnectGatewayId', 'directConnectGatewayAssociation_directConnectGatewayId' - The ID of the Direct Connect gateway.
--
-- 'allowedPrefixesToDirectConnectGateway', 'directConnectGatewayAssociation_allowedPrefixesToDirectConnectGateway' - The Amazon VPC prefixes to advertise to the Direct Connect gateway.
--
-- 'directConnectGatewayOwnerAccount', 'directConnectGatewayAssociation_directConnectGatewayOwnerAccount' - The ID of the AWS account that owns the associated gateway.
newDirectConnectGatewayAssociation ::
  DirectConnectGatewayAssociation
newDirectConnectGatewayAssociation =
  DirectConnectGatewayAssociation'
    { virtualGatewayId =
        Core.Nothing,
      virtualGatewayOwnerAccount = Core.Nothing,
      stateChangeError = Core.Nothing,
      virtualGatewayRegion = Core.Nothing,
      associationState = Core.Nothing,
      associatedGateway = Core.Nothing,
      associationId = Core.Nothing,
      directConnectGatewayId = Core.Nothing,
      allowedPrefixesToDirectConnectGateway =
        Core.Nothing,
      directConnectGatewayOwnerAccount =
        Core.Nothing
    }

-- | The ID of the virtual private gateway. Applies only to private virtual
-- interfaces.
directConnectGatewayAssociation_virtualGatewayId :: Lens.Lens' DirectConnectGatewayAssociation (Core.Maybe Core.Text)
directConnectGatewayAssociation_virtualGatewayId = Lens.lens (\DirectConnectGatewayAssociation' {virtualGatewayId} -> virtualGatewayId) (\s@DirectConnectGatewayAssociation' {} a -> s {virtualGatewayId = a} :: DirectConnectGatewayAssociation)

-- | The ID of the AWS account that owns the virtual private gateway.
directConnectGatewayAssociation_virtualGatewayOwnerAccount :: Lens.Lens' DirectConnectGatewayAssociation (Core.Maybe Core.Text)
directConnectGatewayAssociation_virtualGatewayOwnerAccount = Lens.lens (\DirectConnectGatewayAssociation' {virtualGatewayOwnerAccount} -> virtualGatewayOwnerAccount) (\s@DirectConnectGatewayAssociation' {} a -> s {virtualGatewayOwnerAccount = a} :: DirectConnectGatewayAssociation)

-- | The error message if the state of an object failed to advance.
directConnectGatewayAssociation_stateChangeError :: Lens.Lens' DirectConnectGatewayAssociation (Core.Maybe Core.Text)
directConnectGatewayAssociation_stateChangeError = Lens.lens (\DirectConnectGatewayAssociation' {stateChangeError} -> stateChangeError) (\s@DirectConnectGatewayAssociation' {} a -> s {stateChangeError = a} :: DirectConnectGatewayAssociation)

-- | The AWS Region where the virtual private gateway is located.
directConnectGatewayAssociation_virtualGatewayRegion :: Lens.Lens' DirectConnectGatewayAssociation (Core.Maybe Core.Text)
directConnectGatewayAssociation_virtualGatewayRegion = Lens.lens (\DirectConnectGatewayAssociation' {virtualGatewayRegion} -> virtualGatewayRegion) (\s@DirectConnectGatewayAssociation' {} a -> s {virtualGatewayRegion = a} :: DirectConnectGatewayAssociation)

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
directConnectGatewayAssociation_associationState :: Lens.Lens' DirectConnectGatewayAssociation (Core.Maybe DirectConnectGatewayAssociationState)
directConnectGatewayAssociation_associationState = Lens.lens (\DirectConnectGatewayAssociation' {associationState} -> associationState) (\s@DirectConnectGatewayAssociation' {} a -> s {associationState = a} :: DirectConnectGatewayAssociation)

-- | Information about the associated gateway.
directConnectGatewayAssociation_associatedGateway :: Lens.Lens' DirectConnectGatewayAssociation (Core.Maybe AssociatedGateway)
directConnectGatewayAssociation_associatedGateway = Lens.lens (\DirectConnectGatewayAssociation' {associatedGateway} -> associatedGateway) (\s@DirectConnectGatewayAssociation' {} a -> s {associatedGateway = a} :: DirectConnectGatewayAssociation)

-- | The ID of the Direct Connect gateway association.
directConnectGatewayAssociation_associationId :: Lens.Lens' DirectConnectGatewayAssociation (Core.Maybe Core.Text)
directConnectGatewayAssociation_associationId = Lens.lens (\DirectConnectGatewayAssociation' {associationId} -> associationId) (\s@DirectConnectGatewayAssociation' {} a -> s {associationId = a} :: DirectConnectGatewayAssociation)

-- | The ID of the Direct Connect gateway.
directConnectGatewayAssociation_directConnectGatewayId :: Lens.Lens' DirectConnectGatewayAssociation (Core.Maybe Core.Text)
directConnectGatewayAssociation_directConnectGatewayId = Lens.lens (\DirectConnectGatewayAssociation' {directConnectGatewayId} -> directConnectGatewayId) (\s@DirectConnectGatewayAssociation' {} a -> s {directConnectGatewayId = a} :: DirectConnectGatewayAssociation)

-- | The Amazon VPC prefixes to advertise to the Direct Connect gateway.
directConnectGatewayAssociation_allowedPrefixesToDirectConnectGateway :: Lens.Lens' DirectConnectGatewayAssociation (Core.Maybe [RouteFilterPrefix])
directConnectGatewayAssociation_allowedPrefixesToDirectConnectGateway = Lens.lens (\DirectConnectGatewayAssociation' {allowedPrefixesToDirectConnectGateway} -> allowedPrefixesToDirectConnectGateway) (\s@DirectConnectGatewayAssociation' {} a -> s {allowedPrefixesToDirectConnectGateway = a} :: DirectConnectGatewayAssociation) Core.. Lens.mapping Lens._Coerce

-- | The ID of the AWS account that owns the associated gateway.
directConnectGatewayAssociation_directConnectGatewayOwnerAccount :: Lens.Lens' DirectConnectGatewayAssociation (Core.Maybe Core.Text)
directConnectGatewayAssociation_directConnectGatewayOwnerAccount = Lens.lens (\DirectConnectGatewayAssociation' {directConnectGatewayOwnerAccount} -> directConnectGatewayOwnerAccount) (\s@DirectConnectGatewayAssociation' {} a -> s {directConnectGatewayOwnerAccount = a} :: DirectConnectGatewayAssociation)

instance
  Core.FromJSON
    DirectConnectGatewayAssociation
  where
  parseJSON =
    Core.withObject
      "DirectConnectGatewayAssociation"
      ( \x ->
          DirectConnectGatewayAssociation'
            Core.<$> (x Core..:? "virtualGatewayId")
            Core.<*> (x Core..:? "virtualGatewayOwnerAccount")
            Core.<*> (x Core..:? "stateChangeError")
            Core.<*> (x Core..:? "virtualGatewayRegion")
            Core.<*> (x Core..:? "associationState")
            Core.<*> (x Core..:? "associatedGateway")
            Core.<*> (x Core..:? "associationId")
            Core.<*> (x Core..:? "directConnectGatewayId")
            Core.<*> ( x Core..:? "allowedPrefixesToDirectConnectGateway"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "directConnectGatewayOwnerAccount")
      )

instance
  Core.Hashable
    DirectConnectGatewayAssociation

instance Core.NFData DirectConnectGatewayAssociation
