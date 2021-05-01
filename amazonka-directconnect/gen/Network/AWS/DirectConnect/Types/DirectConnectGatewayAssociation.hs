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
-- Module      : Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociation where

import Network.AWS.DirectConnect.Types.AssociatedGateway
import Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationState
import Network.AWS.DirectConnect.Types.RouteFilterPrefix
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an association between a Direct Connect gateway and a
-- virtual private gateway or transit gateway.
--
-- /See:/ 'newDirectConnectGatewayAssociation' smart constructor.
data DirectConnectGatewayAssociation = DirectConnectGatewayAssociation'
  { -- | The ID of the virtual private gateway. Applies only to private virtual
    -- interfaces.
    virtualGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the AWS account that owns the virtual private gateway.
    virtualGatewayOwnerAccount :: Prelude.Maybe Prelude.Text,
    -- | The error message if the state of an object failed to advance.
    stateChangeError :: Prelude.Maybe Prelude.Text,
    -- | The AWS Region where the virtual private gateway is located.
    virtualGatewayRegion :: Prelude.Maybe Prelude.Text,
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
    -- | Information about the associated gateway.
    associatedGateway :: Prelude.Maybe AssociatedGateway,
    -- | The ID of the Direct Connect gateway association.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon VPC prefixes to advertise to the Direct Connect gateway.
    allowedPrefixesToDirectConnectGateway :: Prelude.Maybe [RouteFilterPrefix],
    -- | The ID of the AWS account that owns the associated gateway.
    directConnectGatewayOwnerAccount :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      virtualGatewayOwnerAccount =
        Prelude.Nothing,
      stateChangeError = Prelude.Nothing,
      virtualGatewayRegion = Prelude.Nothing,
      associationState = Prelude.Nothing,
      associatedGateway = Prelude.Nothing,
      associationId = Prelude.Nothing,
      directConnectGatewayId = Prelude.Nothing,
      allowedPrefixesToDirectConnectGateway =
        Prelude.Nothing,
      directConnectGatewayOwnerAccount =
        Prelude.Nothing
    }

-- | The ID of the virtual private gateway. Applies only to private virtual
-- interfaces.
directConnectGatewayAssociation_virtualGatewayId :: Lens.Lens' DirectConnectGatewayAssociation (Prelude.Maybe Prelude.Text)
directConnectGatewayAssociation_virtualGatewayId = Lens.lens (\DirectConnectGatewayAssociation' {virtualGatewayId} -> virtualGatewayId) (\s@DirectConnectGatewayAssociation' {} a -> s {virtualGatewayId = a} :: DirectConnectGatewayAssociation)

-- | The ID of the AWS account that owns the virtual private gateway.
directConnectGatewayAssociation_virtualGatewayOwnerAccount :: Lens.Lens' DirectConnectGatewayAssociation (Prelude.Maybe Prelude.Text)
directConnectGatewayAssociation_virtualGatewayOwnerAccount = Lens.lens (\DirectConnectGatewayAssociation' {virtualGatewayOwnerAccount} -> virtualGatewayOwnerAccount) (\s@DirectConnectGatewayAssociation' {} a -> s {virtualGatewayOwnerAccount = a} :: DirectConnectGatewayAssociation)

-- | The error message if the state of an object failed to advance.
directConnectGatewayAssociation_stateChangeError :: Lens.Lens' DirectConnectGatewayAssociation (Prelude.Maybe Prelude.Text)
directConnectGatewayAssociation_stateChangeError = Lens.lens (\DirectConnectGatewayAssociation' {stateChangeError} -> stateChangeError) (\s@DirectConnectGatewayAssociation' {} a -> s {stateChangeError = a} :: DirectConnectGatewayAssociation)

-- | The AWS Region where the virtual private gateway is located.
directConnectGatewayAssociation_virtualGatewayRegion :: Lens.Lens' DirectConnectGatewayAssociation (Prelude.Maybe Prelude.Text)
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
directConnectGatewayAssociation_associationState :: Lens.Lens' DirectConnectGatewayAssociation (Prelude.Maybe DirectConnectGatewayAssociationState)
directConnectGatewayAssociation_associationState = Lens.lens (\DirectConnectGatewayAssociation' {associationState} -> associationState) (\s@DirectConnectGatewayAssociation' {} a -> s {associationState = a} :: DirectConnectGatewayAssociation)

-- | Information about the associated gateway.
directConnectGatewayAssociation_associatedGateway :: Lens.Lens' DirectConnectGatewayAssociation (Prelude.Maybe AssociatedGateway)
directConnectGatewayAssociation_associatedGateway = Lens.lens (\DirectConnectGatewayAssociation' {associatedGateway} -> associatedGateway) (\s@DirectConnectGatewayAssociation' {} a -> s {associatedGateway = a} :: DirectConnectGatewayAssociation)

-- | The ID of the Direct Connect gateway association.
directConnectGatewayAssociation_associationId :: Lens.Lens' DirectConnectGatewayAssociation (Prelude.Maybe Prelude.Text)
directConnectGatewayAssociation_associationId = Lens.lens (\DirectConnectGatewayAssociation' {associationId} -> associationId) (\s@DirectConnectGatewayAssociation' {} a -> s {associationId = a} :: DirectConnectGatewayAssociation)

-- | The ID of the Direct Connect gateway.
directConnectGatewayAssociation_directConnectGatewayId :: Lens.Lens' DirectConnectGatewayAssociation (Prelude.Maybe Prelude.Text)
directConnectGatewayAssociation_directConnectGatewayId = Lens.lens (\DirectConnectGatewayAssociation' {directConnectGatewayId} -> directConnectGatewayId) (\s@DirectConnectGatewayAssociation' {} a -> s {directConnectGatewayId = a} :: DirectConnectGatewayAssociation)

-- | The Amazon VPC prefixes to advertise to the Direct Connect gateway.
directConnectGatewayAssociation_allowedPrefixesToDirectConnectGateway :: Lens.Lens' DirectConnectGatewayAssociation (Prelude.Maybe [RouteFilterPrefix])
directConnectGatewayAssociation_allowedPrefixesToDirectConnectGateway = Lens.lens (\DirectConnectGatewayAssociation' {allowedPrefixesToDirectConnectGateway} -> allowedPrefixesToDirectConnectGateway) (\s@DirectConnectGatewayAssociation' {} a -> s {allowedPrefixesToDirectConnectGateway = a} :: DirectConnectGatewayAssociation) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the AWS account that owns the associated gateway.
directConnectGatewayAssociation_directConnectGatewayOwnerAccount :: Lens.Lens' DirectConnectGatewayAssociation (Prelude.Maybe Prelude.Text)
directConnectGatewayAssociation_directConnectGatewayOwnerAccount = Lens.lens (\DirectConnectGatewayAssociation' {directConnectGatewayOwnerAccount} -> directConnectGatewayOwnerAccount) (\s@DirectConnectGatewayAssociation' {} a -> s {directConnectGatewayOwnerAccount = a} :: DirectConnectGatewayAssociation)

instance
  Prelude.FromJSON
    DirectConnectGatewayAssociation
  where
  parseJSON =
    Prelude.withObject
      "DirectConnectGatewayAssociation"
      ( \x ->
          DirectConnectGatewayAssociation'
            Prelude.<$> (x Prelude..:? "virtualGatewayId")
            Prelude.<*> (x Prelude..:? "virtualGatewayOwnerAccount")
            Prelude.<*> (x Prelude..:? "stateChangeError")
            Prelude.<*> (x Prelude..:? "virtualGatewayRegion")
            Prelude.<*> (x Prelude..:? "associationState")
            Prelude.<*> (x Prelude..:? "associatedGateway")
            Prelude.<*> (x Prelude..:? "associationId")
            Prelude.<*> (x Prelude..:? "directConnectGatewayId")
            Prelude.<*> ( x
                            Prelude..:? "allowedPrefixesToDirectConnectGateway"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "directConnectGatewayOwnerAccount")
      )

instance
  Prelude.Hashable
    DirectConnectGatewayAssociation

instance
  Prelude.NFData
    DirectConnectGatewayAssociation
