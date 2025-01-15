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
-- Module      : Amazonka.EC2.Types.TransitGatewayAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.TransitGatewayAssociationState
import Amazonka.EC2.Types.TransitGatewayAttachmentResourceType
import qualified Amazonka.Prelude as Prelude

-- | Describes an association between a resource attachment and a transit
-- gateway route table.
--
-- /See:/ 'newTransitGatewayAssociation' smart constructor.
data TransitGatewayAssociation = TransitGatewayAssociation'
  { -- | The ID of the resource.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The resource type. Note that the @tgw-peering@ resource type has been
    -- deprecated.
    resourceType :: Prelude.Maybe TransitGatewayAttachmentResourceType,
    -- | The state of the association.
    state :: Prelude.Maybe TransitGatewayAssociationState,
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'transitGatewayAssociation_resourceId' - The ID of the resource.
--
-- 'resourceType', 'transitGatewayAssociation_resourceType' - The resource type. Note that the @tgw-peering@ resource type has been
-- deprecated.
--
-- 'state', 'transitGatewayAssociation_state' - The state of the association.
--
-- 'transitGatewayAttachmentId', 'transitGatewayAssociation_transitGatewayAttachmentId' - The ID of the attachment.
--
-- 'transitGatewayRouteTableId', 'transitGatewayAssociation_transitGatewayRouteTableId' - The ID of the transit gateway route table.
newTransitGatewayAssociation ::
  TransitGatewayAssociation
newTransitGatewayAssociation =
  TransitGatewayAssociation'
    { resourceId =
        Prelude.Nothing,
      resourceType = Prelude.Nothing,
      state = Prelude.Nothing,
      transitGatewayAttachmentId = Prelude.Nothing,
      transitGatewayRouteTableId = Prelude.Nothing
    }

-- | The ID of the resource.
transitGatewayAssociation_resourceId :: Lens.Lens' TransitGatewayAssociation (Prelude.Maybe Prelude.Text)
transitGatewayAssociation_resourceId = Lens.lens (\TransitGatewayAssociation' {resourceId} -> resourceId) (\s@TransitGatewayAssociation' {} a -> s {resourceId = a} :: TransitGatewayAssociation)

-- | The resource type. Note that the @tgw-peering@ resource type has been
-- deprecated.
transitGatewayAssociation_resourceType :: Lens.Lens' TransitGatewayAssociation (Prelude.Maybe TransitGatewayAttachmentResourceType)
transitGatewayAssociation_resourceType = Lens.lens (\TransitGatewayAssociation' {resourceType} -> resourceType) (\s@TransitGatewayAssociation' {} a -> s {resourceType = a} :: TransitGatewayAssociation)

-- | The state of the association.
transitGatewayAssociation_state :: Lens.Lens' TransitGatewayAssociation (Prelude.Maybe TransitGatewayAssociationState)
transitGatewayAssociation_state = Lens.lens (\TransitGatewayAssociation' {state} -> state) (\s@TransitGatewayAssociation' {} a -> s {state = a} :: TransitGatewayAssociation)

-- | The ID of the attachment.
transitGatewayAssociation_transitGatewayAttachmentId :: Lens.Lens' TransitGatewayAssociation (Prelude.Maybe Prelude.Text)
transitGatewayAssociation_transitGatewayAttachmentId = Lens.lens (\TransitGatewayAssociation' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@TransitGatewayAssociation' {} a -> s {transitGatewayAttachmentId = a} :: TransitGatewayAssociation)

-- | The ID of the transit gateway route table.
transitGatewayAssociation_transitGatewayRouteTableId :: Lens.Lens' TransitGatewayAssociation (Prelude.Maybe Prelude.Text)
transitGatewayAssociation_transitGatewayRouteTableId = Lens.lens (\TransitGatewayAssociation' {transitGatewayRouteTableId} -> transitGatewayRouteTableId) (\s@TransitGatewayAssociation' {} a -> s {transitGatewayRouteTableId = a} :: TransitGatewayAssociation)

instance Data.FromXML TransitGatewayAssociation where
  parseXML x =
    TransitGatewayAssociation'
      Prelude.<$> (x Data..@? "resourceId")
      Prelude.<*> (x Data..@? "resourceType")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> (x Data..@? "transitGatewayAttachmentId")
      Prelude.<*> (x Data..@? "transitGatewayRouteTableId")

instance Prelude.Hashable TransitGatewayAssociation where
  hashWithSalt _salt TransitGatewayAssociation' {..} =
    _salt
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` transitGatewayAttachmentId
      `Prelude.hashWithSalt` transitGatewayRouteTableId

instance Prelude.NFData TransitGatewayAssociation where
  rnf TransitGatewayAssociation' {..} =
    Prelude.rnf resourceId `Prelude.seq`
      Prelude.rnf resourceType `Prelude.seq`
        Prelude.rnf state `Prelude.seq`
          Prelude.rnf transitGatewayAttachmentId `Prelude.seq`
            Prelude.rnf transitGatewayRouteTableId
