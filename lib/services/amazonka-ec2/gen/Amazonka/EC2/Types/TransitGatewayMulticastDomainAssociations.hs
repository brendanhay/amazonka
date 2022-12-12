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
-- Module      : Amazonka.EC2.Types.TransitGatewayMulticastDomainAssociations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayMulticastDomainAssociations where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.SubnetAssociation
import Amazonka.EC2.Types.TransitGatewayAttachmentResourceType
import qualified Amazonka.Prelude as Prelude

-- | Describes the multicast domain associations.
--
-- /See:/ 'newTransitGatewayMulticastDomainAssociations' smart constructor.
data TransitGatewayMulticastDomainAssociations = TransitGatewayMulticastDomainAssociations'
  { -- | The ID of the resource.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the resource.
    resourceOwnerId :: Prelude.Maybe Prelude.Text,
    -- | The type of resource, for example a VPC attachment.
    resourceType :: Prelude.Maybe TransitGatewayAttachmentResourceType,
    -- | The subnets associated with the multicast domain.
    subnets :: Prelude.Maybe [SubnetAssociation],
    -- | The ID of the transit gateway attachment.
    transitGatewayAttachmentId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayMulticastDomainAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'transitGatewayMulticastDomainAssociations_resourceId' - The ID of the resource.
--
-- 'resourceOwnerId', 'transitGatewayMulticastDomainAssociations_resourceOwnerId' - The ID of the Amazon Web Services account that owns the resource.
--
-- 'resourceType', 'transitGatewayMulticastDomainAssociations_resourceType' - The type of resource, for example a VPC attachment.
--
-- 'subnets', 'transitGatewayMulticastDomainAssociations_subnets' - The subnets associated with the multicast domain.
--
-- 'transitGatewayAttachmentId', 'transitGatewayMulticastDomainAssociations_transitGatewayAttachmentId' - The ID of the transit gateway attachment.
--
-- 'transitGatewayMulticastDomainId', 'transitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
newTransitGatewayMulticastDomainAssociations ::
  TransitGatewayMulticastDomainAssociations
newTransitGatewayMulticastDomainAssociations =
  TransitGatewayMulticastDomainAssociations'
    { resourceId =
        Prelude.Nothing,
      resourceOwnerId =
        Prelude.Nothing,
      resourceType = Prelude.Nothing,
      subnets = Prelude.Nothing,
      transitGatewayAttachmentId =
        Prelude.Nothing,
      transitGatewayMulticastDomainId =
        Prelude.Nothing
    }

-- | The ID of the resource.
transitGatewayMulticastDomainAssociations_resourceId :: Lens.Lens' TransitGatewayMulticastDomainAssociations (Prelude.Maybe Prelude.Text)
transitGatewayMulticastDomainAssociations_resourceId = Lens.lens (\TransitGatewayMulticastDomainAssociations' {resourceId} -> resourceId) (\s@TransitGatewayMulticastDomainAssociations' {} a -> s {resourceId = a} :: TransitGatewayMulticastDomainAssociations)

-- | The ID of the Amazon Web Services account that owns the resource.
transitGatewayMulticastDomainAssociations_resourceOwnerId :: Lens.Lens' TransitGatewayMulticastDomainAssociations (Prelude.Maybe Prelude.Text)
transitGatewayMulticastDomainAssociations_resourceOwnerId = Lens.lens (\TransitGatewayMulticastDomainAssociations' {resourceOwnerId} -> resourceOwnerId) (\s@TransitGatewayMulticastDomainAssociations' {} a -> s {resourceOwnerId = a} :: TransitGatewayMulticastDomainAssociations)

-- | The type of resource, for example a VPC attachment.
transitGatewayMulticastDomainAssociations_resourceType :: Lens.Lens' TransitGatewayMulticastDomainAssociations (Prelude.Maybe TransitGatewayAttachmentResourceType)
transitGatewayMulticastDomainAssociations_resourceType = Lens.lens (\TransitGatewayMulticastDomainAssociations' {resourceType} -> resourceType) (\s@TransitGatewayMulticastDomainAssociations' {} a -> s {resourceType = a} :: TransitGatewayMulticastDomainAssociations)

-- | The subnets associated with the multicast domain.
transitGatewayMulticastDomainAssociations_subnets :: Lens.Lens' TransitGatewayMulticastDomainAssociations (Prelude.Maybe [SubnetAssociation])
transitGatewayMulticastDomainAssociations_subnets = Lens.lens (\TransitGatewayMulticastDomainAssociations' {subnets} -> subnets) (\s@TransitGatewayMulticastDomainAssociations' {} a -> s {subnets = a} :: TransitGatewayMulticastDomainAssociations) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the transit gateway attachment.
transitGatewayMulticastDomainAssociations_transitGatewayAttachmentId :: Lens.Lens' TransitGatewayMulticastDomainAssociations (Prelude.Maybe Prelude.Text)
transitGatewayMulticastDomainAssociations_transitGatewayAttachmentId = Lens.lens (\TransitGatewayMulticastDomainAssociations' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@TransitGatewayMulticastDomainAssociations' {} a -> s {transitGatewayAttachmentId = a} :: TransitGatewayMulticastDomainAssociations)

-- | The ID of the transit gateway multicast domain.
transitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId :: Lens.Lens' TransitGatewayMulticastDomainAssociations (Prelude.Maybe Prelude.Text)
transitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId = Lens.lens (\TransitGatewayMulticastDomainAssociations' {transitGatewayMulticastDomainId} -> transitGatewayMulticastDomainId) (\s@TransitGatewayMulticastDomainAssociations' {} a -> s {transitGatewayMulticastDomainId = a} :: TransitGatewayMulticastDomainAssociations)

instance
  Data.FromXML
    TransitGatewayMulticastDomainAssociations
  where
  parseXML x =
    TransitGatewayMulticastDomainAssociations'
      Prelude.<$> (x Data..@? "resourceId")
        Prelude.<*> (x Data..@? "resourceOwnerId")
        Prelude.<*> (x Data..@? "resourceType")
        Prelude.<*> ( x Data..@? "subnets" Core..!@ Prelude.mempty
                        Prelude.>>= Core.may (Data.parseXMLList "item")
                    )
        Prelude.<*> (x Data..@? "transitGatewayAttachmentId")
        Prelude.<*> (x Data..@? "transitGatewayMulticastDomainId")

instance
  Prelude.Hashable
    TransitGatewayMulticastDomainAssociations
  where
  hashWithSalt
    _salt
    TransitGatewayMulticastDomainAssociations' {..} =
      _salt `Prelude.hashWithSalt` resourceId
        `Prelude.hashWithSalt` resourceOwnerId
        `Prelude.hashWithSalt` resourceType
        `Prelude.hashWithSalt` subnets
        `Prelude.hashWithSalt` transitGatewayAttachmentId
        `Prelude.hashWithSalt` transitGatewayMulticastDomainId

instance
  Prelude.NFData
    TransitGatewayMulticastDomainAssociations
  where
  rnf TransitGatewayMulticastDomainAssociations' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceOwnerId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf subnets
      `Prelude.seq` Prelude.rnf transitGatewayAttachmentId
      `Prelude.seq` Prelude.rnf transitGatewayMulticastDomainId
