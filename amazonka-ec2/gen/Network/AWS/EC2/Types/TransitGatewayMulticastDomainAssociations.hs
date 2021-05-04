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
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulticastDomainAssociations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayMulticastDomainAssociations where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.SubnetAssociation
import Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the multicast domain associations.
--
-- /See:/ 'newTransitGatewayMulticastDomainAssociations' smart constructor.
data TransitGatewayMulticastDomainAssociations = TransitGatewayMulticastDomainAssociations'
  { -- | The ID of the resource.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Prelude.Maybe Prelude.Text,
    -- | The type of resource, for example a VPC attachment.
    resourceType :: Prelude.Maybe TransitGatewayAttachmentResourceType,
    -- | The ID of the AWS account that owns the resource.
    resourceOwnerId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway attachment.
    transitGatewayAttachmentId :: Prelude.Maybe Prelude.Text,
    -- | The subnets associated with the multicast domain.
    subnets :: Prelude.Maybe [SubnetAssociation]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'transitGatewayMulticastDomainId', 'transitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
--
-- 'resourceType', 'transitGatewayMulticastDomainAssociations_resourceType' - The type of resource, for example a VPC attachment.
--
-- 'resourceOwnerId', 'transitGatewayMulticastDomainAssociations_resourceOwnerId' - The ID of the AWS account that owns the resource.
--
-- 'transitGatewayAttachmentId', 'transitGatewayMulticastDomainAssociations_transitGatewayAttachmentId' - The ID of the transit gateway attachment.
--
-- 'subnets', 'transitGatewayMulticastDomainAssociations_subnets' - The subnets associated with the multicast domain.
newTransitGatewayMulticastDomainAssociations ::
  TransitGatewayMulticastDomainAssociations
newTransitGatewayMulticastDomainAssociations =
  TransitGatewayMulticastDomainAssociations'
    { resourceId =
        Prelude.Nothing,
      transitGatewayMulticastDomainId =
        Prelude.Nothing,
      resourceType = Prelude.Nothing,
      resourceOwnerId =
        Prelude.Nothing,
      transitGatewayAttachmentId =
        Prelude.Nothing,
      subnets = Prelude.Nothing
    }

-- | The ID of the resource.
transitGatewayMulticastDomainAssociations_resourceId :: Lens.Lens' TransitGatewayMulticastDomainAssociations (Prelude.Maybe Prelude.Text)
transitGatewayMulticastDomainAssociations_resourceId = Lens.lens (\TransitGatewayMulticastDomainAssociations' {resourceId} -> resourceId) (\s@TransitGatewayMulticastDomainAssociations' {} a -> s {resourceId = a} :: TransitGatewayMulticastDomainAssociations)

-- | The ID of the transit gateway multicast domain.
transitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId :: Lens.Lens' TransitGatewayMulticastDomainAssociations (Prelude.Maybe Prelude.Text)
transitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId = Lens.lens (\TransitGatewayMulticastDomainAssociations' {transitGatewayMulticastDomainId} -> transitGatewayMulticastDomainId) (\s@TransitGatewayMulticastDomainAssociations' {} a -> s {transitGatewayMulticastDomainId = a} :: TransitGatewayMulticastDomainAssociations)

-- | The type of resource, for example a VPC attachment.
transitGatewayMulticastDomainAssociations_resourceType :: Lens.Lens' TransitGatewayMulticastDomainAssociations (Prelude.Maybe TransitGatewayAttachmentResourceType)
transitGatewayMulticastDomainAssociations_resourceType = Lens.lens (\TransitGatewayMulticastDomainAssociations' {resourceType} -> resourceType) (\s@TransitGatewayMulticastDomainAssociations' {} a -> s {resourceType = a} :: TransitGatewayMulticastDomainAssociations)

-- | The ID of the AWS account that owns the resource.
transitGatewayMulticastDomainAssociations_resourceOwnerId :: Lens.Lens' TransitGatewayMulticastDomainAssociations (Prelude.Maybe Prelude.Text)
transitGatewayMulticastDomainAssociations_resourceOwnerId = Lens.lens (\TransitGatewayMulticastDomainAssociations' {resourceOwnerId} -> resourceOwnerId) (\s@TransitGatewayMulticastDomainAssociations' {} a -> s {resourceOwnerId = a} :: TransitGatewayMulticastDomainAssociations)

-- | The ID of the transit gateway attachment.
transitGatewayMulticastDomainAssociations_transitGatewayAttachmentId :: Lens.Lens' TransitGatewayMulticastDomainAssociations (Prelude.Maybe Prelude.Text)
transitGatewayMulticastDomainAssociations_transitGatewayAttachmentId = Lens.lens (\TransitGatewayMulticastDomainAssociations' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@TransitGatewayMulticastDomainAssociations' {} a -> s {transitGatewayAttachmentId = a} :: TransitGatewayMulticastDomainAssociations)

-- | The subnets associated with the multicast domain.
transitGatewayMulticastDomainAssociations_subnets :: Lens.Lens' TransitGatewayMulticastDomainAssociations (Prelude.Maybe [SubnetAssociation])
transitGatewayMulticastDomainAssociations_subnets = Lens.lens (\TransitGatewayMulticastDomainAssociations' {subnets} -> subnets) (\s@TransitGatewayMulticastDomainAssociations' {} a -> s {subnets = a} :: TransitGatewayMulticastDomainAssociations) Prelude.. Lens.mapping Prelude._Coerce

instance
  Prelude.FromXML
    TransitGatewayMulticastDomainAssociations
  where
  parseXML x =
    TransitGatewayMulticastDomainAssociations'
      Prelude.<$> (x Prelude..@? "resourceId")
        Prelude.<*> (x Prelude..@? "transitGatewayMulticastDomainId")
        Prelude.<*> (x Prelude..@? "resourceType")
        Prelude.<*> (x Prelude..@? "resourceOwnerId")
        Prelude.<*> (x Prelude..@? "transitGatewayAttachmentId")
        Prelude.<*> ( x Prelude..@? "subnets" Prelude..!@ Prelude.mempty
                        Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                    )

instance
  Prelude.Hashable
    TransitGatewayMulticastDomainAssociations

instance
  Prelude.NFData
    TransitGatewayMulticastDomainAssociations
