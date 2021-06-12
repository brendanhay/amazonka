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
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulticastDomainAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayMulticastDomainAssociation where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.SubnetAssociation
import Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType
import qualified Network.AWS.Lens as Lens

-- | Describes the resources associated with the transit gateway multicast
-- domain.
--
-- /See:/ 'newTransitGatewayMulticastDomainAssociation' smart constructor.
data TransitGatewayMulticastDomainAssociation = TransitGatewayMulticastDomainAssociation'
  { -- | The ID of the resource.
    resourceId :: Core.Maybe Core.Text,
    -- | The subnet associated with the transit gateway multicast domain.
    subnet :: Core.Maybe SubnetAssociation,
    -- | The type of resource, for example a VPC attachment.
    resourceType :: Core.Maybe TransitGatewayAttachmentResourceType,
    -- | The ID of the AWS account that owns the transit gateway multicast domain
    -- association resource.
    resourceOwnerId :: Core.Maybe Core.Text,
    -- | The ID of the transit gateway attachment.
    transitGatewayAttachmentId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TransitGatewayMulticastDomainAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'transitGatewayMulticastDomainAssociation_resourceId' - The ID of the resource.
--
-- 'subnet', 'transitGatewayMulticastDomainAssociation_subnet' - The subnet associated with the transit gateway multicast domain.
--
-- 'resourceType', 'transitGatewayMulticastDomainAssociation_resourceType' - The type of resource, for example a VPC attachment.
--
-- 'resourceOwnerId', 'transitGatewayMulticastDomainAssociation_resourceOwnerId' - The ID of the AWS account that owns the transit gateway multicast domain
-- association resource.
--
-- 'transitGatewayAttachmentId', 'transitGatewayMulticastDomainAssociation_transitGatewayAttachmentId' - The ID of the transit gateway attachment.
newTransitGatewayMulticastDomainAssociation ::
  TransitGatewayMulticastDomainAssociation
newTransitGatewayMulticastDomainAssociation =
  TransitGatewayMulticastDomainAssociation'
    { resourceId =
        Core.Nothing,
      subnet = Core.Nothing,
      resourceType = Core.Nothing,
      resourceOwnerId = Core.Nothing,
      transitGatewayAttachmentId =
        Core.Nothing
    }

-- | The ID of the resource.
transitGatewayMulticastDomainAssociation_resourceId :: Lens.Lens' TransitGatewayMulticastDomainAssociation (Core.Maybe Core.Text)
transitGatewayMulticastDomainAssociation_resourceId = Lens.lens (\TransitGatewayMulticastDomainAssociation' {resourceId} -> resourceId) (\s@TransitGatewayMulticastDomainAssociation' {} a -> s {resourceId = a} :: TransitGatewayMulticastDomainAssociation)

-- | The subnet associated with the transit gateway multicast domain.
transitGatewayMulticastDomainAssociation_subnet :: Lens.Lens' TransitGatewayMulticastDomainAssociation (Core.Maybe SubnetAssociation)
transitGatewayMulticastDomainAssociation_subnet = Lens.lens (\TransitGatewayMulticastDomainAssociation' {subnet} -> subnet) (\s@TransitGatewayMulticastDomainAssociation' {} a -> s {subnet = a} :: TransitGatewayMulticastDomainAssociation)

-- | The type of resource, for example a VPC attachment.
transitGatewayMulticastDomainAssociation_resourceType :: Lens.Lens' TransitGatewayMulticastDomainAssociation (Core.Maybe TransitGatewayAttachmentResourceType)
transitGatewayMulticastDomainAssociation_resourceType = Lens.lens (\TransitGatewayMulticastDomainAssociation' {resourceType} -> resourceType) (\s@TransitGatewayMulticastDomainAssociation' {} a -> s {resourceType = a} :: TransitGatewayMulticastDomainAssociation)

-- | The ID of the AWS account that owns the transit gateway multicast domain
-- association resource.
transitGatewayMulticastDomainAssociation_resourceOwnerId :: Lens.Lens' TransitGatewayMulticastDomainAssociation (Core.Maybe Core.Text)
transitGatewayMulticastDomainAssociation_resourceOwnerId = Lens.lens (\TransitGatewayMulticastDomainAssociation' {resourceOwnerId} -> resourceOwnerId) (\s@TransitGatewayMulticastDomainAssociation' {} a -> s {resourceOwnerId = a} :: TransitGatewayMulticastDomainAssociation)

-- | The ID of the transit gateway attachment.
transitGatewayMulticastDomainAssociation_transitGatewayAttachmentId :: Lens.Lens' TransitGatewayMulticastDomainAssociation (Core.Maybe Core.Text)
transitGatewayMulticastDomainAssociation_transitGatewayAttachmentId = Lens.lens (\TransitGatewayMulticastDomainAssociation' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@TransitGatewayMulticastDomainAssociation' {} a -> s {transitGatewayAttachmentId = a} :: TransitGatewayMulticastDomainAssociation)

instance
  Core.FromXML
    TransitGatewayMulticastDomainAssociation
  where
  parseXML x =
    TransitGatewayMulticastDomainAssociation'
      Core.<$> (x Core..@? "resourceId")
      Core.<*> (x Core..@? "subnet")
      Core.<*> (x Core..@? "resourceType")
      Core.<*> (x Core..@? "resourceOwnerId")
      Core.<*> (x Core..@? "transitGatewayAttachmentId")

instance
  Core.Hashable
    TransitGatewayMulticastDomainAssociation

instance
  Core.NFData
    TransitGatewayMulticastDomainAssociation
