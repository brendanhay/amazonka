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
-- Module      : Amazonka.EC2.Types.TransitGatewayMulticastDomainAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayMulticastDomainAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.SubnetAssociation
import Amazonka.EC2.Types.TransitGatewayAttachmentResourceType
import qualified Amazonka.Prelude as Prelude

-- | Describes the resources associated with the transit gateway multicast
-- domain.
--
-- /See:/ 'newTransitGatewayMulticastDomainAssociation' smart constructor.
data TransitGatewayMulticastDomainAssociation = TransitGatewayMulticastDomainAssociation'
  { -- | The ID of the resource.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the transit gateway
    -- multicast domain association resource.
    resourceOwnerId :: Prelude.Maybe Prelude.Text,
    -- | The type of resource, for example a VPC attachment.
    resourceType :: Prelude.Maybe TransitGatewayAttachmentResourceType,
    -- | The subnet associated with the transit gateway multicast domain.
    subnet :: Prelude.Maybe SubnetAssociation,
    -- | The ID of the transit gateway attachment.
    transitGatewayAttachmentId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'resourceOwnerId', 'transitGatewayMulticastDomainAssociation_resourceOwnerId' - The ID of the Amazon Web Services account that owns the transit gateway
-- multicast domain association resource.
--
-- 'resourceType', 'transitGatewayMulticastDomainAssociation_resourceType' - The type of resource, for example a VPC attachment.
--
-- 'subnet', 'transitGatewayMulticastDomainAssociation_subnet' - The subnet associated with the transit gateway multicast domain.
--
-- 'transitGatewayAttachmentId', 'transitGatewayMulticastDomainAssociation_transitGatewayAttachmentId' - The ID of the transit gateway attachment.
newTransitGatewayMulticastDomainAssociation ::
  TransitGatewayMulticastDomainAssociation
newTransitGatewayMulticastDomainAssociation =
  TransitGatewayMulticastDomainAssociation'
    { resourceId =
        Prelude.Nothing,
      resourceOwnerId = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      subnet = Prelude.Nothing,
      transitGatewayAttachmentId =
        Prelude.Nothing
    }

-- | The ID of the resource.
transitGatewayMulticastDomainAssociation_resourceId :: Lens.Lens' TransitGatewayMulticastDomainAssociation (Prelude.Maybe Prelude.Text)
transitGatewayMulticastDomainAssociation_resourceId = Lens.lens (\TransitGatewayMulticastDomainAssociation' {resourceId} -> resourceId) (\s@TransitGatewayMulticastDomainAssociation' {} a -> s {resourceId = a} :: TransitGatewayMulticastDomainAssociation)

-- | The ID of the Amazon Web Services account that owns the transit gateway
-- multicast domain association resource.
transitGatewayMulticastDomainAssociation_resourceOwnerId :: Lens.Lens' TransitGatewayMulticastDomainAssociation (Prelude.Maybe Prelude.Text)
transitGatewayMulticastDomainAssociation_resourceOwnerId = Lens.lens (\TransitGatewayMulticastDomainAssociation' {resourceOwnerId} -> resourceOwnerId) (\s@TransitGatewayMulticastDomainAssociation' {} a -> s {resourceOwnerId = a} :: TransitGatewayMulticastDomainAssociation)

-- | The type of resource, for example a VPC attachment.
transitGatewayMulticastDomainAssociation_resourceType :: Lens.Lens' TransitGatewayMulticastDomainAssociation (Prelude.Maybe TransitGatewayAttachmentResourceType)
transitGatewayMulticastDomainAssociation_resourceType = Lens.lens (\TransitGatewayMulticastDomainAssociation' {resourceType} -> resourceType) (\s@TransitGatewayMulticastDomainAssociation' {} a -> s {resourceType = a} :: TransitGatewayMulticastDomainAssociation)

-- | The subnet associated with the transit gateway multicast domain.
transitGatewayMulticastDomainAssociation_subnet :: Lens.Lens' TransitGatewayMulticastDomainAssociation (Prelude.Maybe SubnetAssociation)
transitGatewayMulticastDomainAssociation_subnet = Lens.lens (\TransitGatewayMulticastDomainAssociation' {subnet} -> subnet) (\s@TransitGatewayMulticastDomainAssociation' {} a -> s {subnet = a} :: TransitGatewayMulticastDomainAssociation)

-- | The ID of the transit gateway attachment.
transitGatewayMulticastDomainAssociation_transitGatewayAttachmentId :: Lens.Lens' TransitGatewayMulticastDomainAssociation (Prelude.Maybe Prelude.Text)
transitGatewayMulticastDomainAssociation_transitGatewayAttachmentId = Lens.lens (\TransitGatewayMulticastDomainAssociation' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@TransitGatewayMulticastDomainAssociation' {} a -> s {transitGatewayAttachmentId = a} :: TransitGatewayMulticastDomainAssociation)

instance
  Data.FromXML
    TransitGatewayMulticastDomainAssociation
  where
  parseXML x =
    TransitGatewayMulticastDomainAssociation'
      Prelude.<$> (x Data..@? "resourceId")
      Prelude.<*> (x Data..@? "resourceOwnerId")
      Prelude.<*> (x Data..@? "resourceType")
      Prelude.<*> (x Data..@? "subnet")
      Prelude.<*> (x Data..@? "transitGatewayAttachmentId")

instance
  Prelude.Hashable
    TransitGatewayMulticastDomainAssociation
  where
  hashWithSalt
    _salt
    TransitGatewayMulticastDomainAssociation' {..} =
      _salt
        `Prelude.hashWithSalt` resourceId
        `Prelude.hashWithSalt` resourceOwnerId
        `Prelude.hashWithSalt` resourceType
        `Prelude.hashWithSalt` subnet
        `Prelude.hashWithSalt` transitGatewayAttachmentId

instance
  Prelude.NFData
    TransitGatewayMulticastDomainAssociation
  where
  rnf TransitGatewayMulticastDomainAssociation' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceOwnerId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf subnet
      `Prelude.seq` Prelude.rnf transitGatewayAttachmentId
