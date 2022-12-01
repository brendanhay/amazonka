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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2TransitGatewayDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2TransitGatewayDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about an Amazon Web Services Amazon EC2 Transit Gateway that
-- interconnects virtual private clouds (VPCs) and on-premises networks.
--
-- /See:/ 'newAwsEc2TransitGatewayDetails' smart constructor.
data AwsEc2TransitGatewayDetails = AwsEc2TransitGatewayDetails'
  { -- | The ID of the default association route table.
    associationDefaultRouteTableId :: Prelude.Maybe Prelude.Text,
    -- | Turn on or turn off DNS support.
    dnsSupport :: Prelude.Maybe Prelude.Text,
    -- | Turn on or turn off automatic association with the default association
    -- route table.
    defaultRouteTableAssociation :: Prelude.Maybe Prelude.Text,
    -- | The ID of the default propagation route table.
    propagationDefaultRouteTableId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway.
    id :: Prelude.Maybe Prelude.Text,
    -- | The description of the transit gateway.
    description :: Prelude.Maybe Prelude.Text,
    -- | Turn on or turn off automatic acceptance of attachment requests.
    autoAcceptSharedAttachments :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether multicast is supported on the transit gateway.
    multicastSupport :: Prelude.Maybe Prelude.Text,
    -- | A private Autonomous System Number (ASN) for the Amazon side of a BGP
    -- session.
    amazonSideAsn :: Prelude.Maybe Prelude.Int,
    -- | Turn on or turn off Equal Cost Multipath Protocol (ECMP) support.
    vpnEcmpSupport :: Prelude.Maybe Prelude.Text,
    -- | Turn on or turn off automatic propagation of routes to the default
    -- propagation route table.
    defaultRouteTablePropagation :: Prelude.Maybe Prelude.Text,
    -- | The transit gateway Classless Inter-Domain Routing (CIDR) blocks.
    transitGatewayCidrBlocks :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2TransitGatewayDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationDefaultRouteTableId', 'awsEc2TransitGatewayDetails_associationDefaultRouteTableId' - The ID of the default association route table.
--
-- 'dnsSupport', 'awsEc2TransitGatewayDetails_dnsSupport' - Turn on or turn off DNS support.
--
-- 'defaultRouteTableAssociation', 'awsEc2TransitGatewayDetails_defaultRouteTableAssociation' - Turn on or turn off automatic association with the default association
-- route table.
--
-- 'propagationDefaultRouteTableId', 'awsEc2TransitGatewayDetails_propagationDefaultRouteTableId' - The ID of the default propagation route table.
--
-- 'id', 'awsEc2TransitGatewayDetails_id' - The ID of the transit gateway.
--
-- 'description', 'awsEc2TransitGatewayDetails_description' - The description of the transit gateway.
--
-- 'autoAcceptSharedAttachments', 'awsEc2TransitGatewayDetails_autoAcceptSharedAttachments' - Turn on or turn off automatic acceptance of attachment requests.
--
-- 'multicastSupport', 'awsEc2TransitGatewayDetails_multicastSupport' - Indicates whether multicast is supported on the transit gateway.
--
-- 'amazonSideAsn', 'awsEc2TransitGatewayDetails_amazonSideAsn' - A private Autonomous System Number (ASN) for the Amazon side of a BGP
-- session.
--
-- 'vpnEcmpSupport', 'awsEc2TransitGatewayDetails_vpnEcmpSupport' - Turn on or turn off Equal Cost Multipath Protocol (ECMP) support.
--
-- 'defaultRouteTablePropagation', 'awsEc2TransitGatewayDetails_defaultRouteTablePropagation' - Turn on or turn off automatic propagation of routes to the default
-- propagation route table.
--
-- 'transitGatewayCidrBlocks', 'awsEc2TransitGatewayDetails_transitGatewayCidrBlocks' - The transit gateway Classless Inter-Domain Routing (CIDR) blocks.
newAwsEc2TransitGatewayDetails ::
  AwsEc2TransitGatewayDetails
newAwsEc2TransitGatewayDetails =
  AwsEc2TransitGatewayDetails'
    { associationDefaultRouteTableId =
        Prelude.Nothing,
      dnsSupport = Prelude.Nothing,
      defaultRouteTableAssociation = Prelude.Nothing,
      propagationDefaultRouteTableId =
        Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing,
      autoAcceptSharedAttachments = Prelude.Nothing,
      multicastSupport = Prelude.Nothing,
      amazonSideAsn = Prelude.Nothing,
      vpnEcmpSupport = Prelude.Nothing,
      defaultRouteTablePropagation = Prelude.Nothing,
      transitGatewayCidrBlocks = Prelude.Nothing
    }

-- | The ID of the default association route table.
awsEc2TransitGatewayDetails_associationDefaultRouteTableId :: Lens.Lens' AwsEc2TransitGatewayDetails (Prelude.Maybe Prelude.Text)
awsEc2TransitGatewayDetails_associationDefaultRouteTableId = Lens.lens (\AwsEc2TransitGatewayDetails' {associationDefaultRouteTableId} -> associationDefaultRouteTableId) (\s@AwsEc2TransitGatewayDetails' {} a -> s {associationDefaultRouteTableId = a} :: AwsEc2TransitGatewayDetails)

-- | Turn on or turn off DNS support.
awsEc2TransitGatewayDetails_dnsSupport :: Lens.Lens' AwsEc2TransitGatewayDetails (Prelude.Maybe Prelude.Text)
awsEc2TransitGatewayDetails_dnsSupport = Lens.lens (\AwsEc2TransitGatewayDetails' {dnsSupport} -> dnsSupport) (\s@AwsEc2TransitGatewayDetails' {} a -> s {dnsSupport = a} :: AwsEc2TransitGatewayDetails)

-- | Turn on or turn off automatic association with the default association
-- route table.
awsEc2TransitGatewayDetails_defaultRouteTableAssociation :: Lens.Lens' AwsEc2TransitGatewayDetails (Prelude.Maybe Prelude.Text)
awsEc2TransitGatewayDetails_defaultRouteTableAssociation = Lens.lens (\AwsEc2TransitGatewayDetails' {defaultRouteTableAssociation} -> defaultRouteTableAssociation) (\s@AwsEc2TransitGatewayDetails' {} a -> s {defaultRouteTableAssociation = a} :: AwsEc2TransitGatewayDetails)

-- | The ID of the default propagation route table.
awsEc2TransitGatewayDetails_propagationDefaultRouteTableId :: Lens.Lens' AwsEc2TransitGatewayDetails (Prelude.Maybe Prelude.Text)
awsEc2TransitGatewayDetails_propagationDefaultRouteTableId = Lens.lens (\AwsEc2TransitGatewayDetails' {propagationDefaultRouteTableId} -> propagationDefaultRouteTableId) (\s@AwsEc2TransitGatewayDetails' {} a -> s {propagationDefaultRouteTableId = a} :: AwsEc2TransitGatewayDetails)

-- | The ID of the transit gateway.
awsEc2TransitGatewayDetails_id :: Lens.Lens' AwsEc2TransitGatewayDetails (Prelude.Maybe Prelude.Text)
awsEc2TransitGatewayDetails_id = Lens.lens (\AwsEc2TransitGatewayDetails' {id} -> id) (\s@AwsEc2TransitGatewayDetails' {} a -> s {id = a} :: AwsEc2TransitGatewayDetails)

-- | The description of the transit gateway.
awsEc2TransitGatewayDetails_description :: Lens.Lens' AwsEc2TransitGatewayDetails (Prelude.Maybe Prelude.Text)
awsEc2TransitGatewayDetails_description = Lens.lens (\AwsEc2TransitGatewayDetails' {description} -> description) (\s@AwsEc2TransitGatewayDetails' {} a -> s {description = a} :: AwsEc2TransitGatewayDetails)

-- | Turn on or turn off automatic acceptance of attachment requests.
awsEc2TransitGatewayDetails_autoAcceptSharedAttachments :: Lens.Lens' AwsEc2TransitGatewayDetails (Prelude.Maybe Prelude.Text)
awsEc2TransitGatewayDetails_autoAcceptSharedAttachments = Lens.lens (\AwsEc2TransitGatewayDetails' {autoAcceptSharedAttachments} -> autoAcceptSharedAttachments) (\s@AwsEc2TransitGatewayDetails' {} a -> s {autoAcceptSharedAttachments = a} :: AwsEc2TransitGatewayDetails)

-- | Indicates whether multicast is supported on the transit gateway.
awsEc2TransitGatewayDetails_multicastSupport :: Lens.Lens' AwsEc2TransitGatewayDetails (Prelude.Maybe Prelude.Text)
awsEc2TransitGatewayDetails_multicastSupport = Lens.lens (\AwsEc2TransitGatewayDetails' {multicastSupport} -> multicastSupport) (\s@AwsEc2TransitGatewayDetails' {} a -> s {multicastSupport = a} :: AwsEc2TransitGatewayDetails)

-- | A private Autonomous System Number (ASN) for the Amazon side of a BGP
-- session.
awsEc2TransitGatewayDetails_amazonSideAsn :: Lens.Lens' AwsEc2TransitGatewayDetails (Prelude.Maybe Prelude.Int)
awsEc2TransitGatewayDetails_amazonSideAsn = Lens.lens (\AwsEc2TransitGatewayDetails' {amazonSideAsn} -> amazonSideAsn) (\s@AwsEc2TransitGatewayDetails' {} a -> s {amazonSideAsn = a} :: AwsEc2TransitGatewayDetails)

-- | Turn on or turn off Equal Cost Multipath Protocol (ECMP) support.
awsEc2TransitGatewayDetails_vpnEcmpSupport :: Lens.Lens' AwsEc2TransitGatewayDetails (Prelude.Maybe Prelude.Text)
awsEc2TransitGatewayDetails_vpnEcmpSupport = Lens.lens (\AwsEc2TransitGatewayDetails' {vpnEcmpSupport} -> vpnEcmpSupport) (\s@AwsEc2TransitGatewayDetails' {} a -> s {vpnEcmpSupport = a} :: AwsEc2TransitGatewayDetails)

-- | Turn on or turn off automatic propagation of routes to the default
-- propagation route table.
awsEc2TransitGatewayDetails_defaultRouteTablePropagation :: Lens.Lens' AwsEc2TransitGatewayDetails (Prelude.Maybe Prelude.Text)
awsEc2TransitGatewayDetails_defaultRouteTablePropagation = Lens.lens (\AwsEc2TransitGatewayDetails' {defaultRouteTablePropagation} -> defaultRouteTablePropagation) (\s@AwsEc2TransitGatewayDetails' {} a -> s {defaultRouteTablePropagation = a} :: AwsEc2TransitGatewayDetails)

-- | The transit gateway Classless Inter-Domain Routing (CIDR) blocks.
awsEc2TransitGatewayDetails_transitGatewayCidrBlocks :: Lens.Lens' AwsEc2TransitGatewayDetails (Prelude.Maybe [Prelude.Text])
awsEc2TransitGatewayDetails_transitGatewayCidrBlocks = Lens.lens (\AwsEc2TransitGatewayDetails' {transitGatewayCidrBlocks} -> transitGatewayCidrBlocks) (\s@AwsEc2TransitGatewayDetails' {} a -> s {transitGatewayCidrBlocks = a} :: AwsEc2TransitGatewayDetails) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON AwsEc2TransitGatewayDetails where
  parseJSON =
    Core.withObject
      "AwsEc2TransitGatewayDetails"
      ( \x ->
          AwsEc2TransitGatewayDetails'
            Prelude.<$> (x Core..:? "AssociationDefaultRouteTableId")
            Prelude.<*> (x Core..:? "DnsSupport")
            Prelude.<*> (x Core..:? "DefaultRouteTableAssociation")
            Prelude.<*> (x Core..:? "PropagationDefaultRouteTableId")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "AutoAcceptSharedAttachments")
            Prelude.<*> (x Core..:? "MulticastSupport")
            Prelude.<*> (x Core..:? "AmazonSideAsn")
            Prelude.<*> (x Core..:? "VpnEcmpSupport")
            Prelude.<*> (x Core..:? "DefaultRouteTablePropagation")
            Prelude.<*> ( x Core..:? "TransitGatewayCidrBlocks"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AwsEc2TransitGatewayDetails where
  hashWithSalt _salt AwsEc2TransitGatewayDetails' {..} =
    _salt
      `Prelude.hashWithSalt` associationDefaultRouteTableId
      `Prelude.hashWithSalt` dnsSupport
      `Prelude.hashWithSalt` defaultRouteTableAssociation
      `Prelude.hashWithSalt` propagationDefaultRouteTableId
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` autoAcceptSharedAttachments
      `Prelude.hashWithSalt` multicastSupport
      `Prelude.hashWithSalt` amazonSideAsn
      `Prelude.hashWithSalt` vpnEcmpSupport
      `Prelude.hashWithSalt` defaultRouteTablePropagation
      `Prelude.hashWithSalt` transitGatewayCidrBlocks

instance Prelude.NFData AwsEc2TransitGatewayDetails where
  rnf AwsEc2TransitGatewayDetails' {..} =
    Prelude.rnf associationDefaultRouteTableId
      `Prelude.seq` Prelude.rnf dnsSupport
      `Prelude.seq` Prelude.rnf defaultRouteTableAssociation
      `Prelude.seq` Prelude.rnf propagationDefaultRouteTableId
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf autoAcceptSharedAttachments
      `Prelude.seq` Prelude.rnf multicastSupport
      `Prelude.seq` Prelude.rnf amazonSideAsn
      `Prelude.seq` Prelude.rnf vpnEcmpSupport
      `Prelude.seq` Prelude.rnf defaultRouteTablePropagation
      `Prelude.seq` Prelude.rnf transitGatewayCidrBlocks

instance Core.ToJSON AwsEc2TransitGatewayDetails where
  toJSON AwsEc2TransitGatewayDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AssociationDefaultRouteTableId" Core..=)
              Prelude.<$> associationDefaultRouteTableId,
            ("DnsSupport" Core..=) Prelude.<$> dnsSupport,
            ("DefaultRouteTableAssociation" Core..=)
              Prelude.<$> defaultRouteTableAssociation,
            ("PropagationDefaultRouteTableId" Core..=)
              Prelude.<$> propagationDefaultRouteTableId,
            ("Id" Core..=) Prelude.<$> id,
            ("Description" Core..=) Prelude.<$> description,
            ("AutoAcceptSharedAttachments" Core..=)
              Prelude.<$> autoAcceptSharedAttachments,
            ("MulticastSupport" Core..=)
              Prelude.<$> multicastSupport,
            ("AmazonSideAsn" Core..=) Prelude.<$> amazonSideAsn,
            ("VpnEcmpSupport" Core..=)
              Prelude.<$> vpnEcmpSupport,
            ("DefaultRouteTablePropagation" Core..=)
              Prelude.<$> defaultRouteTablePropagation,
            ("TransitGatewayCidrBlocks" Core..=)
              Prelude.<$> transitGatewayCidrBlocks
          ]
      )
