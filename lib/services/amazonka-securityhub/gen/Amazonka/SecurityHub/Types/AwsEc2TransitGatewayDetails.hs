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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2TransitGatewayDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an Amazon Web Services Amazon EC2 Transit Gateway that
-- interconnects virtual private clouds (VPCs) and on-premises networks.
--
-- /See:/ 'newAwsEc2TransitGatewayDetails' smart constructor.
data AwsEc2TransitGatewayDetails = AwsEc2TransitGatewayDetails'
  { -- | A private Autonomous System Number (ASN) for the Amazon side of a BGP
    -- session.
    amazonSideAsn :: Prelude.Maybe Prelude.Int,
    -- | The ID of the default association route table.
    associationDefaultRouteTableId :: Prelude.Maybe Prelude.Text,
    -- | Turn on or turn off automatic acceptance of attachment requests.
    autoAcceptSharedAttachments :: Prelude.Maybe Prelude.Text,
    -- | Turn on or turn off automatic association with the default association
    -- route table.
    defaultRouteTableAssociation :: Prelude.Maybe Prelude.Text,
    -- | Turn on or turn off automatic propagation of routes to the default
    -- propagation route table.
    defaultRouteTablePropagation :: Prelude.Maybe Prelude.Text,
    -- | The description of the transit gateway.
    description :: Prelude.Maybe Prelude.Text,
    -- | Turn on or turn off DNS support.
    dnsSupport :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway.
    id :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether multicast is supported on the transit gateway.
    multicastSupport :: Prelude.Maybe Prelude.Text,
    -- | The ID of the default propagation route table.
    propagationDefaultRouteTableId :: Prelude.Maybe Prelude.Text,
    -- | The transit gateway Classless Inter-Domain Routing (CIDR) blocks.
    transitGatewayCidrBlocks :: Prelude.Maybe [Prelude.Text],
    -- | Turn on or turn off Equal Cost Multipath Protocol (ECMP) support.
    vpnEcmpSupport :: Prelude.Maybe Prelude.Text
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
-- 'amazonSideAsn', 'awsEc2TransitGatewayDetails_amazonSideAsn' - A private Autonomous System Number (ASN) for the Amazon side of a BGP
-- session.
--
-- 'associationDefaultRouteTableId', 'awsEc2TransitGatewayDetails_associationDefaultRouteTableId' - The ID of the default association route table.
--
-- 'autoAcceptSharedAttachments', 'awsEc2TransitGatewayDetails_autoAcceptSharedAttachments' - Turn on or turn off automatic acceptance of attachment requests.
--
-- 'defaultRouteTableAssociation', 'awsEc2TransitGatewayDetails_defaultRouteTableAssociation' - Turn on or turn off automatic association with the default association
-- route table.
--
-- 'defaultRouteTablePropagation', 'awsEc2TransitGatewayDetails_defaultRouteTablePropagation' - Turn on or turn off automatic propagation of routes to the default
-- propagation route table.
--
-- 'description', 'awsEc2TransitGatewayDetails_description' - The description of the transit gateway.
--
-- 'dnsSupport', 'awsEc2TransitGatewayDetails_dnsSupport' - Turn on or turn off DNS support.
--
-- 'id', 'awsEc2TransitGatewayDetails_id' - The ID of the transit gateway.
--
-- 'multicastSupport', 'awsEc2TransitGatewayDetails_multicastSupport' - Indicates whether multicast is supported on the transit gateway.
--
-- 'propagationDefaultRouteTableId', 'awsEc2TransitGatewayDetails_propagationDefaultRouteTableId' - The ID of the default propagation route table.
--
-- 'transitGatewayCidrBlocks', 'awsEc2TransitGatewayDetails_transitGatewayCidrBlocks' - The transit gateway Classless Inter-Domain Routing (CIDR) blocks.
--
-- 'vpnEcmpSupport', 'awsEc2TransitGatewayDetails_vpnEcmpSupport' - Turn on or turn off Equal Cost Multipath Protocol (ECMP) support.
newAwsEc2TransitGatewayDetails ::
  AwsEc2TransitGatewayDetails
newAwsEc2TransitGatewayDetails =
  AwsEc2TransitGatewayDetails'
    { amazonSideAsn =
        Prelude.Nothing,
      associationDefaultRouteTableId =
        Prelude.Nothing,
      autoAcceptSharedAttachments = Prelude.Nothing,
      defaultRouteTableAssociation = Prelude.Nothing,
      defaultRouteTablePropagation = Prelude.Nothing,
      description = Prelude.Nothing,
      dnsSupport = Prelude.Nothing,
      id = Prelude.Nothing,
      multicastSupport = Prelude.Nothing,
      propagationDefaultRouteTableId =
        Prelude.Nothing,
      transitGatewayCidrBlocks = Prelude.Nothing,
      vpnEcmpSupport = Prelude.Nothing
    }

-- | A private Autonomous System Number (ASN) for the Amazon side of a BGP
-- session.
awsEc2TransitGatewayDetails_amazonSideAsn :: Lens.Lens' AwsEc2TransitGatewayDetails (Prelude.Maybe Prelude.Int)
awsEc2TransitGatewayDetails_amazonSideAsn = Lens.lens (\AwsEc2TransitGatewayDetails' {amazonSideAsn} -> amazonSideAsn) (\s@AwsEc2TransitGatewayDetails' {} a -> s {amazonSideAsn = a} :: AwsEc2TransitGatewayDetails)

-- | The ID of the default association route table.
awsEc2TransitGatewayDetails_associationDefaultRouteTableId :: Lens.Lens' AwsEc2TransitGatewayDetails (Prelude.Maybe Prelude.Text)
awsEc2TransitGatewayDetails_associationDefaultRouteTableId = Lens.lens (\AwsEc2TransitGatewayDetails' {associationDefaultRouteTableId} -> associationDefaultRouteTableId) (\s@AwsEc2TransitGatewayDetails' {} a -> s {associationDefaultRouteTableId = a} :: AwsEc2TransitGatewayDetails)

-- | Turn on or turn off automatic acceptance of attachment requests.
awsEc2TransitGatewayDetails_autoAcceptSharedAttachments :: Lens.Lens' AwsEc2TransitGatewayDetails (Prelude.Maybe Prelude.Text)
awsEc2TransitGatewayDetails_autoAcceptSharedAttachments = Lens.lens (\AwsEc2TransitGatewayDetails' {autoAcceptSharedAttachments} -> autoAcceptSharedAttachments) (\s@AwsEc2TransitGatewayDetails' {} a -> s {autoAcceptSharedAttachments = a} :: AwsEc2TransitGatewayDetails)

-- | Turn on or turn off automatic association with the default association
-- route table.
awsEc2TransitGatewayDetails_defaultRouteTableAssociation :: Lens.Lens' AwsEc2TransitGatewayDetails (Prelude.Maybe Prelude.Text)
awsEc2TransitGatewayDetails_defaultRouteTableAssociation = Lens.lens (\AwsEc2TransitGatewayDetails' {defaultRouteTableAssociation} -> defaultRouteTableAssociation) (\s@AwsEc2TransitGatewayDetails' {} a -> s {defaultRouteTableAssociation = a} :: AwsEc2TransitGatewayDetails)

-- | Turn on or turn off automatic propagation of routes to the default
-- propagation route table.
awsEc2TransitGatewayDetails_defaultRouteTablePropagation :: Lens.Lens' AwsEc2TransitGatewayDetails (Prelude.Maybe Prelude.Text)
awsEc2TransitGatewayDetails_defaultRouteTablePropagation = Lens.lens (\AwsEc2TransitGatewayDetails' {defaultRouteTablePropagation} -> defaultRouteTablePropagation) (\s@AwsEc2TransitGatewayDetails' {} a -> s {defaultRouteTablePropagation = a} :: AwsEc2TransitGatewayDetails)

-- | The description of the transit gateway.
awsEc2TransitGatewayDetails_description :: Lens.Lens' AwsEc2TransitGatewayDetails (Prelude.Maybe Prelude.Text)
awsEc2TransitGatewayDetails_description = Lens.lens (\AwsEc2TransitGatewayDetails' {description} -> description) (\s@AwsEc2TransitGatewayDetails' {} a -> s {description = a} :: AwsEc2TransitGatewayDetails)

-- | Turn on or turn off DNS support.
awsEc2TransitGatewayDetails_dnsSupport :: Lens.Lens' AwsEc2TransitGatewayDetails (Prelude.Maybe Prelude.Text)
awsEc2TransitGatewayDetails_dnsSupport = Lens.lens (\AwsEc2TransitGatewayDetails' {dnsSupport} -> dnsSupport) (\s@AwsEc2TransitGatewayDetails' {} a -> s {dnsSupport = a} :: AwsEc2TransitGatewayDetails)

-- | The ID of the transit gateway.
awsEc2TransitGatewayDetails_id :: Lens.Lens' AwsEc2TransitGatewayDetails (Prelude.Maybe Prelude.Text)
awsEc2TransitGatewayDetails_id = Lens.lens (\AwsEc2TransitGatewayDetails' {id} -> id) (\s@AwsEc2TransitGatewayDetails' {} a -> s {id = a} :: AwsEc2TransitGatewayDetails)

-- | Indicates whether multicast is supported on the transit gateway.
awsEc2TransitGatewayDetails_multicastSupport :: Lens.Lens' AwsEc2TransitGatewayDetails (Prelude.Maybe Prelude.Text)
awsEc2TransitGatewayDetails_multicastSupport = Lens.lens (\AwsEc2TransitGatewayDetails' {multicastSupport} -> multicastSupport) (\s@AwsEc2TransitGatewayDetails' {} a -> s {multicastSupport = a} :: AwsEc2TransitGatewayDetails)

-- | The ID of the default propagation route table.
awsEc2TransitGatewayDetails_propagationDefaultRouteTableId :: Lens.Lens' AwsEc2TransitGatewayDetails (Prelude.Maybe Prelude.Text)
awsEc2TransitGatewayDetails_propagationDefaultRouteTableId = Lens.lens (\AwsEc2TransitGatewayDetails' {propagationDefaultRouteTableId} -> propagationDefaultRouteTableId) (\s@AwsEc2TransitGatewayDetails' {} a -> s {propagationDefaultRouteTableId = a} :: AwsEc2TransitGatewayDetails)

-- | The transit gateway Classless Inter-Domain Routing (CIDR) blocks.
awsEc2TransitGatewayDetails_transitGatewayCidrBlocks :: Lens.Lens' AwsEc2TransitGatewayDetails (Prelude.Maybe [Prelude.Text])
awsEc2TransitGatewayDetails_transitGatewayCidrBlocks = Lens.lens (\AwsEc2TransitGatewayDetails' {transitGatewayCidrBlocks} -> transitGatewayCidrBlocks) (\s@AwsEc2TransitGatewayDetails' {} a -> s {transitGatewayCidrBlocks = a} :: AwsEc2TransitGatewayDetails) Prelude.. Lens.mapping Lens.coerced

-- | Turn on or turn off Equal Cost Multipath Protocol (ECMP) support.
awsEc2TransitGatewayDetails_vpnEcmpSupport :: Lens.Lens' AwsEc2TransitGatewayDetails (Prelude.Maybe Prelude.Text)
awsEc2TransitGatewayDetails_vpnEcmpSupport = Lens.lens (\AwsEc2TransitGatewayDetails' {vpnEcmpSupport} -> vpnEcmpSupport) (\s@AwsEc2TransitGatewayDetails' {} a -> s {vpnEcmpSupport = a} :: AwsEc2TransitGatewayDetails)

instance Data.FromJSON AwsEc2TransitGatewayDetails where
  parseJSON =
    Data.withObject
      "AwsEc2TransitGatewayDetails"
      ( \x ->
          AwsEc2TransitGatewayDetails'
            Prelude.<$> (x Data..:? "AmazonSideAsn")
            Prelude.<*> (x Data..:? "AssociationDefaultRouteTableId")
            Prelude.<*> (x Data..:? "AutoAcceptSharedAttachments")
            Prelude.<*> (x Data..:? "DefaultRouteTableAssociation")
            Prelude.<*> (x Data..:? "DefaultRouteTablePropagation")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "DnsSupport")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "MulticastSupport")
            Prelude.<*> (x Data..:? "PropagationDefaultRouteTableId")
            Prelude.<*> ( x Data..:? "TransitGatewayCidrBlocks"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "VpnEcmpSupport")
      )

instance Prelude.Hashable AwsEc2TransitGatewayDetails where
  hashWithSalt _salt AwsEc2TransitGatewayDetails' {..} =
    _salt `Prelude.hashWithSalt` amazonSideAsn
      `Prelude.hashWithSalt` associationDefaultRouteTableId
      `Prelude.hashWithSalt` autoAcceptSharedAttachments
      `Prelude.hashWithSalt` defaultRouteTableAssociation
      `Prelude.hashWithSalt` defaultRouteTablePropagation
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dnsSupport
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` multicastSupport
      `Prelude.hashWithSalt` propagationDefaultRouteTableId
      `Prelude.hashWithSalt` transitGatewayCidrBlocks
      `Prelude.hashWithSalt` vpnEcmpSupport

instance Prelude.NFData AwsEc2TransitGatewayDetails where
  rnf AwsEc2TransitGatewayDetails' {..} =
    Prelude.rnf amazonSideAsn
      `Prelude.seq` Prelude.rnf associationDefaultRouteTableId
      `Prelude.seq` Prelude.rnf autoAcceptSharedAttachments
      `Prelude.seq` Prelude.rnf defaultRouteTableAssociation
      `Prelude.seq` Prelude.rnf defaultRouteTablePropagation
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf dnsSupport
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf multicastSupport
      `Prelude.seq` Prelude.rnf propagationDefaultRouteTableId
      `Prelude.seq` Prelude.rnf transitGatewayCidrBlocks
      `Prelude.seq` Prelude.rnf vpnEcmpSupport

instance Data.ToJSON AwsEc2TransitGatewayDetails where
  toJSON AwsEc2TransitGatewayDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AmazonSideAsn" Data..=) Prelude.<$> amazonSideAsn,
            ("AssociationDefaultRouteTableId" Data..=)
              Prelude.<$> associationDefaultRouteTableId,
            ("AutoAcceptSharedAttachments" Data..=)
              Prelude.<$> autoAcceptSharedAttachments,
            ("DefaultRouteTableAssociation" Data..=)
              Prelude.<$> defaultRouteTableAssociation,
            ("DefaultRouteTablePropagation" Data..=)
              Prelude.<$> defaultRouteTablePropagation,
            ("Description" Data..=) Prelude.<$> description,
            ("DnsSupport" Data..=) Prelude.<$> dnsSupport,
            ("Id" Data..=) Prelude.<$> id,
            ("MulticastSupport" Data..=)
              Prelude.<$> multicastSupport,
            ("PropagationDefaultRouteTableId" Data..=)
              Prelude.<$> propagationDefaultRouteTableId,
            ("TransitGatewayCidrBlocks" Data..=)
              Prelude.<$> transitGatewayCidrBlocks,
            ("VpnEcmpSupport" Data..=)
              Prelude.<$> vpnEcmpSupport
          ]
      )
