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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2VpcDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2VpcDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.CidrBlockAssociation
import Amazonka.SecurityHub.Types.Ipv6CidrBlockAssociation

-- | Details about an EC2 VPC.
--
-- /See:/ 'newAwsEc2VpcDetails' smart constructor.
data AwsEc2VpcDetails = AwsEc2VpcDetails'
  { -- | The current state of the VPC. Valid values are @available@ or @pending@.
    state :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the set of Dynamic Host Configuration Protocol (DHCP)
    -- options that are associated with the VPC. If the default options are
    -- associated with the VPC, then this is default.
    dhcpOptionsId :: Prelude.Maybe Prelude.Text,
    -- | Information about the IPv6 CIDR blocks associated with the VPC.
    ipv6CidrBlockAssociationSet :: Prelude.Maybe [Ipv6CidrBlockAssociation],
    -- | Information about the IPv4 CIDR blocks associated with the VPC.
    cidrBlockAssociationSet :: Prelude.Maybe [CidrBlockAssociation]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2VpcDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'awsEc2VpcDetails_state' - The current state of the VPC. Valid values are @available@ or @pending@.
--
-- 'dhcpOptionsId', 'awsEc2VpcDetails_dhcpOptionsId' - The identifier of the set of Dynamic Host Configuration Protocol (DHCP)
-- options that are associated with the VPC. If the default options are
-- associated with the VPC, then this is default.
--
-- 'ipv6CidrBlockAssociationSet', 'awsEc2VpcDetails_ipv6CidrBlockAssociationSet' - Information about the IPv6 CIDR blocks associated with the VPC.
--
-- 'cidrBlockAssociationSet', 'awsEc2VpcDetails_cidrBlockAssociationSet' - Information about the IPv4 CIDR blocks associated with the VPC.
newAwsEc2VpcDetails ::
  AwsEc2VpcDetails
newAwsEc2VpcDetails =
  AwsEc2VpcDetails'
    { state = Prelude.Nothing,
      dhcpOptionsId = Prelude.Nothing,
      ipv6CidrBlockAssociationSet = Prelude.Nothing,
      cidrBlockAssociationSet = Prelude.Nothing
    }

-- | The current state of the VPC. Valid values are @available@ or @pending@.
awsEc2VpcDetails_state :: Lens.Lens' AwsEc2VpcDetails (Prelude.Maybe Prelude.Text)
awsEc2VpcDetails_state = Lens.lens (\AwsEc2VpcDetails' {state} -> state) (\s@AwsEc2VpcDetails' {} a -> s {state = a} :: AwsEc2VpcDetails)

-- | The identifier of the set of Dynamic Host Configuration Protocol (DHCP)
-- options that are associated with the VPC. If the default options are
-- associated with the VPC, then this is default.
awsEc2VpcDetails_dhcpOptionsId :: Lens.Lens' AwsEc2VpcDetails (Prelude.Maybe Prelude.Text)
awsEc2VpcDetails_dhcpOptionsId = Lens.lens (\AwsEc2VpcDetails' {dhcpOptionsId} -> dhcpOptionsId) (\s@AwsEc2VpcDetails' {} a -> s {dhcpOptionsId = a} :: AwsEc2VpcDetails)

-- | Information about the IPv6 CIDR blocks associated with the VPC.
awsEc2VpcDetails_ipv6CidrBlockAssociationSet :: Lens.Lens' AwsEc2VpcDetails (Prelude.Maybe [Ipv6CidrBlockAssociation])
awsEc2VpcDetails_ipv6CidrBlockAssociationSet = Lens.lens (\AwsEc2VpcDetails' {ipv6CidrBlockAssociationSet} -> ipv6CidrBlockAssociationSet) (\s@AwsEc2VpcDetails' {} a -> s {ipv6CidrBlockAssociationSet = a} :: AwsEc2VpcDetails) Prelude.. Lens.mapping Lens.coerced

-- | Information about the IPv4 CIDR blocks associated with the VPC.
awsEc2VpcDetails_cidrBlockAssociationSet :: Lens.Lens' AwsEc2VpcDetails (Prelude.Maybe [CidrBlockAssociation])
awsEc2VpcDetails_cidrBlockAssociationSet = Lens.lens (\AwsEc2VpcDetails' {cidrBlockAssociationSet} -> cidrBlockAssociationSet) (\s@AwsEc2VpcDetails' {} a -> s {cidrBlockAssociationSet = a} :: AwsEc2VpcDetails) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON AwsEc2VpcDetails where
  parseJSON =
    Core.withObject
      "AwsEc2VpcDetails"
      ( \x ->
          AwsEc2VpcDetails'
            Prelude.<$> (x Core..:? "State")
            Prelude.<*> (x Core..:? "DhcpOptionsId")
            Prelude.<*> ( x Core..:? "Ipv6CidrBlockAssociationSet"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "CidrBlockAssociationSet"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AwsEc2VpcDetails where
  hashWithSalt _salt AwsEc2VpcDetails' {..} =
    _salt `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` dhcpOptionsId
      `Prelude.hashWithSalt` ipv6CidrBlockAssociationSet
      `Prelude.hashWithSalt` cidrBlockAssociationSet

instance Prelude.NFData AwsEc2VpcDetails where
  rnf AwsEc2VpcDetails' {..} =
    Prelude.rnf state
      `Prelude.seq` Prelude.rnf dhcpOptionsId
      `Prelude.seq` Prelude.rnf ipv6CidrBlockAssociationSet
      `Prelude.seq` Prelude.rnf cidrBlockAssociationSet

instance Core.ToJSON AwsEc2VpcDetails where
  toJSON AwsEc2VpcDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("State" Core..=) Prelude.<$> state,
            ("DhcpOptionsId" Core..=) Prelude.<$> dhcpOptionsId,
            ("Ipv6CidrBlockAssociationSet" Core..=)
              Prelude.<$> ipv6CidrBlockAssociationSet,
            ("CidrBlockAssociationSet" Core..=)
              Prelude.<$> cidrBlockAssociationSet
          ]
      )
