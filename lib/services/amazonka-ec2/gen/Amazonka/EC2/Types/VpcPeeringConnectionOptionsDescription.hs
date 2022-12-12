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
-- Module      : Amazonka.EC2.Types.VpcPeeringConnectionOptionsDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VpcPeeringConnectionOptionsDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | We are retiring EC2-Classic. We recommend that you migrate from
-- EC2-Classic to a VPC. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-migrate.html Migrate from EC2-Classic to a VPC>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- Describes the VPC peering connection options.
--
-- /See:/ 'newVpcPeeringConnectionOptionsDescription' smart constructor.
data VpcPeeringConnectionOptionsDescription = VpcPeeringConnectionOptionsDescription'
  { -- | Indicates whether a local VPC can resolve public DNS hostnames to
    -- private IP addresses when queried from instances in a peer VPC.
    allowDnsResolutionFromRemoteVpc :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether a local ClassicLink connection can communicate with
    -- the peer VPC over the VPC peering connection.
    allowEgressFromLocalClassicLinkToRemoteVpc :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether a local VPC can communicate with a ClassicLink
    -- connection in the peer VPC over the VPC peering connection.
    allowEgressFromLocalVpcToRemoteClassicLink :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcPeeringConnectionOptionsDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowDnsResolutionFromRemoteVpc', 'vpcPeeringConnectionOptionsDescription_allowDnsResolutionFromRemoteVpc' - Indicates whether a local VPC can resolve public DNS hostnames to
-- private IP addresses when queried from instances in a peer VPC.
--
-- 'allowEgressFromLocalClassicLinkToRemoteVpc', 'vpcPeeringConnectionOptionsDescription_allowEgressFromLocalClassicLinkToRemoteVpc' - Indicates whether a local ClassicLink connection can communicate with
-- the peer VPC over the VPC peering connection.
--
-- 'allowEgressFromLocalVpcToRemoteClassicLink', 'vpcPeeringConnectionOptionsDescription_allowEgressFromLocalVpcToRemoteClassicLink' - Indicates whether a local VPC can communicate with a ClassicLink
-- connection in the peer VPC over the VPC peering connection.
newVpcPeeringConnectionOptionsDescription ::
  VpcPeeringConnectionOptionsDescription
newVpcPeeringConnectionOptionsDescription =
  VpcPeeringConnectionOptionsDescription'
    { allowDnsResolutionFromRemoteVpc =
        Prelude.Nothing,
      allowEgressFromLocalClassicLinkToRemoteVpc =
        Prelude.Nothing,
      allowEgressFromLocalVpcToRemoteClassicLink =
        Prelude.Nothing
    }

-- | Indicates whether a local VPC can resolve public DNS hostnames to
-- private IP addresses when queried from instances in a peer VPC.
vpcPeeringConnectionOptionsDescription_allowDnsResolutionFromRemoteVpc :: Lens.Lens' VpcPeeringConnectionOptionsDescription (Prelude.Maybe Prelude.Bool)
vpcPeeringConnectionOptionsDescription_allowDnsResolutionFromRemoteVpc = Lens.lens (\VpcPeeringConnectionOptionsDescription' {allowDnsResolutionFromRemoteVpc} -> allowDnsResolutionFromRemoteVpc) (\s@VpcPeeringConnectionOptionsDescription' {} a -> s {allowDnsResolutionFromRemoteVpc = a} :: VpcPeeringConnectionOptionsDescription)

-- | Indicates whether a local ClassicLink connection can communicate with
-- the peer VPC over the VPC peering connection.
vpcPeeringConnectionOptionsDescription_allowEgressFromLocalClassicLinkToRemoteVpc :: Lens.Lens' VpcPeeringConnectionOptionsDescription (Prelude.Maybe Prelude.Bool)
vpcPeeringConnectionOptionsDescription_allowEgressFromLocalClassicLinkToRemoteVpc = Lens.lens (\VpcPeeringConnectionOptionsDescription' {allowEgressFromLocalClassicLinkToRemoteVpc} -> allowEgressFromLocalClassicLinkToRemoteVpc) (\s@VpcPeeringConnectionOptionsDescription' {} a -> s {allowEgressFromLocalClassicLinkToRemoteVpc = a} :: VpcPeeringConnectionOptionsDescription)

-- | Indicates whether a local VPC can communicate with a ClassicLink
-- connection in the peer VPC over the VPC peering connection.
vpcPeeringConnectionOptionsDescription_allowEgressFromLocalVpcToRemoteClassicLink :: Lens.Lens' VpcPeeringConnectionOptionsDescription (Prelude.Maybe Prelude.Bool)
vpcPeeringConnectionOptionsDescription_allowEgressFromLocalVpcToRemoteClassicLink = Lens.lens (\VpcPeeringConnectionOptionsDescription' {allowEgressFromLocalVpcToRemoteClassicLink} -> allowEgressFromLocalVpcToRemoteClassicLink) (\s@VpcPeeringConnectionOptionsDescription' {} a -> s {allowEgressFromLocalVpcToRemoteClassicLink = a} :: VpcPeeringConnectionOptionsDescription)

instance
  Data.FromXML
    VpcPeeringConnectionOptionsDescription
  where
  parseXML x =
    VpcPeeringConnectionOptionsDescription'
      Prelude.<$> (x Data..@? "allowDnsResolutionFromRemoteVpc")
      Prelude.<*> ( x
                      Data..@? "allowEgressFromLocalClassicLinkToRemoteVpc"
                  )
      Prelude.<*> ( x
                      Data..@? "allowEgressFromLocalVpcToRemoteClassicLink"
                  )

instance
  Prelude.Hashable
    VpcPeeringConnectionOptionsDescription
  where
  hashWithSalt
    _salt
    VpcPeeringConnectionOptionsDescription' {..} =
      _salt
        `Prelude.hashWithSalt` allowDnsResolutionFromRemoteVpc
        `Prelude.hashWithSalt` allowEgressFromLocalClassicLinkToRemoteVpc
        `Prelude.hashWithSalt` allowEgressFromLocalVpcToRemoteClassicLink

instance
  Prelude.NFData
    VpcPeeringConnectionOptionsDescription
  where
  rnf VpcPeeringConnectionOptionsDescription' {..} =
    Prelude.rnf allowDnsResolutionFromRemoteVpc
      `Prelude.seq` Prelude.rnf
        allowEgressFromLocalClassicLinkToRemoteVpc
      `Prelude.seq` Prelude.rnf
        allowEgressFromLocalVpcToRemoteClassicLink
