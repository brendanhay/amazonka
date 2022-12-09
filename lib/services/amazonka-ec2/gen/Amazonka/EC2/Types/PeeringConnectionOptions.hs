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
-- Module      : Amazonka.EC2.Types.PeeringConnectionOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.PeeringConnectionOptions where

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
-- /See:/ 'newPeeringConnectionOptions' smart constructor.
data PeeringConnectionOptions = PeeringConnectionOptions'
  { -- | If true, the public DNS hostnames of instances in the specified VPC
    -- resolve to private IP addresses when queried from instances in the peer
    -- VPC.
    allowDnsResolutionFromRemoteVpc :: Prelude.Maybe Prelude.Bool,
    -- | If true, enables outbound communication from an EC2-Classic instance
    -- that\'s linked to a local VPC using ClassicLink to instances in a peer
    -- VPC.
    allowEgressFromLocalClassicLinkToRemoteVpc :: Prelude.Maybe Prelude.Bool,
    -- | If true, enables outbound communication from instances in a local VPC to
    -- an EC2-Classic instance that\'s linked to a peer VPC using ClassicLink.
    allowEgressFromLocalVpcToRemoteClassicLink :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PeeringConnectionOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowDnsResolutionFromRemoteVpc', 'peeringConnectionOptions_allowDnsResolutionFromRemoteVpc' - If true, the public DNS hostnames of instances in the specified VPC
-- resolve to private IP addresses when queried from instances in the peer
-- VPC.
--
-- 'allowEgressFromLocalClassicLinkToRemoteVpc', 'peeringConnectionOptions_allowEgressFromLocalClassicLinkToRemoteVpc' - If true, enables outbound communication from an EC2-Classic instance
-- that\'s linked to a local VPC using ClassicLink to instances in a peer
-- VPC.
--
-- 'allowEgressFromLocalVpcToRemoteClassicLink', 'peeringConnectionOptions_allowEgressFromLocalVpcToRemoteClassicLink' - If true, enables outbound communication from instances in a local VPC to
-- an EC2-Classic instance that\'s linked to a peer VPC using ClassicLink.
newPeeringConnectionOptions ::
  PeeringConnectionOptions
newPeeringConnectionOptions =
  PeeringConnectionOptions'
    { allowDnsResolutionFromRemoteVpc =
        Prelude.Nothing,
      allowEgressFromLocalClassicLinkToRemoteVpc =
        Prelude.Nothing,
      allowEgressFromLocalVpcToRemoteClassicLink =
        Prelude.Nothing
    }

-- | If true, the public DNS hostnames of instances in the specified VPC
-- resolve to private IP addresses when queried from instances in the peer
-- VPC.
peeringConnectionOptions_allowDnsResolutionFromRemoteVpc :: Lens.Lens' PeeringConnectionOptions (Prelude.Maybe Prelude.Bool)
peeringConnectionOptions_allowDnsResolutionFromRemoteVpc = Lens.lens (\PeeringConnectionOptions' {allowDnsResolutionFromRemoteVpc} -> allowDnsResolutionFromRemoteVpc) (\s@PeeringConnectionOptions' {} a -> s {allowDnsResolutionFromRemoteVpc = a} :: PeeringConnectionOptions)

-- | If true, enables outbound communication from an EC2-Classic instance
-- that\'s linked to a local VPC using ClassicLink to instances in a peer
-- VPC.
peeringConnectionOptions_allowEgressFromLocalClassicLinkToRemoteVpc :: Lens.Lens' PeeringConnectionOptions (Prelude.Maybe Prelude.Bool)
peeringConnectionOptions_allowEgressFromLocalClassicLinkToRemoteVpc = Lens.lens (\PeeringConnectionOptions' {allowEgressFromLocalClassicLinkToRemoteVpc} -> allowEgressFromLocalClassicLinkToRemoteVpc) (\s@PeeringConnectionOptions' {} a -> s {allowEgressFromLocalClassicLinkToRemoteVpc = a} :: PeeringConnectionOptions)

-- | If true, enables outbound communication from instances in a local VPC to
-- an EC2-Classic instance that\'s linked to a peer VPC using ClassicLink.
peeringConnectionOptions_allowEgressFromLocalVpcToRemoteClassicLink :: Lens.Lens' PeeringConnectionOptions (Prelude.Maybe Prelude.Bool)
peeringConnectionOptions_allowEgressFromLocalVpcToRemoteClassicLink = Lens.lens (\PeeringConnectionOptions' {allowEgressFromLocalVpcToRemoteClassicLink} -> allowEgressFromLocalVpcToRemoteClassicLink) (\s@PeeringConnectionOptions' {} a -> s {allowEgressFromLocalVpcToRemoteClassicLink = a} :: PeeringConnectionOptions)

instance Data.FromXML PeeringConnectionOptions where
  parseXML x =
    PeeringConnectionOptions'
      Prelude.<$> (x Data..@? "allowDnsResolutionFromRemoteVpc")
      Prelude.<*> ( x
                      Data..@? "allowEgressFromLocalClassicLinkToRemoteVpc"
                  )
      Prelude.<*> ( x
                      Data..@? "allowEgressFromLocalVpcToRemoteClassicLink"
                  )

instance Prelude.Hashable PeeringConnectionOptions where
  hashWithSalt _salt PeeringConnectionOptions' {..} =
    _salt
      `Prelude.hashWithSalt` allowDnsResolutionFromRemoteVpc
      `Prelude.hashWithSalt` allowEgressFromLocalClassicLinkToRemoteVpc
      `Prelude.hashWithSalt` allowEgressFromLocalVpcToRemoteClassicLink

instance Prelude.NFData PeeringConnectionOptions where
  rnf PeeringConnectionOptions' {..} =
    Prelude.rnf allowDnsResolutionFromRemoteVpc
      `Prelude.seq` Prelude.rnf
        allowEgressFromLocalClassicLinkToRemoteVpc
      `Prelude.seq` Prelude.rnf
        allowEgressFromLocalVpcToRemoteClassicLink
