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
-- Module      : Network.AWS.EC2.Types.PeeringConnectionOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PeeringConnectionOptions where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the VPC peering connection options.
--
-- /See:/ 'newPeeringConnectionOptions' smart constructor.
data PeeringConnectionOptions = PeeringConnectionOptions'
  { -- | If true, the public DNS hostnames of instances in the specified VPC
    -- resolve to private IP addresses when queried from instances in the peer
    -- VPC.
    allowDnsResolutionFromRemoteVpc :: Prelude.Maybe Prelude.Bool,
    -- | If true, enables outbound communication from instances in a local VPC to
    -- an EC2-Classic instance that\'s linked to a peer VPC using ClassicLink.
    allowEgressFromLocalVpcToRemoteClassicLink :: Prelude.Maybe Prelude.Bool,
    -- | If true, enables outbound communication from an EC2-Classic instance
    -- that\'s linked to a local VPC using ClassicLink to instances in a peer
    -- VPC.
    allowEgressFromLocalClassicLinkToRemoteVpc :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'allowEgressFromLocalVpcToRemoteClassicLink', 'peeringConnectionOptions_allowEgressFromLocalVpcToRemoteClassicLink' - If true, enables outbound communication from instances in a local VPC to
-- an EC2-Classic instance that\'s linked to a peer VPC using ClassicLink.
--
-- 'allowEgressFromLocalClassicLinkToRemoteVpc', 'peeringConnectionOptions_allowEgressFromLocalClassicLinkToRemoteVpc' - If true, enables outbound communication from an EC2-Classic instance
-- that\'s linked to a local VPC using ClassicLink to instances in a peer
-- VPC.
newPeeringConnectionOptions ::
  PeeringConnectionOptions
newPeeringConnectionOptions =
  PeeringConnectionOptions'
    { allowDnsResolutionFromRemoteVpc =
        Prelude.Nothing,
      allowEgressFromLocalVpcToRemoteClassicLink =
        Prelude.Nothing,
      allowEgressFromLocalClassicLinkToRemoteVpc =
        Prelude.Nothing
    }

-- | If true, the public DNS hostnames of instances in the specified VPC
-- resolve to private IP addresses when queried from instances in the peer
-- VPC.
peeringConnectionOptions_allowDnsResolutionFromRemoteVpc :: Lens.Lens' PeeringConnectionOptions (Prelude.Maybe Prelude.Bool)
peeringConnectionOptions_allowDnsResolutionFromRemoteVpc = Lens.lens (\PeeringConnectionOptions' {allowDnsResolutionFromRemoteVpc} -> allowDnsResolutionFromRemoteVpc) (\s@PeeringConnectionOptions' {} a -> s {allowDnsResolutionFromRemoteVpc = a} :: PeeringConnectionOptions)

-- | If true, enables outbound communication from instances in a local VPC to
-- an EC2-Classic instance that\'s linked to a peer VPC using ClassicLink.
peeringConnectionOptions_allowEgressFromLocalVpcToRemoteClassicLink :: Lens.Lens' PeeringConnectionOptions (Prelude.Maybe Prelude.Bool)
peeringConnectionOptions_allowEgressFromLocalVpcToRemoteClassicLink = Lens.lens (\PeeringConnectionOptions' {allowEgressFromLocalVpcToRemoteClassicLink} -> allowEgressFromLocalVpcToRemoteClassicLink) (\s@PeeringConnectionOptions' {} a -> s {allowEgressFromLocalVpcToRemoteClassicLink = a} :: PeeringConnectionOptions)

-- | If true, enables outbound communication from an EC2-Classic instance
-- that\'s linked to a local VPC using ClassicLink to instances in a peer
-- VPC.
peeringConnectionOptions_allowEgressFromLocalClassicLinkToRemoteVpc :: Lens.Lens' PeeringConnectionOptions (Prelude.Maybe Prelude.Bool)
peeringConnectionOptions_allowEgressFromLocalClassicLinkToRemoteVpc = Lens.lens (\PeeringConnectionOptions' {allowEgressFromLocalClassicLinkToRemoteVpc} -> allowEgressFromLocalClassicLinkToRemoteVpc) (\s@PeeringConnectionOptions' {} a -> s {allowEgressFromLocalClassicLinkToRemoteVpc = a} :: PeeringConnectionOptions)

instance Prelude.FromXML PeeringConnectionOptions where
  parseXML x =
    PeeringConnectionOptions'
      Prelude.<$> (x Prelude..@? "allowDnsResolutionFromRemoteVpc")
      Prelude.<*> ( x
                      Prelude..@? "allowEgressFromLocalVpcToRemoteClassicLink"
                  )
      Prelude.<*> ( x
                      Prelude..@? "allowEgressFromLocalClassicLinkToRemoteVpc"
                  )

instance Prelude.Hashable PeeringConnectionOptions

instance Prelude.NFData PeeringConnectionOptions
