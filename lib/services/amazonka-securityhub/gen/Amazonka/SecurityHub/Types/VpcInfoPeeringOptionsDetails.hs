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
-- Module      : Amazonka.SecurityHub.Types.VpcInfoPeeringOptionsDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.VpcInfoPeeringOptionsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the VPC peering connection options for the
-- accepter or requester VPC.
--
-- /See:/ 'newVpcInfoPeeringOptionsDetails' smart constructor.
data VpcInfoPeeringOptionsDetails = VpcInfoPeeringOptionsDetails'
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
-- Create a value of 'VpcInfoPeeringOptionsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowDnsResolutionFromRemoteVpc', 'vpcInfoPeeringOptionsDetails_allowDnsResolutionFromRemoteVpc' - Indicates whether a local VPC can resolve public DNS hostnames to
-- private IP addresses when queried from instances in a peer VPC.
--
-- 'allowEgressFromLocalClassicLinkToRemoteVpc', 'vpcInfoPeeringOptionsDetails_allowEgressFromLocalClassicLinkToRemoteVpc' - Indicates whether a local ClassicLink connection can communicate with
-- the peer VPC over the VPC peering connection.
--
-- 'allowEgressFromLocalVpcToRemoteClassicLink', 'vpcInfoPeeringOptionsDetails_allowEgressFromLocalVpcToRemoteClassicLink' - Indicates whether a local VPC can communicate with a ClassicLink
-- connection in the peer VPC over the VPC peering connection.
newVpcInfoPeeringOptionsDetails ::
  VpcInfoPeeringOptionsDetails
newVpcInfoPeeringOptionsDetails =
  VpcInfoPeeringOptionsDetails'
    { allowDnsResolutionFromRemoteVpc =
        Prelude.Nothing,
      allowEgressFromLocalClassicLinkToRemoteVpc =
        Prelude.Nothing,
      allowEgressFromLocalVpcToRemoteClassicLink =
        Prelude.Nothing
    }

-- | Indicates whether a local VPC can resolve public DNS hostnames to
-- private IP addresses when queried from instances in a peer VPC.
vpcInfoPeeringOptionsDetails_allowDnsResolutionFromRemoteVpc :: Lens.Lens' VpcInfoPeeringOptionsDetails (Prelude.Maybe Prelude.Bool)
vpcInfoPeeringOptionsDetails_allowDnsResolutionFromRemoteVpc = Lens.lens (\VpcInfoPeeringOptionsDetails' {allowDnsResolutionFromRemoteVpc} -> allowDnsResolutionFromRemoteVpc) (\s@VpcInfoPeeringOptionsDetails' {} a -> s {allowDnsResolutionFromRemoteVpc = a} :: VpcInfoPeeringOptionsDetails)

-- | Indicates whether a local ClassicLink connection can communicate with
-- the peer VPC over the VPC peering connection.
vpcInfoPeeringOptionsDetails_allowEgressFromLocalClassicLinkToRemoteVpc :: Lens.Lens' VpcInfoPeeringOptionsDetails (Prelude.Maybe Prelude.Bool)
vpcInfoPeeringOptionsDetails_allowEgressFromLocalClassicLinkToRemoteVpc = Lens.lens (\VpcInfoPeeringOptionsDetails' {allowEgressFromLocalClassicLinkToRemoteVpc} -> allowEgressFromLocalClassicLinkToRemoteVpc) (\s@VpcInfoPeeringOptionsDetails' {} a -> s {allowEgressFromLocalClassicLinkToRemoteVpc = a} :: VpcInfoPeeringOptionsDetails)

-- | Indicates whether a local VPC can communicate with a ClassicLink
-- connection in the peer VPC over the VPC peering connection.
vpcInfoPeeringOptionsDetails_allowEgressFromLocalVpcToRemoteClassicLink :: Lens.Lens' VpcInfoPeeringOptionsDetails (Prelude.Maybe Prelude.Bool)
vpcInfoPeeringOptionsDetails_allowEgressFromLocalVpcToRemoteClassicLink = Lens.lens (\VpcInfoPeeringOptionsDetails' {allowEgressFromLocalVpcToRemoteClassicLink} -> allowEgressFromLocalVpcToRemoteClassicLink) (\s@VpcInfoPeeringOptionsDetails' {} a -> s {allowEgressFromLocalVpcToRemoteClassicLink = a} :: VpcInfoPeeringOptionsDetails)

instance Data.FromJSON VpcInfoPeeringOptionsDetails where
  parseJSON =
    Data.withObject
      "VpcInfoPeeringOptionsDetails"
      ( \x ->
          VpcInfoPeeringOptionsDetails'
            Prelude.<$> (x Data..:? "AllowDnsResolutionFromRemoteVpc")
            Prelude.<*> ( x
                            Data..:? "AllowEgressFromLocalClassicLinkToRemoteVpc"
                        )
            Prelude.<*> ( x
                            Data..:? "AllowEgressFromLocalVpcToRemoteClassicLink"
                        )
      )

instance
  Prelude.Hashable
    VpcInfoPeeringOptionsDetails
  where
  hashWithSalt _salt VpcInfoPeeringOptionsDetails' {..} =
    _salt
      `Prelude.hashWithSalt` allowDnsResolutionFromRemoteVpc
      `Prelude.hashWithSalt` allowEgressFromLocalClassicLinkToRemoteVpc
      `Prelude.hashWithSalt` allowEgressFromLocalVpcToRemoteClassicLink

instance Prelude.NFData VpcInfoPeeringOptionsDetails where
  rnf VpcInfoPeeringOptionsDetails' {..} =
    Prelude.rnf allowDnsResolutionFromRemoteVpc
      `Prelude.seq` Prelude.rnf
        allowEgressFromLocalClassicLinkToRemoteVpc
      `Prelude.seq` Prelude.rnf
        allowEgressFromLocalVpcToRemoteClassicLink

instance Data.ToJSON VpcInfoPeeringOptionsDetails where
  toJSON VpcInfoPeeringOptionsDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AllowDnsResolutionFromRemoteVpc" Data..=)
              Prelude.<$> allowDnsResolutionFromRemoteVpc,
            ( "AllowEgressFromLocalClassicLinkToRemoteVpc"
                Data..=
            )
              Prelude.<$> allowEgressFromLocalClassicLinkToRemoteVpc,
            ( "AllowEgressFromLocalVpcToRemoteClassicLink"
                Data..=
            )
              Prelude.<$> allowEgressFromLocalVpcToRemoteClassicLink
          ]
      )
