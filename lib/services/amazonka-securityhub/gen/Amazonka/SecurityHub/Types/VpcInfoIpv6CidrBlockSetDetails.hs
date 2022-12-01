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
-- Module      : Amazonka.SecurityHub.Types.VpcInfoIpv6CidrBlockSetDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.VpcInfoIpv6CidrBlockSetDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides details about the IPv6 CIDR blocks for the VPC.
--
-- /See:/ 'newVpcInfoIpv6CidrBlockSetDetails' smart constructor.
data VpcInfoIpv6CidrBlockSetDetails = VpcInfoIpv6CidrBlockSetDetails'
  { -- | The IPv6 CIDR block for the VPC.
    ipv6CidrBlock :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcInfoIpv6CidrBlockSetDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipv6CidrBlock', 'vpcInfoIpv6CidrBlockSetDetails_ipv6CidrBlock' - The IPv6 CIDR block for the VPC.
newVpcInfoIpv6CidrBlockSetDetails ::
  VpcInfoIpv6CidrBlockSetDetails
newVpcInfoIpv6CidrBlockSetDetails =
  VpcInfoIpv6CidrBlockSetDetails'
    { ipv6CidrBlock =
        Prelude.Nothing
    }

-- | The IPv6 CIDR block for the VPC.
vpcInfoIpv6CidrBlockSetDetails_ipv6CidrBlock :: Lens.Lens' VpcInfoIpv6CidrBlockSetDetails (Prelude.Maybe Prelude.Text)
vpcInfoIpv6CidrBlockSetDetails_ipv6CidrBlock = Lens.lens (\VpcInfoIpv6CidrBlockSetDetails' {ipv6CidrBlock} -> ipv6CidrBlock) (\s@VpcInfoIpv6CidrBlockSetDetails' {} a -> s {ipv6CidrBlock = a} :: VpcInfoIpv6CidrBlockSetDetails)

instance Core.FromJSON VpcInfoIpv6CidrBlockSetDetails where
  parseJSON =
    Core.withObject
      "VpcInfoIpv6CidrBlockSetDetails"
      ( \x ->
          VpcInfoIpv6CidrBlockSetDetails'
            Prelude.<$> (x Core..:? "Ipv6CidrBlock")
      )

instance
  Prelude.Hashable
    VpcInfoIpv6CidrBlockSetDetails
  where
  hashWithSalt
    _salt
    VpcInfoIpv6CidrBlockSetDetails' {..} =
      _salt `Prelude.hashWithSalt` ipv6CidrBlock

instance
  Prelude.NFData
    VpcInfoIpv6CidrBlockSetDetails
  where
  rnf VpcInfoIpv6CidrBlockSetDetails' {..} =
    Prelude.rnf ipv6CidrBlock

instance Core.ToJSON VpcInfoIpv6CidrBlockSetDetails where
  toJSON VpcInfoIpv6CidrBlockSetDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Ipv6CidrBlock" Core..=)
              Prelude.<$> ipv6CidrBlock
          ]
      )
