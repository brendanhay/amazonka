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
-- Module      : Amazonka.EC2.Types.Ipv6CidrAssociation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.Ipv6CidrAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes an IPv6 CIDR block association.
--
-- /See:/ 'newIpv6CidrAssociation' smart constructor.
data Ipv6CidrAssociation = Ipv6CidrAssociation'
  { -- | The resource that\'s associated with the IPv6 CIDR block.
    associatedResource :: Prelude.Maybe Prelude.Text,
    -- | The IPv6 CIDR block.
    ipv6Cidr :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Ipv6CidrAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associatedResource', 'ipv6CidrAssociation_associatedResource' - The resource that\'s associated with the IPv6 CIDR block.
--
-- 'ipv6Cidr', 'ipv6CidrAssociation_ipv6Cidr' - The IPv6 CIDR block.
newIpv6CidrAssociation ::
  Ipv6CidrAssociation
newIpv6CidrAssociation =
  Ipv6CidrAssociation'
    { associatedResource =
        Prelude.Nothing,
      ipv6Cidr = Prelude.Nothing
    }

-- | The resource that\'s associated with the IPv6 CIDR block.
ipv6CidrAssociation_associatedResource :: Lens.Lens' Ipv6CidrAssociation (Prelude.Maybe Prelude.Text)
ipv6CidrAssociation_associatedResource = Lens.lens (\Ipv6CidrAssociation' {associatedResource} -> associatedResource) (\s@Ipv6CidrAssociation' {} a -> s {associatedResource = a} :: Ipv6CidrAssociation)

-- | The IPv6 CIDR block.
ipv6CidrAssociation_ipv6Cidr :: Lens.Lens' Ipv6CidrAssociation (Prelude.Maybe Prelude.Text)
ipv6CidrAssociation_ipv6Cidr = Lens.lens (\Ipv6CidrAssociation' {ipv6Cidr} -> ipv6Cidr) (\s@Ipv6CidrAssociation' {} a -> s {ipv6Cidr = a} :: Ipv6CidrAssociation)

instance Data.FromXML Ipv6CidrAssociation where
  parseXML x =
    Ipv6CidrAssociation'
      Prelude.<$> (x Data..@? "associatedResource")
      Prelude.<*> (x Data..@? "ipv6Cidr")

instance Prelude.Hashable Ipv6CidrAssociation where
  hashWithSalt _salt Ipv6CidrAssociation' {..} =
    _salt `Prelude.hashWithSalt` associatedResource
      `Prelude.hashWithSalt` ipv6Cidr

instance Prelude.NFData Ipv6CidrAssociation where
  rnf Ipv6CidrAssociation' {..} =
    Prelude.rnf associatedResource
      `Prelude.seq` Prelude.rnf ipv6Cidr
