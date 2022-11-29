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
-- Module      : Amazonka.EC2.Types.Ipv6PrefixSpecificationRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.Ipv6PrefixSpecificationRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the IPv4 prefix option for a network interface.
--
-- /See:/ 'newIpv6PrefixSpecificationRequest' smart constructor.
data Ipv6PrefixSpecificationRequest = Ipv6PrefixSpecificationRequest'
  { -- | The IPv6 prefix.
    ipv6Prefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Ipv6PrefixSpecificationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipv6Prefix', 'ipv6PrefixSpecificationRequest_ipv6Prefix' - The IPv6 prefix.
newIpv6PrefixSpecificationRequest ::
  Ipv6PrefixSpecificationRequest
newIpv6PrefixSpecificationRequest =
  Ipv6PrefixSpecificationRequest'
    { ipv6Prefix =
        Prelude.Nothing
    }

-- | The IPv6 prefix.
ipv6PrefixSpecificationRequest_ipv6Prefix :: Lens.Lens' Ipv6PrefixSpecificationRequest (Prelude.Maybe Prelude.Text)
ipv6PrefixSpecificationRequest_ipv6Prefix = Lens.lens (\Ipv6PrefixSpecificationRequest' {ipv6Prefix} -> ipv6Prefix) (\s@Ipv6PrefixSpecificationRequest' {} a -> s {ipv6Prefix = a} :: Ipv6PrefixSpecificationRequest)

instance Core.FromXML Ipv6PrefixSpecificationRequest where
  parseXML x =
    Ipv6PrefixSpecificationRequest'
      Prelude.<$> (x Core..@? "Ipv6Prefix")

instance
  Prelude.Hashable
    Ipv6PrefixSpecificationRequest
  where
  hashWithSalt
    _salt
    Ipv6PrefixSpecificationRequest' {..} =
      _salt `Prelude.hashWithSalt` ipv6Prefix

instance
  Prelude.NFData
    Ipv6PrefixSpecificationRequest
  where
  rnf Ipv6PrefixSpecificationRequest' {..} =
    Prelude.rnf ipv6Prefix

instance Core.ToQuery Ipv6PrefixSpecificationRequest where
  toQuery Ipv6PrefixSpecificationRequest' {..} =
    Prelude.mconcat ["Ipv6Prefix" Core.=: ipv6Prefix]
