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
-- Module      : Amazonka.EC2.Types.Ipv6PrefixSpecificationResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.Ipv6PrefixSpecificationResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Information about the IPv6 delegated prefixes assigned to a network
-- interface.
--
-- /See:/ 'newIpv6PrefixSpecificationResponse' smart constructor.
data Ipv6PrefixSpecificationResponse = Ipv6PrefixSpecificationResponse'
  { -- | The IPv6 delegated prefixes assigned to the network interface.
    ipv6Prefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Ipv6PrefixSpecificationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipv6Prefix', 'ipv6PrefixSpecificationResponse_ipv6Prefix' - The IPv6 delegated prefixes assigned to the network interface.
newIpv6PrefixSpecificationResponse ::
  Ipv6PrefixSpecificationResponse
newIpv6PrefixSpecificationResponse =
  Ipv6PrefixSpecificationResponse'
    { ipv6Prefix =
        Prelude.Nothing
    }

-- | The IPv6 delegated prefixes assigned to the network interface.
ipv6PrefixSpecificationResponse_ipv6Prefix :: Lens.Lens' Ipv6PrefixSpecificationResponse (Prelude.Maybe Prelude.Text)
ipv6PrefixSpecificationResponse_ipv6Prefix = Lens.lens (\Ipv6PrefixSpecificationResponse' {ipv6Prefix} -> ipv6Prefix) (\s@Ipv6PrefixSpecificationResponse' {} a -> s {ipv6Prefix = a} :: Ipv6PrefixSpecificationResponse)

instance Data.FromXML Ipv6PrefixSpecificationResponse where
  parseXML x =
    Ipv6PrefixSpecificationResponse'
      Prelude.<$> (x Data..@? "ipv6Prefix")

instance
  Prelude.Hashable
    Ipv6PrefixSpecificationResponse
  where
  hashWithSalt
    _salt
    Ipv6PrefixSpecificationResponse' {..} =
      _salt `Prelude.hashWithSalt` ipv6Prefix

instance
  Prelude.NFData
    Ipv6PrefixSpecificationResponse
  where
  rnf Ipv6PrefixSpecificationResponse' {..} =
    Prelude.rnf ipv6Prefix
