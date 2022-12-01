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
-- Module      : Amazonka.EC2.Types.Ipv4PrefixSpecificationResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.Ipv4PrefixSpecificationResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Information about the IPv4 delegated prefixes assigned to a network
-- interface.
--
-- /See:/ 'newIpv4PrefixSpecificationResponse' smart constructor.
data Ipv4PrefixSpecificationResponse = Ipv4PrefixSpecificationResponse'
  { -- | The IPv4 delegated prefixes assigned to the network interface.
    ipv4Prefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Ipv4PrefixSpecificationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipv4Prefix', 'ipv4PrefixSpecificationResponse_ipv4Prefix' - The IPv4 delegated prefixes assigned to the network interface.
newIpv4PrefixSpecificationResponse ::
  Ipv4PrefixSpecificationResponse
newIpv4PrefixSpecificationResponse =
  Ipv4PrefixSpecificationResponse'
    { ipv4Prefix =
        Prelude.Nothing
    }

-- | The IPv4 delegated prefixes assigned to the network interface.
ipv4PrefixSpecificationResponse_ipv4Prefix :: Lens.Lens' Ipv4PrefixSpecificationResponse (Prelude.Maybe Prelude.Text)
ipv4PrefixSpecificationResponse_ipv4Prefix = Lens.lens (\Ipv4PrefixSpecificationResponse' {ipv4Prefix} -> ipv4Prefix) (\s@Ipv4PrefixSpecificationResponse' {} a -> s {ipv4Prefix = a} :: Ipv4PrefixSpecificationResponse)

instance Core.FromXML Ipv4PrefixSpecificationResponse where
  parseXML x =
    Ipv4PrefixSpecificationResponse'
      Prelude.<$> (x Core..@? "ipv4Prefix")

instance
  Prelude.Hashable
    Ipv4PrefixSpecificationResponse
  where
  hashWithSalt
    _salt
    Ipv4PrefixSpecificationResponse' {..} =
      _salt `Prelude.hashWithSalt` ipv4Prefix

instance
  Prelude.NFData
    Ipv4PrefixSpecificationResponse
  where
  rnf Ipv4PrefixSpecificationResponse' {..} =
    Prelude.rnf ipv4Prefix
