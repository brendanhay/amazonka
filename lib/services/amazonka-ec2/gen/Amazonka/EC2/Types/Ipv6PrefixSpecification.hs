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
-- Module      : Amazonka.EC2.Types.Ipv6PrefixSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.Ipv6PrefixSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the IPv6 prefix.
--
-- /See:/ 'newIpv6PrefixSpecification' smart constructor.
data Ipv6PrefixSpecification = Ipv6PrefixSpecification'
  { -- | The IPv6 prefix.
    ipv6Prefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Ipv6PrefixSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipv6Prefix', 'ipv6PrefixSpecification_ipv6Prefix' - The IPv6 prefix.
newIpv6PrefixSpecification ::
  Ipv6PrefixSpecification
newIpv6PrefixSpecification =
  Ipv6PrefixSpecification'
    { ipv6Prefix =
        Prelude.Nothing
    }

-- | The IPv6 prefix.
ipv6PrefixSpecification_ipv6Prefix :: Lens.Lens' Ipv6PrefixSpecification (Prelude.Maybe Prelude.Text)
ipv6PrefixSpecification_ipv6Prefix = Lens.lens (\Ipv6PrefixSpecification' {ipv6Prefix} -> ipv6Prefix) (\s@Ipv6PrefixSpecification' {} a -> s {ipv6Prefix = a} :: Ipv6PrefixSpecification)

instance Data.FromXML Ipv6PrefixSpecification where
  parseXML x =
    Ipv6PrefixSpecification'
      Prelude.<$> (x Data..@? "ipv6Prefix")

instance Prelude.Hashable Ipv6PrefixSpecification where
  hashWithSalt _salt Ipv6PrefixSpecification' {..} =
    _salt `Prelude.hashWithSalt` ipv6Prefix

instance Prelude.NFData Ipv6PrefixSpecification where
  rnf Ipv6PrefixSpecification' {..} =
    Prelude.rnf ipv6Prefix
