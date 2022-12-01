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
-- Module      : Amazonka.EC2.Types.InstanceIpv6Prefix
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceIpv6Prefix where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Information about an IPv6 prefix.
--
-- /See:/ 'newInstanceIpv6Prefix' smart constructor.
data InstanceIpv6Prefix = InstanceIpv6Prefix'
  { -- | One or more IPv6 prefixes assigned to the network interface.
    ipv6Prefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceIpv6Prefix' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipv6Prefix', 'instanceIpv6Prefix_ipv6Prefix' - One or more IPv6 prefixes assigned to the network interface.
newInstanceIpv6Prefix ::
  InstanceIpv6Prefix
newInstanceIpv6Prefix =
  InstanceIpv6Prefix' {ipv6Prefix = Prelude.Nothing}

-- | One or more IPv6 prefixes assigned to the network interface.
instanceIpv6Prefix_ipv6Prefix :: Lens.Lens' InstanceIpv6Prefix (Prelude.Maybe Prelude.Text)
instanceIpv6Prefix_ipv6Prefix = Lens.lens (\InstanceIpv6Prefix' {ipv6Prefix} -> ipv6Prefix) (\s@InstanceIpv6Prefix' {} a -> s {ipv6Prefix = a} :: InstanceIpv6Prefix)

instance Core.FromXML InstanceIpv6Prefix where
  parseXML x =
    InstanceIpv6Prefix'
      Prelude.<$> (x Core..@? "ipv6Prefix")

instance Prelude.Hashable InstanceIpv6Prefix where
  hashWithSalt _salt InstanceIpv6Prefix' {..} =
    _salt `Prelude.hashWithSalt` ipv6Prefix

instance Prelude.NFData InstanceIpv6Prefix where
  rnf InstanceIpv6Prefix' {..} = Prelude.rnf ipv6Prefix
