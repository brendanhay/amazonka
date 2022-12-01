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
-- Module      : Amazonka.EC2.Types.InstanceIpv4Prefix
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceIpv4Prefix where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Information about an IPv4 prefix.
--
-- /See:/ 'newInstanceIpv4Prefix' smart constructor.
data InstanceIpv4Prefix = InstanceIpv4Prefix'
  { -- | One or more IPv4 prefixes assigned to the network interface.
    ipv4Prefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceIpv4Prefix' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipv4Prefix', 'instanceIpv4Prefix_ipv4Prefix' - One or more IPv4 prefixes assigned to the network interface.
newInstanceIpv4Prefix ::
  InstanceIpv4Prefix
newInstanceIpv4Prefix =
  InstanceIpv4Prefix' {ipv4Prefix = Prelude.Nothing}

-- | One or more IPv4 prefixes assigned to the network interface.
instanceIpv4Prefix_ipv4Prefix :: Lens.Lens' InstanceIpv4Prefix (Prelude.Maybe Prelude.Text)
instanceIpv4Prefix_ipv4Prefix = Lens.lens (\InstanceIpv4Prefix' {ipv4Prefix} -> ipv4Prefix) (\s@InstanceIpv4Prefix' {} a -> s {ipv4Prefix = a} :: InstanceIpv4Prefix)

instance Core.FromXML InstanceIpv4Prefix where
  parseXML x =
    InstanceIpv4Prefix'
      Prelude.<$> (x Core..@? "ipv4Prefix")

instance Prelude.Hashable InstanceIpv4Prefix where
  hashWithSalt _salt InstanceIpv4Prefix' {..} =
    _salt `Prelude.hashWithSalt` ipv4Prefix

instance Prelude.NFData InstanceIpv4Prefix where
  rnf InstanceIpv4Prefix' {..} = Prelude.rnf ipv4Prefix
