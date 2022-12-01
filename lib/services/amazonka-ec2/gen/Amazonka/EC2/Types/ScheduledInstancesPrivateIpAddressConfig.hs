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
-- Module      : Amazonka.EC2.Types.ScheduledInstancesPrivateIpAddressConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ScheduledInstancesPrivateIpAddressConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes a private IPv4 address for a Scheduled Instance.
--
-- /See:/ 'newScheduledInstancesPrivateIpAddressConfig' smart constructor.
data ScheduledInstancesPrivateIpAddressConfig = ScheduledInstancesPrivateIpAddressConfig'
  { -- | Indicates whether this is a primary IPv4 address. Otherwise, this is a
    -- secondary IPv4 address.
    primary :: Prelude.Maybe Prelude.Bool,
    -- | The IPv4 address.
    privateIpAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduledInstancesPrivateIpAddressConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'primary', 'scheduledInstancesPrivateIpAddressConfig_primary' - Indicates whether this is a primary IPv4 address. Otherwise, this is a
-- secondary IPv4 address.
--
-- 'privateIpAddress', 'scheduledInstancesPrivateIpAddressConfig_privateIpAddress' - The IPv4 address.
newScheduledInstancesPrivateIpAddressConfig ::
  ScheduledInstancesPrivateIpAddressConfig
newScheduledInstancesPrivateIpAddressConfig =
  ScheduledInstancesPrivateIpAddressConfig'
    { primary =
        Prelude.Nothing,
      privateIpAddress =
        Prelude.Nothing
    }

-- | Indicates whether this is a primary IPv4 address. Otherwise, this is a
-- secondary IPv4 address.
scheduledInstancesPrivateIpAddressConfig_primary :: Lens.Lens' ScheduledInstancesPrivateIpAddressConfig (Prelude.Maybe Prelude.Bool)
scheduledInstancesPrivateIpAddressConfig_primary = Lens.lens (\ScheduledInstancesPrivateIpAddressConfig' {primary} -> primary) (\s@ScheduledInstancesPrivateIpAddressConfig' {} a -> s {primary = a} :: ScheduledInstancesPrivateIpAddressConfig)

-- | The IPv4 address.
scheduledInstancesPrivateIpAddressConfig_privateIpAddress :: Lens.Lens' ScheduledInstancesPrivateIpAddressConfig (Prelude.Maybe Prelude.Text)
scheduledInstancesPrivateIpAddressConfig_privateIpAddress = Lens.lens (\ScheduledInstancesPrivateIpAddressConfig' {privateIpAddress} -> privateIpAddress) (\s@ScheduledInstancesPrivateIpAddressConfig' {} a -> s {privateIpAddress = a} :: ScheduledInstancesPrivateIpAddressConfig)

instance
  Prelude.Hashable
    ScheduledInstancesPrivateIpAddressConfig
  where
  hashWithSalt
    _salt
    ScheduledInstancesPrivateIpAddressConfig' {..} =
      _salt `Prelude.hashWithSalt` primary
        `Prelude.hashWithSalt` privateIpAddress

instance
  Prelude.NFData
    ScheduledInstancesPrivateIpAddressConfig
  where
  rnf ScheduledInstancesPrivateIpAddressConfig' {..} =
    Prelude.rnf primary
      `Prelude.seq` Prelude.rnf privateIpAddress

instance
  Core.ToQuery
    ScheduledInstancesPrivateIpAddressConfig
  where
  toQuery ScheduledInstancesPrivateIpAddressConfig' {..} =
    Prelude.mconcat
      [ "Primary" Core.=: primary,
        "PrivateIpAddress" Core.=: privateIpAddress
      ]
