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
-- Module      : Amazonka.ECS.Types.HostEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.HostEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Hostnames and IP address entries that are added to the @\/etc\/hosts@
-- file of a container via the @extraHosts@ parameter of its
-- ContainerDefinition.
--
-- /See:/ 'newHostEntry' smart constructor.
data HostEntry = HostEntry'
  { -- | The hostname to use in the @\/etc\/hosts@ entry.
    hostname :: Prelude.Text,
    -- | The IP address to use in the @\/etc\/hosts@ entry.
    ipAddress :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HostEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostname', 'hostEntry_hostname' - The hostname to use in the @\/etc\/hosts@ entry.
--
-- 'ipAddress', 'hostEntry_ipAddress' - The IP address to use in the @\/etc\/hosts@ entry.
newHostEntry ::
  -- | 'hostname'
  Prelude.Text ->
  -- | 'ipAddress'
  Prelude.Text ->
  HostEntry
newHostEntry pHostname_ pIpAddress_ =
  HostEntry'
    { hostname = pHostname_,
      ipAddress = pIpAddress_
    }

-- | The hostname to use in the @\/etc\/hosts@ entry.
hostEntry_hostname :: Lens.Lens' HostEntry Prelude.Text
hostEntry_hostname = Lens.lens (\HostEntry' {hostname} -> hostname) (\s@HostEntry' {} a -> s {hostname = a} :: HostEntry)

-- | The IP address to use in the @\/etc\/hosts@ entry.
hostEntry_ipAddress :: Lens.Lens' HostEntry Prelude.Text
hostEntry_ipAddress = Lens.lens (\HostEntry' {ipAddress} -> ipAddress) (\s@HostEntry' {} a -> s {ipAddress = a} :: HostEntry)

instance Data.FromJSON HostEntry where
  parseJSON =
    Data.withObject
      "HostEntry"
      ( \x ->
          HostEntry'
            Prelude.<$> (x Data..: "hostname")
            Prelude.<*> (x Data..: "ipAddress")
      )

instance Prelude.Hashable HostEntry where
  hashWithSalt _salt HostEntry' {..} =
    _salt
      `Prelude.hashWithSalt` hostname
      `Prelude.hashWithSalt` ipAddress

instance Prelude.NFData HostEntry where
  rnf HostEntry' {..} =
    Prelude.rnf hostname
      `Prelude.seq` Prelude.rnf ipAddress

instance Data.ToJSON HostEntry where
  toJSON HostEntry' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("hostname" Data..= hostname),
            Prelude.Just ("ipAddress" Data..= ipAddress)
          ]
      )
