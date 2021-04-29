{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ECS.Types.HostEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.HostEntry where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON HostEntry where
  parseJSON =
    Prelude.withObject
      "HostEntry"
      ( \x ->
          HostEntry'
            Prelude.<$> (x Prelude..: "hostname")
            Prelude.<*> (x Prelude..: "ipAddress")
      )

instance Prelude.Hashable HostEntry

instance Prelude.NFData HostEntry

instance Prelude.ToJSON HostEntry where
  toJSON HostEntry' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("hostname" Prelude..= hostname),
            Prelude.Just ("ipAddress" Prelude..= ipAddress)
          ]
      )
