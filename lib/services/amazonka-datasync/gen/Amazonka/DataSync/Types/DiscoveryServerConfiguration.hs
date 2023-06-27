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
-- Module      : Amazonka.DataSync.Types.DiscoveryServerConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.DiscoveryServerConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The network settings that DataSync Discovery uses to connect with your
-- on-premises storage system\'s management interface.
--
-- /See:/ 'newDiscoveryServerConfiguration' smart constructor.
data DiscoveryServerConfiguration = DiscoveryServerConfiguration'
  { -- | The network port for accessing the storage system\'s management
    -- interface.
    serverPort :: Prelude.Maybe Prelude.Natural,
    -- | The domain name or IP address of your storage system\'s management
    -- interface.
    serverHostname :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DiscoveryServerConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverPort', 'discoveryServerConfiguration_serverPort' - The network port for accessing the storage system\'s management
-- interface.
--
-- 'serverHostname', 'discoveryServerConfiguration_serverHostname' - The domain name or IP address of your storage system\'s management
-- interface.
newDiscoveryServerConfiguration ::
  -- | 'serverHostname'
  Prelude.Text ->
  DiscoveryServerConfiguration
newDiscoveryServerConfiguration pServerHostname_ =
  DiscoveryServerConfiguration'
    { serverPort =
        Prelude.Nothing,
      serverHostname = pServerHostname_
    }

-- | The network port for accessing the storage system\'s management
-- interface.
discoveryServerConfiguration_serverPort :: Lens.Lens' DiscoveryServerConfiguration (Prelude.Maybe Prelude.Natural)
discoveryServerConfiguration_serverPort = Lens.lens (\DiscoveryServerConfiguration' {serverPort} -> serverPort) (\s@DiscoveryServerConfiguration' {} a -> s {serverPort = a} :: DiscoveryServerConfiguration)

-- | The domain name or IP address of your storage system\'s management
-- interface.
discoveryServerConfiguration_serverHostname :: Lens.Lens' DiscoveryServerConfiguration Prelude.Text
discoveryServerConfiguration_serverHostname = Lens.lens (\DiscoveryServerConfiguration' {serverHostname} -> serverHostname) (\s@DiscoveryServerConfiguration' {} a -> s {serverHostname = a} :: DiscoveryServerConfiguration)

instance Data.FromJSON DiscoveryServerConfiguration where
  parseJSON =
    Data.withObject
      "DiscoveryServerConfiguration"
      ( \x ->
          DiscoveryServerConfiguration'
            Prelude.<$> (x Data..:? "ServerPort")
            Prelude.<*> (x Data..: "ServerHostname")
      )

instance
  Prelude.Hashable
    DiscoveryServerConfiguration
  where
  hashWithSalt _salt DiscoveryServerConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` serverPort
      `Prelude.hashWithSalt` serverHostname

instance Prelude.NFData DiscoveryServerConfiguration where
  rnf DiscoveryServerConfiguration' {..} =
    Prelude.rnf serverPort
      `Prelude.seq` Prelude.rnf serverHostname

instance Data.ToJSON DiscoveryServerConfiguration where
  toJSON DiscoveryServerConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ServerPort" Data..=) Prelude.<$> serverPort,
            Prelude.Just
              ("ServerHostname" Data..= serverHostname)
          ]
      )
