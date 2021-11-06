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
-- Module      : Amazonka.SMS.Types.ServerGroupReplicationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.ServerGroupReplicationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SMS.Types.ServerReplicationConfiguration

-- | Replication configuration for a server group.
--
-- /See:/ 'newServerGroupReplicationConfiguration' smart constructor.
data ServerGroupReplicationConfiguration = ServerGroupReplicationConfiguration'
  { -- | The ID of the server group with which this replication configuration is
    -- associated.
    serverGroupId :: Prelude.Maybe Prelude.Text,
    -- | The replication configuration for servers in the server group.
    serverReplicationConfigurations :: Prelude.Maybe [ServerReplicationConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServerGroupReplicationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverGroupId', 'serverGroupReplicationConfiguration_serverGroupId' - The ID of the server group with which this replication configuration is
-- associated.
--
-- 'serverReplicationConfigurations', 'serverGroupReplicationConfiguration_serverReplicationConfigurations' - The replication configuration for servers in the server group.
newServerGroupReplicationConfiguration ::
  ServerGroupReplicationConfiguration
newServerGroupReplicationConfiguration =
  ServerGroupReplicationConfiguration'
    { serverGroupId =
        Prelude.Nothing,
      serverReplicationConfigurations =
        Prelude.Nothing
    }

-- | The ID of the server group with which this replication configuration is
-- associated.
serverGroupReplicationConfiguration_serverGroupId :: Lens.Lens' ServerGroupReplicationConfiguration (Prelude.Maybe Prelude.Text)
serverGroupReplicationConfiguration_serverGroupId = Lens.lens (\ServerGroupReplicationConfiguration' {serverGroupId} -> serverGroupId) (\s@ServerGroupReplicationConfiguration' {} a -> s {serverGroupId = a} :: ServerGroupReplicationConfiguration)

-- | The replication configuration for servers in the server group.
serverGroupReplicationConfiguration_serverReplicationConfigurations :: Lens.Lens' ServerGroupReplicationConfiguration (Prelude.Maybe [ServerReplicationConfiguration])
serverGroupReplicationConfiguration_serverReplicationConfigurations = Lens.lens (\ServerGroupReplicationConfiguration' {serverReplicationConfigurations} -> serverReplicationConfigurations) (\s@ServerGroupReplicationConfiguration' {} a -> s {serverReplicationConfigurations = a} :: ServerGroupReplicationConfiguration) Prelude.. Lens.mapping Lens.coerced

instance
  Core.FromJSON
    ServerGroupReplicationConfiguration
  where
  parseJSON =
    Core.withObject
      "ServerGroupReplicationConfiguration"
      ( \x ->
          ServerGroupReplicationConfiguration'
            Prelude.<$> (x Core..:? "serverGroupId")
            Prelude.<*> ( x Core..:? "serverReplicationConfigurations"
                            Core..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    ServerGroupReplicationConfiguration

instance
  Prelude.NFData
    ServerGroupReplicationConfiguration

instance
  Core.ToJSON
    ServerGroupReplicationConfiguration
  where
  toJSON ServerGroupReplicationConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("serverGroupId" Core..=) Prelude.<$> serverGroupId,
            ("serverReplicationConfigurations" Core..=)
              Prelude.<$> serverReplicationConfigurations
          ]
      )
