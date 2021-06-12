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
-- Module      : Network.AWS.SMS.Types.ServerGroupReplicationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ServerGroupReplicationConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SMS.Types.ServerReplicationConfiguration

-- | Replication configuration for a server group.
--
-- /See:/ 'newServerGroupReplicationConfiguration' smart constructor.
data ServerGroupReplicationConfiguration = ServerGroupReplicationConfiguration'
  { -- | The ID of the server group with which this replication configuration is
    -- associated.
    serverGroupId :: Core.Maybe Core.Text,
    -- | The replication configuration for servers in the server group.
    serverReplicationConfigurations :: Core.Maybe [ServerReplicationConfiguration]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      serverReplicationConfigurations =
        Core.Nothing
    }

-- | The ID of the server group with which this replication configuration is
-- associated.
serverGroupReplicationConfiguration_serverGroupId :: Lens.Lens' ServerGroupReplicationConfiguration (Core.Maybe Core.Text)
serverGroupReplicationConfiguration_serverGroupId = Lens.lens (\ServerGroupReplicationConfiguration' {serverGroupId} -> serverGroupId) (\s@ServerGroupReplicationConfiguration' {} a -> s {serverGroupId = a} :: ServerGroupReplicationConfiguration)

-- | The replication configuration for servers in the server group.
serverGroupReplicationConfiguration_serverReplicationConfigurations :: Lens.Lens' ServerGroupReplicationConfiguration (Core.Maybe [ServerReplicationConfiguration])
serverGroupReplicationConfiguration_serverReplicationConfigurations = Lens.lens (\ServerGroupReplicationConfiguration' {serverReplicationConfigurations} -> serverReplicationConfigurations) (\s@ServerGroupReplicationConfiguration' {} a -> s {serverReplicationConfigurations = a} :: ServerGroupReplicationConfiguration) Core.. Lens.mapping Lens._Coerce

instance
  Core.FromJSON
    ServerGroupReplicationConfiguration
  where
  parseJSON =
    Core.withObject
      "ServerGroupReplicationConfiguration"
      ( \x ->
          ServerGroupReplicationConfiguration'
            Core.<$> (x Core..:? "serverGroupId")
            Core.<*> ( x Core..:? "serverReplicationConfigurations"
                         Core..!= Core.mempty
                     )
      )

instance
  Core.Hashable
    ServerGroupReplicationConfiguration

instance
  Core.NFData
    ServerGroupReplicationConfiguration

instance
  Core.ToJSON
    ServerGroupReplicationConfiguration
  where
  toJSON ServerGroupReplicationConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("serverGroupId" Core..=) Core.<$> serverGroupId,
            ("serverReplicationConfigurations" Core..=)
              Core.<$> serverReplicationConfigurations
          ]
      )
