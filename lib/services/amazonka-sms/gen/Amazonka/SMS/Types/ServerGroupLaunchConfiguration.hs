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
-- Module      : Amazonka.SMS.Types.ServerGroupLaunchConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.ServerGroupLaunchConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SMS.Types.ServerLaunchConfiguration

-- | Launch configuration for a server group.
--
-- /See:/ 'newServerGroupLaunchConfiguration' smart constructor.
data ServerGroupLaunchConfiguration = ServerGroupLaunchConfiguration'
  { -- | The launch order of servers in the server group.
    launchOrder :: Prelude.Maybe Prelude.Int,
    -- | The ID of the server group with which the launch configuration is
    -- associated.
    serverGroupId :: Prelude.Maybe Prelude.Text,
    -- | The launch configuration for servers in the server group.
    serverLaunchConfigurations :: Prelude.Maybe [ServerLaunchConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServerGroupLaunchConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchOrder', 'serverGroupLaunchConfiguration_launchOrder' - The launch order of servers in the server group.
--
-- 'serverGroupId', 'serverGroupLaunchConfiguration_serverGroupId' - The ID of the server group with which the launch configuration is
-- associated.
--
-- 'serverLaunchConfigurations', 'serverGroupLaunchConfiguration_serverLaunchConfigurations' - The launch configuration for servers in the server group.
newServerGroupLaunchConfiguration ::
  ServerGroupLaunchConfiguration
newServerGroupLaunchConfiguration =
  ServerGroupLaunchConfiguration'
    { launchOrder =
        Prelude.Nothing,
      serverGroupId = Prelude.Nothing,
      serverLaunchConfigurations =
        Prelude.Nothing
    }

-- | The launch order of servers in the server group.
serverGroupLaunchConfiguration_launchOrder :: Lens.Lens' ServerGroupLaunchConfiguration (Prelude.Maybe Prelude.Int)
serverGroupLaunchConfiguration_launchOrder = Lens.lens (\ServerGroupLaunchConfiguration' {launchOrder} -> launchOrder) (\s@ServerGroupLaunchConfiguration' {} a -> s {launchOrder = a} :: ServerGroupLaunchConfiguration)

-- | The ID of the server group with which the launch configuration is
-- associated.
serverGroupLaunchConfiguration_serverGroupId :: Lens.Lens' ServerGroupLaunchConfiguration (Prelude.Maybe Prelude.Text)
serverGroupLaunchConfiguration_serverGroupId = Lens.lens (\ServerGroupLaunchConfiguration' {serverGroupId} -> serverGroupId) (\s@ServerGroupLaunchConfiguration' {} a -> s {serverGroupId = a} :: ServerGroupLaunchConfiguration)

-- | The launch configuration for servers in the server group.
serverGroupLaunchConfiguration_serverLaunchConfigurations :: Lens.Lens' ServerGroupLaunchConfiguration (Prelude.Maybe [ServerLaunchConfiguration])
serverGroupLaunchConfiguration_serverLaunchConfigurations = Lens.lens (\ServerGroupLaunchConfiguration' {serverLaunchConfigurations} -> serverLaunchConfigurations) (\s@ServerGroupLaunchConfiguration' {} a -> s {serverLaunchConfigurations = a} :: ServerGroupLaunchConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ServerGroupLaunchConfiguration where
  parseJSON =
    Data.withObject
      "ServerGroupLaunchConfiguration"
      ( \x ->
          ServerGroupLaunchConfiguration'
            Prelude.<$> (x Data..:? "launchOrder")
            Prelude.<*> (x Data..:? "serverGroupId")
            Prelude.<*> ( x Data..:? "serverLaunchConfigurations"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    ServerGroupLaunchConfiguration
  where
  hashWithSalt
    _salt
    ServerGroupLaunchConfiguration' {..} =
      _salt `Prelude.hashWithSalt` launchOrder
        `Prelude.hashWithSalt` serverGroupId
        `Prelude.hashWithSalt` serverLaunchConfigurations

instance
  Prelude.NFData
    ServerGroupLaunchConfiguration
  where
  rnf ServerGroupLaunchConfiguration' {..} =
    Prelude.rnf launchOrder
      `Prelude.seq` Prelude.rnf serverGroupId
      `Prelude.seq` Prelude.rnf serverLaunchConfigurations

instance Data.ToJSON ServerGroupLaunchConfiguration where
  toJSON ServerGroupLaunchConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("launchOrder" Data..=) Prelude.<$> launchOrder,
            ("serverGroupId" Data..=) Prelude.<$> serverGroupId,
            ("serverLaunchConfigurations" Data..=)
              Prelude.<$> serverLaunchConfigurations
          ]
      )
