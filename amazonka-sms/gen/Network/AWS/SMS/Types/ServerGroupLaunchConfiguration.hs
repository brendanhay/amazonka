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
-- Module      : Network.AWS.SMS.Types.ServerGroupLaunchConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ServerGroupLaunchConfiguration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SMS.Types.ServerLaunchConfiguration

-- | Launch configuration for a server group.
--
-- /See:/ 'newServerGroupLaunchConfiguration' smart constructor.
data ServerGroupLaunchConfiguration = ServerGroupLaunchConfiguration'
  { -- | The ID of the server group with which the launch configuration is
    -- associated.
    serverGroupId :: Prelude.Maybe Prelude.Text,
    -- | The launch order of servers in the server group.
    launchOrder :: Prelude.Maybe Prelude.Int,
    -- | The launch configuration for servers in the server group.
    serverLaunchConfigurations :: Prelude.Maybe [ServerLaunchConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ServerGroupLaunchConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverGroupId', 'serverGroupLaunchConfiguration_serverGroupId' - The ID of the server group with which the launch configuration is
-- associated.
--
-- 'launchOrder', 'serverGroupLaunchConfiguration_launchOrder' - The launch order of servers in the server group.
--
-- 'serverLaunchConfigurations', 'serverGroupLaunchConfiguration_serverLaunchConfigurations' - The launch configuration for servers in the server group.
newServerGroupLaunchConfiguration ::
  ServerGroupLaunchConfiguration
newServerGroupLaunchConfiguration =
  ServerGroupLaunchConfiguration'
    { serverGroupId =
        Prelude.Nothing,
      launchOrder = Prelude.Nothing,
      serverLaunchConfigurations =
        Prelude.Nothing
    }

-- | The ID of the server group with which the launch configuration is
-- associated.
serverGroupLaunchConfiguration_serverGroupId :: Lens.Lens' ServerGroupLaunchConfiguration (Prelude.Maybe Prelude.Text)
serverGroupLaunchConfiguration_serverGroupId = Lens.lens (\ServerGroupLaunchConfiguration' {serverGroupId} -> serverGroupId) (\s@ServerGroupLaunchConfiguration' {} a -> s {serverGroupId = a} :: ServerGroupLaunchConfiguration)

-- | The launch order of servers in the server group.
serverGroupLaunchConfiguration_launchOrder :: Lens.Lens' ServerGroupLaunchConfiguration (Prelude.Maybe Prelude.Int)
serverGroupLaunchConfiguration_launchOrder = Lens.lens (\ServerGroupLaunchConfiguration' {launchOrder} -> launchOrder) (\s@ServerGroupLaunchConfiguration' {} a -> s {launchOrder = a} :: ServerGroupLaunchConfiguration)

-- | The launch configuration for servers in the server group.
serverGroupLaunchConfiguration_serverLaunchConfigurations :: Lens.Lens' ServerGroupLaunchConfiguration (Prelude.Maybe [ServerLaunchConfiguration])
serverGroupLaunchConfiguration_serverLaunchConfigurations = Lens.lens (\ServerGroupLaunchConfiguration' {serverLaunchConfigurations} -> serverLaunchConfigurations) (\s@ServerGroupLaunchConfiguration' {} a -> s {serverLaunchConfigurations = a} :: ServerGroupLaunchConfiguration) Prelude.. Lens.mapping Prelude._Coerce

instance
  Prelude.FromJSON
    ServerGroupLaunchConfiguration
  where
  parseJSON =
    Prelude.withObject
      "ServerGroupLaunchConfiguration"
      ( \x ->
          ServerGroupLaunchConfiguration'
            Prelude.<$> (x Prelude..:? "serverGroupId")
            Prelude.<*> (x Prelude..:? "launchOrder")
            Prelude.<*> ( x Prelude..:? "serverLaunchConfigurations"
                            Prelude..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    ServerGroupLaunchConfiguration

instance
  Prelude.NFData
    ServerGroupLaunchConfiguration

instance
  Prelude.ToJSON
    ServerGroupLaunchConfiguration
  where
  toJSON ServerGroupLaunchConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("serverGroupId" Prelude..=)
              Prelude.<$> serverGroupId,
            ("launchOrder" Prelude..=) Prelude.<$> launchOrder,
            ("serverLaunchConfigurations" Prelude..=)
              Prelude.<$> serverLaunchConfigurations
          ]
      )
