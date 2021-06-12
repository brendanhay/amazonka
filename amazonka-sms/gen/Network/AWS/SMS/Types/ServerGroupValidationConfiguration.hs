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
-- Module      : Network.AWS.SMS.Types.ServerGroupValidationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ServerGroupValidationConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SMS.Types.ServerValidationConfiguration

-- | Configuration for validating an instance.
--
-- /See:/ 'newServerGroupValidationConfiguration' smart constructor.
data ServerGroupValidationConfiguration = ServerGroupValidationConfiguration'
  { -- | The ID of the server group.
    serverGroupId :: Core.Maybe Core.Text,
    -- | The validation configuration.
    serverValidationConfigurations :: Core.Maybe [ServerValidationConfiguration]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ServerGroupValidationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverGroupId', 'serverGroupValidationConfiguration_serverGroupId' - The ID of the server group.
--
-- 'serverValidationConfigurations', 'serverGroupValidationConfiguration_serverValidationConfigurations' - The validation configuration.
newServerGroupValidationConfiguration ::
  ServerGroupValidationConfiguration
newServerGroupValidationConfiguration =
  ServerGroupValidationConfiguration'
    { serverGroupId =
        Core.Nothing,
      serverValidationConfigurations =
        Core.Nothing
    }

-- | The ID of the server group.
serverGroupValidationConfiguration_serverGroupId :: Lens.Lens' ServerGroupValidationConfiguration (Core.Maybe Core.Text)
serverGroupValidationConfiguration_serverGroupId = Lens.lens (\ServerGroupValidationConfiguration' {serverGroupId} -> serverGroupId) (\s@ServerGroupValidationConfiguration' {} a -> s {serverGroupId = a} :: ServerGroupValidationConfiguration)

-- | The validation configuration.
serverGroupValidationConfiguration_serverValidationConfigurations :: Lens.Lens' ServerGroupValidationConfiguration (Core.Maybe [ServerValidationConfiguration])
serverGroupValidationConfiguration_serverValidationConfigurations = Lens.lens (\ServerGroupValidationConfiguration' {serverValidationConfigurations} -> serverValidationConfigurations) (\s@ServerGroupValidationConfiguration' {} a -> s {serverValidationConfigurations = a} :: ServerGroupValidationConfiguration) Core.. Lens.mapping Lens._Coerce

instance
  Core.FromJSON
    ServerGroupValidationConfiguration
  where
  parseJSON =
    Core.withObject
      "ServerGroupValidationConfiguration"
      ( \x ->
          ServerGroupValidationConfiguration'
            Core.<$> (x Core..:? "serverGroupId")
            Core.<*> ( x Core..:? "serverValidationConfigurations"
                         Core..!= Core.mempty
                     )
      )

instance
  Core.Hashable
    ServerGroupValidationConfiguration

instance
  Core.NFData
    ServerGroupValidationConfiguration

instance
  Core.ToJSON
    ServerGroupValidationConfiguration
  where
  toJSON ServerGroupValidationConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("serverGroupId" Core..=) Core.<$> serverGroupId,
            ("serverValidationConfigurations" Core..=)
              Core.<$> serverValidationConfigurations
          ]
      )
