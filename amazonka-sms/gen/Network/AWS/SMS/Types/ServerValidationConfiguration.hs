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
-- Module      : Network.AWS.SMS.Types.ServerValidationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ServerValidationConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SMS.Types.Server
import Network.AWS.SMS.Types.ServerValidationStrategy
import Network.AWS.SMS.Types.UserDataValidationParameters

-- | Configuration for validating an instance.
--
-- /See:/ 'newServerValidationConfiguration' smart constructor.
data ServerValidationConfiguration = ServerValidationConfiguration'
  { -- | The ID of the validation.
    validationId :: Core.Maybe Core.Text,
    -- | The validation parameters.
    userDataValidationParameters :: Core.Maybe UserDataValidationParameters,
    server :: Core.Maybe Server,
    -- | The name of the configuration.
    name :: Core.Maybe Core.Text,
    -- | The validation strategy.
    serverValidationStrategy :: Core.Maybe ServerValidationStrategy
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ServerValidationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'validationId', 'serverValidationConfiguration_validationId' - The ID of the validation.
--
-- 'userDataValidationParameters', 'serverValidationConfiguration_userDataValidationParameters' - The validation parameters.
--
-- 'server', 'serverValidationConfiguration_server' - Undocumented member.
--
-- 'name', 'serverValidationConfiguration_name' - The name of the configuration.
--
-- 'serverValidationStrategy', 'serverValidationConfiguration_serverValidationStrategy' - The validation strategy.
newServerValidationConfiguration ::
  ServerValidationConfiguration
newServerValidationConfiguration =
  ServerValidationConfiguration'
    { validationId =
        Core.Nothing,
      userDataValidationParameters = Core.Nothing,
      server = Core.Nothing,
      name = Core.Nothing,
      serverValidationStrategy = Core.Nothing
    }

-- | The ID of the validation.
serverValidationConfiguration_validationId :: Lens.Lens' ServerValidationConfiguration (Core.Maybe Core.Text)
serverValidationConfiguration_validationId = Lens.lens (\ServerValidationConfiguration' {validationId} -> validationId) (\s@ServerValidationConfiguration' {} a -> s {validationId = a} :: ServerValidationConfiguration)

-- | The validation parameters.
serverValidationConfiguration_userDataValidationParameters :: Lens.Lens' ServerValidationConfiguration (Core.Maybe UserDataValidationParameters)
serverValidationConfiguration_userDataValidationParameters = Lens.lens (\ServerValidationConfiguration' {userDataValidationParameters} -> userDataValidationParameters) (\s@ServerValidationConfiguration' {} a -> s {userDataValidationParameters = a} :: ServerValidationConfiguration)

-- | Undocumented member.
serverValidationConfiguration_server :: Lens.Lens' ServerValidationConfiguration (Core.Maybe Server)
serverValidationConfiguration_server = Lens.lens (\ServerValidationConfiguration' {server} -> server) (\s@ServerValidationConfiguration' {} a -> s {server = a} :: ServerValidationConfiguration)

-- | The name of the configuration.
serverValidationConfiguration_name :: Lens.Lens' ServerValidationConfiguration (Core.Maybe Core.Text)
serverValidationConfiguration_name = Lens.lens (\ServerValidationConfiguration' {name} -> name) (\s@ServerValidationConfiguration' {} a -> s {name = a} :: ServerValidationConfiguration)

-- | The validation strategy.
serverValidationConfiguration_serverValidationStrategy :: Lens.Lens' ServerValidationConfiguration (Core.Maybe ServerValidationStrategy)
serverValidationConfiguration_serverValidationStrategy = Lens.lens (\ServerValidationConfiguration' {serverValidationStrategy} -> serverValidationStrategy) (\s@ServerValidationConfiguration' {} a -> s {serverValidationStrategy = a} :: ServerValidationConfiguration)

instance Core.FromJSON ServerValidationConfiguration where
  parseJSON =
    Core.withObject
      "ServerValidationConfiguration"
      ( \x ->
          ServerValidationConfiguration'
            Core.<$> (x Core..:? "validationId")
            Core.<*> (x Core..:? "userDataValidationParameters")
            Core.<*> (x Core..:? "server")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "serverValidationStrategy")
      )

instance Core.Hashable ServerValidationConfiguration

instance Core.NFData ServerValidationConfiguration

instance Core.ToJSON ServerValidationConfiguration where
  toJSON ServerValidationConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("validationId" Core..=) Core.<$> validationId,
            ("userDataValidationParameters" Core..=)
              Core.<$> userDataValidationParameters,
            ("server" Core..=) Core.<$> server,
            ("name" Core..=) Core.<$> name,
            ("serverValidationStrategy" Core..=)
              Core.<$> serverValidationStrategy
          ]
      )
