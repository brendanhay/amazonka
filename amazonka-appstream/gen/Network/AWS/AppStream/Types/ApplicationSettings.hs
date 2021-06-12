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
-- Module      : Network.AWS.AppStream.Types.ApplicationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ApplicationSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The persistent application settings for users of a stack.
--
-- /See:/ 'newApplicationSettings' smart constructor.
data ApplicationSettings = ApplicationSettings'
  { -- | The path prefix for the S3 bucket where users’ persistent application
    -- settings are stored. You can allow the same persistent application
    -- settings to be used across multiple stacks by specifying the same
    -- settings group for each stack.
    settingsGroup :: Core.Maybe Core.Text,
    -- | Enables or disables persistent application settings for users during
    -- their streaming sessions.
    enabled :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ApplicationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'settingsGroup', 'applicationSettings_settingsGroup' - The path prefix for the S3 bucket where users’ persistent application
-- settings are stored. You can allow the same persistent application
-- settings to be used across multiple stacks by specifying the same
-- settings group for each stack.
--
-- 'enabled', 'applicationSettings_enabled' - Enables or disables persistent application settings for users during
-- their streaming sessions.
newApplicationSettings ::
  -- | 'enabled'
  Core.Bool ->
  ApplicationSettings
newApplicationSettings pEnabled_ =
  ApplicationSettings'
    { settingsGroup = Core.Nothing,
      enabled = pEnabled_
    }

-- | The path prefix for the S3 bucket where users’ persistent application
-- settings are stored. You can allow the same persistent application
-- settings to be used across multiple stacks by specifying the same
-- settings group for each stack.
applicationSettings_settingsGroup :: Lens.Lens' ApplicationSettings (Core.Maybe Core.Text)
applicationSettings_settingsGroup = Lens.lens (\ApplicationSettings' {settingsGroup} -> settingsGroup) (\s@ApplicationSettings' {} a -> s {settingsGroup = a} :: ApplicationSettings)

-- | Enables or disables persistent application settings for users during
-- their streaming sessions.
applicationSettings_enabled :: Lens.Lens' ApplicationSettings Core.Bool
applicationSettings_enabled = Lens.lens (\ApplicationSettings' {enabled} -> enabled) (\s@ApplicationSettings' {} a -> s {enabled = a} :: ApplicationSettings)

instance Core.Hashable ApplicationSettings

instance Core.NFData ApplicationSettings

instance Core.ToJSON ApplicationSettings where
  toJSON ApplicationSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SettingsGroup" Core..=) Core.<$> settingsGroup,
            Core.Just ("Enabled" Core..= enabled)
          ]
      )
