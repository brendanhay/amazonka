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
-- Module      : Amazonka.AppStream.Types.ApplicationSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.ApplicationSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The persistent application settings for users of a stack.
--
-- /See:/ 'newApplicationSettings' smart constructor.
data ApplicationSettings = ApplicationSettings'
  { -- | The path prefix for the S3 bucket where users’ persistent application
    -- settings are stored. You can allow the same persistent application
    -- settings to be used across multiple stacks by specifying the same
    -- settings group for each stack.
    settingsGroup :: Prelude.Maybe Prelude.Text,
    -- | Enables or disables persistent application settings for users during
    -- their streaming sessions.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Bool ->
  ApplicationSettings
newApplicationSettings pEnabled_ =
  ApplicationSettings'
    { settingsGroup =
        Prelude.Nothing,
      enabled = pEnabled_
    }

-- | The path prefix for the S3 bucket where users’ persistent application
-- settings are stored. You can allow the same persistent application
-- settings to be used across multiple stacks by specifying the same
-- settings group for each stack.
applicationSettings_settingsGroup :: Lens.Lens' ApplicationSettings (Prelude.Maybe Prelude.Text)
applicationSettings_settingsGroup = Lens.lens (\ApplicationSettings' {settingsGroup} -> settingsGroup) (\s@ApplicationSettings' {} a -> s {settingsGroup = a} :: ApplicationSettings)

-- | Enables or disables persistent application settings for users during
-- their streaming sessions.
applicationSettings_enabled :: Lens.Lens' ApplicationSettings Prelude.Bool
applicationSettings_enabled = Lens.lens (\ApplicationSettings' {enabled} -> enabled) (\s@ApplicationSettings' {} a -> s {enabled = a} :: ApplicationSettings)

instance Prelude.Hashable ApplicationSettings where
  hashWithSalt _salt ApplicationSettings' {..} =
    _salt
      `Prelude.hashWithSalt` settingsGroup
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData ApplicationSettings where
  rnf ApplicationSettings' {..} =
    Prelude.rnf settingsGroup
      `Prelude.seq` Prelude.rnf enabled

instance Data.ToJSON ApplicationSettings where
  toJSON ApplicationSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SettingsGroup" Data..=) Prelude.<$> settingsGroup,
            Prelude.Just ("Enabled" Data..= enabled)
          ]
      )
