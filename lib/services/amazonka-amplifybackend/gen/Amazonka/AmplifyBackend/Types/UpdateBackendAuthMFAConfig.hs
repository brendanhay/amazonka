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
-- Module      : Amazonka.AmplifyBackend.Types.UpdateBackendAuthMFAConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.UpdateBackendAuthMFAConfig where

import Amazonka.AmplifyBackend.Types.MFAMode
import Amazonka.AmplifyBackend.Types.Settings
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Updates the multi-factor authentication (MFA) configuration for the
-- backend of your Amplify project.
--
-- /See:/ 'newUpdateBackendAuthMFAConfig' smart constructor.
data UpdateBackendAuthMFAConfig = UpdateBackendAuthMFAConfig'
  { -- | The settings of your MFA configuration for the backend of your Amplify
    -- project.
    settings :: Prelude.Maybe Settings,
    -- | The MFA mode for the backend of your Amplify project.
    mfaMode :: Prelude.Maybe MFAMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBackendAuthMFAConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'settings', 'updateBackendAuthMFAConfig_settings' - The settings of your MFA configuration for the backend of your Amplify
-- project.
--
-- 'mfaMode', 'updateBackendAuthMFAConfig_mfaMode' - The MFA mode for the backend of your Amplify project.
newUpdateBackendAuthMFAConfig ::
  UpdateBackendAuthMFAConfig
newUpdateBackendAuthMFAConfig =
  UpdateBackendAuthMFAConfig'
    { settings =
        Prelude.Nothing,
      mfaMode = Prelude.Nothing
    }

-- | The settings of your MFA configuration for the backend of your Amplify
-- project.
updateBackendAuthMFAConfig_settings :: Lens.Lens' UpdateBackendAuthMFAConfig (Prelude.Maybe Settings)
updateBackendAuthMFAConfig_settings = Lens.lens (\UpdateBackendAuthMFAConfig' {settings} -> settings) (\s@UpdateBackendAuthMFAConfig' {} a -> s {settings = a} :: UpdateBackendAuthMFAConfig)

-- | The MFA mode for the backend of your Amplify project.
updateBackendAuthMFAConfig_mfaMode :: Lens.Lens' UpdateBackendAuthMFAConfig (Prelude.Maybe MFAMode)
updateBackendAuthMFAConfig_mfaMode = Lens.lens (\UpdateBackendAuthMFAConfig' {mfaMode} -> mfaMode) (\s@UpdateBackendAuthMFAConfig' {} a -> s {mfaMode = a} :: UpdateBackendAuthMFAConfig)

instance Prelude.Hashable UpdateBackendAuthMFAConfig where
  hashWithSalt _salt UpdateBackendAuthMFAConfig' {..} =
    _salt `Prelude.hashWithSalt` settings
      `Prelude.hashWithSalt` mfaMode

instance Prelude.NFData UpdateBackendAuthMFAConfig where
  rnf UpdateBackendAuthMFAConfig' {..} =
    Prelude.rnf settings
      `Prelude.seq` Prelude.rnf mfaMode

instance Data.ToJSON UpdateBackendAuthMFAConfig where
  toJSON UpdateBackendAuthMFAConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("settings" Data..=) Prelude.<$> settings,
            ("MFAMode" Data..=) Prelude.<$> mfaMode
          ]
      )
