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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
  { -- | The MFA mode for the backend of your Amplify project.
    mfaMode :: Prelude.Maybe MFAMode,
    -- | The settings of your MFA configuration for the backend of your Amplify
    -- project.
    settings :: Prelude.Maybe Settings
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
-- 'mfaMode', 'updateBackendAuthMFAConfig_mfaMode' - The MFA mode for the backend of your Amplify project.
--
-- 'settings', 'updateBackendAuthMFAConfig_settings' - The settings of your MFA configuration for the backend of your Amplify
-- project.
newUpdateBackendAuthMFAConfig ::
  UpdateBackendAuthMFAConfig
newUpdateBackendAuthMFAConfig =
  UpdateBackendAuthMFAConfig'
    { mfaMode =
        Prelude.Nothing,
      settings = Prelude.Nothing
    }

-- | The MFA mode for the backend of your Amplify project.
updateBackendAuthMFAConfig_mfaMode :: Lens.Lens' UpdateBackendAuthMFAConfig (Prelude.Maybe MFAMode)
updateBackendAuthMFAConfig_mfaMode = Lens.lens (\UpdateBackendAuthMFAConfig' {mfaMode} -> mfaMode) (\s@UpdateBackendAuthMFAConfig' {} a -> s {mfaMode = a} :: UpdateBackendAuthMFAConfig)

-- | The settings of your MFA configuration for the backend of your Amplify
-- project.
updateBackendAuthMFAConfig_settings :: Lens.Lens' UpdateBackendAuthMFAConfig (Prelude.Maybe Settings)
updateBackendAuthMFAConfig_settings = Lens.lens (\UpdateBackendAuthMFAConfig' {settings} -> settings) (\s@UpdateBackendAuthMFAConfig' {} a -> s {settings = a} :: UpdateBackendAuthMFAConfig)

instance Prelude.Hashable UpdateBackendAuthMFAConfig where
  hashWithSalt _salt UpdateBackendAuthMFAConfig' {..} =
    _salt
      `Prelude.hashWithSalt` mfaMode
      `Prelude.hashWithSalt` settings

instance Prelude.NFData UpdateBackendAuthMFAConfig where
  rnf UpdateBackendAuthMFAConfig' {..} =
    Prelude.rnf mfaMode `Prelude.seq`
      Prelude.rnf settings

instance Data.ToJSON UpdateBackendAuthMFAConfig where
  toJSON UpdateBackendAuthMFAConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MFAMode" Data..=) Prelude.<$> mfaMode,
            ("settings" Data..=) Prelude.<$> settings
          ]
      )
