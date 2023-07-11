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
-- Module      : Amazonka.AmplifyBackend.Types.CreateBackendAuthMFAConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.CreateBackendAuthMFAConfig where

import Amazonka.AmplifyBackend.Types.MFAMode
import Amazonka.AmplifyBackend.Types.Settings
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes whether to apply multi-factor authentication policies for your
-- Amazon Cognito user pool configured as a part of your Amplify project.
--
-- /See:/ 'newCreateBackendAuthMFAConfig' smart constructor.
data CreateBackendAuthMFAConfig = CreateBackendAuthMFAConfig'
  { -- | Describes the configuration settings and methods for your Amplify app
    -- users to use MFA.
    settings :: Prelude.Maybe Settings,
    -- | Describes whether MFA should be [ON, OFF, or OPTIONAL] for
    -- authentication in your Amplify project.
    mfaMode :: MFAMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBackendAuthMFAConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'settings', 'createBackendAuthMFAConfig_settings' - Describes the configuration settings and methods for your Amplify app
-- users to use MFA.
--
-- 'mfaMode', 'createBackendAuthMFAConfig_mfaMode' - Describes whether MFA should be [ON, OFF, or OPTIONAL] for
-- authentication in your Amplify project.
newCreateBackendAuthMFAConfig ::
  -- | 'mfaMode'
  MFAMode ->
  CreateBackendAuthMFAConfig
newCreateBackendAuthMFAConfig pMFAMode_ =
  CreateBackendAuthMFAConfig'
    { settings =
        Prelude.Nothing,
      mfaMode = pMFAMode_
    }

-- | Describes the configuration settings and methods for your Amplify app
-- users to use MFA.
createBackendAuthMFAConfig_settings :: Lens.Lens' CreateBackendAuthMFAConfig (Prelude.Maybe Settings)
createBackendAuthMFAConfig_settings = Lens.lens (\CreateBackendAuthMFAConfig' {settings} -> settings) (\s@CreateBackendAuthMFAConfig' {} a -> s {settings = a} :: CreateBackendAuthMFAConfig)

-- | Describes whether MFA should be [ON, OFF, or OPTIONAL] for
-- authentication in your Amplify project.
createBackendAuthMFAConfig_mfaMode :: Lens.Lens' CreateBackendAuthMFAConfig MFAMode
createBackendAuthMFAConfig_mfaMode = Lens.lens (\CreateBackendAuthMFAConfig' {mfaMode} -> mfaMode) (\s@CreateBackendAuthMFAConfig' {} a -> s {mfaMode = a} :: CreateBackendAuthMFAConfig)

instance Data.FromJSON CreateBackendAuthMFAConfig where
  parseJSON =
    Data.withObject
      "CreateBackendAuthMFAConfig"
      ( \x ->
          CreateBackendAuthMFAConfig'
            Prelude.<$> (x Data..:? "settings")
            Prelude.<*> (x Data..: "MFAMode")
      )

instance Prelude.Hashable CreateBackendAuthMFAConfig where
  hashWithSalt _salt CreateBackendAuthMFAConfig' {..} =
    _salt
      `Prelude.hashWithSalt` settings
      `Prelude.hashWithSalt` mfaMode

instance Prelude.NFData CreateBackendAuthMFAConfig where
  rnf CreateBackendAuthMFAConfig' {..} =
    Prelude.rnf settings
      `Prelude.seq` Prelude.rnf mfaMode

instance Data.ToJSON CreateBackendAuthMFAConfig where
  toJSON CreateBackendAuthMFAConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("settings" Data..=) Prelude.<$> settings,
            Prelude.Just ("MFAMode" Data..= mfaMode)
          ]
      )
