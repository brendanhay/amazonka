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
-- Module      : Amazonka.AmplifyBackend.Types.UpdateBackendAuthPasswordPolicyConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.UpdateBackendAuthPasswordPolicyConfig where

import Amazonka.AmplifyBackend.Types.AdditionalConstraintsElement
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the password policy for your Amazon Cognito user pool
-- configured as a part of your Amplify project.
--
-- /See:/ 'newUpdateBackendAuthPasswordPolicyConfig' smart constructor.
data UpdateBackendAuthPasswordPolicyConfig = UpdateBackendAuthPasswordPolicyConfig'
  { -- | Describes additional constraints on password requirements to sign in to
    -- the auth resource, configured as a part of your Amplify project.
    additionalConstraints :: Prelude.Maybe [AdditionalConstraintsElement],
    -- | Describes the minimum length of the password required to sign in to the
    -- auth resource, configured as a part of your Amplify project.
    minimumLength :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBackendAuthPasswordPolicyConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalConstraints', 'updateBackendAuthPasswordPolicyConfig_additionalConstraints' - Describes additional constraints on password requirements to sign in to
-- the auth resource, configured as a part of your Amplify project.
--
-- 'minimumLength', 'updateBackendAuthPasswordPolicyConfig_minimumLength' - Describes the minimum length of the password required to sign in to the
-- auth resource, configured as a part of your Amplify project.
newUpdateBackendAuthPasswordPolicyConfig ::
  UpdateBackendAuthPasswordPolicyConfig
newUpdateBackendAuthPasswordPolicyConfig =
  UpdateBackendAuthPasswordPolicyConfig'
    { additionalConstraints =
        Prelude.Nothing,
      minimumLength = Prelude.Nothing
    }

-- | Describes additional constraints on password requirements to sign in to
-- the auth resource, configured as a part of your Amplify project.
updateBackendAuthPasswordPolicyConfig_additionalConstraints :: Lens.Lens' UpdateBackendAuthPasswordPolicyConfig (Prelude.Maybe [AdditionalConstraintsElement])
updateBackendAuthPasswordPolicyConfig_additionalConstraints = Lens.lens (\UpdateBackendAuthPasswordPolicyConfig' {additionalConstraints} -> additionalConstraints) (\s@UpdateBackendAuthPasswordPolicyConfig' {} a -> s {additionalConstraints = a} :: UpdateBackendAuthPasswordPolicyConfig) Prelude.. Lens.mapping Lens.coerced

-- | Describes the minimum length of the password required to sign in to the
-- auth resource, configured as a part of your Amplify project.
updateBackendAuthPasswordPolicyConfig_minimumLength :: Lens.Lens' UpdateBackendAuthPasswordPolicyConfig (Prelude.Maybe Prelude.Double)
updateBackendAuthPasswordPolicyConfig_minimumLength = Lens.lens (\UpdateBackendAuthPasswordPolicyConfig' {minimumLength} -> minimumLength) (\s@UpdateBackendAuthPasswordPolicyConfig' {} a -> s {minimumLength = a} :: UpdateBackendAuthPasswordPolicyConfig)

instance
  Prelude.Hashable
    UpdateBackendAuthPasswordPolicyConfig
  where
  hashWithSalt
    _salt
    UpdateBackendAuthPasswordPolicyConfig' {..} =
      _salt `Prelude.hashWithSalt` additionalConstraints
        `Prelude.hashWithSalt` minimumLength

instance
  Prelude.NFData
    UpdateBackendAuthPasswordPolicyConfig
  where
  rnf UpdateBackendAuthPasswordPolicyConfig' {..} =
    Prelude.rnf additionalConstraints
      `Prelude.seq` Prelude.rnf minimumLength

instance
  Core.ToJSON
    UpdateBackendAuthPasswordPolicyConfig
  where
  toJSON UpdateBackendAuthPasswordPolicyConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("additionalConstraints" Core..=)
              Prelude.<$> additionalConstraints,
            ("minimumLength" Core..=) Prelude.<$> minimumLength
          ]
      )
