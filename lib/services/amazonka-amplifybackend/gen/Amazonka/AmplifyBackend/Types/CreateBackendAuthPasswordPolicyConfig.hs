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
-- Module      : Amazonka.AmplifyBackend.Types.CreateBackendAuthPasswordPolicyConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.CreateBackendAuthPasswordPolicyConfig where

import Amazonka.AmplifyBackend.Types.AdditionalConstraintsElement
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The password policy configuration for the backend to your Amplify
-- project.
--
-- /See:/ 'newCreateBackendAuthPasswordPolicyConfig' smart constructor.
data CreateBackendAuthPasswordPolicyConfig = CreateBackendAuthPasswordPolicyConfig'
  { -- | Additional constraints for the password used to access the backend of
    -- your Amplify project.
    additionalConstraints :: Prelude.Maybe [AdditionalConstraintsElement],
    -- | The minimum length of the password used to access the backend of your
    -- Amplify project.
    minimumLength :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBackendAuthPasswordPolicyConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalConstraints', 'createBackendAuthPasswordPolicyConfig_additionalConstraints' - Additional constraints for the password used to access the backend of
-- your Amplify project.
--
-- 'minimumLength', 'createBackendAuthPasswordPolicyConfig_minimumLength' - The minimum length of the password used to access the backend of your
-- Amplify project.
newCreateBackendAuthPasswordPolicyConfig ::
  -- | 'minimumLength'
  Prelude.Double ->
  CreateBackendAuthPasswordPolicyConfig
newCreateBackendAuthPasswordPolicyConfig
  pMinimumLength_ =
    CreateBackendAuthPasswordPolicyConfig'
      { additionalConstraints =
          Prelude.Nothing,
        minimumLength = pMinimumLength_
      }

-- | Additional constraints for the password used to access the backend of
-- your Amplify project.
createBackendAuthPasswordPolicyConfig_additionalConstraints :: Lens.Lens' CreateBackendAuthPasswordPolicyConfig (Prelude.Maybe [AdditionalConstraintsElement])
createBackendAuthPasswordPolicyConfig_additionalConstraints = Lens.lens (\CreateBackendAuthPasswordPolicyConfig' {additionalConstraints} -> additionalConstraints) (\s@CreateBackendAuthPasswordPolicyConfig' {} a -> s {additionalConstraints = a} :: CreateBackendAuthPasswordPolicyConfig) Prelude.. Lens.mapping Lens.coerced

-- | The minimum length of the password used to access the backend of your
-- Amplify project.
createBackendAuthPasswordPolicyConfig_minimumLength :: Lens.Lens' CreateBackendAuthPasswordPolicyConfig Prelude.Double
createBackendAuthPasswordPolicyConfig_minimumLength = Lens.lens (\CreateBackendAuthPasswordPolicyConfig' {minimumLength} -> minimumLength) (\s@CreateBackendAuthPasswordPolicyConfig' {} a -> s {minimumLength = a} :: CreateBackendAuthPasswordPolicyConfig)

instance
  Core.FromJSON
    CreateBackendAuthPasswordPolicyConfig
  where
  parseJSON =
    Core.withObject
      "CreateBackendAuthPasswordPolicyConfig"
      ( \x ->
          CreateBackendAuthPasswordPolicyConfig'
            Prelude.<$> ( x Core..:? "additionalConstraints"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "minimumLength")
      )

instance
  Prelude.Hashable
    CreateBackendAuthPasswordPolicyConfig
  where
  hashWithSalt
    _salt
    CreateBackendAuthPasswordPolicyConfig' {..} =
      _salt `Prelude.hashWithSalt` additionalConstraints
        `Prelude.hashWithSalt` minimumLength

instance
  Prelude.NFData
    CreateBackendAuthPasswordPolicyConfig
  where
  rnf CreateBackendAuthPasswordPolicyConfig' {..} =
    Prelude.rnf additionalConstraints
      `Prelude.seq` Prelude.rnf minimumLength

instance
  Core.ToJSON
    CreateBackendAuthPasswordPolicyConfig
  where
  toJSON CreateBackendAuthPasswordPolicyConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("additionalConstraints" Core..=)
              Prelude.<$> additionalConstraints,
            Prelude.Just
              ("minimumLength" Core..= minimumLength)
          ]
      )
