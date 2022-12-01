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
-- Module      : Amazonka.CognitoIdentityProvider.Types.SoftwareTokenMfaSettingsType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.SoftwareTokenMfaSettingsType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The type used for enabling software token MFA at the user level. If an
-- MFA type is activated for a user, the user will be prompted for MFA
-- during all sign-in attempts, unless device tracking is turned on and the
-- device has been trusted. If you want MFA to be applied selectively based
-- on the assessed risk level of sign-in attempts, deactivate MFA for users
-- and turn on Adaptive Authentication for the user pool.
--
-- /See:/ 'newSoftwareTokenMfaSettingsType' smart constructor.
data SoftwareTokenMfaSettingsType = SoftwareTokenMfaSettingsType'
  { -- | Specifies whether software token MFA is the preferred MFA method.
    preferredMfa :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether software token MFA is activated. If an MFA type is
    -- activated for a user, the user will be prompted for MFA during all
    -- sign-in attempts, unless device tracking is turned on and the device has
    -- been trusted.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SoftwareTokenMfaSettingsType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'preferredMfa', 'softwareTokenMfaSettingsType_preferredMfa' - Specifies whether software token MFA is the preferred MFA method.
--
-- 'enabled', 'softwareTokenMfaSettingsType_enabled' - Specifies whether software token MFA is activated. If an MFA type is
-- activated for a user, the user will be prompted for MFA during all
-- sign-in attempts, unless device tracking is turned on and the device has
-- been trusted.
newSoftwareTokenMfaSettingsType ::
  SoftwareTokenMfaSettingsType
newSoftwareTokenMfaSettingsType =
  SoftwareTokenMfaSettingsType'
    { preferredMfa =
        Prelude.Nothing,
      enabled = Prelude.Nothing
    }

-- | Specifies whether software token MFA is the preferred MFA method.
softwareTokenMfaSettingsType_preferredMfa :: Lens.Lens' SoftwareTokenMfaSettingsType (Prelude.Maybe Prelude.Bool)
softwareTokenMfaSettingsType_preferredMfa = Lens.lens (\SoftwareTokenMfaSettingsType' {preferredMfa} -> preferredMfa) (\s@SoftwareTokenMfaSettingsType' {} a -> s {preferredMfa = a} :: SoftwareTokenMfaSettingsType)

-- | Specifies whether software token MFA is activated. If an MFA type is
-- activated for a user, the user will be prompted for MFA during all
-- sign-in attempts, unless device tracking is turned on and the device has
-- been trusted.
softwareTokenMfaSettingsType_enabled :: Lens.Lens' SoftwareTokenMfaSettingsType (Prelude.Maybe Prelude.Bool)
softwareTokenMfaSettingsType_enabled = Lens.lens (\SoftwareTokenMfaSettingsType' {enabled} -> enabled) (\s@SoftwareTokenMfaSettingsType' {} a -> s {enabled = a} :: SoftwareTokenMfaSettingsType)

instance
  Prelude.Hashable
    SoftwareTokenMfaSettingsType
  where
  hashWithSalt _salt SoftwareTokenMfaSettingsType' {..} =
    _salt `Prelude.hashWithSalt` preferredMfa
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData SoftwareTokenMfaSettingsType where
  rnf SoftwareTokenMfaSettingsType' {..} =
    Prelude.rnf preferredMfa
      `Prelude.seq` Prelude.rnf enabled

instance Core.ToJSON SoftwareTokenMfaSettingsType where
  toJSON SoftwareTokenMfaSettingsType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PreferredMfa" Core..=) Prelude.<$> preferredMfa,
            ("Enabled" Core..=) Prelude.<$> enabled
          ]
      )
