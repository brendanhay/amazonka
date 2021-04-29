{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.SMSMfaSettingsType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.SMSMfaSettingsType where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The type used for enabling SMS MFA at the user level. Phone numbers
-- don\'t need to be verified to be used for SMS MFA. If an MFA type is
-- enabled for a user, the user will be prompted for MFA during all sign in
-- attempts, unless device tracking is turned on and the device has been
-- trusted. If you would like MFA to be applied selectively based on the
-- assessed risk level of sign in attempts, disable MFA for users and turn
-- on Adaptive Authentication for the user pool.
--
-- /See:/ 'newSMSMfaSettingsType' smart constructor.
data SMSMfaSettingsType = SMSMfaSettingsType'
  { -- | Specifies whether SMS text message MFA is enabled. If an MFA type is
    -- enabled for a user, the user will be prompted for MFA during all sign in
    -- attempts, unless device tracking is turned on and the device has been
    -- trusted.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether SMS is the preferred MFA method.
    preferredMfa :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SMSMfaSettingsType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'sMSMfaSettingsType_enabled' - Specifies whether SMS text message MFA is enabled. If an MFA type is
-- enabled for a user, the user will be prompted for MFA during all sign in
-- attempts, unless device tracking is turned on and the device has been
-- trusted.
--
-- 'preferredMfa', 'sMSMfaSettingsType_preferredMfa' - Specifies whether SMS is the preferred MFA method.
newSMSMfaSettingsType ::
  SMSMfaSettingsType
newSMSMfaSettingsType =
  SMSMfaSettingsType'
    { enabled = Prelude.Nothing,
      preferredMfa = Prelude.Nothing
    }

-- | Specifies whether SMS text message MFA is enabled. If an MFA type is
-- enabled for a user, the user will be prompted for MFA during all sign in
-- attempts, unless device tracking is turned on and the device has been
-- trusted.
sMSMfaSettingsType_enabled :: Lens.Lens' SMSMfaSettingsType (Prelude.Maybe Prelude.Bool)
sMSMfaSettingsType_enabled = Lens.lens (\SMSMfaSettingsType' {enabled} -> enabled) (\s@SMSMfaSettingsType' {} a -> s {enabled = a} :: SMSMfaSettingsType)

-- | Specifies whether SMS is the preferred MFA method.
sMSMfaSettingsType_preferredMfa :: Lens.Lens' SMSMfaSettingsType (Prelude.Maybe Prelude.Bool)
sMSMfaSettingsType_preferredMfa = Lens.lens (\SMSMfaSettingsType' {preferredMfa} -> preferredMfa) (\s@SMSMfaSettingsType' {} a -> s {preferredMfa = a} :: SMSMfaSettingsType)

instance Prelude.Hashable SMSMfaSettingsType

instance Prelude.NFData SMSMfaSettingsType

instance Prelude.ToJSON SMSMfaSettingsType where
  toJSON SMSMfaSettingsType' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Enabled" Prelude..=) Prelude.<$> enabled,
            ("PreferredMfa" Prelude..=)
              Prelude.<$> preferredMfa
          ]
      )
