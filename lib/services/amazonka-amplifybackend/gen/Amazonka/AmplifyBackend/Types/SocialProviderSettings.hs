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
-- Module      : Amazonka.AmplifyBackend.Types.SocialProviderSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.SocialProviderSettings where

import Amazonka.AmplifyBackend.Types.BackendAuthAppleProviderConfig
import Amazonka.AmplifyBackend.Types.BackendAuthSocialProviderConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The settings for using the social identity providers for access to your
-- Amplify app.
--
-- /See:/ 'newSocialProviderSettings' smart constructor.
data SocialProviderSettings = SocialProviderSettings'
  { facebook :: Prelude.Maybe BackendAuthSocialProviderConfig,
    google :: Prelude.Maybe BackendAuthSocialProviderConfig,
    loginWithAmazon :: Prelude.Maybe BackendAuthSocialProviderConfig,
    signInWithApple :: Prelude.Maybe BackendAuthAppleProviderConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SocialProviderSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'facebook', 'socialProviderSettings_facebook' - Undocumented member.
--
-- 'google', 'socialProviderSettings_google' - Undocumented member.
--
-- 'loginWithAmazon', 'socialProviderSettings_loginWithAmazon' - Undocumented member.
--
-- 'signInWithApple', 'socialProviderSettings_signInWithApple' - Undocumented member.
newSocialProviderSettings ::
  SocialProviderSettings
newSocialProviderSettings =
  SocialProviderSettings'
    { facebook = Prelude.Nothing,
      google = Prelude.Nothing,
      loginWithAmazon = Prelude.Nothing,
      signInWithApple = Prelude.Nothing
    }

-- | Undocumented member.
socialProviderSettings_facebook :: Lens.Lens' SocialProviderSettings (Prelude.Maybe BackendAuthSocialProviderConfig)
socialProviderSettings_facebook = Lens.lens (\SocialProviderSettings' {facebook} -> facebook) (\s@SocialProviderSettings' {} a -> s {facebook = a} :: SocialProviderSettings)

-- | Undocumented member.
socialProviderSettings_google :: Lens.Lens' SocialProviderSettings (Prelude.Maybe BackendAuthSocialProviderConfig)
socialProviderSettings_google = Lens.lens (\SocialProviderSettings' {google} -> google) (\s@SocialProviderSettings' {} a -> s {google = a} :: SocialProviderSettings)

-- | Undocumented member.
socialProviderSettings_loginWithAmazon :: Lens.Lens' SocialProviderSettings (Prelude.Maybe BackendAuthSocialProviderConfig)
socialProviderSettings_loginWithAmazon = Lens.lens (\SocialProviderSettings' {loginWithAmazon} -> loginWithAmazon) (\s@SocialProviderSettings' {} a -> s {loginWithAmazon = a} :: SocialProviderSettings)

-- | Undocumented member.
socialProviderSettings_signInWithApple :: Lens.Lens' SocialProviderSettings (Prelude.Maybe BackendAuthAppleProviderConfig)
socialProviderSettings_signInWithApple = Lens.lens (\SocialProviderSettings' {signInWithApple} -> signInWithApple) (\s@SocialProviderSettings' {} a -> s {signInWithApple = a} :: SocialProviderSettings)

instance Data.FromJSON SocialProviderSettings where
  parseJSON =
    Data.withObject
      "SocialProviderSettings"
      ( \x ->
          SocialProviderSettings'
            Prelude.<$> (x Data..:? "Facebook")
            Prelude.<*> (x Data..:? "Google")
            Prelude.<*> (x Data..:? "LoginWithAmazon")
            Prelude.<*> (x Data..:? "SignInWithApple")
      )

instance Prelude.Hashable SocialProviderSettings where
  hashWithSalt _salt SocialProviderSettings' {..} =
    _salt
      `Prelude.hashWithSalt` facebook
      `Prelude.hashWithSalt` google
      `Prelude.hashWithSalt` loginWithAmazon
      `Prelude.hashWithSalt` signInWithApple

instance Prelude.NFData SocialProviderSettings where
  rnf SocialProviderSettings' {..} =
    Prelude.rnf facebook
      `Prelude.seq` Prelude.rnf google
      `Prelude.seq` Prelude.rnf loginWithAmazon
      `Prelude.seq` Prelude.rnf signInWithApple

instance Data.ToJSON SocialProviderSettings where
  toJSON SocialProviderSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Facebook" Data..=) Prelude.<$> facebook,
            ("Google" Data..=) Prelude.<$> google,
            ("LoginWithAmazon" Data..=)
              Prelude.<$> loginWithAmazon,
            ("SignInWithApple" Data..=)
              Prelude.<$> signInWithApple
          ]
      )
