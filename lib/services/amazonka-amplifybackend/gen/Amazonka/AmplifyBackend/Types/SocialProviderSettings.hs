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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.SocialProviderSettings where

import Amazonka.AmplifyBackend.Types.BackendAuthAppleProviderConfig
import Amazonka.AmplifyBackend.Types.BackendAuthSocialProviderConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The settings for using the social identity providers for access to your
-- Amplify app.
--
-- /See:/ 'newSocialProviderSettings' smart constructor.
data SocialProviderSettings = SocialProviderSettings'
  { loginWithAmazon :: Prelude.Maybe BackendAuthSocialProviderConfig,
    signInWithApple :: Prelude.Maybe BackendAuthAppleProviderConfig,
    facebook :: Prelude.Maybe BackendAuthSocialProviderConfig,
    google :: Prelude.Maybe BackendAuthSocialProviderConfig
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
-- 'loginWithAmazon', 'socialProviderSettings_loginWithAmazon' - Undocumented member.
--
-- 'signInWithApple', 'socialProviderSettings_signInWithApple' - Undocumented member.
--
-- 'facebook', 'socialProviderSettings_facebook' - Undocumented member.
--
-- 'google', 'socialProviderSettings_google' - Undocumented member.
newSocialProviderSettings ::
  SocialProviderSettings
newSocialProviderSettings =
  SocialProviderSettings'
    { loginWithAmazon =
        Prelude.Nothing,
      signInWithApple = Prelude.Nothing,
      facebook = Prelude.Nothing,
      google = Prelude.Nothing
    }

-- | Undocumented member.
socialProviderSettings_loginWithAmazon :: Lens.Lens' SocialProviderSettings (Prelude.Maybe BackendAuthSocialProviderConfig)
socialProviderSettings_loginWithAmazon = Lens.lens (\SocialProviderSettings' {loginWithAmazon} -> loginWithAmazon) (\s@SocialProviderSettings' {} a -> s {loginWithAmazon = a} :: SocialProviderSettings)

-- | Undocumented member.
socialProviderSettings_signInWithApple :: Lens.Lens' SocialProviderSettings (Prelude.Maybe BackendAuthAppleProviderConfig)
socialProviderSettings_signInWithApple = Lens.lens (\SocialProviderSettings' {signInWithApple} -> signInWithApple) (\s@SocialProviderSettings' {} a -> s {signInWithApple = a} :: SocialProviderSettings)

-- | Undocumented member.
socialProviderSettings_facebook :: Lens.Lens' SocialProviderSettings (Prelude.Maybe BackendAuthSocialProviderConfig)
socialProviderSettings_facebook = Lens.lens (\SocialProviderSettings' {facebook} -> facebook) (\s@SocialProviderSettings' {} a -> s {facebook = a} :: SocialProviderSettings)

-- | Undocumented member.
socialProviderSettings_google :: Lens.Lens' SocialProviderSettings (Prelude.Maybe BackendAuthSocialProviderConfig)
socialProviderSettings_google = Lens.lens (\SocialProviderSettings' {google} -> google) (\s@SocialProviderSettings' {} a -> s {google = a} :: SocialProviderSettings)

instance Core.FromJSON SocialProviderSettings where
  parseJSON =
    Core.withObject
      "SocialProviderSettings"
      ( \x ->
          SocialProviderSettings'
            Prelude.<$> (x Core..:? "LoginWithAmazon")
            Prelude.<*> (x Core..:? "SignInWithApple")
            Prelude.<*> (x Core..:? "Facebook")
            Prelude.<*> (x Core..:? "Google")
      )

instance Prelude.Hashable SocialProviderSettings where
  hashWithSalt _salt SocialProviderSettings' {..} =
    _salt `Prelude.hashWithSalt` loginWithAmazon
      `Prelude.hashWithSalt` signInWithApple
      `Prelude.hashWithSalt` facebook
      `Prelude.hashWithSalt` google

instance Prelude.NFData SocialProviderSettings where
  rnf SocialProviderSettings' {..} =
    Prelude.rnf loginWithAmazon
      `Prelude.seq` Prelude.rnf signInWithApple
      `Prelude.seq` Prelude.rnf facebook
      `Prelude.seq` Prelude.rnf google

instance Core.ToJSON SocialProviderSettings where
  toJSON SocialProviderSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("LoginWithAmazon" Core..=)
              Prelude.<$> loginWithAmazon,
            ("SignInWithApple" Core..=)
              Prelude.<$> signInWithApple,
            ("Facebook" Core..=) Prelude.<$> facebook,
            ("Google" Core..=) Prelude.<$> google
          ]
      )
