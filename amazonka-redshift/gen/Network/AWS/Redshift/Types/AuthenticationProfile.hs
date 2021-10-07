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
-- Module      : Network.AWS.Redshift.Types.AuthenticationProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.AuthenticationProfile where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal

-- | Describes an authentication profile.
--
-- /See:/ 'newAuthenticationProfile' smart constructor.
data AuthenticationProfile = AuthenticationProfile'
  { -- | The name of the authentication profile.
    authenticationProfileName :: Prelude.Maybe Prelude.Text,
    -- | The content of the authentication profile in JSON format. The maximum
    -- length of the JSON string is determined by a quota for your account.
    authenticationProfileContent :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuthenticationProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationProfileName', 'authenticationProfile_authenticationProfileName' - The name of the authentication profile.
--
-- 'authenticationProfileContent', 'authenticationProfile_authenticationProfileContent' - The content of the authentication profile in JSON format. The maximum
-- length of the JSON string is determined by a quota for your account.
newAuthenticationProfile ::
  AuthenticationProfile
newAuthenticationProfile =
  AuthenticationProfile'
    { authenticationProfileName =
        Prelude.Nothing,
      authenticationProfileContent = Prelude.Nothing
    }

-- | The name of the authentication profile.
authenticationProfile_authenticationProfileName :: Lens.Lens' AuthenticationProfile (Prelude.Maybe Prelude.Text)
authenticationProfile_authenticationProfileName = Lens.lens (\AuthenticationProfile' {authenticationProfileName} -> authenticationProfileName) (\s@AuthenticationProfile' {} a -> s {authenticationProfileName = a} :: AuthenticationProfile)

-- | The content of the authentication profile in JSON format. The maximum
-- length of the JSON string is determined by a quota for your account.
authenticationProfile_authenticationProfileContent :: Lens.Lens' AuthenticationProfile (Prelude.Maybe Prelude.Text)
authenticationProfile_authenticationProfileContent = Lens.lens (\AuthenticationProfile' {authenticationProfileContent} -> authenticationProfileContent) (\s@AuthenticationProfile' {} a -> s {authenticationProfileContent = a} :: AuthenticationProfile)

instance Core.FromXML AuthenticationProfile where
  parseXML x =
    AuthenticationProfile'
      Prelude.<$> (x Core..@? "AuthenticationProfileName")
      Prelude.<*> (x Core..@? "AuthenticationProfileContent")

instance Prelude.Hashable AuthenticationProfile

instance Prelude.NFData AuthenticationProfile
