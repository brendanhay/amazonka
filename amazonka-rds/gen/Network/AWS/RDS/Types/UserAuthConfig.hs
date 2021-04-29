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
-- Module      : Network.AWS.RDS.Types.UserAuthConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.UserAuthConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types.AuthScheme
import Network.AWS.RDS.Types.IAMAuthMode

-- | Specifies the details of authentication used by a proxy to log in as a
-- specific database user.
--
-- /See:/ 'newUserAuthConfig' smart constructor.
data UserAuthConfig = UserAuthConfig'
  { -- | The Amazon Resource Name (ARN) representing the secret that the proxy
    -- uses to authenticate to the RDS DB instance or Aurora DB cluster. These
    -- secrets are stored within Amazon Secrets Manager.
    secretArn :: Prelude.Maybe Prelude.Text,
    -- | Whether to require or disallow AWS Identity and Access Management (IAM)
    -- authentication for connections to the proxy.
    iAMAuth :: Prelude.Maybe IAMAuthMode,
    -- | The type of authentication that the proxy uses for connections from the
    -- proxy to the underlying database.
    authScheme :: Prelude.Maybe AuthScheme,
    -- | The name of the database user to which the proxy connects.
    userName :: Prelude.Maybe Prelude.Text,
    -- | A user-specified description about the authentication used by a proxy to
    -- log in as a specific database user.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UserAuthConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secretArn', 'userAuthConfig_secretArn' - The Amazon Resource Name (ARN) representing the secret that the proxy
-- uses to authenticate to the RDS DB instance or Aurora DB cluster. These
-- secrets are stored within Amazon Secrets Manager.
--
-- 'iAMAuth', 'userAuthConfig_iAMAuth' - Whether to require or disallow AWS Identity and Access Management (IAM)
-- authentication for connections to the proxy.
--
-- 'authScheme', 'userAuthConfig_authScheme' - The type of authentication that the proxy uses for connections from the
-- proxy to the underlying database.
--
-- 'userName', 'userAuthConfig_userName' - The name of the database user to which the proxy connects.
--
-- 'description', 'userAuthConfig_description' - A user-specified description about the authentication used by a proxy to
-- log in as a specific database user.
newUserAuthConfig ::
  UserAuthConfig
newUserAuthConfig =
  UserAuthConfig'
    { secretArn = Prelude.Nothing,
      iAMAuth = Prelude.Nothing,
      authScheme = Prelude.Nothing,
      userName = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) representing the secret that the proxy
-- uses to authenticate to the RDS DB instance or Aurora DB cluster. These
-- secrets are stored within Amazon Secrets Manager.
userAuthConfig_secretArn :: Lens.Lens' UserAuthConfig (Prelude.Maybe Prelude.Text)
userAuthConfig_secretArn = Lens.lens (\UserAuthConfig' {secretArn} -> secretArn) (\s@UserAuthConfig' {} a -> s {secretArn = a} :: UserAuthConfig)

-- | Whether to require or disallow AWS Identity and Access Management (IAM)
-- authentication for connections to the proxy.
userAuthConfig_iAMAuth :: Lens.Lens' UserAuthConfig (Prelude.Maybe IAMAuthMode)
userAuthConfig_iAMAuth = Lens.lens (\UserAuthConfig' {iAMAuth} -> iAMAuth) (\s@UserAuthConfig' {} a -> s {iAMAuth = a} :: UserAuthConfig)

-- | The type of authentication that the proxy uses for connections from the
-- proxy to the underlying database.
userAuthConfig_authScheme :: Lens.Lens' UserAuthConfig (Prelude.Maybe AuthScheme)
userAuthConfig_authScheme = Lens.lens (\UserAuthConfig' {authScheme} -> authScheme) (\s@UserAuthConfig' {} a -> s {authScheme = a} :: UserAuthConfig)

-- | The name of the database user to which the proxy connects.
userAuthConfig_userName :: Lens.Lens' UserAuthConfig (Prelude.Maybe Prelude.Text)
userAuthConfig_userName = Lens.lens (\UserAuthConfig' {userName} -> userName) (\s@UserAuthConfig' {} a -> s {userName = a} :: UserAuthConfig)

-- | A user-specified description about the authentication used by a proxy to
-- log in as a specific database user.
userAuthConfig_description :: Lens.Lens' UserAuthConfig (Prelude.Maybe Prelude.Text)
userAuthConfig_description = Lens.lens (\UserAuthConfig' {description} -> description) (\s@UserAuthConfig' {} a -> s {description = a} :: UserAuthConfig)

instance Prelude.Hashable UserAuthConfig

instance Prelude.NFData UserAuthConfig

instance Prelude.ToQuery UserAuthConfig where
  toQuery UserAuthConfig' {..} =
    Prelude.mconcat
      [ "SecretArn" Prelude.=: secretArn,
        "IAMAuth" Prelude.=: iAMAuth,
        "AuthScheme" Prelude.=: authScheme,
        "UserName" Prelude.=: userName,
        "Description" Prelude.=: description
      ]
