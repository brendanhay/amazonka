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
-- Module      : Amazonka.RDS.Types.UserAuthConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.UserAuthConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types.AuthScheme
import Amazonka.RDS.Types.IAMAuthMode

-- | Specifies the details of authentication used by a proxy to log in as a
-- specific database user.
--
-- /See:/ 'newUserAuthConfig' smart constructor.
data UserAuthConfig = UserAuthConfig'
  { -- | The type of authentication that the proxy uses for connections from the
    -- proxy to the underlying database.
    authScheme :: Prelude.Maybe AuthScheme,
    -- | A user-specified description about the authentication used by a proxy to
    -- log in as a specific database user.
    description :: Prelude.Maybe Prelude.Text,
    -- | Whether to require or disallow Amazon Web Services Identity and Access
    -- Management (IAM) authentication for connections to the proxy. The
    -- @ENABLED@ value is valid only for proxies with RDS for Microsoft SQL
    -- Server.
    iAMAuth :: Prelude.Maybe IAMAuthMode,
    -- | The Amazon Resource Name (ARN) representing the secret that the proxy
    -- uses to authenticate to the RDS DB instance or Aurora DB cluster. These
    -- secrets are stored within Amazon Secrets Manager.
    secretArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the database user to which the proxy connects.
    userName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserAuthConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authScheme', 'userAuthConfig_authScheme' - The type of authentication that the proxy uses for connections from the
-- proxy to the underlying database.
--
-- 'description', 'userAuthConfig_description' - A user-specified description about the authentication used by a proxy to
-- log in as a specific database user.
--
-- 'iAMAuth', 'userAuthConfig_iAMAuth' - Whether to require or disallow Amazon Web Services Identity and Access
-- Management (IAM) authentication for connections to the proxy. The
-- @ENABLED@ value is valid only for proxies with RDS for Microsoft SQL
-- Server.
--
-- 'secretArn', 'userAuthConfig_secretArn' - The Amazon Resource Name (ARN) representing the secret that the proxy
-- uses to authenticate to the RDS DB instance or Aurora DB cluster. These
-- secrets are stored within Amazon Secrets Manager.
--
-- 'userName', 'userAuthConfig_userName' - The name of the database user to which the proxy connects.
newUserAuthConfig ::
  UserAuthConfig
newUserAuthConfig =
  UserAuthConfig'
    { authScheme = Prelude.Nothing,
      description = Prelude.Nothing,
      iAMAuth = Prelude.Nothing,
      secretArn = Prelude.Nothing,
      userName = Prelude.Nothing
    }

-- | The type of authentication that the proxy uses for connections from the
-- proxy to the underlying database.
userAuthConfig_authScheme :: Lens.Lens' UserAuthConfig (Prelude.Maybe AuthScheme)
userAuthConfig_authScheme = Lens.lens (\UserAuthConfig' {authScheme} -> authScheme) (\s@UserAuthConfig' {} a -> s {authScheme = a} :: UserAuthConfig)

-- | A user-specified description about the authentication used by a proxy to
-- log in as a specific database user.
userAuthConfig_description :: Lens.Lens' UserAuthConfig (Prelude.Maybe Prelude.Text)
userAuthConfig_description = Lens.lens (\UserAuthConfig' {description} -> description) (\s@UserAuthConfig' {} a -> s {description = a} :: UserAuthConfig)

-- | Whether to require or disallow Amazon Web Services Identity and Access
-- Management (IAM) authentication for connections to the proxy. The
-- @ENABLED@ value is valid only for proxies with RDS for Microsoft SQL
-- Server.
userAuthConfig_iAMAuth :: Lens.Lens' UserAuthConfig (Prelude.Maybe IAMAuthMode)
userAuthConfig_iAMAuth = Lens.lens (\UserAuthConfig' {iAMAuth} -> iAMAuth) (\s@UserAuthConfig' {} a -> s {iAMAuth = a} :: UserAuthConfig)

-- | The Amazon Resource Name (ARN) representing the secret that the proxy
-- uses to authenticate to the RDS DB instance or Aurora DB cluster. These
-- secrets are stored within Amazon Secrets Manager.
userAuthConfig_secretArn :: Lens.Lens' UserAuthConfig (Prelude.Maybe Prelude.Text)
userAuthConfig_secretArn = Lens.lens (\UserAuthConfig' {secretArn} -> secretArn) (\s@UserAuthConfig' {} a -> s {secretArn = a} :: UserAuthConfig)

-- | The name of the database user to which the proxy connects.
userAuthConfig_userName :: Lens.Lens' UserAuthConfig (Prelude.Maybe Prelude.Text)
userAuthConfig_userName = Lens.lens (\UserAuthConfig' {userName} -> userName) (\s@UserAuthConfig' {} a -> s {userName = a} :: UserAuthConfig)

instance Prelude.Hashable UserAuthConfig where
  hashWithSalt _salt UserAuthConfig' {..} =
    _salt `Prelude.hashWithSalt` authScheme
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` iAMAuth
      `Prelude.hashWithSalt` secretArn
      `Prelude.hashWithSalt` userName

instance Prelude.NFData UserAuthConfig where
  rnf UserAuthConfig' {..} =
    Prelude.rnf authScheme
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf iAMAuth
      `Prelude.seq` Prelude.rnf secretArn
      `Prelude.seq` Prelude.rnf userName

instance Data.ToQuery UserAuthConfig where
  toQuery UserAuthConfig' {..} =
    Prelude.mconcat
      [ "AuthScheme" Data.=: authScheme,
        "Description" Data.=: description,
        "IAMAuth" Data.=: iAMAuth,
        "SecretArn" Data.=: secretArn,
        "UserName" Data.=: userName
      ]
