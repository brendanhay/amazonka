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
-- Module      : Amazonka.RDS.Types.UserAuthConfigInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.UserAuthConfigInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types.AuthScheme
import Amazonka.RDS.Types.ClientPasswordAuthType
import Amazonka.RDS.Types.IAMAuthMode

-- | Returns the details of authentication used by a proxy to log in as a
-- specific database user.
--
-- /See:/ 'newUserAuthConfigInfo' smart constructor.
data UserAuthConfigInfo = UserAuthConfigInfo'
  { -- | The type of authentication that the proxy uses for connections from the
    -- proxy to the underlying database.
    authScheme :: Prelude.Maybe AuthScheme,
    -- | The type of authentication the proxy uses for connections from clients.
    clientPasswordAuthType :: Prelude.Maybe ClientPasswordAuthType,
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
-- Create a value of 'UserAuthConfigInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authScheme', 'userAuthConfigInfo_authScheme' - The type of authentication that the proxy uses for connections from the
-- proxy to the underlying database.
--
-- 'clientPasswordAuthType', 'userAuthConfigInfo_clientPasswordAuthType' - The type of authentication the proxy uses for connections from clients.
--
-- 'description', 'userAuthConfigInfo_description' - A user-specified description about the authentication used by a proxy to
-- log in as a specific database user.
--
-- 'iAMAuth', 'userAuthConfigInfo_iAMAuth' - Whether to require or disallow Amazon Web Services Identity and Access
-- Management (IAM) authentication for connections to the proxy. The
-- @ENABLED@ value is valid only for proxies with RDS for Microsoft SQL
-- Server.
--
-- 'secretArn', 'userAuthConfigInfo_secretArn' - The Amazon Resource Name (ARN) representing the secret that the proxy
-- uses to authenticate to the RDS DB instance or Aurora DB cluster. These
-- secrets are stored within Amazon Secrets Manager.
--
-- 'userName', 'userAuthConfigInfo_userName' - The name of the database user to which the proxy connects.
newUserAuthConfigInfo ::
  UserAuthConfigInfo
newUserAuthConfigInfo =
  UserAuthConfigInfo'
    { authScheme = Prelude.Nothing,
      clientPasswordAuthType = Prelude.Nothing,
      description = Prelude.Nothing,
      iAMAuth = Prelude.Nothing,
      secretArn = Prelude.Nothing,
      userName = Prelude.Nothing
    }

-- | The type of authentication that the proxy uses for connections from the
-- proxy to the underlying database.
userAuthConfigInfo_authScheme :: Lens.Lens' UserAuthConfigInfo (Prelude.Maybe AuthScheme)
userAuthConfigInfo_authScheme = Lens.lens (\UserAuthConfigInfo' {authScheme} -> authScheme) (\s@UserAuthConfigInfo' {} a -> s {authScheme = a} :: UserAuthConfigInfo)

-- | The type of authentication the proxy uses for connections from clients.
userAuthConfigInfo_clientPasswordAuthType :: Lens.Lens' UserAuthConfigInfo (Prelude.Maybe ClientPasswordAuthType)
userAuthConfigInfo_clientPasswordAuthType = Lens.lens (\UserAuthConfigInfo' {clientPasswordAuthType} -> clientPasswordAuthType) (\s@UserAuthConfigInfo' {} a -> s {clientPasswordAuthType = a} :: UserAuthConfigInfo)

-- | A user-specified description about the authentication used by a proxy to
-- log in as a specific database user.
userAuthConfigInfo_description :: Lens.Lens' UserAuthConfigInfo (Prelude.Maybe Prelude.Text)
userAuthConfigInfo_description = Lens.lens (\UserAuthConfigInfo' {description} -> description) (\s@UserAuthConfigInfo' {} a -> s {description = a} :: UserAuthConfigInfo)

-- | Whether to require or disallow Amazon Web Services Identity and Access
-- Management (IAM) authentication for connections to the proxy. The
-- @ENABLED@ value is valid only for proxies with RDS for Microsoft SQL
-- Server.
userAuthConfigInfo_iAMAuth :: Lens.Lens' UserAuthConfigInfo (Prelude.Maybe IAMAuthMode)
userAuthConfigInfo_iAMAuth = Lens.lens (\UserAuthConfigInfo' {iAMAuth} -> iAMAuth) (\s@UserAuthConfigInfo' {} a -> s {iAMAuth = a} :: UserAuthConfigInfo)

-- | The Amazon Resource Name (ARN) representing the secret that the proxy
-- uses to authenticate to the RDS DB instance or Aurora DB cluster. These
-- secrets are stored within Amazon Secrets Manager.
userAuthConfigInfo_secretArn :: Lens.Lens' UserAuthConfigInfo (Prelude.Maybe Prelude.Text)
userAuthConfigInfo_secretArn = Lens.lens (\UserAuthConfigInfo' {secretArn} -> secretArn) (\s@UserAuthConfigInfo' {} a -> s {secretArn = a} :: UserAuthConfigInfo)

-- | The name of the database user to which the proxy connects.
userAuthConfigInfo_userName :: Lens.Lens' UserAuthConfigInfo (Prelude.Maybe Prelude.Text)
userAuthConfigInfo_userName = Lens.lens (\UserAuthConfigInfo' {userName} -> userName) (\s@UserAuthConfigInfo' {} a -> s {userName = a} :: UserAuthConfigInfo)

instance Data.FromXML UserAuthConfigInfo where
  parseXML x =
    UserAuthConfigInfo'
      Prelude.<$> (x Data..@? "AuthScheme")
      Prelude.<*> (x Data..@? "ClientPasswordAuthType")
      Prelude.<*> (x Data..@? "Description")
      Prelude.<*> (x Data..@? "IAMAuth")
      Prelude.<*> (x Data..@? "SecretArn")
      Prelude.<*> (x Data..@? "UserName")

instance Prelude.Hashable UserAuthConfigInfo where
  hashWithSalt _salt UserAuthConfigInfo' {..} =
    _salt `Prelude.hashWithSalt` authScheme
      `Prelude.hashWithSalt` clientPasswordAuthType
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` iAMAuth
      `Prelude.hashWithSalt` secretArn
      `Prelude.hashWithSalt` userName

instance Prelude.NFData UserAuthConfigInfo where
  rnf UserAuthConfigInfo' {..} =
    Prelude.rnf authScheme
      `Prelude.seq` Prelude.rnf clientPasswordAuthType
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf iAMAuth
      `Prelude.seq` Prelude.rnf secretArn
      `Prelude.seq` Prelude.rnf userName
