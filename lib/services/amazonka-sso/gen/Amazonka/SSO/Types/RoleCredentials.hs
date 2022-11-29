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
-- Module      : Amazonka.SSO.Types.RoleCredentials
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSO.Types.RoleCredentials where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the role credentials that are assigned to the
-- user.
--
-- /See:/ 'newRoleCredentials' smart constructor.
data RoleCredentials = RoleCredentials'
  { -- | The date on which temporary security credentials expire.
    expiration :: Prelude.Maybe Prelude.Integer,
    -- | The token used for temporary credentials. For more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_use-resources.html Using Temporary Security Credentials to Request Access to AWS Resources>
    -- in the /AWS IAM User Guide/.
    sessionToken :: Prelude.Maybe (Core.Sensitive Core.SessionToken),
    -- | The identifier used for the temporary security credentials. For more
    -- information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_use-resources.html Using Temporary Security Credentials to Request Access to AWS Resources>
    -- in the /AWS IAM User Guide/.
    accessKeyId :: Core.AccessKey,
    -- | The key that is used to sign the request. For more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_use-resources.html Using Temporary Security Credentials to Request Access to AWS Resources>
    -- in the /AWS IAM User Guide/.
    secretAccessKey :: Core.Sensitive Core.SecretKey
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RoleCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expiration', 'roleCredentials_expiration' - The date on which temporary security credentials expire.
--
-- 'sessionToken', 'roleCredentials_sessionToken' - The token used for temporary credentials. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_use-resources.html Using Temporary Security Credentials to Request Access to AWS Resources>
-- in the /AWS IAM User Guide/.
--
-- 'accessKeyId', 'roleCredentials_accessKeyId' - The identifier used for the temporary security credentials. For more
-- information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_use-resources.html Using Temporary Security Credentials to Request Access to AWS Resources>
-- in the /AWS IAM User Guide/.
--
-- 'secretAccessKey', 'roleCredentials_secretAccessKey' - The key that is used to sign the request. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_use-resources.html Using Temporary Security Credentials to Request Access to AWS Resources>
-- in the /AWS IAM User Guide/.
newRoleCredentials ::
  -- | 'accessKeyId'
  Core.AccessKey ->
  -- | 'secretAccessKey'
  Core.SecretKey ->
  RoleCredentials
newRoleCredentials pAccessKeyId_ pSecretAccessKey_ =
  RoleCredentials'
    { expiration = Prelude.Nothing,
      sessionToken = Prelude.Nothing,
      accessKeyId = pAccessKeyId_,
      secretAccessKey =
        Core._Sensitive Lens.# pSecretAccessKey_
    }

-- | The date on which temporary security credentials expire.
roleCredentials_expiration :: Lens.Lens' RoleCredentials (Prelude.Maybe Prelude.Integer)
roleCredentials_expiration = Lens.lens (\RoleCredentials' {expiration} -> expiration) (\s@RoleCredentials' {} a -> s {expiration = a} :: RoleCredentials)

-- | The token used for temporary credentials. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_use-resources.html Using Temporary Security Credentials to Request Access to AWS Resources>
-- in the /AWS IAM User Guide/.
roleCredentials_sessionToken :: Lens.Lens' RoleCredentials (Prelude.Maybe Core.SessionToken)
roleCredentials_sessionToken = Lens.lens (\RoleCredentials' {sessionToken} -> sessionToken) (\s@RoleCredentials' {} a -> s {sessionToken = a} :: RoleCredentials) Prelude.. Lens.mapping Core._Sensitive

-- | The identifier used for the temporary security credentials. For more
-- information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_use-resources.html Using Temporary Security Credentials to Request Access to AWS Resources>
-- in the /AWS IAM User Guide/.
roleCredentials_accessKeyId :: Lens.Lens' RoleCredentials Core.AccessKey
roleCredentials_accessKeyId = Lens.lens (\RoleCredentials' {accessKeyId} -> accessKeyId) (\s@RoleCredentials' {} a -> s {accessKeyId = a} :: RoleCredentials)

-- | The key that is used to sign the request. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_use-resources.html Using Temporary Security Credentials to Request Access to AWS Resources>
-- in the /AWS IAM User Guide/.
roleCredentials_secretAccessKey :: Lens.Lens' RoleCredentials Core.SecretKey
roleCredentials_secretAccessKey = Lens.lens (\RoleCredentials' {secretAccessKey} -> secretAccessKey) (\s@RoleCredentials' {} a -> s {secretAccessKey = a} :: RoleCredentials) Prelude.. Core._Sensitive

instance Core.FromJSON RoleCredentials where
  parseJSON =
    Core.withObject
      "RoleCredentials"
      ( \x ->
          RoleCredentials'
            Prelude.<$> (x Core..:? "expiration")
            Prelude.<*> (x Core..:? "sessionToken")
            Prelude.<*> (x Core..: "accessKeyId")
            Prelude.<*> (x Core..: "secretAccessKey")
      )

instance Prelude.Hashable RoleCredentials where
  hashWithSalt _salt RoleCredentials' {..} =
    _salt `Prelude.hashWithSalt` expiration
      `Prelude.hashWithSalt` sessionToken
      `Prelude.hashWithSalt` accessKeyId
      `Prelude.hashWithSalt` secretAccessKey

instance Prelude.NFData RoleCredentials where
  rnf RoleCredentials' {..} =
    Prelude.rnf expiration
      `Prelude.seq` Prelude.rnf sessionToken
      `Prelude.seq` Prelude.rnf accessKeyId
      `Prelude.seq` Prelude.rnf secretAccessKey
