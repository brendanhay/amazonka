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
-- Module      : Amazonka.ECRPublic.Types.AuthorizationData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECRPublic.Types.AuthorizationData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An authorization token data object that corresponds to a public
-- registry.
--
-- /See:/ 'newAuthorizationData' smart constructor.
data AuthorizationData = AuthorizationData'
  { -- | A base64-encoded string that contains authorization data for a public
    -- Amazon ECR registry. When the string is decoded, it is presented in the
    -- format @user:password@ for public registry authentication using
    -- @docker login@.
    authorizationToken :: Prelude.Maybe Prelude.Text,
    -- | The Unix time in seconds and milliseconds when the authorization token
    -- expires. Authorization tokens are valid for 12 hours.
    expiresAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuthorizationData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizationToken', 'authorizationData_authorizationToken' - A base64-encoded string that contains authorization data for a public
-- Amazon ECR registry. When the string is decoded, it is presented in the
-- format @user:password@ for public registry authentication using
-- @docker login@.
--
-- 'expiresAt', 'authorizationData_expiresAt' - The Unix time in seconds and milliseconds when the authorization token
-- expires. Authorization tokens are valid for 12 hours.
newAuthorizationData ::
  AuthorizationData
newAuthorizationData =
  AuthorizationData'
    { authorizationToken =
        Prelude.Nothing,
      expiresAt = Prelude.Nothing
    }

-- | A base64-encoded string that contains authorization data for a public
-- Amazon ECR registry. When the string is decoded, it is presented in the
-- format @user:password@ for public registry authentication using
-- @docker login@.
authorizationData_authorizationToken :: Lens.Lens' AuthorizationData (Prelude.Maybe Prelude.Text)
authorizationData_authorizationToken = Lens.lens (\AuthorizationData' {authorizationToken} -> authorizationToken) (\s@AuthorizationData' {} a -> s {authorizationToken = a} :: AuthorizationData)

-- | The Unix time in seconds and milliseconds when the authorization token
-- expires. Authorization tokens are valid for 12 hours.
authorizationData_expiresAt :: Lens.Lens' AuthorizationData (Prelude.Maybe Prelude.UTCTime)
authorizationData_expiresAt = Lens.lens (\AuthorizationData' {expiresAt} -> expiresAt) (\s@AuthorizationData' {} a -> s {expiresAt = a} :: AuthorizationData) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON AuthorizationData where
  parseJSON =
    Data.withObject
      "AuthorizationData"
      ( \x ->
          AuthorizationData'
            Prelude.<$> (x Data..:? "authorizationToken")
            Prelude.<*> (x Data..:? "expiresAt")
      )

instance Prelude.Hashable AuthorizationData where
  hashWithSalt _salt AuthorizationData' {..} =
    _salt `Prelude.hashWithSalt` authorizationToken
      `Prelude.hashWithSalt` expiresAt

instance Prelude.NFData AuthorizationData where
  rnf AuthorizationData' {..} =
    Prelude.rnf authorizationToken
      `Prelude.seq` Prelude.rnf expiresAt
