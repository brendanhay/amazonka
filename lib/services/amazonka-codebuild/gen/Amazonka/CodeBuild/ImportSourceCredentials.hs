{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeBuild.ImportSourceCredentials
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports the source repository credentials for an CodeBuild project that
-- has its source code stored in a GitHub, GitHub Enterprise, or Bitbucket
-- repository.
module Amazonka.CodeBuild.ImportSourceCredentials
  ( -- * Creating a Request
    ImportSourceCredentials (..),
    newImportSourceCredentials,

    -- * Request Lenses
    importSourceCredentials_shouldOverwrite,
    importSourceCredentials_username,
    importSourceCredentials_token,
    importSourceCredentials_serverType,
    importSourceCredentials_authType,

    -- * Destructuring the Response
    ImportSourceCredentialsResponse (..),
    newImportSourceCredentialsResponse,

    -- * Response Lenses
    importSourceCredentialsResponse_arn,
    importSourceCredentialsResponse_httpStatus,
  )
where

import Amazonka.CodeBuild.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newImportSourceCredentials' smart constructor.
data ImportSourceCredentials = ImportSourceCredentials'
  { -- | Set to @false@ to prevent overwriting the repository source credentials.
    -- Set to @true@ to overwrite the repository source credentials. The
    -- default value is @true@.
    shouldOverwrite :: Prelude.Maybe Prelude.Bool,
    -- | The Bitbucket username when the @authType@ is BASIC_AUTH. This parameter
    -- is not valid for other types of source providers or connections.
    username :: Prelude.Maybe Prelude.Text,
    -- | For GitHub or GitHub Enterprise, this is the personal access token. For
    -- Bitbucket, this is the app password.
    token :: Core.Sensitive Prelude.Text,
    -- | The source provider used for this project.
    serverType :: ServerType,
    -- | The type of authentication used to connect to a GitHub, GitHub
    -- Enterprise, or Bitbucket repository. An OAUTH connection is not
    -- supported by the API and must be created using the CodeBuild console.
    authType :: AuthType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportSourceCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shouldOverwrite', 'importSourceCredentials_shouldOverwrite' - Set to @false@ to prevent overwriting the repository source credentials.
-- Set to @true@ to overwrite the repository source credentials. The
-- default value is @true@.
--
-- 'username', 'importSourceCredentials_username' - The Bitbucket username when the @authType@ is BASIC_AUTH. This parameter
-- is not valid for other types of source providers or connections.
--
-- 'token', 'importSourceCredentials_token' - For GitHub or GitHub Enterprise, this is the personal access token. For
-- Bitbucket, this is the app password.
--
-- 'serverType', 'importSourceCredentials_serverType' - The source provider used for this project.
--
-- 'authType', 'importSourceCredentials_authType' - The type of authentication used to connect to a GitHub, GitHub
-- Enterprise, or Bitbucket repository. An OAUTH connection is not
-- supported by the API and must be created using the CodeBuild console.
newImportSourceCredentials ::
  -- | 'token'
  Prelude.Text ->
  -- | 'serverType'
  ServerType ->
  -- | 'authType'
  AuthType ->
  ImportSourceCredentials
newImportSourceCredentials
  pToken_
  pServerType_
  pAuthType_ =
    ImportSourceCredentials'
      { shouldOverwrite =
          Prelude.Nothing,
        username = Prelude.Nothing,
        token = Core._Sensitive Lens.# pToken_,
        serverType = pServerType_,
        authType = pAuthType_
      }

-- | Set to @false@ to prevent overwriting the repository source credentials.
-- Set to @true@ to overwrite the repository source credentials. The
-- default value is @true@.
importSourceCredentials_shouldOverwrite :: Lens.Lens' ImportSourceCredentials (Prelude.Maybe Prelude.Bool)
importSourceCredentials_shouldOverwrite = Lens.lens (\ImportSourceCredentials' {shouldOverwrite} -> shouldOverwrite) (\s@ImportSourceCredentials' {} a -> s {shouldOverwrite = a} :: ImportSourceCredentials)

-- | The Bitbucket username when the @authType@ is BASIC_AUTH. This parameter
-- is not valid for other types of source providers or connections.
importSourceCredentials_username :: Lens.Lens' ImportSourceCredentials (Prelude.Maybe Prelude.Text)
importSourceCredentials_username = Lens.lens (\ImportSourceCredentials' {username} -> username) (\s@ImportSourceCredentials' {} a -> s {username = a} :: ImportSourceCredentials)

-- | For GitHub or GitHub Enterprise, this is the personal access token. For
-- Bitbucket, this is the app password.
importSourceCredentials_token :: Lens.Lens' ImportSourceCredentials Prelude.Text
importSourceCredentials_token = Lens.lens (\ImportSourceCredentials' {token} -> token) (\s@ImportSourceCredentials' {} a -> s {token = a} :: ImportSourceCredentials) Prelude.. Core._Sensitive

-- | The source provider used for this project.
importSourceCredentials_serverType :: Lens.Lens' ImportSourceCredentials ServerType
importSourceCredentials_serverType = Lens.lens (\ImportSourceCredentials' {serverType} -> serverType) (\s@ImportSourceCredentials' {} a -> s {serverType = a} :: ImportSourceCredentials)

-- | The type of authentication used to connect to a GitHub, GitHub
-- Enterprise, or Bitbucket repository. An OAUTH connection is not
-- supported by the API and must be created using the CodeBuild console.
importSourceCredentials_authType :: Lens.Lens' ImportSourceCredentials AuthType
importSourceCredentials_authType = Lens.lens (\ImportSourceCredentials' {authType} -> authType) (\s@ImportSourceCredentials' {} a -> s {authType = a} :: ImportSourceCredentials)

instance Core.AWSRequest ImportSourceCredentials where
  type
    AWSResponse ImportSourceCredentials =
      ImportSourceCredentialsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportSourceCredentialsResponse'
            Prelude.<$> (x Core..?> "arn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportSourceCredentials where
  hashWithSalt _salt ImportSourceCredentials' {..} =
    _salt `Prelude.hashWithSalt` shouldOverwrite
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` token
      `Prelude.hashWithSalt` serverType
      `Prelude.hashWithSalt` authType

instance Prelude.NFData ImportSourceCredentials where
  rnf ImportSourceCredentials' {..} =
    Prelude.rnf shouldOverwrite
      `Prelude.seq` Prelude.rnf username
      `Prelude.seq` Prelude.rnf token
      `Prelude.seq` Prelude.rnf serverType
      `Prelude.seq` Prelude.rnf authType

instance Core.ToHeaders ImportSourceCredentials where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.ImportSourceCredentials" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ImportSourceCredentials where
  toJSON ImportSourceCredentials' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("shouldOverwrite" Core..=)
              Prelude.<$> shouldOverwrite,
            ("username" Core..=) Prelude.<$> username,
            Prelude.Just ("token" Core..= token),
            Prelude.Just ("serverType" Core..= serverType),
            Prelude.Just ("authType" Core..= authType)
          ]
      )

instance Core.ToPath ImportSourceCredentials where
  toPath = Prelude.const "/"

instance Core.ToQuery ImportSourceCredentials where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newImportSourceCredentialsResponse' smart constructor.
data ImportSourceCredentialsResponse = ImportSourceCredentialsResponse'
  { -- | The Amazon Resource Name (ARN) of the token.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportSourceCredentialsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'importSourceCredentialsResponse_arn' - The Amazon Resource Name (ARN) of the token.
--
-- 'httpStatus', 'importSourceCredentialsResponse_httpStatus' - The response's http status code.
newImportSourceCredentialsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ImportSourceCredentialsResponse
newImportSourceCredentialsResponse pHttpStatus_ =
  ImportSourceCredentialsResponse'
    { arn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the token.
importSourceCredentialsResponse_arn :: Lens.Lens' ImportSourceCredentialsResponse (Prelude.Maybe Prelude.Text)
importSourceCredentialsResponse_arn = Lens.lens (\ImportSourceCredentialsResponse' {arn} -> arn) (\s@ImportSourceCredentialsResponse' {} a -> s {arn = a} :: ImportSourceCredentialsResponse)

-- | The response's http status code.
importSourceCredentialsResponse_httpStatus :: Lens.Lens' ImportSourceCredentialsResponse Prelude.Int
importSourceCredentialsResponse_httpStatus = Lens.lens (\ImportSourceCredentialsResponse' {httpStatus} -> httpStatus) (\s@ImportSourceCredentialsResponse' {} a -> s {httpStatus = a} :: ImportSourceCredentialsResponse)

instance
  Prelude.NFData
    ImportSourceCredentialsResponse
  where
  rnf ImportSourceCredentialsResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf httpStatus
