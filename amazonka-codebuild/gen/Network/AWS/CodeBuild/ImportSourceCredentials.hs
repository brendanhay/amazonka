{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodeBuild.ImportSourceCredentials
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports the source repository credentials for an AWS CodeBuild project
-- that has its source code stored in a GitHub, GitHub Enterprise, or
-- Bitbucket repository.
module Network.AWS.CodeBuild.ImportSourceCredentials
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

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
    token :: Prelude.Sensitive Prelude.Text,
    -- | The source provider used for this project.
    serverType :: ServerType,
    -- | The type of authentication used to connect to a GitHub, GitHub
    -- Enterprise, or Bitbucket repository. An OAUTH connection is not
    -- supported by the API and must be created using the AWS CodeBuild
    -- console.
    authType :: AuthType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- supported by the API and must be created using the AWS CodeBuild
-- console.
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
        token = Prelude._Sensitive Lens.# pToken_,
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
importSourceCredentials_token = Lens.lens (\ImportSourceCredentials' {token} -> token) (\s@ImportSourceCredentials' {} a -> s {token = a} :: ImportSourceCredentials) Prelude.. Prelude._Sensitive

-- | The source provider used for this project.
importSourceCredentials_serverType :: Lens.Lens' ImportSourceCredentials ServerType
importSourceCredentials_serverType = Lens.lens (\ImportSourceCredentials' {serverType} -> serverType) (\s@ImportSourceCredentials' {} a -> s {serverType = a} :: ImportSourceCredentials)

-- | The type of authentication used to connect to a GitHub, GitHub
-- Enterprise, or Bitbucket repository. An OAUTH connection is not
-- supported by the API and must be created using the AWS CodeBuild
-- console.
importSourceCredentials_authType :: Lens.Lens' ImportSourceCredentials AuthType
importSourceCredentials_authType = Lens.lens (\ImportSourceCredentials' {authType} -> authType) (\s@ImportSourceCredentials' {} a -> s {authType = a} :: ImportSourceCredentials)

instance Prelude.AWSRequest ImportSourceCredentials where
  type
    Rs ImportSourceCredentials =
      ImportSourceCredentialsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportSourceCredentialsResponse'
            Prelude.<$> (x Prelude..?> "arn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportSourceCredentials

instance Prelude.NFData ImportSourceCredentials

instance Prelude.ToHeaders ImportSourceCredentials where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeBuild_20161006.ImportSourceCredentials" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ImportSourceCredentials where
  toJSON ImportSourceCredentials' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("shouldOverwrite" Prelude..=)
              Prelude.<$> shouldOverwrite,
            ("username" Prelude..=) Prelude.<$> username,
            Prelude.Just ("token" Prelude..= token),
            Prelude.Just ("serverType" Prelude..= serverType),
            Prelude.Just ("authType" Prelude..= authType)
          ]
      )

instance Prelude.ToPath ImportSourceCredentials where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ImportSourceCredentials where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newImportSourceCredentialsResponse' smart constructor.
data ImportSourceCredentialsResponse = ImportSourceCredentialsResponse'
  { -- | The Amazon Resource Name (ARN) of the token.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
