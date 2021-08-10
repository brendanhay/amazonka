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
-- Module      : Network.AWS.IoT.CreateAuthorizer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an authorizer.
module Network.AWS.IoT.CreateAuthorizer
  ( -- * Creating a Request
    CreateAuthorizer (..),
    newCreateAuthorizer,

    -- * Request Lenses
    createAuthorizer_status,
    createAuthorizer_tokenSigningPublicKeys,
    createAuthorizer_tags,
    createAuthorizer_signingDisabled,
    createAuthorizer_tokenKeyName,
    createAuthorizer_authorizerName,
    createAuthorizer_authorizerFunctionArn,

    -- * Destructuring the Response
    CreateAuthorizerResponse (..),
    newCreateAuthorizerResponse,

    -- * Response Lenses
    createAuthorizerResponse_authorizerArn,
    createAuthorizerResponse_authorizerName,
    createAuthorizerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateAuthorizer' smart constructor.
data CreateAuthorizer = CreateAuthorizer'
  { -- | The status of the create authorizer request.
    status :: Prelude.Maybe AuthorizerStatus,
    -- | The public keys used to verify the digital signature returned by your
    -- custom authentication service.
    tokenSigningPublicKeys :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Metadata which can be used to manage the custom authorizer.
    --
    -- For URI Request parameters use format: ...key1=value1&key2=value2...
    --
    -- For the CLI command-line parameter use format: &&tags
    -- \"key1=value1&key2=value2...\"
    --
    -- For the cli-input-json file use format: \"tags\":
    -- \"key1=value1&key2=value2...\"
    tags :: Prelude.Maybe [Tag],
    -- | Specifies whether AWS IoT validates the token signature in an
    -- authorization request.
    signingDisabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the token key used to extract the token from the HTTP
    -- headers.
    tokenKeyName :: Prelude.Maybe Prelude.Text,
    -- | The authorizer name.
    authorizerName :: Prelude.Text,
    -- | The ARN of the authorizer\'s Lambda function.
    authorizerFunctionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAuthorizer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'createAuthorizer_status' - The status of the create authorizer request.
--
-- 'tokenSigningPublicKeys', 'createAuthorizer_tokenSigningPublicKeys' - The public keys used to verify the digital signature returned by your
-- custom authentication service.
--
-- 'tags', 'createAuthorizer_tags' - Metadata which can be used to manage the custom authorizer.
--
-- For URI Request parameters use format: ...key1=value1&key2=value2...
--
-- For the CLI command-line parameter use format: &&tags
-- \"key1=value1&key2=value2...\"
--
-- For the cli-input-json file use format: \"tags\":
-- \"key1=value1&key2=value2...\"
--
-- 'signingDisabled', 'createAuthorizer_signingDisabled' - Specifies whether AWS IoT validates the token signature in an
-- authorization request.
--
-- 'tokenKeyName', 'createAuthorizer_tokenKeyName' - The name of the token key used to extract the token from the HTTP
-- headers.
--
-- 'authorizerName', 'createAuthorizer_authorizerName' - The authorizer name.
--
-- 'authorizerFunctionArn', 'createAuthorizer_authorizerFunctionArn' - The ARN of the authorizer\'s Lambda function.
newCreateAuthorizer ::
  -- | 'authorizerName'
  Prelude.Text ->
  -- | 'authorizerFunctionArn'
  Prelude.Text ->
  CreateAuthorizer
newCreateAuthorizer
  pAuthorizerName_
  pAuthorizerFunctionArn_ =
    CreateAuthorizer'
      { status = Prelude.Nothing,
        tokenSigningPublicKeys = Prelude.Nothing,
        tags = Prelude.Nothing,
        signingDisabled = Prelude.Nothing,
        tokenKeyName = Prelude.Nothing,
        authorizerName = pAuthorizerName_,
        authorizerFunctionArn = pAuthorizerFunctionArn_
      }

-- | The status of the create authorizer request.
createAuthorizer_status :: Lens.Lens' CreateAuthorizer (Prelude.Maybe AuthorizerStatus)
createAuthorizer_status = Lens.lens (\CreateAuthorizer' {status} -> status) (\s@CreateAuthorizer' {} a -> s {status = a} :: CreateAuthorizer)

-- | The public keys used to verify the digital signature returned by your
-- custom authentication service.
createAuthorizer_tokenSigningPublicKeys :: Lens.Lens' CreateAuthorizer (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createAuthorizer_tokenSigningPublicKeys = Lens.lens (\CreateAuthorizer' {tokenSigningPublicKeys} -> tokenSigningPublicKeys) (\s@CreateAuthorizer' {} a -> s {tokenSigningPublicKeys = a} :: CreateAuthorizer) Prelude.. Lens.mapping Lens._Coerce

-- | Metadata which can be used to manage the custom authorizer.
--
-- For URI Request parameters use format: ...key1=value1&key2=value2...
--
-- For the CLI command-line parameter use format: &&tags
-- \"key1=value1&key2=value2...\"
--
-- For the cli-input-json file use format: \"tags\":
-- \"key1=value1&key2=value2...\"
createAuthorizer_tags :: Lens.Lens' CreateAuthorizer (Prelude.Maybe [Tag])
createAuthorizer_tags = Lens.lens (\CreateAuthorizer' {tags} -> tags) (\s@CreateAuthorizer' {} a -> s {tags = a} :: CreateAuthorizer) Prelude.. Lens.mapping Lens._Coerce

-- | Specifies whether AWS IoT validates the token signature in an
-- authorization request.
createAuthorizer_signingDisabled :: Lens.Lens' CreateAuthorizer (Prelude.Maybe Prelude.Bool)
createAuthorizer_signingDisabled = Lens.lens (\CreateAuthorizer' {signingDisabled} -> signingDisabled) (\s@CreateAuthorizer' {} a -> s {signingDisabled = a} :: CreateAuthorizer)

-- | The name of the token key used to extract the token from the HTTP
-- headers.
createAuthorizer_tokenKeyName :: Lens.Lens' CreateAuthorizer (Prelude.Maybe Prelude.Text)
createAuthorizer_tokenKeyName = Lens.lens (\CreateAuthorizer' {tokenKeyName} -> tokenKeyName) (\s@CreateAuthorizer' {} a -> s {tokenKeyName = a} :: CreateAuthorizer)

-- | The authorizer name.
createAuthorizer_authorizerName :: Lens.Lens' CreateAuthorizer Prelude.Text
createAuthorizer_authorizerName = Lens.lens (\CreateAuthorizer' {authorizerName} -> authorizerName) (\s@CreateAuthorizer' {} a -> s {authorizerName = a} :: CreateAuthorizer)

-- | The ARN of the authorizer\'s Lambda function.
createAuthorizer_authorizerFunctionArn :: Lens.Lens' CreateAuthorizer Prelude.Text
createAuthorizer_authorizerFunctionArn = Lens.lens (\CreateAuthorizer' {authorizerFunctionArn} -> authorizerFunctionArn) (\s@CreateAuthorizer' {} a -> s {authorizerFunctionArn = a} :: CreateAuthorizer)

instance Core.AWSRequest CreateAuthorizer where
  type
    AWSResponse CreateAuthorizer =
      CreateAuthorizerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAuthorizerResponse'
            Prelude.<$> (x Core..?> "authorizerArn")
            Prelude.<*> (x Core..?> "authorizerName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAuthorizer

instance Prelude.NFData CreateAuthorizer

instance Core.ToHeaders CreateAuthorizer where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CreateAuthorizer where
  toJSON CreateAuthorizer' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("status" Core..=) Prelude.<$> status,
            ("tokenSigningPublicKeys" Core..=)
              Prelude.<$> tokenSigningPublicKeys,
            ("tags" Core..=) Prelude.<$> tags,
            ("signingDisabled" Core..=)
              Prelude.<$> signingDisabled,
            ("tokenKeyName" Core..=) Prelude.<$> tokenKeyName,
            Prelude.Just
              ( "authorizerFunctionArn"
                  Core..= authorizerFunctionArn
              )
          ]
      )

instance Core.ToPath CreateAuthorizer where
  toPath CreateAuthorizer' {..} =
    Prelude.mconcat
      ["/authorizer/", Core.toBS authorizerName]

instance Core.ToQuery CreateAuthorizer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAuthorizerResponse' smart constructor.
data CreateAuthorizerResponse = CreateAuthorizerResponse'
  { -- | The authorizer ARN.
    authorizerArn :: Prelude.Maybe Prelude.Text,
    -- | The authorizer\'s name.
    authorizerName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAuthorizerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizerArn', 'createAuthorizerResponse_authorizerArn' - The authorizer ARN.
--
-- 'authorizerName', 'createAuthorizerResponse_authorizerName' - The authorizer\'s name.
--
-- 'httpStatus', 'createAuthorizerResponse_httpStatus' - The response's http status code.
newCreateAuthorizerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAuthorizerResponse
newCreateAuthorizerResponse pHttpStatus_ =
  CreateAuthorizerResponse'
    { authorizerArn =
        Prelude.Nothing,
      authorizerName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The authorizer ARN.
createAuthorizerResponse_authorizerArn :: Lens.Lens' CreateAuthorizerResponse (Prelude.Maybe Prelude.Text)
createAuthorizerResponse_authorizerArn = Lens.lens (\CreateAuthorizerResponse' {authorizerArn} -> authorizerArn) (\s@CreateAuthorizerResponse' {} a -> s {authorizerArn = a} :: CreateAuthorizerResponse)

-- | The authorizer\'s name.
createAuthorizerResponse_authorizerName :: Lens.Lens' CreateAuthorizerResponse (Prelude.Maybe Prelude.Text)
createAuthorizerResponse_authorizerName = Lens.lens (\CreateAuthorizerResponse' {authorizerName} -> authorizerName) (\s@CreateAuthorizerResponse' {} a -> s {authorizerName = a} :: CreateAuthorizerResponse)

-- | The response's http status code.
createAuthorizerResponse_httpStatus :: Lens.Lens' CreateAuthorizerResponse Prelude.Int
createAuthorizerResponse_httpStatus = Lens.lens (\CreateAuthorizerResponse' {httpStatus} -> httpStatus) (\s@CreateAuthorizerResponse' {} a -> s {httpStatus = a} :: CreateAuthorizerResponse)

instance Prelude.NFData CreateAuthorizerResponse
