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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateAuthorizer' smart constructor.
data CreateAuthorizer = CreateAuthorizer'
  { -- | The status of the create authorizer request.
    status :: Core.Maybe AuthorizerStatus,
    -- | The public keys used to verify the digital signature returned by your
    -- custom authentication service.
    tokenSigningPublicKeys :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Metadata which can be used to manage the custom authorizer.
    --
    -- For URI Request parameters use format: ...key1=value1&key2=value2...
    --
    -- For the CLI command-line parameter use format: &&tags
    -- \"key1=value1&key2=value2...\"
    --
    -- For the cli-input-json file use format: \"tags\":
    -- \"key1=value1&key2=value2...\"
    tags :: Core.Maybe [Tag],
    -- | Specifies whether AWS IoT validates the token signature in an
    -- authorization request.
    signingDisabled :: Core.Maybe Core.Bool,
    -- | The name of the token key used to extract the token from the HTTP
    -- headers.
    tokenKeyName :: Core.Maybe Core.Text,
    -- | The authorizer name.
    authorizerName :: Core.Text,
    -- | The ARN of the authorizer\'s Lambda function.
    authorizerFunctionArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'authorizerFunctionArn'
  Core.Text ->
  CreateAuthorizer
newCreateAuthorizer
  pAuthorizerName_
  pAuthorizerFunctionArn_ =
    CreateAuthorizer'
      { status = Core.Nothing,
        tokenSigningPublicKeys = Core.Nothing,
        tags = Core.Nothing,
        signingDisabled = Core.Nothing,
        tokenKeyName = Core.Nothing,
        authorizerName = pAuthorizerName_,
        authorizerFunctionArn = pAuthorizerFunctionArn_
      }

-- | The status of the create authorizer request.
createAuthorizer_status :: Lens.Lens' CreateAuthorizer (Core.Maybe AuthorizerStatus)
createAuthorizer_status = Lens.lens (\CreateAuthorizer' {status} -> status) (\s@CreateAuthorizer' {} a -> s {status = a} :: CreateAuthorizer)

-- | The public keys used to verify the digital signature returned by your
-- custom authentication service.
createAuthorizer_tokenSigningPublicKeys :: Lens.Lens' CreateAuthorizer (Core.Maybe (Core.HashMap Core.Text Core.Text))
createAuthorizer_tokenSigningPublicKeys = Lens.lens (\CreateAuthorizer' {tokenSigningPublicKeys} -> tokenSigningPublicKeys) (\s@CreateAuthorizer' {} a -> s {tokenSigningPublicKeys = a} :: CreateAuthorizer) Core.. Lens.mapping Lens._Coerce

-- | Metadata which can be used to manage the custom authorizer.
--
-- For URI Request parameters use format: ...key1=value1&key2=value2...
--
-- For the CLI command-line parameter use format: &&tags
-- \"key1=value1&key2=value2...\"
--
-- For the cli-input-json file use format: \"tags\":
-- \"key1=value1&key2=value2...\"
createAuthorizer_tags :: Lens.Lens' CreateAuthorizer (Core.Maybe [Tag])
createAuthorizer_tags = Lens.lens (\CreateAuthorizer' {tags} -> tags) (\s@CreateAuthorizer' {} a -> s {tags = a} :: CreateAuthorizer) Core.. Lens.mapping Lens._Coerce

-- | Specifies whether AWS IoT validates the token signature in an
-- authorization request.
createAuthorizer_signingDisabled :: Lens.Lens' CreateAuthorizer (Core.Maybe Core.Bool)
createAuthorizer_signingDisabled = Lens.lens (\CreateAuthorizer' {signingDisabled} -> signingDisabled) (\s@CreateAuthorizer' {} a -> s {signingDisabled = a} :: CreateAuthorizer)

-- | The name of the token key used to extract the token from the HTTP
-- headers.
createAuthorizer_tokenKeyName :: Lens.Lens' CreateAuthorizer (Core.Maybe Core.Text)
createAuthorizer_tokenKeyName = Lens.lens (\CreateAuthorizer' {tokenKeyName} -> tokenKeyName) (\s@CreateAuthorizer' {} a -> s {tokenKeyName = a} :: CreateAuthorizer)

-- | The authorizer name.
createAuthorizer_authorizerName :: Lens.Lens' CreateAuthorizer Core.Text
createAuthorizer_authorizerName = Lens.lens (\CreateAuthorizer' {authorizerName} -> authorizerName) (\s@CreateAuthorizer' {} a -> s {authorizerName = a} :: CreateAuthorizer)

-- | The ARN of the authorizer\'s Lambda function.
createAuthorizer_authorizerFunctionArn :: Lens.Lens' CreateAuthorizer Core.Text
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
            Core.<$> (x Core..?> "authorizerArn")
            Core.<*> (x Core..?> "authorizerName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateAuthorizer

instance Core.NFData CreateAuthorizer

instance Core.ToHeaders CreateAuthorizer where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON CreateAuthorizer where
  toJSON CreateAuthorizer' {..} =
    Core.object
      ( Core.catMaybes
          [ ("status" Core..=) Core.<$> status,
            ("tokenSigningPublicKeys" Core..=)
              Core.<$> tokenSigningPublicKeys,
            ("tags" Core..=) Core.<$> tags,
            ("signingDisabled" Core..=) Core.<$> signingDisabled,
            ("tokenKeyName" Core..=) Core.<$> tokenKeyName,
            Core.Just
              ( "authorizerFunctionArn"
                  Core..= authorizerFunctionArn
              )
          ]
      )

instance Core.ToPath CreateAuthorizer where
  toPath CreateAuthorizer' {..} =
    Core.mconcat
      ["/authorizer/", Core.toBS authorizerName]

instance Core.ToQuery CreateAuthorizer where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateAuthorizerResponse' smart constructor.
data CreateAuthorizerResponse = CreateAuthorizerResponse'
  { -- | The authorizer ARN.
    authorizerArn :: Core.Maybe Core.Text,
    -- | The authorizer\'s name.
    authorizerName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateAuthorizerResponse
newCreateAuthorizerResponse pHttpStatus_ =
  CreateAuthorizerResponse'
    { authorizerArn =
        Core.Nothing,
      authorizerName = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The authorizer ARN.
createAuthorizerResponse_authorizerArn :: Lens.Lens' CreateAuthorizerResponse (Core.Maybe Core.Text)
createAuthorizerResponse_authorizerArn = Lens.lens (\CreateAuthorizerResponse' {authorizerArn} -> authorizerArn) (\s@CreateAuthorizerResponse' {} a -> s {authorizerArn = a} :: CreateAuthorizerResponse)

-- | The authorizer\'s name.
createAuthorizerResponse_authorizerName :: Lens.Lens' CreateAuthorizerResponse (Core.Maybe Core.Text)
createAuthorizerResponse_authorizerName = Lens.lens (\CreateAuthorizerResponse' {authorizerName} -> authorizerName) (\s@CreateAuthorizerResponse' {} a -> s {authorizerName = a} :: CreateAuthorizerResponse)

-- | The response's http status code.
createAuthorizerResponse_httpStatus :: Lens.Lens' CreateAuthorizerResponse Core.Int
createAuthorizerResponse_httpStatus = Lens.lens (\CreateAuthorizerResponse' {httpStatus} -> httpStatus) (\s@CreateAuthorizerResponse' {} a -> s {httpStatus = a} :: CreateAuthorizerResponse)

instance Core.NFData CreateAuthorizerResponse
