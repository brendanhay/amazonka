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
-- Module      : Amazonka.IoT.CreateAuthorizer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an authorizer.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions CreateAuthorizer>
-- action.
module Amazonka.IoT.CreateAuthorizer
  ( -- * Creating a Request
    CreateAuthorizer (..),
    newCreateAuthorizer,

    -- * Request Lenses
    createAuthorizer_tags,
    createAuthorizer_tokenKeyName,
    createAuthorizer_status,
    createAuthorizer_signingDisabled,
    createAuthorizer_tokenSigningPublicKeys,
    createAuthorizer_enableCachingForHttp,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAuthorizer' smart constructor.
data CreateAuthorizer = CreateAuthorizer'
  { -- | Metadata which can be used to manage the custom authorizer.
    --
    -- For URI Request parameters use format: ...key1=value1&key2=value2...
    --
    -- For the CLI command-line parameter use format: &&tags
    -- \"key1=value1&key2=value2...\"
    --
    -- For the cli-input-json file use format: \"tags\":
    -- \"key1=value1&key2=value2...\"
    tags :: Prelude.Maybe [Tag],
    -- | The name of the token key used to extract the token from the HTTP
    -- headers.
    tokenKeyName :: Prelude.Maybe Prelude.Text,
    -- | The status of the create authorizer request.
    status :: Prelude.Maybe AuthorizerStatus,
    -- | Specifies whether IoT validates the token signature in an authorization
    -- request.
    signingDisabled :: Prelude.Maybe Prelude.Bool,
    -- | The public keys used to verify the digital signature returned by your
    -- custom authentication service.
    tokenSigningPublicKeys :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | When @true@, the result from the authorizer’s Lambda function is cached
    -- for clients that use persistent HTTP connections. The results are cached
    -- for the time specified by the Lambda function in
    -- @refreshAfterInSeconds@. This value does not affect authorization of
    -- clients that use MQTT connections.
    --
    -- The default value is @false@.
    enableCachingForHttp :: Prelude.Maybe Prelude.Bool,
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
-- 'tokenKeyName', 'createAuthorizer_tokenKeyName' - The name of the token key used to extract the token from the HTTP
-- headers.
--
-- 'status', 'createAuthorizer_status' - The status of the create authorizer request.
--
-- 'signingDisabled', 'createAuthorizer_signingDisabled' - Specifies whether IoT validates the token signature in an authorization
-- request.
--
-- 'tokenSigningPublicKeys', 'createAuthorizer_tokenSigningPublicKeys' - The public keys used to verify the digital signature returned by your
-- custom authentication service.
--
-- 'enableCachingForHttp', 'createAuthorizer_enableCachingForHttp' - When @true@, the result from the authorizer’s Lambda function is cached
-- for clients that use persistent HTTP connections. The results are cached
-- for the time specified by the Lambda function in
-- @refreshAfterInSeconds@. This value does not affect authorization of
-- clients that use MQTT connections.
--
-- The default value is @false@.
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
      { tags = Prelude.Nothing,
        tokenKeyName = Prelude.Nothing,
        status = Prelude.Nothing,
        signingDisabled = Prelude.Nothing,
        tokenSigningPublicKeys = Prelude.Nothing,
        enableCachingForHttp = Prelude.Nothing,
        authorizerName = pAuthorizerName_,
        authorizerFunctionArn = pAuthorizerFunctionArn_
      }

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
createAuthorizer_tags = Lens.lens (\CreateAuthorizer' {tags} -> tags) (\s@CreateAuthorizer' {} a -> s {tags = a} :: CreateAuthorizer) Prelude.. Lens.mapping Lens.coerced

-- | The name of the token key used to extract the token from the HTTP
-- headers.
createAuthorizer_tokenKeyName :: Lens.Lens' CreateAuthorizer (Prelude.Maybe Prelude.Text)
createAuthorizer_tokenKeyName = Lens.lens (\CreateAuthorizer' {tokenKeyName} -> tokenKeyName) (\s@CreateAuthorizer' {} a -> s {tokenKeyName = a} :: CreateAuthorizer)

-- | The status of the create authorizer request.
createAuthorizer_status :: Lens.Lens' CreateAuthorizer (Prelude.Maybe AuthorizerStatus)
createAuthorizer_status = Lens.lens (\CreateAuthorizer' {status} -> status) (\s@CreateAuthorizer' {} a -> s {status = a} :: CreateAuthorizer)

-- | Specifies whether IoT validates the token signature in an authorization
-- request.
createAuthorizer_signingDisabled :: Lens.Lens' CreateAuthorizer (Prelude.Maybe Prelude.Bool)
createAuthorizer_signingDisabled = Lens.lens (\CreateAuthorizer' {signingDisabled} -> signingDisabled) (\s@CreateAuthorizer' {} a -> s {signingDisabled = a} :: CreateAuthorizer)

-- | The public keys used to verify the digital signature returned by your
-- custom authentication service.
createAuthorizer_tokenSigningPublicKeys :: Lens.Lens' CreateAuthorizer (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createAuthorizer_tokenSigningPublicKeys = Lens.lens (\CreateAuthorizer' {tokenSigningPublicKeys} -> tokenSigningPublicKeys) (\s@CreateAuthorizer' {} a -> s {tokenSigningPublicKeys = a} :: CreateAuthorizer) Prelude.. Lens.mapping Lens.coerced

-- | When @true@, the result from the authorizer’s Lambda function is cached
-- for clients that use persistent HTTP connections. The results are cached
-- for the time specified by the Lambda function in
-- @refreshAfterInSeconds@. This value does not affect authorization of
-- clients that use MQTT connections.
--
-- The default value is @false@.
createAuthorizer_enableCachingForHttp :: Lens.Lens' CreateAuthorizer (Prelude.Maybe Prelude.Bool)
createAuthorizer_enableCachingForHttp = Lens.lens (\CreateAuthorizer' {enableCachingForHttp} -> enableCachingForHttp) (\s@CreateAuthorizer' {} a -> s {enableCachingForHttp = a} :: CreateAuthorizer)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAuthorizerResponse'
            Prelude.<$> (x Core..?> "authorizerArn")
            Prelude.<*> (x Core..?> "authorizerName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAuthorizer where
  hashWithSalt _salt CreateAuthorizer' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` tokenKeyName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` signingDisabled
      `Prelude.hashWithSalt` tokenSigningPublicKeys
      `Prelude.hashWithSalt` enableCachingForHttp
      `Prelude.hashWithSalt` authorizerName
      `Prelude.hashWithSalt` authorizerFunctionArn

instance Prelude.NFData CreateAuthorizer where
  rnf CreateAuthorizer' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf tokenKeyName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf signingDisabled
      `Prelude.seq` Prelude.rnf tokenSigningPublicKeys
      `Prelude.seq` Prelude.rnf enableCachingForHttp
      `Prelude.seq` Prelude.rnf authorizerName
      `Prelude.seq` Prelude.rnf authorizerFunctionArn

instance Core.ToHeaders CreateAuthorizer where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CreateAuthorizer where
  toJSON CreateAuthorizer' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("tokenKeyName" Core..=) Prelude.<$> tokenKeyName,
            ("status" Core..=) Prelude.<$> status,
            ("signingDisabled" Core..=)
              Prelude.<$> signingDisabled,
            ("tokenSigningPublicKeys" Core..=)
              Prelude.<$> tokenSigningPublicKeys,
            ("enableCachingForHttp" Core..=)
              Prelude.<$> enableCachingForHttp,
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

instance Prelude.NFData CreateAuthorizerResponse where
  rnf CreateAuthorizerResponse' {..} =
    Prelude.rnf authorizerArn
      `Prelude.seq` Prelude.rnf authorizerName
      `Prelude.seq` Prelude.rnf httpStatus
