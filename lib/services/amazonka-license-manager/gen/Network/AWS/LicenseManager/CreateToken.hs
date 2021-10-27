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
-- Module      : Network.AWS.LicenseManager.CreateToken
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a long-lived token.
--
-- A refresh token is a JWT token used to get an access token. With an
-- access token, you can call AssumeRoleWithWebIdentity to get role
-- credentials that you can use to call License Manager to manage the
-- specified license.
module Network.AWS.LicenseManager.CreateToken
  ( -- * Creating a Request
    CreateToken (..),
    newCreateToken,

    -- * Request Lenses
    createToken_tokenProperties,
    createToken_roleArns,
    createToken_expirationInDays,
    createToken_licenseArn,
    createToken_clientToken,

    -- * Destructuring the Response
    CreateTokenResponse (..),
    newCreateTokenResponse,

    -- * Response Lenses
    createTokenResponse_token,
    createTokenResponse_tokenId,
    createTokenResponse_tokenType,
    createTokenResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LicenseManager.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateToken' smart constructor.
data CreateToken = CreateToken'
  { -- | Data specified by the caller to be included in the JWT token. The data
    -- is mapped to the amr claim of the JWT token.
    tokenProperties :: Prelude.Maybe [Prelude.Text],
    -- | Amazon Resource Name (ARN) of the IAM roles to embed in the token.
    -- License Manager does not check whether the roles are in use.
    roleArns :: Prelude.Maybe [Prelude.Text],
    -- | Token expiration, in days, counted from token creation. The default is
    -- 365 days.
    expirationInDays :: Prelude.Maybe Prelude.Int,
    -- | Amazon Resource Name (ARN) of the license. The ARN is mapped to the aud
    -- claim of the JWT token.
    licenseArn :: Prelude.Text,
    -- | Idempotency token, valid for 10 minutes.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateToken' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tokenProperties', 'createToken_tokenProperties' - Data specified by the caller to be included in the JWT token. The data
-- is mapped to the amr claim of the JWT token.
--
-- 'roleArns', 'createToken_roleArns' - Amazon Resource Name (ARN) of the IAM roles to embed in the token.
-- License Manager does not check whether the roles are in use.
--
-- 'expirationInDays', 'createToken_expirationInDays' - Token expiration, in days, counted from token creation. The default is
-- 365 days.
--
-- 'licenseArn', 'createToken_licenseArn' - Amazon Resource Name (ARN) of the license. The ARN is mapped to the aud
-- claim of the JWT token.
--
-- 'clientToken', 'createToken_clientToken' - Idempotency token, valid for 10 minutes.
newCreateToken ::
  -- | 'licenseArn'
  Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  CreateToken
newCreateToken pLicenseArn_ pClientToken_ =
  CreateToken'
    { tokenProperties = Prelude.Nothing,
      roleArns = Prelude.Nothing,
      expirationInDays = Prelude.Nothing,
      licenseArn = pLicenseArn_,
      clientToken = pClientToken_
    }

-- | Data specified by the caller to be included in the JWT token. The data
-- is mapped to the amr claim of the JWT token.
createToken_tokenProperties :: Lens.Lens' CreateToken (Prelude.Maybe [Prelude.Text])
createToken_tokenProperties = Lens.lens (\CreateToken' {tokenProperties} -> tokenProperties) (\s@CreateToken' {} a -> s {tokenProperties = a} :: CreateToken) Prelude.. Lens.mapping Lens.coerced

-- | Amazon Resource Name (ARN) of the IAM roles to embed in the token.
-- License Manager does not check whether the roles are in use.
createToken_roleArns :: Lens.Lens' CreateToken (Prelude.Maybe [Prelude.Text])
createToken_roleArns = Lens.lens (\CreateToken' {roleArns} -> roleArns) (\s@CreateToken' {} a -> s {roleArns = a} :: CreateToken) Prelude.. Lens.mapping Lens.coerced

-- | Token expiration, in days, counted from token creation. The default is
-- 365 days.
createToken_expirationInDays :: Lens.Lens' CreateToken (Prelude.Maybe Prelude.Int)
createToken_expirationInDays = Lens.lens (\CreateToken' {expirationInDays} -> expirationInDays) (\s@CreateToken' {} a -> s {expirationInDays = a} :: CreateToken)

-- | Amazon Resource Name (ARN) of the license. The ARN is mapped to the aud
-- claim of the JWT token.
createToken_licenseArn :: Lens.Lens' CreateToken Prelude.Text
createToken_licenseArn = Lens.lens (\CreateToken' {licenseArn} -> licenseArn) (\s@CreateToken' {} a -> s {licenseArn = a} :: CreateToken)

-- | Idempotency token, valid for 10 minutes.
createToken_clientToken :: Lens.Lens' CreateToken Prelude.Text
createToken_clientToken = Lens.lens (\CreateToken' {clientToken} -> clientToken) (\s@CreateToken' {} a -> s {clientToken = a} :: CreateToken)

instance Core.AWSRequest CreateToken where
  type AWSResponse CreateToken = CreateTokenResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTokenResponse'
            Prelude.<$> (x Core..?> "Token")
            Prelude.<*> (x Core..?> "TokenId")
            Prelude.<*> (x Core..?> "TokenType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateToken

instance Prelude.NFData CreateToken

instance Core.ToHeaders CreateToken where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSLicenseManager.CreateToken" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateToken where
  toJSON CreateToken' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TokenProperties" Core..=)
              Prelude.<$> tokenProperties,
            ("RoleArns" Core..=) Prelude.<$> roleArns,
            ("ExpirationInDays" Core..=)
              Prelude.<$> expirationInDays,
            Prelude.Just ("LicenseArn" Core..= licenseArn),
            Prelude.Just ("ClientToken" Core..= clientToken)
          ]
      )

instance Core.ToPath CreateToken where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateToken where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTokenResponse' smart constructor.
data CreateTokenResponse = CreateTokenResponse'
  { -- | Refresh token, encoded as a JWT token.
    token :: Prelude.Maybe Prelude.Text,
    -- | Token ID.
    tokenId :: Prelude.Maybe Prelude.Text,
    -- | Token type.
    tokenType :: Prelude.Maybe TokenType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTokenResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'token', 'createTokenResponse_token' - Refresh token, encoded as a JWT token.
--
-- 'tokenId', 'createTokenResponse_tokenId' - Token ID.
--
-- 'tokenType', 'createTokenResponse_tokenType' - Token type.
--
-- 'httpStatus', 'createTokenResponse_httpStatus' - The response's http status code.
newCreateTokenResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTokenResponse
newCreateTokenResponse pHttpStatus_ =
  CreateTokenResponse'
    { token = Prelude.Nothing,
      tokenId = Prelude.Nothing,
      tokenType = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Refresh token, encoded as a JWT token.
createTokenResponse_token :: Lens.Lens' CreateTokenResponse (Prelude.Maybe Prelude.Text)
createTokenResponse_token = Lens.lens (\CreateTokenResponse' {token} -> token) (\s@CreateTokenResponse' {} a -> s {token = a} :: CreateTokenResponse)

-- | Token ID.
createTokenResponse_tokenId :: Lens.Lens' CreateTokenResponse (Prelude.Maybe Prelude.Text)
createTokenResponse_tokenId = Lens.lens (\CreateTokenResponse' {tokenId} -> tokenId) (\s@CreateTokenResponse' {} a -> s {tokenId = a} :: CreateTokenResponse)

-- | Token type.
createTokenResponse_tokenType :: Lens.Lens' CreateTokenResponse (Prelude.Maybe TokenType)
createTokenResponse_tokenType = Lens.lens (\CreateTokenResponse' {tokenType} -> tokenType) (\s@CreateTokenResponse' {} a -> s {tokenType = a} :: CreateTokenResponse)

-- | The response's http status code.
createTokenResponse_httpStatus :: Lens.Lens' CreateTokenResponse Prelude.Int
createTokenResponse_httpStatus = Lens.lens (\CreateTokenResponse' {httpStatus} -> httpStatus) (\s@CreateTokenResponse' {} a -> s {httpStatus = a} :: CreateTokenResponse)

instance Prelude.NFData CreateTokenResponse
