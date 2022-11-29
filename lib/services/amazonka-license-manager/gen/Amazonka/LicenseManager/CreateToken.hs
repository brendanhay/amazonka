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
-- Module      : Amazonka.LicenseManager.CreateToken
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.LicenseManager.CreateToken
  ( -- * Creating a Request
    CreateToken (..),
    newCreateToken,

    -- * Request Lenses
    createToken_roleArns,
    createToken_expirationInDays,
    createToken_tokenProperties,
    createToken_licenseArn,
    createToken_clientToken,

    -- * Destructuring the Response
    CreateTokenResponse (..),
    newCreateTokenResponse,

    -- * Response Lenses
    createTokenResponse_tokenId,
    createTokenResponse_tokenType,
    createTokenResponse_token,
    createTokenResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateToken' smart constructor.
data CreateToken = CreateToken'
  { -- | Amazon Resource Name (ARN) of the IAM roles to embed in the token.
    -- License Manager does not check whether the roles are in use.
    roleArns :: Prelude.Maybe [Prelude.Text],
    -- | Token expiration, in days, counted from token creation. The default is
    -- 365 days.
    expirationInDays :: Prelude.Maybe Prelude.Int,
    -- | Data specified by the caller to be included in the JWT token. The data
    -- is mapped to the amr claim of the JWT token.
    tokenProperties :: Prelude.Maybe [Prelude.Text],
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
-- 'roleArns', 'createToken_roleArns' - Amazon Resource Name (ARN) of the IAM roles to embed in the token.
-- License Manager does not check whether the roles are in use.
--
-- 'expirationInDays', 'createToken_expirationInDays' - Token expiration, in days, counted from token creation. The default is
-- 365 days.
--
-- 'tokenProperties', 'createToken_tokenProperties' - Data specified by the caller to be included in the JWT token. The data
-- is mapped to the amr claim of the JWT token.
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
    { roleArns = Prelude.Nothing,
      expirationInDays = Prelude.Nothing,
      tokenProperties = Prelude.Nothing,
      licenseArn = pLicenseArn_,
      clientToken = pClientToken_
    }

-- | Amazon Resource Name (ARN) of the IAM roles to embed in the token.
-- License Manager does not check whether the roles are in use.
createToken_roleArns :: Lens.Lens' CreateToken (Prelude.Maybe [Prelude.Text])
createToken_roleArns = Lens.lens (\CreateToken' {roleArns} -> roleArns) (\s@CreateToken' {} a -> s {roleArns = a} :: CreateToken) Prelude.. Lens.mapping Lens.coerced

-- | Token expiration, in days, counted from token creation. The default is
-- 365 days.
createToken_expirationInDays :: Lens.Lens' CreateToken (Prelude.Maybe Prelude.Int)
createToken_expirationInDays = Lens.lens (\CreateToken' {expirationInDays} -> expirationInDays) (\s@CreateToken' {} a -> s {expirationInDays = a} :: CreateToken)

-- | Data specified by the caller to be included in the JWT token. The data
-- is mapped to the amr claim of the JWT token.
createToken_tokenProperties :: Lens.Lens' CreateToken (Prelude.Maybe [Prelude.Text])
createToken_tokenProperties = Lens.lens (\CreateToken' {tokenProperties} -> tokenProperties) (\s@CreateToken' {} a -> s {tokenProperties = a} :: CreateToken) Prelude.. Lens.mapping Lens.coerced

-- | Amazon Resource Name (ARN) of the license. The ARN is mapped to the aud
-- claim of the JWT token.
createToken_licenseArn :: Lens.Lens' CreateToken Prelude.Text
createToken_licenseArn = Lens.lens (\CreateToken' {licenseArn} -> licenseArn) (\s@CreateToken' {} a -> s {licenseArn = a} :: CreateToken)

-- | Idempotency token, valid for 10 minutes.
createToken_clientToken :: Lens.Lens' CreateToken Prelude.Text
createToken_clientToken = Lens.lens (\CreateToken' {clientToken} -> clientToken) (\s@CreateToken' {} a -> s {clientToken = a} :: CreateToken)

instance Core.AWSRequest CreateToken where
  type AWSResponse CreateToken = CreateTokenResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTokenResponse'
            Prelude.<$> (x Core..?> "TokenId")
            Prelude.<*> (x Core..?> "TokenType")
            Prelude.<*> (x Core..?> "Token")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateToken where
  hashWithSalt _salt CreateToken' {..} =
    _salt `Prelude.hashWithSalt` roleArns
      `Prelude.hashWithSalt` expirationInDays
      `Prelude.hashWithSalt` tokenProperties
      `Prelude.hashWithSalt` licenseArn
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData CreateToken where
  rnf CreateToken' {..} =
    Prelude.rnf roleArns
      `Prelude.seq` Prelude.rnf expirationInDays
      `Prelude.seq` Prelude.rnf tokenProperties
      `Prelude.seq` Prelude.rnf licenseArn
      `Prelude.seq` Prelude.rnf clientToken

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
          [ ("RoleArns" Core..=) Prelude.<$> roleArns,
            ("ExpirationInDays" Core..=)
              Prelude.<$> expirationInDays,
            ("TokenProperties" Core..=)
              Prelude.<$> tokenProperties,
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
  { -- | Token ID.
    tokenId :: Prelude.Maybe Prelude.Text,
    -- | Token type.
    tokenType :: Prelude.Maybe TokenType,
    -- | Refresh token, encoded as a JWT token.
    token :: Prelude.Maybe Prelude.Text,
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
-- 'tokenId', 'createTokenResponse_tokenId' - Token ID.
--
-- 'tokenType', 'createTokenResponse_tokenType' - Token type.
--
-- 'token', 'createTokenResponse_token' - Refresh token, encoded as a JWT token.
--
-- 'httpStatus', 'createTokenResponse_httpStatus' - The response's http status code.
newCreateTokenResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTokenResponse
newCreateTokenResponse pHttpStatus_ =
  CreateTokenResponse'
    { tokenId = Prelude.Nothing,
      tokenType = Prelude.Nothing,
      token = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token ID.
createTokenResponse_tokenId :: Lens.Lens' CreateTokenResponse (Prelude.Maybe Prelude.Text)
createTokenResponse_tokenId = Lens.lens (\CreateTokenResponse' {tokenId} -> tokenId) (\s@CreateTokenResponse' {} a -> s {tokenId = a} :: CreateTokenResponse)

-- | Token type.
createTokenResponse_tokenType :: Lens.Lens' CreateTokenResponse (Prelude.Maybe TokenType)
createTokenResponse_tokenType = Lens.lens (\CreateTokenResponse' {tokenType} -> tokenType) (\s@CreateTokenResponse' {} a -> s {tokenType = a} :: CreateTokenResponse)

-- | Refresh token, encoded as a JWT token.
createTokenResponse_token :: Lens.Lens' CreateTokenResponse (Prelude.Maybe Prelude.Text)
createTokenResponse_token = Lens.lens (\CreateTokenResponse' {token} -> token) (\s@CreateTokenResponse' {} a -> s {token = a} :: CreateTokenResponse)

-- | The response's http status code.
createTokenResponse_httpStatus :: Lens.Lens' CreateTokenResponse Prelude.Int
createTokenResponse_httpStatus = Lens.lens (\CreateTokenResponse' {httpStatus} -> httpStatus) (\s@CreateTokenResponse' {} a -> s {httpStatus = a} :: CreateTokenResponse)

instance Prelude.NFData CreateTokenResponse where
  rnf CreateTokenResponse' {..} =
    Prelude.rnf tokenId
      `Prelude.seq` Prelude.rnf tokenType
      `Prelude.seq` Prelude.rnf token
      `Prelude.seq` Prelude.rnf httpStatus
