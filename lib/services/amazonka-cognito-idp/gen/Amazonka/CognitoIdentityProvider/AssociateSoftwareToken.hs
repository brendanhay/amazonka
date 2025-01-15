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
-- Module      : Amazonka.CognitoIdentityProvider.AssociateSoftwareToken
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Begins setup of time-based one-time password (TOTP) multi-factor
-- authentication (MFA) for a user, with a unique private key that Amazon
-- Cognito generates and returns in the API response. You can authorize an
-- @AssociateSoftwareToken@ request with either the user\'s access token,
-- or a session string from a challenge response that you received from
-- Amazon Cognito.
--
-- Amazon Cognito disassociates an existing software token when you verify
-- the new token in a
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_VerifySoftwareToken.html VerifySoftwareToken>
-- API request. If you don\'t verify the software token and your user pool
-- doesn\'t require MFA, the user can then authenticate with user name and
-- password credentials alone. If your user pool requires TOTP MFA, Amazon
-- Cognito generates an @MFA_SETUP@ or @SOFTWARE_TOKEN_SETUP@ challenge
-- each time your user signs. Complete setup with @AssociateSoftwareToken@
-- and @VerifySoftwareToken@.
--
-- After you set up software token MFA for your user, Amazon Cognito
-- generates a @SOFTWARE_TOKEN_MFA@ challenge when they authenticate.
-- Respond to this challenge with your user\'s TOTP.
module Amazonka.CognitoIdentityProvider.AssociateSoftwareToken
  ( -- * Creating a Request
    AssociateSoftwareToken (..),
    newAssociateSoftwareToken,

    -- * Request Lenses
    associateSoftwareToken_accessToken,
    associateSoftwareToken_session,

    -- * Destructuring the Response
    AssociateSoftwareTokenResponse (..),
    newAssociateSoftwareTokenResponse,

    -- * Response Lenses
    associateSoftwareTokenResponse_secretCode,
    associateSoftwareTokenResponse_session,
    associateSoftwareTokenResponse_httpStatus,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateSoftwareToken' smart constructor.
data AssociateSoftwareToken = AssociateSoftwareToken'
  { -- | A valid access token that Amazon Cognito issued to the user whose
    -- software token you want to generate.
    accessToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The session that should be passed both ways in challenge-response calls
    -- to the service. This allows authentication of the user as part of the
    -- MFA setup process.
    session :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateSoftwareToken' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessToken', 'associateSoftwareToken_accessToken' - A valid access token that Amazon Cognito issued to the user whose
-- software token you want to generate.
--
-- 'session', 'associateSoftwareToken_session' - The session that should be passed both ways in challenge-response calls
-- to the service. This allows authentication of the user as part of the
-- MFA setup process.
newAssociateSoftwareToken ::
  AssociateSoftwareToken
newAssociateSoftwareToken =
  AssociateSoftwareToken'
    { accessToken =
        Prelude.Nothing,
      session = Prelude.Nothing
    }

-- | A valid access token that Amazon Cognito issued to the user whose
-- software token you want to generate.
associateSoftwareToken_accessToken :: Lens.Lens' AssociateSoftwareToken (Prelude.Maybe Prelude.Text)
associateSoftwareToken_accessToken = Lens.lens (\AssociateSoftwareToken' {accessToken} -> accessToken) (\s@AssociateSoftwareToken' {} a -> s {accessToken = a} :: AssociateSoftwareToken) Prelude.. Lens.mapping Data._Sensitive

-- | The session that should be passed both ways in challenge-response calls
-- to the service. This allows authentication of the user as part of the
-- MFA setup process.
associateSoftwareToken_session :: Lens.Lens' AssociateSoftwareToken (Prelude.Maybe Prelude.Text)
associateSoftwareToken_session = Lens.lens (\AssociateSoftwareToken' {session} -> session) (\s@AssociateSoftwareToken' {} a -> s {session = a} :: AssociateSoftwareToken)

instance Core.AWSRequest AssociateSoftwareToken where
  type
    AWSResponse AssociateSoftwareToken =
      AssociateSoftwareTokenResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateSoftwareTokenResponse'
            Prelude.<$> (x Data..?> "SecretCode")
            Prelude.<*> (x Data..?> "Session")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateSoftwareToken where
  hashWithSalt _salt AssociateSoftwareToken' {..} =
    _salt
      `Prelude.hashWithSalt` accessToken
      `Prelude.hashWithSalt` session

instance Prelude.NFData AssociateSoftwareToken where
  rnf AssociateSoftwareToken' {..} =
    Prelude.rnf accessToken `Prelude.seq`
      Prelude.rnf session

instance Data.ToHeaders AssociateSoftwareToken where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.AssociateSoftwareToken" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateSoftwareToken where
  toJSON AssociateSoftwareToken' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccessToken" Data..=) Prelude.<$> accessToken,
            ("Session" Data..=) Prelude.<$> session
          ]
      )

instance Data.ToPath AssociateSoftwareToken where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateSoftwareToken where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateSoftwareTokenResponse' smart constructor.
data AssociateSoftwareTokenResponse = AssociateSoftwareTokenResponse'
  { -- | A unique generated shared secret code that is used in the TOTP algorithm
    -- to generate a one-time code.
    secretCode :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The session that should be passed both ways in challenge-response calls
    -- to the service. This allows authentication of the user as part of the
    -- MFA setup process.
    session :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateSoftwareTokenResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secretCode', 'associateSoftwareTokenResponse_secretCode' - A unique generated shared secret code that is used in the TOTP algorithm
-- to generate a one-time code.
--
-- 'session', 'associateSoftwareTokenResponse_session' - The session that should be passed both ways in challenge-response calls
-- to the service. This allows authentication of the user as part of the
-- MFA setup process.
--
-- 'httpStatus', 'associateSoftwareTokenResponse_httpStatus' - The response's http status code.
newAssociateSoftwareTokenResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateSoftwareTokenResponse
newAssociateSoftwareTokenResponse pHttpStatus_ =
  AssociateSoftwareTokenResponse'
    { secretCode =
        Prelude.Nothing,
      session = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique generated shared secret code that is used in the TOTP algorithm
-- to generate a one-time code.
associateSoftwareTokenResponse_secretCode :: Lens.Lens' AssociateSoftwareTokenResponse (Prelude.Maybe Prelude.Text)
associateSoftwareTokenResponse_secretCode = Lens.lens (\AssociateSoftwareTokenResponse' {secretCode} -> secretCode) (\s@AssociateSoftwareTokenResponse' {} a -> s {secretCode = a} :: AssociateSoftwareTokenResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The session that should be passed both ways in challenge-response calls
-- to the service. This allows authentication of the user as part of the
-- MFA setup process.
associateSoftwareTokenResponse_session :: Lens.Lens' AssociateSoftwareTokenResponse (Prelude.Maybe Prelude.Text)
associateSoftwareTokenResponse_session = Lens.lens (\AssociateSoftwareTokenResponse' {session} -> session) (\s@AssociateSoftwareTokenResponse' {} a -> s {session = a} :: AssociateSoftwareTokenResponse)

-- | The response's http status code.
associateSoftwareTokenResponse_httpStatus :: Lens.Lens' AssociateSoftwareTokenResponse Prelude.Int
associateSoftwareTokenResponse_httpStatus = Lens.lens (\AssociateSoftwareTokenResponse' {httpStatus} -> httpStatus) (\s@AssociateSoftwareTokenResponse' {} a -> s {httpStatus = a} :: AssociateSoftwareTokenResponse)

instance
  Prelude.NFData
    AssociateSoftwareTokenResponse
  where
  rnf AssociateSoftwareTokenResponse' {..} =
    Prelude.rnf secretCode `Prelude.seq`
      Prelude.rnf session `Prelude.seq`
        Prelude.rnf httpStatus
