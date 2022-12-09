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
-- Module      : Amazonka.CognitoIdentityProvider.VerifySoftwareToken
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this API to register a user\'s entered time-based one-time password
-- (TOTP) code and mark the user\'s software token MFA status as
-- \"verified\" if successful. The request takes an access token or a
-- session string, but not both.
module Amazonka.CognitoIdentityProvider.VerifySoftwareToken
  ( -- * Creating a Request
    VerifySoftwareToken (..),
    newVerifySoftwareToken,

    -- * Request Lenses
    verifySoftwareToken_accessToken,
    verifySoftwareToken_friendlyDeviceName,
    verifySoftwareToken_session,
    verifySoftwareToken_userCode,

    -- * Destructuring the Response
    VerifySoftwareTokenResponse (..),
    newVerifySoftwareTokenResponse,

    -- * Response Lenses
    verifySoftwareTokenResponse_session,
    verifySoftwareTokenResponse_status,
    verifySoftwareTokenResponse_httpStatus,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newVerifySoftwareToken' smart constructor.
data VerifySoftwareToken = VerifySoftwareToken'
  { -- | A valid access token that Amazon Cognito issued to the user whose
    -- software token you want to verify.
    accessToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The friendly device name.
    friendlyDeviceName :: Prelude.Maybe Prelude.Text,
    -- | The session that should be passed both ways in challenge-response calls
    -- to the service.
    session :: Prelude.Maybe Prelude.Text,
    -- | The one- time password computed using the secret code returned by
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AssociateSoftwareToken.html AssociateSoftwareToken>.
    userCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifySoftwareToken' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessToken', 'verifySoftwareToken_accessToken' - A valid access token that Amazon Cognito issued to the user whose
-- software token you want to verify.
--
-- 'friendlyDeviceName', 'verifySoftwareToken_friendlyDeviceName' - The friendly device name.
--
-- 'session', 'verifySoftwareToken_session' - The session that should be passed both ways in challenge-response calls
-- to the service.
--
-- 'userCode', 'verifySoftwareToken_userCode' - The one- time password computed using the secret code returned by
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AssociateSoftwareToken.html AssociateSoftwareToken>.
newVerifySoftwareToken ::
  -- | 'userCode'
  Prelude.Text ->
  VerifySoftwareToken
newVerifySoftwareToken pUserCode_ =
  VerifySoftwareToken'
    { accessToken = Prelude.Nothing,
      friendlyDeviceName = Prelude.Nothing,
      session = Prelude.Nothing,
      userCode = pUserCode_
    }

-- | A valid access token that Amazon Cognito issued to the user whose
-- software token you want to verify.
verifySoftwareToken_accessToken :: Lens.Lens' VerifySoftwareToken (Prelude.Maybe Prelude.Text)
verifySoftwareToken_accessToken = Lens.lens (\VerifySoftwareToken' {accessToken} -> accessToken) (\s@VerifySoftwareToken' {} a -> s {accessToken = a} :: VerifySoftwareToken) Prelude.. Lens.mapping Data._Sensitive

-- | The friendly device name.
verifySoftwareToken_friendlyDeviceName :: Lens.Lens' VerifySoftwareToken (Prelude.Maybe Prelude.Text)
verifySoftwareToken_friendlyDeviceName = Lens.lens (\VerifySoftwareToken' {friendlyDeviceName} -> friendlyDeviceName) (\s@VerifySoftwareToken' {} a -> s {friendlyDeviceName = a} :: VerifySoftwareToken)

-- | The session that should be passed both ways in challenge-response calls
-- to the service.
verifySoftwareToken_session :: Lens.Lens' VerifySoftwareToken (Prelude.Maybe Prelude.Text)
verifySoftwareToken_session = Lens.lens (\VerifySoftwareToken' {session} -> session) (\s@VerifySoftwareToken' {} a -> s {session = a} :: VerifySoftwareToken)

-- | The one- time password computed using the secret code returned by
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AssociateSoftwareToken.html AssociateSoftwareToken>.
verifySoftwareToken_userCode :: Lens.Lens' VerifySoftwareToken Prelude.Text
verifySoftwareToken_userCode = Lens.lens (\VerifySoftwareToken' {userCode} -> userCode) (\s@VerifySoftwareToken' {} a -> s {userCode = a} :: VerifySoftwareToken)

instance Core.AWSRequest VerifySoftwareToken where
  type
    AWSResponse VerifySoftwareToken =
      VerifySoftwareTokenResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          VerifySoftwareTokenResponse'
            Prelude.<$> (x Data..?> "Session")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable VerifySoftwareToken where
  hashWithSalt _salt VerifySoftwareToken' {..} =
    _salt `Prelude.hashWithSalt` accessToken
      `Prelude.hashWithSalt` friendlyDeviceName
      `Prelude.hashWithSalt` session
      `Prelude.hashWithSalt` userCode

instance Prelude.NFData VerifySoftwareToken where
  rnf VerifySoftwareToken' {..} =
    Prelude.rnf accessToken
      `Prelude.seq` Prelude.rnf friendlyDeviceName
      `Prelude.seq` Prelude.rnf session
      `Prelude.seq` Prelude.rnf userCode

instance Data.ToHeaders VerifySoftwareToken where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.VerifySoftwareToken" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON VerifySoftwareToken where
  toJSON VerifySoftwareToken' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccessToken" Data..=) Prelude.<$> accessToken,
            ("FriendlyDeviceName" Data..=)
              Prelude.<$> friendlyDeviceName,
            ("Session" Data..=) Prelude.<$> session,
            Prelude.Just ("UserCode" Data..= userCode)
          ]
      )

instance Data.ToPath VerifySoftwareToken where
  toPath = Prelude.const "/"

instance Data.ToQuery VerifySoftwareToken where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newVerifySoftwareTokenResponse' smart constructor.
data VerifySoftwareTokenResponse = VerifySoftwareTokenResponse'
  { -- | The session that should be passed both ways in challenge-response calls
    -- to the service.
    session :: Prelude.Maybe Prelude.Text,
    -- | The status of the verify software token.
    status :: Prelude.Maybe VerifySoftwareTokenResponseType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifySoftwareTokenResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'session', 'verifySoftwareTokenResponse_session' - The session that should be passed both ways in challenge-response calls
-- to the service.
--
-- 'status', 'verifySoftwareTokenResponse_status' - The status of the verify software token.
--
-- 'httpStatus', 'verifySoftwareTokenResponse_httpStatus' - The response's http status code.
newVerifySoftwareTokenResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  VerifySoftwareTokenResponse
newVerifySoftwareTokenResponse pHttpStatus_ =
  VerifySoftwareTokenResponse'
    { session =
        Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The session that should be passed both ways in challenge-response calls
-- to the service.
verifySoftwareTokenResponse_session :: Lens.Lens' VerifySoftwareTokenResponse (Prelude.Maybe Prelude.Text)
verifySoftwareTokenResponse_session = Lens.lens (\VerifySoftwareTokenResponse' {session} -> session) (\s@VerifySoftwareTokenResponse' {} a -> s {session = a} :: VerifySoftwareTokenResponse)

-- | The status of the verify software token.
verifySoftwareTokenResponse_status :: Lens.Lens' VerifySoftwareTokenResponse (Prelude.Maybe VerifySoftwareTokenResponseType)
verifySoftwareTokenResponse_status = Lens.lens (\VerifySoftwareTokenResponse' {status} -> status) (\s@VerifySoftwareTokenResponse' {} a -> s {status = a} :: VerifySoftwareTokenResponse)

-- | The response's http status code.
verifySoftwareTokenResponse_httpStatus :: Lens.Lens' VerifySoftwareTokenResponse Prelude.Int
verifySoftwareTokenResponse_httpStatus = Lens.lens (\VerifySoftwareTokenResponse' {httpStatus} -> httpStatus) (\s@VerifySoftwareTokenResponse' {} a -> s {httpStatus = a} :: VerifySoftwareTokenResponse)

instance Prelude.NFData VerifySoftwareTokenResponse where
  rnf VerifySoftwareTokenResponse' {..} =
    Prelude.rnf session
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
