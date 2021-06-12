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
-- Module      : Network.AWS.CognitoIdentityProvider.VerifySoftwareToken
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this API to register a user\'s entered TOTP code and mark the
-- user\'s software token MFA status as \"verified\" if successful. The
-- request takes an access token or a session string, but not both.
module Network.AWS.CognitoIdentityProvider.VerifySoftwareToken
  ( -- * Creating a Request
    VerifySoftwareToken (..),
    newVerifySoftwareToken,

    -- * Request Lenses
    verifySoftwareToken_friendlyDeviceName,
    verifySoftwareToken_accessToken,
    verifySoftwareToken_session,
    verifySoftwareToken_userCode,

    -- * Destructuring the Response
    VerifySoftwareTokenResponse (..),
    newVerifySoftwareTokenResponse,

    -- * Response Lenses
    verifySoftwareTokenResponse_status,
    verifySoftwareTokenResponse_session,
    verifySoftwareTokenResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newVerifySoftwareToken' smart constructor.
data VerifySoftwareToken = VerifySoftwareToken'
  { -- | The friendly device name.
    friendlyDeviceName :: Core.Maybe Core.Text,
    -- | The access token.
    accessToken :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The session which should be passed both ways in challenge-response calls
    -- to the service.
    session :: Core.Maybe Core.Text,
    -- | The one time password computed using the secret code returned by
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AssociateSoftwareToken.html AssociateSoftwareToken\">.
    userCode :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'VerifySoftwareToken' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'friendlyDeviceName', 'verifySoftwareToken_friendlyDeviceName' - The friendly device name.
--
-- 'accessToken', 'verifySoftwareToken_accessToken' - The access token.
--
-- 'session', 'verifySoftwareToken_session' - The session which should be passed both ways in challenge-response calls
-- to the service.
--
-- 'userCode', 'verifySoftwareToken_userCode' - The one time password computed using the secret code returned by
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AssociateSoftwareToken.html AssociateSoftwareToken\">.
newVerifySoftwareToken ::
  -- | 'userCode'
  Core.Text ->
  VerifySoftwareToken
newVerifySoftwareToken pUserCode_ =
  VerifySoftwareToken'
    { friendlyDeviceName =
        Core.Nothing,
      accessToken = Core.Nothing,
      session = Core.Nothing,
      userCode = pUserCode_
    }

-- | The friendly device name.
verifySoftwareToken_friendlyDeviceName :: Lens.Lens' VerifySoftwareToken (Core.Maybe Core.Text)
verifySoftwareToken_friendlyDeviceName = Lens.lens (\VerifySoftwareToken' {friendlyDeviceName} -> friendlyDeviceName) (\s@VerifySoftwareToken' {} a -> s {friendlyDeviceName = a} :: VerifySoftwareToken)

-- | The access token.
verifySoftwareToken_accessToken :: Lens.Lens' VerifySoftwareToken (Core.Maybe Core.Text)
verifySoftwareToken_accessToken = Lens.lens (\VerifySoftwareToken' {accessToken} -> accessToken) (\s@VerifySoftwareToken' {} a -> s {accessToken = a} :: VerifySoftwareToken) Core.. Lens.mapping Core._Sensitive

-- | The session which should be passed both ways in challenge-response calls
-- to the service.
verifySoftwareToken_session :: Lens.Lens' VerifySoftwareToken (Core.Maybe Core.Text)
verifySoftwareToken_session = Lens.lens (\VerifySoftwareToken' {session} -> session) (\s@VerifySoftwareToken' {} a -> s {session = a} :: VerifySoftwareToken)

-- | The one time password computed using the secret code returned by
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AssociateSoftwareToken.html AssociateSoftwareToken\">.
verifySoftwareToken_userCode :: Lens.Lens' VerifySoftwareToken Core.Text
verifySoftwareToken_userCode = Lens.lens (\VerifySoftwareToken' {userCode} -> userCode) (\s@VerifySoftwareToken' {} a -> s {userCode = a} :: VerifySoftwareToken)

instance Core.AWSRequest VerifySoftwareToken where
  type
    AWSResponse VerifySoftwareToken =
      VerifySoftwareTokenResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          VerifySoftwareTokenResponse'
            Core.<$> (x Core..?> "Status")
            Core.<*> (x Core..?> "Session")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable VerifySoftwareToken

instance Core.NFData VerifySoftwareToken

instance Core.ToHeaders VerifySoftwareToken where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.VerifySoftwareToken" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON VerifySoftwareToken where
  toJSON VerifySoftwareToken' {..} =
    Core.object
      ( Core.catMaybes
          [ ("FriendlyDeviceName" Core..=)
              Core.<$> friendlyDeviceName,
            ("AccessToken" Core..=) Core.<$> accessToken,
            ("Session" Core..=) Core.<$> session,
            Core.Just ("UserCode" Core..= userCode)
          ]
      )

instance Core.ToPath VerifySoftwareToken where
  toPath = Core.const "/"

instance Core.ToQuery VerifySoftwareToken where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newVerifySoftwareTokenResponse' smart constructor.
data VerifySoftwareTokenResponse = VerifySoftwareTokenResponse'
  { -- | The status of the verify software token.
    status :: Core.Maybe VerifySoftwareTokenResponseType,
    -- | The session which should be passed both ways in challenge-response calls
    -- to the service.
    session :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VerifySoftwareTokenResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'verifySoftwareTokenResponse_status' - The status of the verify software token.
--
-- 'session', 'verifySoftwareTokenResponse_session' - The session which should be passed both ways in challenge-response calls
-- to the service.
--
-- 'httpStatus', 'verifySoftwareTokenResponse_httpStatus' - The response's http status code.
newVerifySoftwareTokenResponse ::
  -- | 'httpStatus'
  Core.Int ->
  VerifySoftwareTokenResponse
newVerifySoftwareTokenResponse pHttpStatus_ =
  VerifySoftwareTokenResponse'
    { status = Core.Nothing,
      session = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the verify software token.
verifySoftwareTokenResponse_status :: Lens.Lens' VerifySoftwareTokenResponse (Core.Maybe VerifySoftwareTokenResponseType)
verifySoftwareTokenResponse_status = Lens.lens (\VerifySoftwareTokenResponse' {status} -> status) (\s@VerifySoftwareTokenResponse' {} a -> s {status = a} :: VerifySoftwareTokenResponse)

-- | The session which should be passed both ways in challenge-response calls
-- to the service.
verifySoftwareTokenResponse_session :: Lens.Lens' VerifySoftwareTokenResponse (Core.Maybe Core.Text)
verifySoftwareTokenResponse_session = Lens.lens (\VerifySoftwareTokenResponse' {session} -> session) (\s@VerifySoftwareTokenResponse' {} a -> s {session = a} :: VerifySoftwareTokenResponse)

-- | The response's http status code.
verifySoftwareTokenResponse_httpStatus :: Lens.Lens' VerifySoftwareTokenResponse Core.Int
verifySoftwareTokenResponse_httpStatus = Lens.lens (\VerifySoftwareTokenResponse' {httpStatus} -> httpStatus) (\s@VerifySoftwareTokenResponse' {} a -> s {httpStatus = a} :: VerifySoftwareTokenResponse)

instance Core.NFData VerifySoftwareTokenResponse
