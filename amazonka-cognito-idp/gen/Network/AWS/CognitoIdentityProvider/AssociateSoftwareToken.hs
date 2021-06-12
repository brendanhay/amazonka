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
-- Module      : Network.AWS.CognitoIdentityProvider.AssociateSoftwareToken
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a unique generated shared secret key code for the user account.
-- The request takes an access token or a session string, but not both.
module Network.AWS.CognitoIdentityProvider.AssociateSoftwareToken
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

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateSoftwareToken' smart constructor.
data AssociateSoftwareToken = AssociateSoftwareToken'
  { -- | The access token.
    accessToken :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The session which should be passed both ways in challenge-response calls
    -- to the service. This allows authentication of the user as part of the
    -- MFA setup process.
    session :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateSoftwareToken' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessToken', 'associateSoftwareToken_accessToken' - The access token.
--
-- 'session', 'associateSoftwareToken_session' - The session which should be passed both ways in challenge-response calls
-- to the service. This allows authentication of the user as part of the
-- MFA setup process.
newAssociateSoftwareToken ::
  AssociateSoftwareToken
newAssociateSoftwareToken =
  AssociateSoftwareToken'
    { accessToken = Core.Nothing,
      session = Core.Nothing
    }

-- | The access token.
associateSoftwareToken_accessToken :: Lens.Lens' AssociateSoftwareToken (Core.Maybe Core.Text)
associateSoftwareToken_accessToken = Lens.lens (\AssociateSoftwareToken' {accessToken} -> accessToken) (\s@AssociateSoftwareToken' {} a -> s {accessToken = a} :: AssociateSoftwareToken) Core.. Lens.mapping Core._Sensitive

-- | The session which should be passed both ways in challenge-response calls
-- to the service. This allows authentication of the user as part of the
-- MFA setup process.
associateSoftwareToken_session :: Lens.Lens' AssociateSoftwareToken (Core.Maybe Core.Text)
associateSoftwareToken_session = Lens.lens (\AssociateSoftwareToken' {session} -> session) (\s@AssociateSoftwareToken' {} a -> s {session = a} :: AssociateSoftwareToken)

instance Core.AWSRequest AssociateSoftwareToken where
  type
    AWSResponse AssociateSoftwareToken =
      AssociateSoftwareTokenResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateSoftwareTokenResponse'
            Core.<$> (x Core..?> "SecretCode")
            Core.<*> (x Core..?> "Session")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AssociateSoftwareToken

instance Core.NFData AssociateSoftwareToken

instance Core.ToHeaders AssociateSoftwareToken where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.AssociateSoftwareToken" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AssociateSoftwareToken where
  toJSON AssociateSoftwareToken' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AccessToken" Core..=) Core.<$> accessToken,
            ("Session" Core..=) Core.<$> session
          ]
      )

instance Core.ToPath AssociateSoftwareToken where
  toPath = Core.const "/"

instance Core.ToQuery AssociateSoftwareToken where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAssociateSoftwareTokenResponse' smart constructor.
data AssociateSoftwareTokenResponse = AssociateSoftwareTokenResponse'
  { -- | A unique generated shared secret code that is used in the TOTP algorithm
    -- to generate a one time code.
    secretCode :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The session which should be passed both ways in challenge-response calls
    -- to the service. This allows authentication of the user as part of the
    -- MFA setup process.
    session :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateSoftwareTokenResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secretCode', 'associateSoftwareTokenResponse_secretCode' - A unique generated shared secret code that is used in the TOTP algorithm
-- to generate a one time code.
--
-- 'session', 'associateSoftwareTokenResponse_session' - The session which should be passed both ways in challenge-response calls
-- to the service. This allows authentication of the user as part of the
-- MFA setup process.
--
-- 'httpStatus', 'associateSoftwareTokenResponse_httpStatus' - The response's http status code.
newAssociateSoftwareTokenResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AssociateSoftwareTokenResponse
newAssociateSoftwareTokenResponse pHttpStatus_ =
  AssociateSoftwareTokenResponse'
    { secretCode =
        Core.Nothing,
      session = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique generated shared secret code that is used in the TOTP algorithm
-- to generate a one time code.
associateSoftwareTokenResponse_secretCode :: Lens.Lens' AssociateSoftwareTokenResponse (Core.Maybe Core.Text)
associateSoftwareTokenResponse_secretCode = Lens.lens (\AssociateSoftwareTokenResponse' {secretCode} -> secretCode) (\s@AssociateSoftwareTokenResponse' {} a -> s {secretCode = a} :: AssociateSoftwareTokenResponse) Core.. Lens.mapping Core._Sensitive

-- | The session which should be passed both ways in challenge-response calls
-- to the service. This allows authentication of the user as part of the
-- MFA setup process.
associateSoftwareTokenResponse_session :: Lens.Lens' AssociateSoftwareTokenResponse (Core.Maybe Core.Text)
associateSoftwareTokenResponse_session = Lens.lens (\AssociateSoftwareTokenResponse' {session} -> session) (\s@AssociateSoftwareTokenResponse' {} a -> s {session = a} :: AssociateSoftwareTokenResponse)

-- | The response's http status code.
associateSoftwareTokenResponse_httpStatus :: Lens.Lens' AssociateSoftwareTokenResponse Core.Int
associateSoftwareTokenResponse_httpStatus = Lens.lens (\AssociateSoftwareTokenResponse' {httpStatus} -> httpStatus) (\s@AssociateSoftwareTokenResponse' {} a -> s {httpStatus = a} :: AssociateSoftwareTokenResponse)

instance Core.NFData AssociateSoftwareTokenResponse
