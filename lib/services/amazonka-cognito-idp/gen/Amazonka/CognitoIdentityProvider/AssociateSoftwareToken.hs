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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a unique generated shared secret key code for the user account.
-- The request takes an access token or a session string, but not both.
--
-- Calling AssociateSoftwareToken immediately disassociates the existing
-- software token from the user account. If the user doesn\'t subsequently
-- verify the software token, their account is essentially set up to
-- authenticate without MFA. If MFA config is set to Optional at the user
-- pool level, the user can then login without MFA. However, if MFA is set
-- to Required for the user pool, the user will be asked to setup a new
-- software token MFA during sign in.
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
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateSoftwareToken' smart constructor.
data AssociateSoftwareToken = AssociateSoftwareToken'
  { -- | The access token.
    accessToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The session which should be passed both ways in challenge-response calls
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
-- 'accessToken', 'associateSoftwareToken_accessToken' - The access token.
--
-- 'session', 'associateSoftwareToken_session' - The session which should be passed both ways in challenge-response calls
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

-- | The access token.
associateSoftwareToken_accessToken :: Lens.Lens' AssociateSoftwareToken (Prelude.Maybe Prelude.Text)
associateSoftwareToken_accessToken = Lens.lens (\AssociateSoftwareToken' {accessToken} -> accessToken) (\s@AssociateSoftwareToken' {} a -> s {accessToken = a} :: AssociateSoftwareToken) Prelude.. Lens.mapping Core._Sensitive

-- | The session which should be passed both ways in challenge-response calls
-- to the service. This allows authentication of the user as part of the
-- MFA setup process.
associateSoftwareToken_session :: Lens.Lens' AssociateSoftwareToken (Prelude.Maybe Prelude.Text)
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
            Prelude.<$> (x Core..?> "SecretCode")
            Prelude.<*> (x Core..?> "Session")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateSoftwareToken where
  hashWithSalt _salt AssociateSoftwareToken' {..} =
    _salt `Prelude.hashWithSalt` accessToken
      `Prelude.hashWithSalt` session

instance Prelude.NFData AssociateSoftwareToken where
  rnf AssociateSoftwareToken' {..} =
    Prelude.rnf accessToken
      `Prelude.seq` Prelude.rnf session

instance Core.ToHeaders AssociateSoftwareToken where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.AssociateSoftwareToken" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AssociateSoftwareToken where
  toJSON AssociateSoftwareToken' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AccessToken" Core..=) Prelude.<$> accessToken,
            ("Session" Core..=) Prelude.<$> session
          ]
      )

instance Core.ToPath AssociateSoftwareToken where
  toPath = Prelude.const "/"

instance Core.ToQuery AssociateSoftwareToken where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateSoftwareTokenResponse' smart constructor.
data AssociateSoftwareTokenResponse = AssociateSoftwareTokenResponse'
  { -- | A unique generated shared secret code that is used in the TOTP algorithm
    -- to generate a one time code.
    secretCode :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The session which should be passed both ways in challenge-response calls
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
-- to generate a one time code.
--
-- 'session', 'associateSoftwareTokenResponse_session' - The session which should be passed both ways in challenge-response calls
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
-- to generate a one time code.
associateSoftwareTokenResponse_secretCode :: Lens.Lens' AssociateSoftwareTokenResponse (Prelude.Maybe Prelude.Text)
associateSoftwareTokenResponse_secretCode = Lens.lens (\AssociateSoftwareTokenResponse' {secretCode} -> secretCode) (\s@AssociateSoftwareTokenResponse' {} a -> s {secretCode = a} :: AssociateSoftwareTokenResponse) Prelude.. Lens.mapping Core._Sensitive

-- | The session which should be passed both ways in challenge-response calls
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
    Prelude.rnf secretCode
      `Prelude.seq` Prelude.rnf session
      `Prelude.seq` Prelude.rnf httpStatus
