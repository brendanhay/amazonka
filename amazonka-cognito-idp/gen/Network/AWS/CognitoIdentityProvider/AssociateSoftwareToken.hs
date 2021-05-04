{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateSoftwareToken' smart constructor.
data AssociateSoftwareToken = AssociateSoftwareToken'
  { -- | The access token.
    accessToken :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The session which should be passed both ways in challenge-response calls
    -- to the service. This allows authentication of the user as part of the
    -- MFA setup process.
    session :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
associateSoftwareToken_accessToken = Lens.lens (\AssociateSoftwareToken' {accessToken} -> accessToken) (\s@AssociateSoftwareToken' {} a -> s {accessToken = a} :: AssociateSoftwareToken) Prelude.. Lens.mapping Prelude._Sensitive

-- | The session which should be passed both ways in challenge-response calls
-- to the service. This allows authentication of the user as part of the
-- MFA setup process.
associateSoftwareToken_session :: Lens.Lens' AssociateSoftwareToken (Prelude.Maybe Prelude.Text)
associateSoftwareToken_session = Lens.lens (\AssociateSoftwareToken' {session} -> session) (\s@AssociateSoftwareToken' {} a -> s {session = a} :: AssociateSoftwareToken)

instance Prelude.AWSRequest AssociateSoftwareToken where
  type
    Rs AssociateSoftwareToken =
      AssociateSoftwareTokenResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateSoftwareTokenResponse'
            Prelude.<$> (x Prelude..?> "SecretCode")
            Prelude.<*> (x Prelude..?> "Session")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateSoftwareToken

instance Prelude.NFData AssociateSoftwareToken

instance Prelude.ToHeaders AssociateSoftwareToken where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.AssociateSoftwareToken" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AssociateSoftwareToken where
  toJSON AssociateSoftwareToken' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AccessToken" Prelude..=) Prelude.<$> accessToken,
            ("Session" Prelude..=) Prelude.<$> session
          ]
      )

instance Prelude.ToPath AssociateSoftwareToken where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AssociateSoftwareToken where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateSoftwareTokenResponse' smart constructor.
data AssociateSoftwareTokenResponse = AssociateSoftwareTokenResponse'
  { -- | A unique generated shared secret code that is used in the TOTP algorithm
    -- to generate a one time code.
    secretCode :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The session which should be passed both ways in challenge-response calls
    -- to the service. This allows authentication of the user as part of the
    -- MFA setup process.
    session :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
associateSoftwareTokenResponse_secretCode = Lens.lens (\AssociateSoftwareTokenResponse' {secretCode} -> secretCode) (\s@AssociateSoftwareTokenResponse' {} a -> s {secretCode = a} :: AssociateSoftwareTokenResponse) Prelude.. Lens.mapping Prelude._Sensitive

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
