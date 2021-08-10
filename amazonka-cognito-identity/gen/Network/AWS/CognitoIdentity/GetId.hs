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
-- Module      : Network.AWS.CognitoIdentity.GetId
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates (or retrieves) a Cognito ID. Supplying multiple logins will
-- create an implicit linked account.
--
-- This is a public API. You do not need any credentials to call this API.
module Network.AWS.CognitoIdentity.GetId
  ( -- * Creating a Request
    GetId (..),
    newGetId,

    -- * Request Lenses
    getId_accountId,
    getId_logins,
    getId_identityPoolId,

    -- * Destructuring the Response
    GetIdResponse (..),
    newGetIdResponse,

    -- * Response Lenses
    getIdResponse_identityId,
    getIdResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentity.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the GetId action.
--
-- /See:/ 'newGetId' smart constructor.
data GetId = GetId'
  { -- | A standard AWS account ID (9+ digits).
    accountId :: Prelude.Maybe Prelude.Text,
    -- | A set of optional name-value pairs that map provider names to provider
    -- tokens. The available provider names for @Logins@ are as follows:
    --
    -- -   Facebook: @graph.facebook.com@
    --
    -- -   Amazon Cognito user pool:
    --     @cognito-idp.\<region>.amazonaws.com\/\<YOUR_USER_POOL_ID>@, for
    --     example, @cognito-idp.us-east-1.amazonaws.com\/us-east-1_123456789@.
    --
    -- -   Google: @accounts.google.com@
    --
    -- -   Amazon: @www.amazon.com@
    --
    -- -   Twitter: @api.twitter.com@
    --
    -- -   Digits: @www.digits.com@
    logins :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | An identity pool ID in the format REGION:GUID.
    identityPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'getId_accountId' - A standard AWS account ID (9+ digits).
--
-- 'logins', 'getId_logins' - A set of optional name-value pairs that map provider names to provider
-- tokens. The available provider names for @Logins@ are as follows:
--
-- -   Facebook: @graph.facebook.com@
--
-- -   Amazon Cognito user pool:
--     @cognito-idp.\<region>.amazonaws.com\/\<YOUR_USER_POOL_ID>@, for
--     example, @cognito-idp.us-east-1.amazonaws.com\/us-east-1_123456789@.
--
-- -   Google: @accounts.google.com@
--
-- -   Amazon: @www.amazon.com@
--
-- -   Twitter: @api.twitter.com@
--
-- -   Digits: @www.digits.com@
--
-- 'identityPoolId', 'getId_identityPoolId' - An identity pool ID in the format REGION:GUID.
newGetId ::
  -- | 'identityPoolId'
  Prelude.Text ->
  GetId
newGetId pIdentityPoolId_ =
  GetId'
    { accountId = Prelude.Nothing,
      logins = Prelude.Nothing,
      identityPoolId = pIdentityPoolId_
    }

-- | A standard AWS account ID (9+ digits).
getId_accountId :: Lens.Lens' GetId (Prelude.Maybe Prelude.Text)
getId_accountId = Lens.lens (\GetId' {accountId} -> accountId) (\s@GetId' {} a -> s {accountId = a} :: GetId)

-- | A set of optional name-value pairs that map provider names to provider
-- tokens. The available provider names for @Logins@ are as follows:
--
-- -   Facebook: @graph.facebook.com@
--
-- -   Amazon Cognito user pool:
--     @cognito-idp.\<region>.amazonaws.com\/\<YOUR_USER_POOL_ID>@, for
--     example, @cognito-idp.us-east-1.amazonaws.com\/us-east-1_123456789@.
--
-- -   Google: @accounts.google.com@
--
-- -   Amazon: @www.amazon.com@
--
-- -   Twitter: @api.twitter.com@
--
-- -   Digits: @www.digits.com@
getId_logins :: Lens.Lens' GetId (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getId_logins = Lens.lens (\GetId' {logins} -> logins) (\s@GetId' {} a -> s {logins = a} :: GetId) Prelude.. Lens.mapping Lens._Coerce

-- | An identity pool ID in the format REGION:GUID.
getId_identityPoolId :: Lens.Lens' GetId Prelude.Text
getId_identityPoolId = Lens.lens (\GetId' {identityPoolId} -> identityPoolId) (\s@GetId' {} a -> s {identityPoolId = a} :: GetId)

instance Core.AWSRequest GetId where
  type AWSResponse GetId = GetIdResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetIdResponse'
            Prelude.<$> (x Core..?> "IdentityId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetId

instance Prelude.NFData GetId

instance Core.ToHeaders GetId where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityService.GetId" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetId where
  toJSON GetId' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AccountId" Core..=) Prelude.<$> accountId,
            ("Logins" Core..=) Prelude.<$> logins,
            Prelude.Just
              ("IdentityPoolId" Core..= identityPoolId)
          ]
      )

instance Core.ToPath GetId where
  toPath = Prelude.const "/"

instance Core.ToQuery GetId where
  toQuery = Prelude.const Prelude.mempty

-- | Returned in response to a GetId request.
--
-- /See:/ 'newGetIdResponse' smart constructor.
data GetIdResponse = GetIdResponse'
  { -- | A unique identifier in the format REGION:GUID.
    identityId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIdResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityId', 'getIdResponse_identityId' - A unique identifier in the format REGION:GUID.
--
-- 'httpStatus', 'getIdResponse_httpStatus' - The response's http status code.
newGetIdResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetIdResponse
newGetIdResponse pHttpStatus_ =
  GetIdResponse'
    { identityId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique identifier in the format REGION:GUID.
getIdResponse_identityId :: Lens.Lens' GetIdResponse (Prelude.Maybe Prelude.Text)
getIdResponse_identityId = Lens.lens (\GetIdResponse' {identityId} -> identityId) (\s@GetIdResponse' {} a -> s {identityId = a} :: GetIdResponse)

-- | The response's http status code.
getIdResponse_httpStatus :: Lens.Lens' GetIdResponse Prelude.Int
getIdResponse_httpStatus = Lens.lens (\GetIdResponse' {httpStatus} -> httpStatus) (\s@GetIdResponse' {} a -> s {httpStatus = a} :: GetIdResponse)

instance Prelude.NFData GetIdResponse
