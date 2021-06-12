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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the GetId action.
--
-- /See:/ 'newGetId' smart constructor.
data GetId = GetId'
  { -- | A standard AWS account ID (9+ digits).
    accountId :: Core.Maybe Core.Text,
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
    logins :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | An identity pool ID in the format REGION:GUID.
    identityPoolId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetId
newGetId pIdentityPoolId_ =
  GetId'
    { accountId = Core.Nothing,
      logins = Core.Nothing,
      identityPoolId = pIdentityPoolId_
    }

-- | A standard AWS account ID (9+ digits).
getId_accountId :: Lens.Lens' GetId (Core.Maybe Core.Text)
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
getId_logins :: Lens.Lens' GetId (Core.Maybe (Core.HashMap Core.Text Core.Text))
getId_logins = Lens.lens (\GetId' {logins} -> logins) (\s@GetId' {} a -> s {logins = a} :: GetId) Core.. Lens.mapping Lens._Coerce

-- | An identity pool ID in the format REGION:GUID.
getId_identityPoolId :: Lens.Lens' GetId Core.Text
getId_identityPoolId = Lens.lens (\GetId' {identityPoolId} -> identityPoolId) (\s@GetId' {} a -> s {identityPoolId = a} :: GetId)

instance Core.AWSRequest GetId where
  type AWSResponse GetId = GetIdResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetIdResponse'
            Core.<$> (x Core..?> "IdentityId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetId

instance Core.NFData GetId

instance Core.ToHeaders GetId where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityService.GetId" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetId where
  toJSON GetId' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AccountId" Core..=) Core.<$> accountId,
            ("Logins" Core..=) Core.<$> logins,
            Core.Just ("IdentityPoolId" Core..= identityPoolId)
          ]
      )

instance Core.ToPath GetId where
  toPath = Core.const "/"

instance Core.ToQuery GetId where
  toQuery = Core.const Core.mempty

-- | Returned in response to a GetId request.
--
-- /See:/ 'newGetIdResponse' smart constructor.
data GetIdResponse = GetIdResponse'
  { -- | A unique identifier in the format REGION:GUID.
    identityId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetIdResponse
newGetIdResponse pHttpStatus_ =
  GetIdResponse'
    { identityId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique identifier in the format REGION:GUID.
getIdResponse_identityId :: Lens.Lens' GetIdResponse (Core.Maybe Core.Text)
getIdResponse_identityId = Lens.lens (\GetIdResponse' {identityId} -> identityId) (\s@GetIdResponse' {} a -> s {identityId = a} :: GetIdResponse)

-- | The response's http status code.
getIdResponse_httpStatus :: Lens.Lens' GetIdResponse Core.Int
getIdResponse_httpStatus = Lens.lens (\GetIdResponse' {httpStatus} -> httpStatus) (\s@GetIdResponse' {} a -> s {httpStatus = a} :: GetIdResponse)

instance Core.NFData GetIdResponse
