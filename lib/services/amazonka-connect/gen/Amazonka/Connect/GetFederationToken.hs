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
-- Module      : Amazonka.Connect.GetFederationToken
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a token for federation.
--
-- This API doesn\'t support root users. If you try to invoke
-- GetFederationToken with root credentials, an error message similar to
-- the following one appears:
--
-- @Provided identity: Principal: .... User: .... cannot be used for federation with Amazon Connect@
module Amazonka.Connect.GetFederationToken
  ( -- * Creating a Request
    GetFederationToken (..),
    newGetFederationToken,

    -- * Request Lenses
    getFederationToken_instanceId,

    -- * Destructuring the Response
    GetFederationTokenResponse (..),
    newGetFederationTokenResponse,

    -- * Response Lenses
    getFederationTokenResponse_credentials,
    getFederationTokenResponse_signInUrl,
    getFederationTokenResponse_userArn,
    getFederationTokenResponse_userId,
    getFederationTokenResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetFederationToken' smart constructor.
data GetFederationToken = GetFederationToken'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFederationToken' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'getFederationToken_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
newGetFederationToken ::
  -- | 'instanceId'
  Prelude.Text ->
  GetFederationToken
newGetFederationToken pInstanceId_ =
  GetFederationToken' {instanceId = pInstanceId_}

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
getFederationToken_instanceId :: Lens.Lens' GetFederationToken Prelude.Text
getFederationToken_instanceId = Lens.lens (\GetFederationToken' {instanceId} -> instanceId) (\s@GetFederationToken' {} a -> s {instanceId = a} :: GetFederationToken)

instance Core.AWSRequest GetFederationToken where
  type
    AWSResponse GetFederationToken =
      GetFederationTokenResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFederationTokenResponse'
            Prelude.<$> (x Data..?> "Credentials")
            Prelude.<*> (x Data..?> "SignInUrl")
            Prelude.<*> (x Data..?> "UserArn")
            Prelude.<*> (x Data..?> "UserId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetFederationToken where
  hashWithSalt _salt GetFederationToken' {..} =
    _salt `Prelude.hashWithSalt` instanceId

instance Prelude.NFData GetFederationToken where
  rnf GetFederationToken' {..} = Prelude.rnf instanceId

instance Data.ToHeaders GetFederationToken where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetFederationToken where
  toPath GetFederationToken' {..} =
    Prelude.mconcat
      ["/user/federate/", Data.toBS instanceId]

instance Data.ToQuery GetFederationToken where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFederationTokenResponse' smart constructor.
data GetFederationTokenResponse = GetFederationTokenResponse'
  { -- | The credentials to use for federation.
    credentials :: Prelude.Maybe Credentials,
    -- | The URL to sign into the user\'s instance.
    signInUrl :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the user.
    userArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the user.
    userId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFederationTokenResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'credentials', 'getFederationTokenResponse_credentials' - The credentials to use for federation.
--
-- 'signInUrl', 'getFederationTokenResponse_signInUrl' - The URL to sign into the user\'s instance.
--
-- 'userArn', 'getFederationTokenResponse_userArn' - The Amazon Resource Name (ARN) of the user.
--
-- 'userId', 'getFederationTokenResponse_userId' - The identifier for the user.
--
-- 'httpStatus', 'getFederationTokenResponse_httpStatus' - The response's http status code.
newGetFederationTokenResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetFederationTokenResponse
newGetFederationTokenResponse pHttpStatus_ =
  GetFederationTokenResponse'
    { credentials =
        Prelude.Nothing,
      signInUrl = Prelude.Nothing,
      userArn = Prelude.Nothing,
      userId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The credentials to use for federation.
getFederationTokenResponse_credentials :: Lens.Lens' GetFederationTokenResponse (Prelude.Maybe Credentials)
getFederationTokenResponse_credentials = Lens.lens (\GetFederationTokenResponse' {credentials} -> credentials) (\s@GetFederationTokenResponse' {} a -> s {credentials = a} :: GetFederationTokenResponse)

-- | The URL to sign into the user\'s instance.
getFederationTokenResponse_signInUrl :: Lens.Lens' GetFederationTokenResponse (Prelude.Maybe Prelude.Text)
getFederationTokenResponse_signInUrl = Lens.lens (\GetFederationTokenResponse' {signInUrl} -> signInUrl) (\s@GetFederationTokenResponse' {} a -> s {signInUrl = a} :: GetFederationTokenResponse)

-- | The Amazon Resource Name (ARN) of the user.
getFederationTokenResponse_userArn :: Lens.Lens' GetFederationTokenResponse (Prelude.Maybe Prelude.Text)
getFederationTokenResponse_userArn = Lens.lens (\GetFederationTokenResponse' {userArn} -> userArn) (\s@GetFederationTokenResponse' {} a -> s {userArn = a} :: GetFederationTokenResponse)

-- | The identifier for the user.
getFederationTokenResponse_userId :: Lens.Lens' GetFederationTokenResponse (Prelude.Maybe Prelude.Text)
getFederationTokenResponse_userId = Lens.lens (\GetFederationTokenResponse' {userId} -> userId) (\s@GetFederationTokenResponse' {} a -> s {userId = a} :: GetFederationTokenResponse)

-- | The response's http status code.
getFederationTokenResponse_httpStatus :: Lens.Lens' GetFederationTokenResponse Prelude.Int
getFederationTokenResponse_httpStatus = Lens.lens (\GetFederationTokenResponse' {httpStatus} -> httpStatus) (\s@GetFederationTokenResponse' {} a -> s {httpStatus = a} :: GetFederationTokenResponse)

instance Prelude.NFData GetFederationTokenResponse where
  rnf GetFederationTokenResponse' {..} =
    Prelude.rnf credentials
      `Prelude.seq` Prelude.rnf signInUrl
      `Prelude.seq` Prelude.rnf userArn
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf httpStatus
