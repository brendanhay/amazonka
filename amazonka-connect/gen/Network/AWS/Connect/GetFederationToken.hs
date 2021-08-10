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
-- Module      : Network.AWS.Connect.GetFederationToken
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a token for federation.
module Network.AWS.Connect.GetFederationToken
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
    getFederationTokenResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetFederationToken' smart constructor.
data GetFederationToken = GetFederationToken'
  { -- | The identifier of the Amazon Connect instance.
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
-- 'instanceId', 'getFederationToken_instanceId' - The identifier of the Amazon Connect instance.
newGetFederationToken ::
  -- | 'instanceId'
  Prelude.Text ->
  GetFederationToken
newGetFederationToken pInstanceId_ =
  GetFederationToken' {instanceId = pInstanceId_}

-- | The identifier of the Amazon Connect instance.
getFederationToken_instanceId :: Lens.Lens' GetFederationToken Prelude.Text
getFederationToken_instanceId = Lens.lens (\GetFederationToken' {instanceId} -> instanceId) (\s@GetFederationToken' {} a -> s {instanceId = a} :: GetFederationToken)

instance Core.AWSRequest GetFederationToken where
  type
    AWSResponse GetFederationToken =
      GetFederationTokenResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFederationTokenResponse'
            Prelude.<$> (x Core..?> "Credentials")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetFederationToken

instance Prelude.NFData GetFederationToken

instance Core.ToHeaders GetFederationToken where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetFederationToken where
  toPath GetFederationToken' {..} =
    Prelude.mconcat
      ["/user/federate/", Core.toBS instanceId]

instance Core.ToQuery GetFederationToken where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFederationTokenResponse' smart constructor.
data GetFederationTokenResponse = GetFederationTokenResponse'
  { -- | The credentials to use for federation.
    credentials :: Prelude.Maybe Credentials,
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
-- 'httpStatus', 'getFederationTokenResponse_httpStatus' - The response's http status code.
newGetFederationTokenResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetFederationTokenResponse
newGetFederationTokenResponse pHttpStatus_ =
  GetFederationTokenResponse'
    { credentials =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The credentials to use for federation.
getFederationTokenResponse_credentials :: Lens.Lens' GetFederationTokenResponse (Prelude.Maybe Credentials)
getFederationTokenResponse_credentials = Lens.lens (\GetFederationTokenResponse' {credentials} -> credentials) (\s@GetFederationTokenResponse' {} a -> s {credentials = a} :: GetFederationTokenResponse)

-- | The response's http status code.
getFederationTokenResponse_httpStatus :: Lens.Lens' GetFederationTokenResponse Prelude.Int
getFederationTokenResponse_httpStatus = Lens.lens (\GetFederationTokenResponse' {httpStatus} -> httpStatus) (\s@GetFederationTokenResponse' {} a -> s {httpStatus = a} :: GetFederationTokenResponse)

instance Prelude.NFData GetFederationTokenResponse
