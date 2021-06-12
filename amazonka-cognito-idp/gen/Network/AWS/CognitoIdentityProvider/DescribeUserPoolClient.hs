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
-- Module      : Network.AWS.CognitoIdentityProvider.DescribeUserPoolClient
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Client method for returning the configuration information and metadata
-- of the specified user pool app client.
module Network.AWS.CognitoIdentityProvider.DescribeUserPoolClient
  ( -- * Creating a Request
    DescribeUserPoolClient (..),
    newDescribeUserPoolClient,

    -- * Request Lenses
    describeUserPoolClient_userPoolId,
    describeUserPoolClient_clientId,

    -- * Destructuring the Response
    DescribeUserPoolClientResponse (..),
    newDescribeUserPoolClientResponse,

    -- * Response Lenses
    describeUserPoolClientResponse_userPoolClient,
    describeUserPoolClientResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to describe a user pool client.
--
-- /See:/ 'newDescribeUserPoolClient' smart constructor.
data DescribeUserPoolClient = DescribeUserPoolClient'
  { -- | The user pool ID for the user pool you want to describe.
    userPoolId :: Core.Text,
    -- | The app client ID of the app associated with the user pool.
    clientId :: Core.Sensitive Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeUserPoolClient' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolId', 'describeUserPoolClient_userPoolId' - The user pool ID for the user pool you want to describe.
--
-- 'clientId', 'describeUserPoolClient_clientId' - The app client ID of the app associated with the user pool.
newDescribeUserPoolClient ::
  -- | 'userPoolId'
  Core.Text ->
  -- | 'clientId'
  Core.Text ->
  DescribeUserPoolClient
newDescribeUserPoolClient pUserPoolId_ pClientId_ =
  DescribeUserPoolClient'
    { userPoolId = pUserPoolId_,
      clientId = Core._Sensitive Lens.# pClientId_
    }

-- | The user pool ID for the user pool you want to describe.
describeUserPoolClient_userPoolId :: Lens.Lens' DescribeUserPoolClient Core.Text
describeUserPoolClient_userPoolId = Lens.lens (\DescribeUserPoolClient' {userPoolId} -> userPoolId) (\s@DescribeUserPoolClient' {} a -> s {userPoolId = a} :: DescribeUserPoolClient)

-- | The app client ID of the app associated with the user pool.
describeUserPoolClient_clientId :: Lens.Lens' DescribeUserPoolClient Core.Text
describeUserPoolClient_clientId = Lens.lens (\DescribeUserPoolClient' {clientId} -> clientId) (\s@DescribeUserPoolClient' {} a -> s {clientId = a} :: DescribeUserPoolClient) Core.. Core._Sensitive

instance Core.AWSRequest DescribeUserPoolClient where
  type
    AWSResponse DescribeUserPoolClient =
      DescribeUserPoolClientResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserPoolClientResponse'
            Core.<$> (x Core..?> "UserPoolClient")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeUserPoolClient

instance Core.NFData DescribeUserPoolClient

instance Core.ToHeaders DescribeUserPoolClient where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.DescribeUserPoolClient" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeUserPoolClient where
  toJSON DescribeUserPoolClient' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("ClientId" Core..= clientId)
          ]
      )

instance Core.ToPath DescribeUserPoolClient where
  toPath = Core.const "/"

instance Core.ToQuery DescribeUserPoolClient where
  toQuery = Core.const Core.mempty

-- | Represents the response from the server from a request to describe the
-- user pool client.
--
-- /See:/ 'newDescribeUserPoolClientResponse' smart constructor.
data DescribeUserPoolClientResponse = DescribeUserPoolClientResponse'
  { -- | The user pool client from a server response to describe the user pool
    -- client.
    userPoolClient :: Core.Maybe UserPoolClientType,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeUserPoolClientResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolClient', 'describeUserPoolClientResponse_userPoolClient' - The user pool client from a server response to describe the user pool
-- client.
--
-- 'httpStatus', 'describeUserPoolClientResponse_httpStatus' - The response's http status code.
newDescribeUserPoolClientResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeUserPoolClientResponse
newDescribeUserPoolClientResponse pHttpStatus_ =
  DescribeUserPoolClientResponse'
    { userPoolClient =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The user pool client from a server response to describe the user pool
-- client.
describeUserPoolClientResponse_userPoolClient :: Lens.Lens' DescribeUserPoolClientResponse (Core.Maybe UserPoolClientType)
describeUserPoolClientResponse_userPoolClient = Lens.lens (\DescribeUserPoolClientResponse' {userPoolClient} -> userPoolClient) (\s@DescribeUserPoolClientResponse' {} a -> s {userPoolClient = a} :: DescribeUserPoolClientResponse)

-- | The response's http status code.
describeUserPoolClientResponse_httpStatus :: Lens.Lens' DescribeUserPoolClientResponse Core.Int
describeUserPoolClientResponse_httpStatus = Lens.lens (\DescribeUserPoolClientResponse' {httpStatus} -> httpStatus) (\s@DescribeUserPoolClientResponse' {} a -> s {httpStatus = a} :: DescribeUserPoolClientResponse)

instance Core.NFData DescribeUserPoolClientResponse
