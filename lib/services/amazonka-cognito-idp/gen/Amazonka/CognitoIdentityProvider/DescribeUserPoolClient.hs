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
-- Module      : Amazonka.CognitoIdentityProvider.DescribeUserPoolClient
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Client method for returning the configuration information and metadata
-- of the specified user pool app client.
module Amazonka.CognitoIdentityProvider.DescribeUserPoolClient
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

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to describe a user pool client.
--
-- /See:/ 'newDescribeUserPoolClient' smart constructor.
data DescribeUserPoolClient = DescribeUserPoolClient'
  { -- | The user pool ID for the user pool you want to describe.
    userPoolId :: Prelude.Text,
    -- | The app client ID of the app associated with the user pool.
    clientId :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'clientId'
  Prelude.Text ->
  DescribeUserPoolClient
newDescribeUserPoolClient pUserPoolId_ pClientId_ =
  DescribeUserPoolClient'
    { userPoolId = pUserPoolId_,
      clientId = Data._Sensitive Lens.# pClientId_
    }

-- | The user pool ID for the user pool you want to describe.
describeUserPoolClient_userPoolId :: Lens.Lens' DescribeUserPoolClient Prelude.Text
describeUserPoolClient_userPoolId = Lens.lens (\DescribeUserPoolClient' {userPoolId} -> userPoolId) (\s@DescribeUserPoolClient' {} a -> s {userPoolId = a} :: DescribeUserPoolClient)

-- | The app client ID of the app associated with the user pool.
describeUserPoolClient_clientId :: Lens.Lens' DescribeUserPoolClient Prelude.Text
describeUserPoolClient_clientId = Lens.lens (\DescribeUserPoolClient' {clientId} -> clientId) (\s@DescribeUserPoolClient' {} a -> s {clientId = a} :: DescribeUserPoolClient) Prelude.. Data._Sensitive

instance Core.AWSRequest DescribeUserPoolClient where
  type
    AWSResponse DescribeUserPoolClient =
      DescribeUserPoolClientResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserPoolClientResponse'
            Prelude.<$> (x Data..?> "UserPoolClient")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeUserPoolClient where
  hashWithSalt _salt DescribeUserPoolClient' {..} =
    _salt
      `Prelude.hashWithSalt` userPoolId
      `Prelude.hashWithSalt` clientId

instance Prelude.NFData DescribeUserPoolClient where
  rnf DescribeUserPoolClient' {..} =
    Prelude.rnf userPoolId
      `Prelude.seq` Prelude.rnf clientId

instance Data.ToHeaders DescribeUserPoolClient where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.DescribeUserPoolClient" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeUserPoolClient where
  toJSON DescribeUserPoolClient' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserPoolId" Data..= userPoolId),
            Prelude.Just ("ClientId" Data..= clientId)
          ]
      )

instance Data.ToPath DescribeUserPoolClient where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeUserPoolClient where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response from the server from a request to describe the
-- user pool client.
--
-- /See:/ 'newDescribeUserPoolClientResponse' smart constructor.
data DescribeUserPoolClientResponse = DescribeUserPoolClientResponse'
  { -- | The user pool client from a server response to describe the user pool
    -- client.
    userPoolClient :: Prelude.Maybe UserPoolClientType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeUserPoolClientResponse
newDescribeUserPoolClientResponse pHttpStatus_ =
  DescribeUserPoolClientResponse'
    { userPoolClient =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The user pool client from a server response to describe the user pool
-- client.
describeUserPoolClientResponse_userPoolClient :: Lens.Lens' DescribeUserPoolClientResponse (Prelude.Maybe UserPoolClientType)
describeUserPoolClientResponse_userPoolClient = Lens.lens (\DescribeUserPoolClientResponse' {userPoolClient} -> userPoolClient) (\s@DescribeUserPoolClientResponse' {} a -> s {userPoolClient = a} :: DescribeUserPoolClientResponse)

-- | The response's http status code.
describeUserPoolClientResponse_httpStatus :: Lens.Lens' DescribeUserPoolClientResponse Prelude.Int
describeUserPoolClientResponse_httpStatus = Lens.lens (\DescribeUserPoolClientResponse' {httpStatus} -> httpStatus) (\s@DescribeUserPoolClientResponse' {} a -> s {httpStatus = a} :: DescribeUserPoolClientResponse)

instance
  Prelude.NFData
    DescribeUserPoolClientResponse
  where
  rnf DescribeUserPoolClientResponse' {..} =
    Prelude.rnf userPoolClient
      `Prelude.seq` Prelude.rnf httpStatus
