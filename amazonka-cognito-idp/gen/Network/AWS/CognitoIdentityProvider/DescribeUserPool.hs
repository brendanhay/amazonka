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
-- Module      : Network.AWS.CognitoIdentityProvider.DescribeUserPool
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the configuration information and metadata of the specified user
-- pool.
module Network.AWS.CognitoIdentityProvider.DescribeUserPool
  ( -- * Creating a Request
    DescribeUserPool (..),
    newDescribeUserPool,

    -- * Request Lenses
    describeUserPool_userPoolId,

    -- * Destructuring the Response
    DescribeUserPoolResponse (..),
    newDescribeUserPoolResponse,

    -- * Response Lenses
    describeUserPoolResponse_userPool,
    describeUserPoolResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to describe the user pool.
--
-- /See:/ 'newDescribeUserPool' smart constructor.
data DescribeUserPool = DescribeUserPool'
  { -- | The user pool ID for the user pool you want to describe.
    userPoolId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeUserPool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolId', 'describeUserPool_userPoolId' - The user pool ID for the user pool you want to describe.
newDescribeUserPool ::
  -- | 'userPoolId'
  Core.Text ->
  DescribeUserPool
newDescribeUserPool pUserPoolId_ =
  DescribeUserPool' {userPoolId = pUserPoolId_}

-- | The user pool ID for the user pool you want to describe.
describeUserPool_userPoolId :: Lens.Lens' DescribeUserPool Core.Text
describeUserPool_userPoolId = Lens.lens (\DescribeUserPool' {userPoolId} -> userPoolId) (\s@DescribeUserPool' {} a -> s {userPoolId = a} :: DescribeUserPool)

instance Core.AWSRequest DescribeUserPool where
  type
    AWSResponse DescribeUserPool =
      DescribeUserPoolResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserPoolResponse'
            Core.<$> (x Core..?> "UserPool")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeUserPool

instance Core.NFData DescribeUserPool

instance Core.ToHeaders DescribeUserPool where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.DescribeUserPool" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeUserPool where
  toJSON DescribeUserPool' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("UserPoolId" Core..= userPoolId)]
      )

instance Core.ToPath DescribeUserPool where
  toPath = Core.const "/"

instance Core.ToQuery DescribeUserPool where
  toQuery = Core.const Core.mempty

-- | Represents the response to describe the user pool.
--
-- /See:/ 'newDescribeUserPoolResponse' smart constructor.
data DescribeUserPoolResponse = DescribeUserPoolResponse'
  { -- | The container of metadata returned by the server to describe the pool.
    userPool :: Core.Maybe UserPoolType,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeUserPoolResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPool', 'describeUserPoolResponse_userPool' - The container of metadata returned by the server to describe the pool.
--
-- 'httpStatus', 'describeUserPoolResponse_httpStatus' - The response's http status code.
newDescribeUserPoolResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeUserPoolResponse
newDescribeUserPoolResponse pHttpStatus_ =
  DescribeUserPoolResponse'
    { userPool = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The container of metadata returned by the server to describe the pool.
describeUserPoolResponse_userPool :: Lens.Lens' DescribeUserPoolResponse (Core.Maybe UserPoolType)
describeUserPoolResponse_userPool = Lens.lens (\DescribeUserPoolResponse' {userPool} -> userPool) (\s@DescribeUserPoolResponse' {} a -> s {userPool = a} :: DescribeUserPoolResponse)

-- | The response's http status code.
describeUserPoolResponse_httpStatus :: Lens.Lens' DescribeUserPoolResponse Core.Int
describeUserPoolResponse_httpStatus = Lens.lens (\DescribeUserPoolResponse' {httpStatus} -> httpStatus) (\s@DescribeUserPoolResponse' {} a -> s {httpStatus = a} :: DescribeUserPoolResponse)

instance Core.NFData DescribeUserPoolResponse
