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
-- Module      : Network.AWS.CognitoSync.DescribeIdentityPoolUsage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets usage details (for example, data storage) about a particular
-- identity pool.
--
-- This API can only be called with developer credentials. You cannot call
-- this API with the temporary user credentials provided by Cognito
-- Identity.
module Network.AWS.CognitoSync.DescribeIdentityPoolUsage
  ( -- * Creating a Request
    DescribeIdentityPoolUsage (..),
    newDescribeIdentityPoolUsage,

    -- * Request Lenses
    describeIdentityPoolUsage_identityPoolId,

    -- * Destructuring the Response
    DescribeIdentityPoolUsageResponse (..),
    newDescribeIdentityPoolUsageResponse,

    -- * Response Lenses
    describeIdentityPoolUsageResponse_identityPoolUsage,
    describeIdentityPoolUsageResponse_httpStatus,
  )
where

import Network.AWS.CognitoSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request for usage information about the identity pool.
--
-- /See:/ 'newDescribeIdentityPoolUsage' smart constructor.
data DescribeIdentityPoolUsage = DescribeIdentityPoolUsage'
  { -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. GUID generation is unique within a region.
    identityPoolId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeIdentityPoolUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityPoolId', 'describeIdentityPoolUsage_identityPoolId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
newDescribeIdentityPoolUsage ::
  -- | 'identityPoolId'
  Core.Text ->
  DescribeIdentityPoolUsage
newDescribeIdentityPoolUsage pIdentityPoolId_ =
  DescribeIdentityPoolUsage'
    { identityPoolId =
        pIdentityPoolId_
    }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
describeIdentityPoolUsage_identityPoolId :: Lens.Lens' DescribeIdentityPoolUsage Core.Text
describeIdentityPoolUsage_identityPoolId = Lens.lens (\DescribeIdentityPoolUsage' {identityPoolId} -> identityPoolId) (\s@DescribeIdentityPoolUsage' {} a -> s {identityPoolId = a} :: DescribeIdentityPoolUsage)

instance Core.AWSRequest DescribeIdentityPoolUsage where
  type
    AWSResponse DescribeIdentityPoolUsage =
      DescribeIdentityPoolUsageResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeIdentityPoolUsageResponse'
            Core.<$> (x Core..?> "IdentityPoolUsage")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeIdentityPoolUsage

instance Core.NFData DescribeIdentityPoolUsage

instance Core.ToHeaders DescribeIdentityPoolUsage where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DescribeIdentityPoolUsage where
  toPath DescribeIdentityPoolUsage' {..} =
    Core.mconcat
      ["/identitypools/", Core.toBS identityPoolId]

instance Core.ToQuery DescribeIdentityPoolUsage where
  toQuery = Core.const Core.mempty

-- | Response to a successful DescribeIdentityPoolUsage request.
--
-- /See:/ 'newDescribeIdentityPoolUsageResponse' smart constructor.
data DescribeIdentityPoolUsageResponse = DescribeIdentityPoolUsageResponse'
  { -- | Information about the usage of the identity pool.
    identityPoolUsage :: Core.Maybe IdentityPoolUsage,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeIdentityPoolUsageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityPoolUsage', 'describeIdentityPoolUsageResponse_identityPoolUsage' - Information about the usage of the identity pool.
--
-- 'httpStatus', 'describeIdentityPoolUsageResponse_httpStatus' - The response's http status code.
newDescribeIdentityPoolUsageResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeIdentityPoolUsageResponse
newDescribeIdentityPoolUsageResponse pHttpStatus_ =
  DescribeIdentityPoolUsageResponse'
    { identityPoolUsage =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the usage of the identity pool.
describeIdentityPoolUsageResponse_identityPoolUsage :: Lens.Lens' DescribeIdentityPoolUsageResponse (Core.Maybe IdentityPoolUsage)
describeIdentityPoolUsageResponse_identityPoolUsage = Lens.lens (\DescribeIdentityPoolUsageResponse' {identityPoolUsage} -> identityPoolUsage) (\s@DescribeIdentityPoolUsageResponse' {} a -> s {identityPoolUsage = a} :: DescribeIdentityPoolUsageResponse)

-- | The response's http status code.
describeIdentityPoolUsageResponse_httpStatus :: Lens.Lens' DescribeIdentityPoolUsageResponse Core.Int
describeIdentityPoolUsageResponse_httpStatus = Lens.lens (\DescribeIdentityPoolUsageResponse' {httpStatus} -> httpStatus) (\s@DescribeIdentityPoolUsageResponse' {} a -> s {httpStatus = a} :: DescribeIdentityPoolUsageResponse)

instance
  Core.NFData
    DescribeIdentityPoolUsageResponse
