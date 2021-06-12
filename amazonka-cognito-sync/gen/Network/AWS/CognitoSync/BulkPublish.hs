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
-- Module      : Network.AWS.CognitoSync.BulkPublish
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a bulk publish of all existing datasets for an Identity Pool
-- to the configured stream. Customers are limited to one successful bulk
-- publish per 24 hours. Bulk publish is an asynchronous request, customers
-- can see the status of the request via the GetBulkPublishDetails
-- operation.
--
-- This API can only be called with developer credentials. You cannot call
-- this API with the temporary user credentials provided by Cognito
-- Identity.
module Network.AWS.CognitoSync.BulkPublish
  ( -- * Creating a Request
    BulkPublish (..),
    newBulkPublish,

    -- * Request Lenses
    bulkPublish_identityPoolId,

    -- * Destructuring the Response
    BulkPublishResponse (..),
    newBulkPublishResponse,

    -- * Response Lenses
    bulkPublishResponse_identityPoolId,
    bulkPublishResponse_httpStatus,
  )
where

import Network.AWS.CognitoSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the BulkPublish operation.
--
-- /See:/ 'newBulkPublish' smart constructor.
data BulkPublish = BulkPublish'
  { -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. GUID generation is unique within a region.
    identityPoolId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BulkPublish' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityPoolId', 'bulkPublish_identityPoolId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
newBulkPublish ::
  -- | 'identityPoolId'
  Core.Text ->
  BulkPublish
newBulkPublish pIdentityPoolId_ =
  BulkPublish' {identityPoolId = pIdentityPoolId_}

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
bulkPublish_identityPoolId :: Lens.Lens' BulkPublish Core.Text
bulkPublish_identityPoolId = Lens.lens (\BulkPublish' {identityPoolId} -> identityPoolId) (\s@BulkPublish' {} a -> s {identityPoolId = a} :: BulkPublish)

instance Core.AWSRequest BulkPublish where
  type AWSResponse BulkPublish = BulkPublishResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BulkPublishResponse'
            Core.<$> (x Core..?> "IdentityPoolId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable BulkPublish

instance Core.NFData BulkPublish

instance Core.ToHeaders BulkPublish where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON BulkPublish where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath BulkPublish where
  toPath BulkPublish' {..} =
    Core.mconcat
      [ "/identitypools/",
        Core.toBS identityPoolId,
        "/bulkpublish"
      ]

instance Core.ToQuery BulkPublish where
  toQuery = Core.const Core.mempty

-- | The output for the BulkPublish operation.
--
-- /See:/ 'newBulkPublishResponse' smart constructor.
data BulkPublishResponse = BulkPublishResponse'
  { -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. GUID generation is unique within a region.
    identityPoolId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BulkPublishResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityPoolId', 'bulkPublishResponse_identityPoolId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
--
-- 'httpStatus', 'bulkPublishResponse_httpStatus' - The response's http status code.
newBulkPublishResponse ::
  -- | 'httpStatus'
  Core.Int ->
  BulkPublishResponse
newBulkPublishResponse pHttpStatus_ =
  BulkPublishResponse'
    { identityPoolId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
bulkPublishResponse_identityPoolId :: Lens.Lens' BulkPublishResponse (Core.Maybe Core.Text)
bulkPublishResponse_identityPoolId = Lens.lens (\BulkPublishResponse' {identityPoolId} -> identityPoolId) (\s@BulkPublishResponse' {} a -> s {identityPoolId = a} :: BulkPublishResponse)

-- | The response's http status code.
bulkPublishResponse_httpStatus :: Lens.Lens' BulkPublishResponse Core.Int
bulkPublishResponse_httpStatus = Lens.lens (\BulkPublishResponse' {httpStatus} -> httpStatus) (\s@BulkPublishResponse' {} a -> s {httpStatus = a} :: BulkPublishResponse)

instance Core.NFData BulkPublishResponse
