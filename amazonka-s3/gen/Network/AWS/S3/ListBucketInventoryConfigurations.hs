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
-- Module      : Network.AWS.S3.ListBucketInventoryConfigurations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of inventory configurations for the bucket. You can have
-- up to 1,000 analytics configurations per bucket.
--
-- This operation supports list pagination and does not return more than
-- 100 configurations at a time. Always check the @IsTruncated@ element in
-- the response. If there are no more configurations to list, @IsTruncated@
-- is set to false. If there are more configurations to list, @IsTruncated@
-- is set to true, and there is a value in @NextContinuationToken@. You use
-- the @NextContinuationToken@ value to continue the pagination of the list
-- by passing the value in continuation-token in the request to @GET@ the
-- next page.
--
-- To use this operation, you must have permissions to perform the
-- @s3:GetInventoryConfiguration@ action. The bucket owner has this
-- permission by default. The bucket owner can grant this permission to
-- others. For more information about permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources>.
--
-- For information about the Amazon S3 inventory feature, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-inventory.html Amazon S3 Inventory>
--
-- The following operations are related to
-- @ListBucketInventoryConfigurations@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketInventoryConfiguration.html GetBucketInventoryConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketInventoryConfiguration.html DeleteBucketInventoryConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketInventoryConfiguration.html PutBucketInventoryConfiguration>
module Network.AWS.S3.ListBucketInventoryConfigurations
  ( -- * Creating a Request
    ListBucketInventoryConfigurations (..),
    newListBucketInventoryConfigurations,

    -- * Request Lenses
    listBucketInventoryConfigurations_expectedBucketOwner,
    listBucketInventoryConfigurations_continuationToken,
    listBucketInventoryConfigurations_bucket,

    -- * Destructuring the Response
    ListBucketInventoryConfigurationsResponse (..),
    newListBucketInventoryConfigurationsResponse,

    -- * Response Lenses
    listBucketInventoryConfigurationsResponse_inventoryConfigurationList,
    listBucketInventoryConfigurationsResponse_isTruncated,
    listBucketInventoryConfigurationsResponse_nextContinuationToken,
    listBucketInventoryConfigurationsResponse_continuationToken,
    listBucketInventoryConfigurationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newListBucketInventoryConfigurations' smart constructor.
data ListBucketInventoryConfigurations = ListBucketInventoryConfigurations'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The marker used to continue an inventory configuration listing that has
    -- been truncated. Use the NextContinuationToken from a previously
    -- truncated list response to continue the listing. The continuation token
    -- is an opaque value that Amazon S3 understands.
    continuationToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket containing the inventory configurations to
    -- retrieve.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBucketInventoryConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'listBucketInventoryConfigurations_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'continuationToken', 'listBucketInventoryConfigurations_continuationToken' - The marker used to continue an inventory configuration listing that has
-- been truncated. Use the NextContinuationToken from a previously
-- truncated list response to continue the listing. The continuation token
-- is an opaque value that Amazon S3 understands.
--
-- 'bucket', 'listBucketInventoryConfigurations_bucket' - The name of the bucket containing the inventory configurations to
-- retrieve.
newListBucketInventoryConfigurations ::
  -- | 'bucket'
  BucketName ->
  ListBucketInventoryConfigurations
newListBucketInventoryConfigurations pBucket_ =
  ListBucketInventoryConfigurations'
    { expectedBucketOwner =
        Prelude.Nothing,
      continuationToken = Prelude.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
listBucketInventoryConfigurations_expectedBucketOwner :: Lens.Lens' ListBucketInventoryConfigurations (Prelude.Maybe Prelude.Text)
listBucketInventoryConfigurations_expectedBucketOwner = Lens.lens (\ListBucketInventoryConfigurations' {expectedBucketOwner} -> expectedBucketOwner) (\s@ListBucketInventoryConfigurations' {} a -> s {expectedBucketOwner = a} :: ListBucketInventoryConfigurations)

-- | The marker used to continue an inventory configuration listing that has
-- been truncated. Use the NextContinuationToken from a previously
-- truncated list response to continue the listing. The continuation token
-- is an opaque value that Amazon S3 understands.
listBucketInventoryConfigurations_continuationToken :: Lens.Lens' ListBucketInventoryConfigurations (Prelude.Maybe Prelude.Text)
listBucketInventoryConfigurations_continuationToken = Lens.lens (\ListBucketInventoryConfigurations' {continuationToken} -> continuationToken) (\s@ListBucketInventoryConfigurations' {} a -> s {continuationToken = a} :: ListBucketInventoryConfigurations)

-- | The name of the bucket containing the inventory configurations to
-- retrieve.
listBucketInventoryConfigurations_bucket :: Lens.Lens' ListBucketInventoryConfigurations BucketName
listBucketInventoryConfigurations_bucket = Lens.lens (\ListBucketInventoryConfigurations' {bucket} -> bucket) (\s@ListBucketInventoryConfigurations' {} a -> s {bucket = a} :: ListBucketInventoryConfigurations)

instance
  Core.AWSRequest
    ListBucketInventoryConfigurations
  where
  type
    AWSResponse ListBucketInventoryConfigurations =
      ListBucketInventoryConfigurationsResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ListBucketInventoryConfigurationsResponse'
            Prelude.<$> ( Core.may
                            (Core.parseXMLList "InventoryConfiguration")
                            x
                        )
              Prelude.<*> (x Core..@? "IsTruncated")
              Prelude.<*> (x Core..@? "NextContinuationToken")
              Prelude.<*> (x Core..@? "ContinuationToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListBucketInventoryConfigurations

instance
  Prelude.NFData
    ListBucketInventoryConfigurations

instance
  Core.ToHeaders
    ListBucketInventoryConfigurations
  where
  toHeaders ListBucketInventoryConfigurations' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner
      ]

instance
  Core.ToPath
    ListBucketInventoryConfigurations
  where
  toPath ListBucketInventoryConfigurations' {..} =
    Prelude.mconcat ["/", Core.toBS bucket]

instance
  Core.ToQuery
    ListBucketInventoryConfigurations
  where
  toQuery ListBucketInventoryConfigurations' {..} =
    Prelude.mconcat
      [ "continuation-token" Core.=: continuationToken,
        "inventory"
      ]

-- | /See:/ 'newListBucketInventoryConfigurationsResponse' smart constructor.
data ListBucketInventoryConfigurationsResponse = ListBucketInventoryConfigurationsResponse'
  { -- | The list of inventory configurations for a bucket.
    inventoryConfigurationList :: Prelude.Maybe [InventoryConfiguration],
    -- | Tells whether the returned list of inventory configurations is complete.
    -- A value of true indicates that the list is not complete and the
    -- NextContinuationToken is provided for a subsequent request.
    isTruncated :: Prelude.Maybe Prelude.Bool,
    -- | The marker used to continue this inventory configuration listing. Use
    -- the @NextContinuationToken@ from this response to continue the listing
    -- in a subsequent request. The continuation token is an opaque value that
    -- Amazon S3 understands.
    nextContinuationToken :: Prelude.Maybe Prelude.Text,
    -- | If sent in the request, the marker that is used as a starting point for
    -- this inventory configuration list response.
    continuationToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBucketInventoryConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inventoryConfigurationList', 'listBucketInventoryConfigurationsResponse_inventoryConfigurationList' - The list of inventory configurations for a bucket.
--
-- 'isTruncated', 'listBucketInventoryConfigurationsResponse_isTruncated' - Tells whether the returned list of inventory configurations is complete.
-- A value of true indicates that the list is not complete and the
-- NextContinuationToken is provided for a subsequent request.
--
-- 'nextContinuationToken', 'listBucketInventoryConfigurationsResponse_nextContinuationToken' - The marker used to continue this inventory configuration listing. Use
-- the @NextContinuationToken@ from this response to continue the listing
-- in a subsequent request. The continuation token is an opaque value that
-- Amazon S3 understands.
--
-- 'continuationToken', 'listBucketInventoryConfigurationsResponse_continuationToken' - If sent in the request, the marker that is used as a starting point for
-- this inventory configuration list response.
--
-- 'httpStatus', 'listBucketInventoryConfigurationsResponse_httpStatus' - The response's http status code.
newListBucketInventoryConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBucketInventoryConfigurationsResponse
newListBucketInventoryConfigurationsResponse
  pHttpStatus_ =
    ListBucketInventoryConfigurationsResponse'
      { inventoryConfigurationList =
          Prelude.Nothing,
        isTruncated = Prelude.Nothing,
        nextContinuationToken =
          Prelude.Nothing,
        continuationToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The list of inventory configurations for a bucket.
listBucketInventoryConfigurationsResponse_inventoryConfigurationList :: Lens.Lens' ListBucketInventoryConfigurationsResponse (Prelude.Maybe [InventoryConfiguration])
listBucketInventoryConfigurationsResponse_inventoryConfigurationList = Lens.lens (\ListBucketInventoryConfigurationsResponse' {inventoryConfigurationList} -> inventoryConfigurationList) (\s@ListBucketInventoryConfigurationsResponse' {} a -> s {inventoryConfigurationList = a} :: ListBucketInventoryConfigurationsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Tells whether the returned list of inventory configurations is complete.
-- A value of true indicates that the list is not complete and the
-- NextContinuationToken is provided for a subsequent request.
listBucketInventoryConfigurationsResponse_isTruncated :: Lens.Lens' ListBucketInventoryConfigurationsResponse (Prelude.Maybe Prelude.Bool)
listBucketInventoryConfigurationsResponse_isTruncated = Lens.lens (\ListBucketInventoryConfigurationsResponse' {isTruncated} -> isTruncated) (\s@ListBucketInventoryConfigurationsResponse' {} a -> s {isTruncated = a} :: ListBucketInventoryConfigurationsResponse)

-- | The marker used to continue this inventory configuration listing. Use
-- the @NextContinuationToken@ from this response to continue the listing
-- in a subsequent request. The continuation token is an opaque value that
-- Amazon S3 understands.
listBucketInventoryConfigurationsResponse_nextContinuationToken :: Lens.Lens' ListBucketInventoryConfigurationsResponse (Prelude.Maybe Prelude.Text)
listBucketInventoryConfigurationsResponse_nextContinuationToken = Lens.lens (\ListBucketInventoryConfigurationsResponse' {nextContinuationToken} -> nextContinuationToken) (\s@ListBucketInventoryConfigurationsResponse' {} a -> s {nextContinuationToken = a} :: ListBucketInventoryConfigurationsResponse)

-- | If sent in the request, the marker that is used as a starting point for
-- this inventory configuration list response.
listBucketInventoryConfigurationsResponse_continuationToken :: Lens.Lens' ListBucketInventoryConfigurationsResponse (Prelude.Maybe Prelude.Text)
listBucketInventoryConfigurationsResponse_continuationToken = Lens.lens (\ListBucketInventoryConfigurationsResponse' {continuationToken} -> continuationToken) (\s@ListBucketInventoryConfigurationsResponse' {} a -> s {continuationToken = a} :: ListBucketInventoryConfigurationsResponse)

-- | The response's http status code.
listBucketInventoryConfigurationsResponse_httpStatus :: Lens.Lens' ListBucketInventoryConfigurationsResponse Prelude.Int
listBucketInventoryConfigurationsResponse_httpStatus = Lens.lens (\ListBucketInventoryConfigurationsResponse' {httpStatus} -> httpStatus) (\s@ListBucketInventoryConfigurationsResponse' {} a -> s {httpStatus = a} :: ListBucketInventoryConfigurationsResponse)

instance
  Prelude.NFData
    ListBucketInventoryConfigurationsResponse
