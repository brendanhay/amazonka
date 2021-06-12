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
-- Module      : Network.AWS.S3.ListBucketAnalyticsConfigurations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the analytics configurations for the bucket. You can have up to
-- 1,000 analytics configurations per bucket.
--
-- This operation supports list pagination and does not return more than
-- 100 configurations at a time. You should always check the @IsTruncated@
-- element in the response. If there are no more configurations to list,
-- @IsTruncated@ is set to false. If there are more configurations to list,
-- @IsTruncated@ is set to true, and there will be a value in
-- @NextContinuationToken@. You use the @NextContinuationToken@ value to
-- continue the pagination of the list by passing the value in
-- continuation-token in the request to @GET@ the next page.
--
-- To use this operation, you must have permissions to perform the
-- @s3:GetAnalyticsConfiguration@ action. The bucket owner has this
-- permission by default. The bucket owner can grant this permission to
-- others. For more information about permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources>.
--
-- For information about Amazon S3 analytics feature, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/analytics-storage-class.html Amazon S3 Analytics – Storage Class Analysis>.
--
-- The following operations are related to
-- @ListBucketAnalyticsConfigurations@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketAnalyticsConfiguration.html GetBucketAnalyticsConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketAnalyticsConfiguration.html DeleteBucketAnalyticsConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketAnalyticsConfiguration.html PutBucketAnalyticsConfiguration>
module Network.AWS.S3.ListBucketAnalyticsConfigurations
  ( -- * Creating a Request
    ListBucketAnalyticsConfigurations (..),
    newListBucketAnalyticsConfigurations,

    -- * Request Lenses
    listBucketAnalyticsConfigurations_expectedBucketOwner,
    listBucketAnalyticsConfigurations_continuationToken,
    listBucketAnalyticsConfigurations_bucket,

    -- * Destructuring the Response
    ListBucketAnalyticsConfigurationsResponse (..),
    newListBucketAnalyticsConfigurationsResponse,

    -- * Response Lenses
    listBucketAnalyticsConfigurationsResponse_analyticsConfigurationList,
    listBucketAnalyticsConfigurationsResponse_isTruncated,
    listBucketAnalyticsConfigurationsResponse_nextContinuationToken,
    listBucketAnalyticsConfigurationsResponse_continuationToken,
    listBucketAnalyticsConfigurationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newListBucketAnalyticsConfigurations' smart constructor.
data ListBucketAnalyticsConfigurations = ListBucketAnalyticsConfigurations'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Core.Text,
    -- | The ContinuationToken that represents a placeholder from where this
    -- request should begin.
    continuationToken :: Core.Maybe Core.Text,
    -- | The name of the bucket from which analytics configurations are
    -- retrieved.
    bucket :: BucketName
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListBucketAnalyticsConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'listBucketAnalyticsConfigurations_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'continuationToken', 'listBucketAnalyticsConfigurations_continuationToken' - The ContinuationToken that represents a placeholder from where this
-- request should begin.
--
-- 'bucket', 'listBucketAnalyticsConfigurations_bucket' - The name of the bucket from which analytics configurations are
-- retrieved.
newListBucketAnalyticsConfigurations ::
  -- | 'bucket'
  BucketName ->
  ListBucketAnalyticsConfigurations
newListBucketAnalyticsConfigurations pBucket_ =
  ListBucketAnalyticsConfigurations'
    { expectedBucketOwner =
        Core.Nothing,
      continuationToken = Core.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
listBucketAnalyticsConfigurations_expectedBucketOwner :: Lens.Lens' ListBucketAnalyticsConfigurations (Core.Maybe Core.Text)
listBucketAnalyticsConfigurations_expectedBucketOwner = Lens.lens (\ListBucketAnalyticsConfigurations' {expectedBucketOwner} -> expectedBucketOwner) (\s@ListBucketAnalyticsConfigurations' {} a -> s {expectedBucketOwner = a} :: ListBucketAnalyticsConfigurations)

-- | The ContinuationToken that represents a placeholder from where this
-- request should begin.
listBucketAnalyticsConfigurations_continuationToken :: Lens.Lens' ListBucketAnalyticsConfigurations (Core.Maybe Core.Text)
listBucketAnalyticsConfigurations_continuationToken = Lens.lens (\ListBucketAnalyticsConfigurations' {continuationToken} -> continuationToken) (\s@ListBucketAnalyticsConfigurations' {} a -> s {continuationToken = a} :: ListBucketAnalyticsConfigurations)

-- | The name of the bucket from which analytics configurations are
-- retrieved.
listBucketAnalyticsConfigurations_bucket :: Lens.Lens' ListBucketAnalyticsConfigurations BucketName
listBucketAnalyticsConfigurations_bucket = Lens.lens (\ListBucketAnalyticsConfigurations' {bucket} -> bucket) (\s@ListBucketAnalyticsConfigurations' {} a -> s {bucket = a} :: ListBucketAnalyticsConfigurations)

instance
  Core.AWSRequest
    ListBucketAnalyticsConfigurations
  where
  type
    AWSResponse ListBucketAnalyticsConfigurations =
      ListBucketAnalyticsConfigurationsResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ListBucketAnalyticsConfigurationsResponse'
            Core.<$> ( Core.may
                         (Core.parseXMLList "AnalyticsConfiguration")
                         x
                     )
            Core.<*> (x Core..@? "IsTruncated")
            Core.<*> (x Core..@? "NextContinuationToken")
            Core.<*> (x Core..@? "ContinuationToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ListBucketAnalyticsConfigurations

instance
  Core.NFData
    ListBucketAnalyticsConfigurations

instance
  Core.ToHeaders
    ListBucketAnalyticsConfigurations
  where
  toHeaders ListBucketAnalyticsConfigurations' {..} =
    Core.mconcat
      [ "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner
      ]

instance
  Core.ToPath
    ListBucketAnalyticsConfigurations
  where
  toPath ListBucketAnalyticsConfigurations' {..} =
    Core.mconcat ["/", Core.toBS bucket]

instance
  Core.ToQuery
    ListBucketAnalyticsConfigurations
  where
  toQuery ListBucketAnalyticsConfigurations' {..} =
    Core.mconcat
      [ "continuation-token" Core.=: continuationToken,
        "analytics"
      ]

-- | /See:/ 'newListBucketAnalyticsConfigurationsResponse' smart constructor.
data ListBucketAnalyticsConfigurationsResponse = ListBucketAnalyticsConfigurationsResponse'
  { -- | The list of analytics configurations for a bucket.
    analyticsConfigurationList :: Core.Maybe [AnalyticsConfiguration],
    -- | Indicates whether the returned list of analytics configurations is
    -- complete. A value of true indicates that the list is not complete and
    -- the NextContinuationToken will be provided for a subsequent request.
    isTruncated :: Core.Maybe Core.Bool,
    -- | @NextContinuationToken@ is sent when @isTruncated@ is true, which
    -- indicates that there are more analytics configurations to list. The next
    -- request must include this @NextContinuationToken@. The token is
    -- obfuscated and is not a usable value.
    nextContinuationToken :: Core.Maybe Core.Text,
    -- | The marker that is used as a starting point for this analytics
    -- configuration list response. This value is present if it was sent in the
    -- request.
    continuationToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListBucketAnalyticsConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analyticsConfigurationList', 'listBucketAnalyticsConfigurationsResponse_analyticsConfigurationList' - The list of analytics configurations for a bucket.
--
-- 'isTruncated', 'listBucketAnalyticsConfigurationsResponse_isTruncated' - Indicates whether the returned list of analytics configurations is
-- complete. A value of true indicates that the list is not complete and
-- the NextContinuationToken will be provided for a subsequent request.
--
-- 'nextContinuationToken', 'listBucketAnalyticsConfigurationsResponse_nextContinuationToken' - @NextContinuationToken@ is sent when @isTruncated@ is true, which
-- indicates that there are more analytics configurations to list. The next
-- request must include this @NextContinuationToken@. The token is
-- obfuscated and is not a usable value.
--
-- 'continuationToken', 'listBucketAnalyticsConfigurationsResponse_continuationToken' - The marker that is used as a starting point for this analytics
-- configuration list response. This value is present if it was sent in the
-- request.
--
-- 'httpStatus', 'listBucketAnalyticsConfigurationsResponse_httpStatus' - The response's http status code.
newListBucketAnalyticsConfigurationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListBucketAnalyticsConfigurationsResponse
newListBucketAnalyticsConfigurationsResponse
  pHttpStatus_ =
    ListBucketAnalyticsConfigurationsResponse'
      { analyticsConfigurationList =
          Core.Nothing,
        isTruncated = Core.Nothing,
        nextContinuationToken =
          Core.Nothing,
        continuationToken = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The list of analytics configurations for a bucket.
listBucketAnalyticsConfigurationsResponse_analyticsConfigurationList :: Lens.Lens' ListBucketAnalyticsConfigurationsResponse (Core.Maybe [AnalyticsConfiguration])
listBucketAnalyticsConfigurationsResponse_analyticsConfigurationList = Lens.lens (\ListBucketAnalyticsConfigurationsResponse' {analyticsConfigurationList} -> analyticsConfigurationList) (\s@ListBucketAnalyticsConfigurationsResponse' {} a -> s {analyticsConfigurationList = a} :: ListBucketAnalyticsConfigurationsResponse) Core.. Lens.mapping Lens._Coerce

-- | Indicates whether the returned list of analytics configurations is
-- complete. A value of true indicates that the list is not complete and
-- the NextContinuationToken will be provided for a subsequent request.
listBucketAnalyticsConfigurationsResponse_isTruncated :: Lens.Lens' ListBucketAnalyticsConfigurationsResponse (Core.Maybe Core.Bool)
listBucketAnalyticsConfigurationsResponse_isTruncated = Lens.lens (\ListBucketAnalyticsConfigurationsResponse' {isTruncated} -> isTruncated) (\s@ListBucketAnalyticsConfigurationsResponse' {} a -> s {isTruncated = a} :: ListBucketAnalyticsConfigurationsResponse)

-- | @NextContinuationToken@ is sent when @isTruncated@ is true, which
-- indicates that there are more analytics configurations to list. The next
-- request must include this @NextContinuationToken@. The token is
-- obfuscated and is not a usable value.
listBucketAnalyticsConfigurationsResponse_nextContinuationToken :: Lens.Lens' ListBucketAnalyticsConfigurationsResponse (Core.Maybe Core.Text)
listBucketAnalyticsConfigurationsResponse_nextContinuationToken = Lens.lens (\ListBucketAnalyticsConfigurationsResponse' {nextContinuationToken} -> nextContinuationToken) (\s@ListBucketAnalyticsConfigurationsResponse' {} a -> s {nextContinuationToken = a} :: ListBucketAnalyticsConfigurationsResponse)

-- | The marker that is used as a starting point for this analytics
-- configuration list response. This value is present if it was sent in the
-- request.
listBucketAnalyticsConfigurationsResponse_continuationToken :: Lens.Lens' ListBucketAnalyticsConfigurationsResponse (Core.Maybe Core.Text)
listBucketAnalyticsConfigurationsResponse_continuationToken = Lens.lens (\ListBucketAnalyticsConfigurationsResponse' {continuationToken} -> continuationToken) (\s@ListBucketAnalyticsConfigurationsResponse' {} a -> s {continuationToken = a} :: ListBucketAnalyticsConfigurationsResponse)

-- | The response's http status code.
listBucketAnalyticsConfigurationsResponse_httpStatus :: Lens.Lens' ListBucketAnalyticsConfigurationsResponse Core.Int
listBucketAnalyticsConfigurationsResponse_httpStatus = Lens.lens (\ListBucketAnalyticsConfigurationsResponse' {httpStatus} -> httpStatus) (\s@ListBucketAnalyticsConfigurationsResponse' {} a -> s {httpStatus = a} :: ListBucketAnalyticsConfigurationsResponse)

instance
  Core.NFData
    ListBucketAnalyticsConfigurationsResponse
