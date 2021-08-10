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
-- Module      : Network.AWS.S3.ListBucketMetricsConfigurations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the metrics configurations for the bucket. The metrics
-- configurations are only for the request metrics of the bucket and do not
-- provide information on daily storage metrics. You can have up to 1,000
-- configurations per bucket.
--
-- This operation supports list pagination and does not return more than
-- 100 configurations at a time. Always check the @IsTruncated@ element in
-- the response. If there are no more configurations to list, @IsTruncated@
-- is set to false. If there are more configurations to list, @IsTruncated@
-- is set to true, and there is a value in @NextContinuationToken@. You use
-- the @NextContinuationToken@ value to continue the pagination of the list
-- by passing the value in @continuation-token@ in the request to @GET@ the
-- next page.
--
-- To use this operation, you must have permissions to perform the
-- @s3:GetMetricsConfiguration@ action. The bucket owner has this
-- permission by default. The bucket owner can grant this permission to
-- others. For more information about permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources>.
--
-- For more information about metrics configurations and CloudWatch request
-- metrics, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/cloudwatch-monitoring.html Monitoring Metrics with Amazon CloudWatch>.
--
-- The following operations are related to
-- @ListBucketMetricsConfigurations@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketMetricsConfiguration.html PutBucketMetricsConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketMetricsConfiguration.html GetBucketMetricsConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketMetricsConfiguration.html DeleteBucketMetricsConfiguration>
module Network.AWS.S3.ListBucketMetricsConfigurations
  ( -- * Creating a Request
    ListBucketMetricsConfigurations (..),
    newListBucketMetricsConfigurations,

    -- * Request Lenses
    listBucketMetricsConfigurations_expectedBucketOwner,
    listBucketMetricsConfigurations_continuationToken,
    listBucketMetricsConfigurations_bucket,

    -- * Destructuring the Response
    ListBucketMetricsConfigurationsResponse (..),
    newListBucketMetricsConfigurationsResponse,

    -- * Response Lenses
    listBucketMetricsConfigurationsResponse_isTruncated,
    listBucketMetricsConfigurationsResponse_nextContinuationToken,
    listBucketMetricsConfigurationsResponse_metricsConfigurationList,
    listBucketMetricsConfigurationsResponse_continuationToken,
    listBucketMetricsConfigurationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newListBucketMetricsConfigurations' smart constructor.
data ListBucketMetricsConfigurations = ListBucketMetricsConfigurations'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The marker that is used to continue a metrics configuration listing that
    -- has been truncated. Use the NextContinuationToken from a previously
    -- truncated list response to continue the listing. The continuation token
    -- is an opaque value that Amazon S3 understands.
    continuationToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket containing the metrics configurations to
    -- retrieve.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBucketMetricsConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'listBucketMetricsConfigurations_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'continuationToken', 'listBucketMetricsConfigurations_continuationToken' - The marker that is used to continue a metrics configuration listing that
-- has been truncated. Use the NextContinuationToken from a previously
-- truncated list response to continue the listing. The continuation token
-- is an opaque value that Amazon S3 understands.
--
-- 'bucket', 'listBucketMetricsConfigurations_bucket' - The name of the bucket containing the metrics configurations to
-- retrieve.
newListBucketMetricsConfigurations ::
  -- | 'bucket'
  BucketName ->
  ListBucketMetricsConfigurations
newListBucketMetricsConfigurations pBucket_ =
  ListBucketMetricsConfigurations'
    { expectedBucketOwner =
        Prelude.Nothing,
      continuationToken = Prelude.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
listBucketMetricsConfigurations_expectedBucketOwner :: Lens.Lens' ListBucketMetricsConfigurations (Prelude.Maybe Prelude.Text)
listBucketMetricsConfigurations_expectedBucketOwner = Lens.lens (\ListBucketMetricsConfigurations' {expectedBucketOwner} -> expectedBucketOwner) (\s@ListBucketMetricsConfigurations' {} a -> s {expectedBucketOwner = a} :: ListBucketMetricsConfigurations)

-- | The marker that is used to continue a metrics configuration listing that
-- has been truncated. Use the NextContinuationToken from a previously
-- truncated list response to continue the listing. The continuation token
-- is an opaque value that Amazon S3 understands.
listBucketMetricsConfigurations_continuationToken :: Lens.Lens' ListBucketMetricsConfigurations (Prelude.Maybe Prelude.Text)
listBucketMetricsConfigurations_continuationToken = Lens.lens (\ListBucketMetricsConfigurations' {continuationToken} -> continuationToken) (\s@ListBucketMetricsConfigurations' {} a -> s {continuationToken = a} :: ListBucketMetricsConfigurations)

-- | The name of the bucket containing the metrics configurations to
-- retrieve.
listBucketMetricsConfigurations_bucket :: Lens.Lens' ListBucketMetricsConfigurations BucketName
listBucketMetricsConfigurations_bucket = Lens.lens (\ListBucketMetricsConfigurations' {bucket} -> bucket) (\s@ListBucketMetricsConfigurations' {} a -> s {bucket = a} :: ListBucketMetricsConfigurations)

instance
  Core.AWSRequest
    ListBucketMetricsConfigurations
  where
  type
    AWSResponse ListBucketMetricsConfigurations =
      ListBucketMetricsConfigurationsResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ListBucketMetricsConfigurationsResponse'
            Prelude.<$> (x Core..@? "IsTruncated")
            Prelude.<*> (x Core..@? "NextContinuationToken")
            Prelude.<*> ( Core.may
                            (Core.parseXMLList "MetricsConfiguration")
                            x
                        )
            Prelude.<*> (x Core..@? "ContinuationToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListBucketMetricsConfigurations

instance
  Prelude.NFData
    ListBucketMetricsConfigurations

instance
  Core.ToHeaders
    ListBucketMetricsConfigurations
  where
  toHeaders ListBucketMetricsConfigurations' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner
      ]

instance Core.ToPath ListBucketMetricsConfigurations where
  toPath ListBucketMetricsConfigurations' {..} =
    Prelude.mconcat ["/", Core.toBS bucket]

instance Core.ToQuery ListBucketMetricsConfigurations where
  toQuery ListBucketMetricsConfigurations' {..} =
    Prelude.mconcat
      [ "continuation-token" Core.=: continuationToken,
        "metrics"
      ]

-- | /See:/ 'newListBucketMetricsConfigurationsResponse' smart constructor.
data ListBucketMetricsConfigurationsResponse = ListBucketMetricsConfigurationsResponse'
  { -- | Indicates whether the returned list of metrics configurations is
    -- complete. A value of true indicates that the list is not complete and
    -- the NextContinuationToken will be provided for a subsequent request.
    isTruncated :: Prelude.Maybe Prelude.Bool,
    -- | The marker used to continue a metrics configuration listing that has
    -- been truncated. Use the @NextContinuationToken@ from a previously
    -- truncated list response to continue the listing. The continuation token
    -- is an opaque value that Amazon S3 understands.
    nextContinuationToken :: Prelude.Maybe Prelude.Text,
    -- | The list of metrics configurations for a bucket.
    metricsConfigurationList :: Prelude.Maybe [MetricsConfiguration],
    -- | The marker that is used as a starting point for this metrics
    -- configuration list response. This value is present if it was sent in the
    -- request.
    continuationToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBucketMetricsConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTruncated', 'listBucketMetricsConfigurationsResponse_isTruncated' - Indicates whether the returned list of metrics configurations is
-- complete. A value of true indicates that the list is not complete and
-- the NextContinuationToken will be provided for a subsequent request.
--
-- 'nextContinuationToken', 'listBucketMetricsConfigurationsResponse_nextContinuationToken' - The marker used to continue a metrics configuration listing that has
-- been truncated. Use the @NextContinuationToken@ from a previously
-- truncated list response to continue the listing. The continuation token
-- is an opaque value that Amazon S3 understands.
--
-- 'metricsConfigurationList', 'listBucketMetricsConfigurationsResponse_metricsConfigurationList' - The list of metrics configurations for a bucket.
--
-- 'continuationToken', 'listBucketMetricsConfigurationsResponse_continuationToken' - The marker that is used as a starting point for this metrics
-- configuration list response. This value is present if it was sent in the
-- request.
--
-- 'httpStatus', 'listBucketMetricsConfigurationsResponse_httpStatus' - The response's http status code.
newListBucketMetricsConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBucketMetricsConfigurationsResponse
newListBucketMetricsConfigurationsResponse
  pHttpStatus_ =
    ListBucketMetricsConfigurationsResponse'
      { isTruncated =
          Prelude.Nothing,
        nextContinuationToken =
          Prelude.Nothing,
        metricsConfigurationList =
          Prelude.Nothing,
        continuationToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Indicates whether the returned list of metrics configurations is
-- complete. A value of true indicates that the list is not complete and
-- the NextContinuationToken will be provided for a subsequent request.
listBucketMetricsConfigurationsResponse_isTruncated :: Lens.Lens' ListBucketMetricsConfigurationsResponse (Prelude.Maybe Prelude.Bool)
listBucketMetricsConfigurationsResponse_isTruncated = Lens.lens (\ListBucketMetricsConfigurationsResponse' {isTruncated} -> isTruncated) (\s@ListBucketMetricsConfigurationsResponse' {} a -> s {isTruncated = a} :: ListBucketMetricsConfigurationsResponse)

-- | The marker used to continue a metrics configuration listing that has
-- been truncated. Use the @NextContinuationToken@ from a previously
-- truncated list response to continue the listing. The continuation token
-- is an opaque value that Amazon S3 understands.
listBucketMetricsConfigurationsResponse_nextContinuationToken :: Lens.Lens' ListBucketMetricsConfigurationsResponse (Prelude.Maybe Prelude.Text)
listBucketMetricsConfigurationsResponse_nextContinuationToken = Lens.lens (\ListBucketMetricsConfigurationsResponse' {nextContinuationToken} -> nextContinuationToken) (\s@ListBucketMetricsConfigurationsResponse' {} a -> s {nextContinuationToken = a} :: ListBucketMetricsConfigurationsResponse)

-- | The list of metrics configurations for a bucket.
listBucketMetricsConfigurationsResponse_metricsConfigurationList :: Lens.Lens' ListBucketMetricsConfigurationsResponse (Prelude.Maybe [MetricsConfiguration])
listBucketMetricsConfigurationsResponse_metricsConfigurationList = Lens.lens (\ListBucketMetricsConfigurationsResponse' {metricsConfigurationList} -> metricsConfigurationList) (\s@ListBucketMetricsConfigurationsResponse' {} a -> s {metricsConfigurationList = a} :: ListBucketMetricsConfigurationsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The marker that is used as a starting point for this metrics
-- configuration list response. This value is present if it was sent in the
-- request.
listBucketMetricsConfigurationsResponse_continuationToken :: Lens.Lens' ListBucketMetricsConfigurationsResponse (Prelude.Maybe Prelude.Text)
listBucketMetricsConfigurationsResponse_continuationToken = Lens.lens (\ListBucketMetricsConfigurationsResponse' {continuationToken} -> continuationToken) (\s@ListBucketMetricsConfigurationsResponse' {} a -> s {continuationToken = a} :: ListBucketMetricsConfigurationsResponse)

-- | The response's http status code.
listBucketMetricsConfigurationsResponse_httpStatus :: Lens.Lens' ListBucketMetricsConfigurationsResponse Prelude.Int
listBucketMetricsConfigurationsResponse_httpStatus = Lens.lens (\ListBucketMetricsConfigurationsResponse' {httpStatus} -> httpStatus) (\s@ListBucketMetricsConfigurationsResponse' {} a -> s {httpStatus = a} :: ListBucketMetricsConfigurationsResponse)

instance
  Prelude.NFData
    ListBucketMetricsConfigurationsResponse
