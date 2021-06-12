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
-- Module      : Network.AWS.S3.ListBucketIntelligentTieringConfigurations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the S3 Intelligent-Tiering configuration from the specified
-- bucket.
--
-- The S3 Intelligent-Tiering storage class is designed to optimize storage
-- costs by automatically moving data to the most cost-effective storage
-- access tier, without additional operational overhead. S3
-- Intelligent-Tiering delivers automatic cost savings by moving data
-- between access tiers, when access patterns change.
--
-- The S3 Intelligent-Tiering storage class is suitable for objects larger
-- than 128 KB that you plan to store for at least 30 days. If the size of
-- an object is less than 128 KB, it is not eligible for auto-tiering.
-- Smaller objects can be stored, but they are always charged at the
-- frequent access tier rates in the S3 Intelligent-Tiering storage class.
--
-- If you delete an object before the end of the 30-day minimum storage
-- duration period, you are charged for 30 days. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html#sc-dynamic-data-access Storage class for automatically optimizing frequently and infrequently accessed objects>.
--
-- Operations related to @ListBucketIntelligentTieringConfigurations@
-- include:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketIntelligentTieringConfiguration.html DeleteBucketIntelligentTieringConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketIntelligentTieringConfiguration.html PutBucketIntelligentTieringConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketIntelligentTieringConfiguration.html GetBucketIntelligentTieringConfiguration>
module Network.AWS.S3.ListBucketIntelligentTieringConfigurations
  ( -- * Creating a Request
    ListBucketIntelligentTieringConfigurations (..),
    newListBucketIntelligentTieringConfigurations,

    -- * Request Lenses
    listBucketIntelligentTieringConfigurations_continuationToken,
    listBucketIntelligentTieringConfigurations_bucket,

    -- * Destructuring the Response
    ListBucketIntelligentTieringConfigurationsResponse (..),
    newListBucketIntelligentTieringConfigurationsResponse,

    -- * Response Lenses
    listBucketIntelligentTieringConfigurationsResponse_isTruncated,
    listBucketIntelligentTieringConfigurationsResponse_intelligentTieringConfigurationList,
    listBucketIntelligentTieringConfigurationsResponse_nextContinuationToken,
    listBucketIntelligentTieringConfigurationsResponse_continuationToken,
    listBucketIntelligentTieringConfigurationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newListBucketIntelligentTieringConfigurations' smart constructor.
data ListBucketIntelligentTieringConfigurations = ListBucketIntelligentTieringConfigurations'
  { -- | The ContinuationToken that represents a placeholder from where this
    -- request should begin.
    continuationToken :: Core.Maybe Core.Text,
    -- | The name of the Amazon S3 bucket whose configuration you want to modify
    -- or retrieve.
    bucket :: BucketName
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListBucketIntelligentTieringConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'continuationToken', 'listBucketIntelligentTieringConfigurations_continuationToken' - The ContinuationToken that represents a placeholder from where this
-- request should begin.
--
-- 'bucket', 'listBucketIntelligentTieringConfigurations_bucket' - The name of the Amazon S3 bucket whose configuration you want to modify
-- or retrieve.
newListBucketIntelligentTieringConfigurations ::
  -- | 'bucket'
  BucketName ->
  ListBucketIntelligentTieringConfigurations
newListBucketIntelligentTieringConfigurations
  pBucket_ =
    ListBucketIntelligentTieringConfigurations'
      { continuationToken =
          Core.Nothing,
        bucket = pBucket_
      }

-- | The ContinuationToken that represents a placeholder from where this
-- request should begin.
listBucketIntelligentTieringConfigurations_continuationToken :: Lens.Lens' ListBucketIntelligentTieringConfigurations (Core.Maybe Core.Text)
listBucketIntelligentTieringConfigurations_continuationToken = Lens.lens (\ListBucketIntelligentTieringConfigurations' {continuationToken} -> continuationToken) (\s@ListBucketIntelligentTieringConfigurations' {} a -> s {continuationToken = a} :: ListBucketIntelligentTieringConfigurations)

-- | The name of the Amazon S3 bucket whose configuration you want to modify
-- or retrieve.
listBucketIntelligentTieringConfigurations_bucket :: Lens.Lens' ListBucketIntelligentTieringConfigurations BucketName
listBucketIntelligentTieringConfigurations_bucket = Lens.lens (\ListBucketIntelligentTieringConfigurations' {bucket} -> bucket) (\s@ListBucketIntelligentTieringConfigurations' {} a -> s {bucket = a} :: ListBucketIntelligentTieringConfigurations)

instance
  Core.AWSRequest
    ListBucketIntelligentTieringConfigurations
  where
  type
    AWSResponse
      ListBucketIntelligentTieringConfigurations =
      ListBucketIntelligentTieringConfigurationsResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ListBucketIntelligentTieringConfigurationsResponse'
            Core.<$> (x Core..@? "IsTruncated")
              Core.<*> ( Core.may
                           (Core.parseXMLList "IntelligentTieringConfiguration")
                           x
                       )
              Core.<*> (x Core..@? "NextContinuationToken")
              Core.<*> (x Core..@? "ContinuationToken")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ListBucketIntelligentTieringConfigurations

instance
  Core.NFData
    ListBucketIntelligentTieringConfigurations

instance
  Core.ToHeaders
    ListBucketIntelligentTieringConfigurations
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    ListBucketIntelligentTieringConfigurations
  where
  toPath
    ListBucketIntelligentTieringConfigurations' {..} =
      Core.mconcat ["/", Core.toBS bucket]

instance
  Core.ToQuery
    ListBucketIntelligentTieringConfigurations
  where
  toQuery
    ListBucketIntelligentTieringConfigurations' {..} =
      Core.mconcat
        [ "continuation-token" Core.=: continuationToken,
          "intelligent-tiering"
        ]

-- | /See:/ 'newListBucketIntelligentTieringConfigurationsResponse' smart constructor.
data ListBucketIntelligentTieringConfigurationsResponse = ListBucketIntelligentTieringConfigurationsResponse'
  { -- | Indicates whether the returned list of analytics configurations is
    -- complete. A value of true indicates that the list is not complete and
    -- the NextContinuationToken will be provided for a subsequent request.
    isTruncated :: Core.Maybe Core.Bool,
    -- | The list of S3 Intelligent-Tiering configurations for a bucket.
    intelligentTieringConfigurationList :: Core.Maybe [IntelligentTieringConfiguration],
    -- | The marker used to continue this inventory configuration listing. Use
    -- the @NextContinuationToken@ from this response to continue the listing
    -- in a subsequent request. The continuation token is an opaque value that
    -- Amazon S3 understands.
    nextContinuationToken :: Core.Maybe Core.Text,
    -- | The ContinuationToken that represents a placeholder from where this
    -- request should begin.
    continuationToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListBucketIntelligentTieringConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTruncated', 'listBucketIntelligentTieringConfigurationsResponse_isTruncated' - Indicates whether the returned list of analytics configurations is
-- complete. A value of true indicates that the list is not complete and
-- the NextContinuationToken will be provided for a subsequent request.
--
-- 'intelligentTieringConfigurationList', 'listBucketIntelligentTieringConfigurationsResponse_intelligentTieringConfigurationList' - The list of S3 Intelligent-Tiering configurations for a bucket.
--
-- 'nextContinuationToken', 'listBucketIntelligentTieringConfigurationsResponse_nextContinuationToken' - The marker used to continue this inventory configuration listing. Use
-- the @NextContinuationToken@ from this response to continue the listing
-- in a subsequent request. The continuation token is an opaque value that
-- Amazon S3 understands.
--
-- 'continuationToken', 'listBucketIntelligentTieringConfigurationsResponse_continuationToken' - The ContinuationToken that represents a placeholder from where this
-- request should begin.
--
-- 'httpStatus', 'listBucketIntelligentTieringConfigurationsResponse_httpStatus' - The response's http status code.
newListBucketIntelligentTieringConfigurationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListBucketIntelligentTieringConfigurationsResponse
newListBucketIntelligentTieringConfigurationsResponse
  pHttpStatus_ =
    ListBucketIntelligentTieringConfigurationsResponse'
      { isTruncated =
          Core.Nothing,
        intelligentTieringConfigurationList =
          Core.Nothing,
        nextContinuationToken =
          Core.Nothing,
        continuationToken =
          Core.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Indicates whether the returned list of analytics configurations is
-- complete. A value of true indicates that the list is not complete and
-- the NextContinuationToken will be provided for a subsequent request.
listBucketIntelligentTieringConfigurationsResponse_isTruncated :: Lens.Lens' ListBucketIntelligentTieringConfigurationsResponse (Core.Maybe Core.Bool)
listBucketIntelligentTieringConfigurationsResponse_isTruncated = Lens.lens (\ListBucketIntelligentTieringConfigurationsResponse' {isTruncated} -> isTruncated) (\s@ListBucketIntelligentTieringConfigurationsResponse' {} a -> s {isTruncated = a} :: ListBucketIntelligentTieringConfigurationsResponse)

-- | The list of S3 Intelligent-Tiering configurations for a bucket.
listBucketIntelligentTieringConfigurationsResponse_intelligentTieringConfigurationList :: Lens.Lens' ListBucketIntelligentTieringConfigurationsResponse (Core.Maybe [IntelligentTieringConfiguration])
listBucketIntelligentTieringConfigurationsResponse_intelligentTieringConfigurationList = Lens.lens (\ListBucketIntelligentTieringConfigurationsResponse' {intelligentTieringConfigurationList} -> intelligentTieringConfigurationList) (\s@ListBucketIntelligentTieringConfigurationsResponse' {} a -> s {intelligentTieringConfigurationList = a} :: ListBucketIntelligentTieringConfigurationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The marker used to continue this inventory configuration listing. Use
-- the @NextContinuationToken@ from this response to continue the listing
-- in a subsequent request. The continuation token is an opaque value that
-- Amazon S3 understands.
listBucketIntelligentTieringConfigurationsResponse_nextContinuationToken :: Lens.Lens' ListBucketIntelligentTieringConfigurationsResponse (Core.Maybe Core.Text)
listBucketIntelligentTieringConfigurationsResponse_nextContinuationToken = Lens.lens (\ListBucketIntelligentTieringConfigurationsResponse' {nextContinuationToken} -> nextContinuationToken) (\s@ListBucketIntelligentTieringConfigurationsResponse' {} a -> s {nextContinuationToken = a} :: ListBucketIntelligentTieringConfigurationsResponse)

-- | The ContinuationToken that represents a placeholder from where this
-- request should begin.
listBucketIntelligentTieringConfigurationsResponse_continuationToken :: Lens.Lens' ListBucketIntelligentTieringConfigurationsResponse (Core.Maybe Core.Text)
listBucketIntelligentTieringConfigurationsResponse_continuationToken = Lens.lens (\ListBucketIntelligentTieringConfigurationsResponse' {continuationToken} -> continuationToken) (\s@ListBucketIntelligentTieringConfigurationsResponse' {} a -> s {continuationToken = a} :: ListBucketIntelligentTieringConfigurationsResponse)

-- | The response's http status code.
listBucketIntelligentTieringConfigurationsResponse_httpStatus :: Lens.Lens' ListBucketIntelligentTieringConfigurationsResponse Core.Int
listBucketIntelligentTieringConfigurationsResponse_httpStatus = Lens.lens (\ListBucketIntelligentTieringConfigurationsResponse' {httpStatus} -> httpStatus) (\s@ListBucketIntelligentTieringConfigurationsResponse' {} a -> s {httpStatus = a} :: ListBucketIntelligentTieringConfigurationsResponse)

instance
  Core.NFData
    ListBucketIntelligentTieringConfigurationsResponse
