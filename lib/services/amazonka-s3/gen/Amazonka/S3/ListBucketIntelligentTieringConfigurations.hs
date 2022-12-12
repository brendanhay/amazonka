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
-- Module      : Amazonka.S3.ListBucketIntelligentTieringConfigurations
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- access tier, without performance impact or operational overhead. S3
-- Intelligent-Tiering delivers automatic cost savings in three low latency
-- and high throughput access tiers. To get the lowest storage cost on data
-- that can be accessed in minutes to hours, you can choose to activate
-- additional archiving capabilities.
--
-- The S3 Intelligent-Tiering storage class is the ideal storage class for
-- data with unknown, changing, or unpredictable access patterns,
-- independent of object size or retention period. If the size of an object
-- is less than 128 KB, it is not monitored and not eligible for
-- auto-tiering. Smaller objects can be stored, but they are always charged
-- at the Frequent Access tier rates in the S3 Intelligent-Tiering storage
-- class.
--
-- For more information, see
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
module Amazonka.S3.ListBucketIntelligentTieringConfigurations
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
    listBucketIntelligentTieringConfigurationsResponse_continuationToken,
    listBucketIntelligentTieringConfigurationsResponse_intelligentTieringConfigurationList,
    listBucketIntelligentTieringConfigurationsResponse_isTruncated,
    listBucketIntelligentTieringConfigurationsResponse_nextContinuationToken,
    listBucketIntelligentTieringConfigurationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newListBucketIntelligentTieringConfigurations' smart constructor.
data ListBucketIntelligentTieringConfigurations = ListBucketIntelligentTieringConfigurations'
  { -- | The @ContinuationToken@ that represents a placeholder from where this
    -- request should begin.
    continuationToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon S3 bucket whose configuration you want to modify
    -- or retrieve.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBucketIntelligentTieringConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'continuationToken', 'listBucketIntelligentTieringConfigurations_continuationToken' - The @ContinuationToken@ that represents a placeholder from where this
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
          Prelude.Nothing,
        bucket = pBucket_
      }

-- | The @ContinuationToken@ that represents a placeholder from where this
-- request should begin.
listBucketIntelligentTieringConfigurations_continuationToken :: Lens.Lens' ListBucketIntelligentTieringConfigurations (Prelude.Maybe Prelude.Text)
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
  request overrides =
    Request.s3vhost
      Prelude.. Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListBucketIntelligentTieringConfigurationsResponse'
            Prelude.<$> (x Data..@? "ContinuationToken")
              Prelude.<*> ( Core.may
                              (Data.parseXMLList "IntelligentTieringConfiguration")
                              x
                          )
              Prelude.<*> (x Data..@? "IsTruncated")
              Prelude.<*> (x Data..@? "NextContinuationToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListBucketIntelligentTieringConfigurations
  where
  hashWithSalt
    _salt
    ListBucketIntelligentTieringConfigurations' {..} =
      _salt `Prelude.hashWithSalt` continuationToken
        `Prelude.hashWithSalt` bucket

instance
  Prelude.NFData
    ListBucketIntelligentTieringConfigurations
  where
  rnf ListBucketIntelligentTieringConfigurations' {..} =
    Prelude.rnf continuationToken
      `Prelude.seq` Prelude.rnf bucket

instance
  Data.ToHeaders
    ListBucketIntelligentTieringConfigurations
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    ListBucketIntelligentTieringConfigurations
  where
  toPath
    ListBucketIntelligentTieringConfigurations' {..} =
      Prelude.mconcat ["/", Data.toBS bucket]

instance
  Data.ToQuery
    ListBucketIntelligentTieringConfigurations
  where
  toQuery
    ListBucketIntelligentTieringConfigurations' {..} =
      Prelude.mconcat
        [ "continuation-token" Data.=: continuationToken,
          "intelligent-tiering"
        ]

-- | /See:/ 'newListBucketIntelligentTieringConfigurationsResponse' smart constructor.
data ListBucketIntelligentTieringConfigurationsResponse = ListBucketIntelligentTieringConfigurationsResponse'
  { -- | The @ContinuationToken@ that represents a placeholder from where this
    -- request should begin.
    continuationToken :: Prelude.Maybe Prelude.Text,
    -- | The list of S3 Intelligent-Tiering configurations for a bucket.
    intelligentTieringConfigurationList :: Prelude.Maybe [IntelligentTieringConfiguration],
    -- | Indicates whether the returned list of analytics configurations is
    -- complete. A value of @true@ indicates that the list is not complete and
    -- the @NextContinuationToken@ will be provided for a subsequent request.
    isTruncated :: Prelude.Maybe Prelude.Bool,
    -- | The marker used to continue this inventory configuration listing. Use
    -- the @NextContinuationToken@ from this response to continue the listing
    -- in a subsequent request. The continuation token is an opaque value that
    -- Amazon S3 understands.
    nextContinuationToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBucketIntelligentTieringConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'continuationToken', 'listBucketIntelligentTieringConfigurationsResponse_continuationToken' - The @ContinuationToken@ that represents a placeholder from where this
-- request should begin.
--
-- 'intelligentTieringConfigurationList', 'listBucketIntelligentTieringConfigurationsResponse_intelligentTieringConfigurationList' - The list of S3 Intelligent-Tiering configurations for a bucket.
--
-- 'isTruncated', 'listBucketIntelligentTieringConfigurationsResponse_isTruncated' - Indicates whether the returned list of analytics configurations is
-- complete. A value of @true@ indicates that the list is not complete and
-- the @NextContinuationToken@ will be provided for a subsequent request.
--
-- 'nextContinuationToken', 'listBucketIntelligentTieringConfigurationsResponse_nextContinuationToken' - The marker used to continue this inventory configuration listing. Use
-- the @NextContinuationToken@ from this response to continue the listing
-- in a subsequent request. The continuation token is an opaque value that
-- Amazon S3 understands.
--
-- 'httpStatus', 'listBucketIntelligentTieringConfigurationsResponse_httpStatus' - The response's http status code.
newListBucketIntelligentTieringConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBucketIntelligentTieringConfigurationsResponse
newListBucketIntelligentTieringConfigurationsResponse
  pHttpStatus_ =
    ListBucketIntelligentTieringConfigurationsResponse'
      { continuationToken =
          Prelude.Nothing,
        intelligentTieringConfigurationList =
          Prelude.Nothing,
        isTruncated =
          Prelude.Nothing,
        nextContinuationToken =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The @ContinuationToken@ that represents a placeholder from where this
-- request should begin.
listBucketIntelligentTieringConfigurationsResponse_continuationToken :: Lens.Lens' ListBucketIntelligentTieringConfigurationsResponse (Prelude.Maybe Prelude.Text)
listBucketIntelligentTieringConfigurationsResponse_continuationToken = Lens.lens (\ListBucketIntelligentTieringConfigurationsResponse' {continuationToken} -> continuationToken) (\s@ListBucketIntelligentTieringConfigurationsResponse' {} a -> s {continuationToken = a} :: ListBucketIntelligentTieringConfigurationsResponse)

-- | The list of S3 Intelligent-Tiering configurations for a bucket.
listBucketIntelligentTieringConfigurationsResponse_intelligentTieringConfigurationList :: Lens.Lens' ListBucketIntelligentTieringConfigurationsResponse (Prelude.Maybe [IntelligentTieringConfiguration])
listBucketIntelligentTieringConfigurationsResponse_intelligentTieringConfigurationList = Lens.lens (\ListBucketIntelligentTieringConfigurationsResponse' {intelligentTieringConfigurationList} -> intelligentTieringConfigurationList) (\s@ListBucketIntelligentTieringConfigurationsResponse' {} a -> s {intelligentTieringConfigurationList = a} :: ListBucketIntelligentTieringConfigurationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether the returned list of analytics configurations is
-- complete. A value of @true@ indicates that the list is not complete and
-- the @NextContinuationToken@ will be provided for a subsequent request.
listBucketIntelligentTieringConfigurationsResponse_isTruncated :: Lens.Lens' ListBucketIntelligentTieringConfigurationsResponse (Prelude.Maybe Prelude.Bool)
listBucketIntelligentTieringConfigurationsResponse_isTruncated = Lens.lens (\ListBucketIntelligentTieringConfigurationsResponse' {isTruncated} -> isTruncated) (\s@ListBucketIntelligentTieringConfigurationsResponse' {} a -> s {isTruncated = a} :: ListBucketIntelligentTieringConfigurationsResponse)

-- | The marker used to continue this inventory configuration listing. Use
-- the @NextContinuationToken@ from this response to continue the listing
-- in a subsequent request. The continuation token is an opaque value that
-- Amazon S3 understands.
listBucketIntelligentTieringConfigurationsResponse_nextContinuationToken :: Lens.Lens' ListBucketIntelligentTieringConfigurationsResponse (Prelude.Maybe Prelude.Text)
listBucketIntelligentTieringConfigurationsResponse_nextContinuationToken = Lens.lens (\ListBucketIntelligentTieringConfigurationsResponse' {nextContinuationToken} -> nextContinuationToken) (\s@ListBucketIntelligentTieringConfigurationsResponse' {} a -> s {nextContinuationToken = a} :: ListBucketIntelligentTieringConfigurationsResponse)

-- | The response's http status code.
listBucketIntelligentTieringConfigurationsResponse_httpStatus :: Lens.Lens' ListBucketIntelligentTieringConfigurationsResponse Prelude.Int
listBucketIntelligentTieringConfigurationsResponse_httpStatus = Lens.lens (\ListBucketIntelligentTieringConfigurationsResponse' {httpStatus} -> httpStatus) (\s@ListBucketIntelligentTieringConfigurationsResponse' {} a -> s {httpStatus = a} :: ListBucketIntelligentTieringConfigurationsResponse)

instance
  Prelude.NFData
    ListBucketIntelligentTieringConfigurationsResponse
  where
  rnf
    ListBucketIntelligentTieringConfigurationsResponse' {..} =
      Prelude.rnf continuationToken
        `Prelude.seq` Prelude.rnf intelligentTieringConfigurationList
        `Prelude.seq` Prelude.rnf isTruncated
        `Prelude.seq` Prelude.rnf nextContinuationToken
        `Prelude.seq` Prelude.rnf httpStatus
