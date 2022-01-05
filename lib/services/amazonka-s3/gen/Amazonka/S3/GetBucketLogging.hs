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
-- Module      : Amazonka.S3.GetBucketLogging
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the logging status of a bucket and the permissions users have to
-- view and modify that status. To use GET, you must be the bucket owner.
--
-- The following operations are related to @GetBucketLogging@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLogging.html PutBucketLogging>
module Amazonka.S3.GetBucketLogging
  ( -- * Creating a Request
    GetBucketLogging (..),
    newGetBucketLogging,

    -- * Request Lenses
    getBucketLogging_expectedBucketOwner,
    getBucketLogging_bucket,

    -- * Destructuring the Response
    GetBucketLoggingResponse (..),
    newGetBucketLoggingResponse,

    -- * Response Lenses
    getBucketLoggingResponse_loggingEnabled,
    getBucketLoggingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newGetBucketLogging' smart constructor.
data GetBucketLogging = GetBucketLogging'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The bucket name for which to get the logging information.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketLogging' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getBucketLogging_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'bucket', 'getBucketLogging_bucket' - The bucket name for which to get the logging information.
newGetBucketLogging ::
  -- | 'bucket'
  BucketName ->
  GetBucketLogging
newGetBucketLogging pBucket_ =
  GetBucketLogging'
    { expectedBucketOwner =
        Prelude.Nothing,
      bucket = pBucket_
    }

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
getBucketLogging_expectedBucketOwner :: Lens.Lens' GetBucketLogging (Prelude.Maybe Prelude.Text)
getBucketLogging_expectedBucketOwner = Lens.lens (\GetBucketLogging' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetBucketLogging' {} a -> s {expectedBucketOwner = a} :: GetBucketLogging)

-- | The bucket name for which to get the logging information.
getBucketLogging_bucket :: Lens.Lens' GetBucketLogging BucketName
getBucketLogging_bucket = Lens.lens (\GetBucketLogging' {bucket} -> bucket) (\s@GetBucketLogging' {} a -> s {bucket = a} :: GetBucketLogging)

instance Core.AWSRequest GetBucketLogging where
  type
    AWSResponse GetBucketLogging =
      GetBucketLoggingResponse
  request =
    Request.s3vhost
      Prelude.. Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketLoggingResponse'
            Prelude.<$> (x Core..@? "LoggingEnabled")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBucketLogging where
  hashWithSalt _salt GetBucketLogging' {..} =
    _salt `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData GetBucketLogging where
  rnf GetBucketLogging' {..} =
    Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket

instance Core.ToHeaders GetBucketLogging where
  toHeaders GetBucketLogging' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner
      ]

instance Core.ToPath GetBucketLogging where
  toPath GetBucketLogging' {..} =
    Prelude.mconcat ["/", Core.toBS bucket]

instance Core.ToQuery GetBucketLogging where
  toQuery = Prelude.const (Prelude.mconcat ["logging"])

-- | /See:/ 'newGetBucketLoggingResponse' smart constructor.
data GetBucketLoggingResponse = GetBucketLoggingResponse'
  { loggingEnabled :: Prelude.Maybe LoggingEnabled,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketLoggingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loggingEnabled', 'getBucketLoggingResponse_loggingEnabled' - Undocumented member.
--
-- 'httpStatus', 'getBucketLoggingResponse_httpStatus' - The response's http status code.
newGetBucketLoggingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBucketLoggingResponse
newGetBucketLoggingResponse pHttpStatus_ =
  GetBucketLoggingResponse'
    { loggingEnabled =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getBucketLoggingResponse_loggingEnabled :: Lens.Lens' GetBucketLoggingResponse (Prelude.Maybe LoggingEnabled)
getBucketLoggingResponse_loggingEnabled = Lens.lens (\GetBucketLoggingResponse' {loggingEnabled} -> loggingEnabled) (\s@GetBucketLoggingResponse' {} a -> s {loggingEnabled = a} :: GetBucketLoggingResponse)

-- | The response's http status code.
getBucketLoggingResponse_httpStatus :: Lens.Lens' GetBucketLoggingResponse Prelude.Int
getBucketLoggingResponse_httpStatus = Lens.lens (\GetBucketLoggingResponse' {httpStatus} -> httpStatus) (\s@GetBucketLoggingResponse' {} a -> s {httpStatus = a} :: GetBucketLoggingResponse)

instance Prelude.NFData GetBucketLoggingResponse where
  rnf GetBucketLoggingResponse' {..} =
    Prelude.rnf loggingEnabled
      `Prelude.seq` Prelude.rnf httpStatus
