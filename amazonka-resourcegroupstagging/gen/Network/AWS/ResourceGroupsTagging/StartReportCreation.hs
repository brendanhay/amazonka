{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ResourceGroupsTagging.StartReportCreation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a report that lists all tagged resources in the accounts
-- across your organization and tells whether each resource is compliant
-- with the effective tag policy. Compliance data is refreshed daily. The
-- report is generated asynchronously.
--
-- The generated report is saved to the following location:
--
-- @s3:\/\/example-bucket\/AwsTagPolicies\/o-exampleorgid\/YYYY-MM-ddTHH:mm:ssZ\/report.csv@
--
-- You can call this operation only from the organization\'s management
-- account and from the us-east-1 Region.
module Network.AWS.ResourceGroupsTagging.StartReportCreation
  ( -- * Creating a Request
    StartReportCreation (..),
    newStartReportCreation,

    -- * Request Lenses
    startReportCreation_s3Bucket,

    -- * Destructuring the Response
    StartReportCreationResponse (..),
    newStartReportCreationResponse,

    -- * Response Lenses
    startReportCreationResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import Network.AWS.ResourceGroupsTagging.Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartReportCreation' smart constructor.
data StartReportCreation = StartReportCreation'
  { -- | The name of the Amazon S3 bucket where the report will be stored; for
    -- example:
    --
    -- @awsexamplebucket@
    --
    -- For more information on S3 bucket requirements, including an example
    -- bucket policy, see the example S3 bucket policy on this page.
    s3Bucket :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartReportCreation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Bucket', 'startReportCreation_s3Bucket' - The name of the Amazon S3 bucket where the report will be stored; for
-- example:
--
-- @awsexamplebucket@
--
-- For more information on S3 bucket requirements, including an example
-- bucket policy, see the example S3 bucket policy on this page.
newStartReportCreation ::
  -- | 's3Bucket'
  Prelude.Text ->
  StartReportCreation
newStartReportCreation pS3Bucket_ =
  StartReportCreation' {s3Bucket = pS3Bucket_}

-- | The name of the Amazon S3 bucket where the report will be stored; for
-- example:
--
-- @awsexamplebucket@
--
-- For more information on S3 bucket requirements, including an example
-- bucket policy, see the example S3 bucket policy on this page.
startReportCreation_s3Bucket :: Lens.Lens' StartReportCreation Prelude.Text
startReportCreation_s3Bucket = Lens.lens (\StartReportCreation' {s3Bucket} -> s3Bucket) (\s@StartReportCreation' {} a -> s {s3Bucket = a} :: StartReportCreation)

instance Prelude.AWSRequest StartReportCreation where
  type
    Rs StartReportCreation =
      StartReportCreationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartReportCreationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartReportCreation

instance Prelude.NFData StartReportCreation

instance Prelude.ToHeaders StartReportCreation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "ResourceGroupsTaggingAPI_20170126.StartReportCreation" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StartReportCreation where
  toJSON StartReportCreation' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("S3Bucket" Prelude..= s3Bucket)]
      )

instance Prelude.ToPath StartReportCreation where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StartReportCreation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartReportCreationResponse' smart constructor.
data StartReportCreationResponse = StartReportCreationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartReportCreationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startReportCreationResponse_httpStatus' - The response's http status code.
newStartReportCreationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartReportCreationResponse
newStartReportCreationResponse pHttpStatus_ =
  StartReportCreationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
startReportCreationResponse_httpStatus :: Lens.Lens' StartReportCreationResponse Prelude.Int
startReportCreationResponse_httpStatus = Lens.lens (\StartReportCreationResponse' {httpStatus} -> httpStatus) (\s@StartReportCreationResponse' {} a -> s {httpStatus = a} :: StartReportCreationResponse)

instance Prelude.NFData StartReportCreationResponse
