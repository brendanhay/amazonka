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
-- Module      : Amazonka.DataSync.DescribeLocationS3
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata, such as bucket name, about an Amazon S3 bucket
-- location.
module Amazonka.DataSync.DescribeLocationS3
  ( -- * Creating a Request
    DescribeLocationS3 (..),
    newDescribeLocationS3,

    -- * Request Lenses
    describeLocationS3_locationArn,

    -- * Destructuring the Response
    DescribeLocationS3Response (..),
    newDescribeLocationS3Response,

    -- * Response Lenses
    describeLocationS3Response_locationArn,
    describeLocationS3Response_s3Config,
    describeLocationS3Response_s3StorageClass,
    describeLocationS3Response_locationUri,
    describeLocationS3Response_creationTime,
    describeLocationS3Response_agentArns,
    describeLocationS3Response_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | DescribeLocationS3Request
--
-- /See:/ 'newDescribeLocationS3' smart constructor.
data DescribeLocationS3 = DescribeLocationS3'
  { -- | The Amazon Resource Name (ARN) of the Amazon S3 bucket location to
    -- describe.
    locationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLocationS3' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locationArn', 'describeLocationS3_locationArn' - The Amazon Resource Name (ARN) of the Amazon S3 bucket location to
-- describe.
newDescribeLocationS3 ::
  -- | 'locationArn'
  Prelude.Text ->
  DescribeLocationS3
newDescribeLocationS3 pLocationArn_ =
  DescribeLocationS3' {locationArn = pLocationArn_}

-- | The Amazon Resource Name (ARN) of the Amazon S3 bucket location to
-- describe.
describeLocationS3_locationArn :: Lens.Lens' DescribeLocationS3 Prelude.Text
describeLocationS3_locationArn = Lens.lens (\DescribeLocationS3' {locationArn} -> locationArn) (\s@DescribeLocationS3' {} a -> s {locationArn = a} :: DescribeLocationS3)

instance Core.AWSRequest DescribeLocationS3 where
  type
    AWSResponse DescribeLocationS3 =
      DescribeLocationS3Response
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLocationS3Response'
            Prelude.<$> (x Core..?> "LocationArn")
            Prelude.<*> (x Core..?> "S3Config")
            Prelude.<*> (x Core..?> "S3StorageClass")
            Prelude.<*> (x Core..?> "LocationUri")
            Prelude.<*> (x Core..?> "CreationTime")
            Prelude.<*> (x Core..?> "AgentArns")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeLocationS3 where
  hashWithSalt _salt DescribeLocationS3' {..} =
    _salt `Prelude.hashWithSalt` locationArn

instance Prelude.NFData DescribeLocationS3 where
  rnf DescribeLocationS3' {..} = Prelude.rnf locationArn

instance Core.ToHeaders DescribeLocationS3 where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "FmrsService.DescribeLocationS3" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeLocationS3 where
  toJSON DescribeLocationS3' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("LocationArn" Core..= locationArn)]
      )

instance Core.ToPath DescribeLocationS3 where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeLocationS3 where
  toQuery = Prelude.const Prelude.mempty

-- | DescribeLocationS3Response
--
-- /See:/ 'newDescribeLocationS3Response' smart constructor.
data DescribeLocationS3Response = DescribeLocationS3Response'
  { -- | The Amazon Resource Name (ARN) of the Amazon S3 bucket or access point.
    locationArn :: Prelude.Maybe Prelude.Text,
    s3Config :: Prelude.Maybe S3Config,
    -- | The Amazon S3 storage class that you chose to store your files in when
    -- this location is used as a task destination. For more information about
    -- S3 storage classes, see
    -- <http://aws.amazon.com/s3/storage-classes/ Amazon S3 Storage Classes>.
    -- Some storage classes have behaviors that can affect your S3 storage
    -- cost. For detailed information, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/create-s3-location.html#using-storage-classes Considerations when working with S3 storage classes in DataSync>.
    s3StorageClass :: Prelude.Maybe S3StorageClass,
    -- | The URL of the Amazon S3 location that was described.
    locationUri :: Prelude.Maybe Prelude.Text,
    -- | The time that the Amazon S3 bucket location was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | If you are using DataSync on an Amazon Web Services Outpost, the Amazon
    -- Resource Name (ARNs) of the EC2 agents deployed on your Outpost. For
    -- more information about launching a DataSync agent on an Amazon Web
    -- Services Outpost, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/deploy-agents.html#outposts-agent Deploy your DataSync agent on Outposts>.
    agentArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLocationS3Response' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locationArn', 'describeLocationS3Response_locationArn' - The Amazon Resource Name (ARN) of the Amazon S3 bucket or access point.
--
-- 's3Config', 'describeLocationS3Response_s3Config' - Undocumented member.
--
-- 's3StorageClass', 'describeLocationS3Response_s3StorageClass' - The Amazon S3 storage class that you chose to store your files in when
-- this location is used as a task destination. For more information about
-- S3 storage classes, see
-- <http://aws.amazon.com/s3/storage-classes/ Amazon S3 Storage Classes>.
-- Some storage classes have behaviors that can affect your S3 storage
-- cost. For detailed information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-s3-location.html#using-storage-classes Considerations when working with S3 storage classes in DataSync>.
--
-- 'locationUri', 'describeLocationS3Response_locationUri' - The URL of the Amazon S3 location that was described.
--
-- 'creationTime', 'describeLocationS3Response_creationTime' - The time that the Amazon S3 bucket location was created.
--
-- 'agentArns', 'describeLocationS3Response_agentArns' - If you are using DataSync on an Amazon Web Services Outpost, the Amazon
-- Resource Name (ARNs) of the EC2 agents deployed on your Outpost. For
-- more information about launching a DataSync agent on an Amazon Web
-- Services Outpost, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/deploy-agents.html#outposts-agent Deploy your DataSync agent on Outposts>.
--
-- 'httpStatus', 'describeLocationS3Response_httpStatus' - The response's http status code.
newDescribeLocationS3Response ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLocationS3Response
newDescribeLocationS3Response pHttpStatus_ =
  DescribeLocationS3Response'
    { locationArn =
        Prelude.Nothing,
      s3Config = Prelude.Nothing,
      s3StorageClass = Prelude.Nothing,
      locationUri = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      agentArns = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the Amazon S3 bucket or access point.
describeLocationS3Response_locationArn :: Lens.Lens' DescribeLocationS3Response (Prelude.Maybe Prelude.Text)
describeLocationS3Response_locationArn = Lens.lens (\DescribeLocationS3Response' {locationArn} -> locationArn) (\s@DescribeLocationS3Response' {} a -> s {locationArn = a} :: DescribeLocationS3Response)

-- | Undocumented member.
describeLocationS3Response_s3Config :: Lens.Lens' DescribeLocationS3Response (Prelude.Maybe S3Config)
describeLocationS3Response_s3Config = Lens.lens (\DescribeLocationS3Response' {s3Config} -> s3Config) (\s@DescribeLocationS3Response' {} a -> s {s3Config = a} :: DescribeLocationS3Response)

-- | The Amazon S3 storage class that you chose to store your files in when
-- this location is used as a task destination. For more information about
-- S3 storage classes, see
-- <http://aws.amazon.com/s3/storage-classes/ Amazon S3 Storage Classes>.
-- Some storage classes have behaviors that can affect your S3 storage
-- cost. For detailed information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-s3-location.html#using-storage-classes Considerations when working with S3 storage classes in DataSync>.
describeLocationS3Response_s3StorageClass :: Lens.Lens' DescribeLocationS3Response (Prelude.Maybe S3StorageClass)
describeLocationS3Response_s3StorageClass = Lens.lens (\DescribeLocationS3Response' {s3StorageClass} -> s3StorageClass) (\s@DescribeLocationS3Response' {} a -> s {s3StorageClass = a} :: DescribeLocationS3Response)

-- | The URL of the Amazon S3 location that was described.
describeLocationS3Response_locationUri :: Lens.Lens' DescribeLocationS3Response (Prelude.Maybe Prelude.Text)
describeLocationS3Response_locationUri = Lens.lens (\DescribeLocationS3Response' {locationUri} -> locationUri) (\s@DescribeLocationS3Response' {} a -> s {locationUri = a} :: DescribeLocationS3Response)

-- | The time that the Amazon S3 bucket location was created.
describeLocationS3Response_creationTime :: Lens.Lens' DescribeLocationS3Response (Prelude.Maybe Prelude.UTCTime)
describeLocationS3Response_creationTime = Lens.lens (\DescribeLocationS3Response' {creationTime} -> creationTime) (\s@DescribeLocationS3Response' {} a -> s {creationTime = a} :: DescribeLocationS3Response) Prelude.. Lens.mapping Core._Time

-- | If you are using DataSync on an Amazon Web Services Outpost, the Amazon
-- Resource Name (ARNs) of the EC2 agents deployed on your Outpost. For
-- more information about launching a DataSync agent on an Amazon Web
-- Services Outpost, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/deploy-agents.html#outposts-agent Deploy your DataSync agent on Outposts>.
describeLocationS3Response_agentArns :: Lens.Lens' DescribeLocationS3Response (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeLocationS3Response_agentArns = Lens.lens (\DescribeLocationS3Response' {agentArns} -> agentArns) (\s@DescribeLocationS3Response' {} a -> s {agentArns = a} :: DescribeLocationS3Response) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeLocationS3Response_httpStatus :: Lens.Lens' DescribeLocationS3Response Prelude.Int
describeLocationS3Response_httpStatus = Lens.lens (\DescribeLocationS3Response' {httpStatus} -> httpStatus) (\s@DescribeLocationS3Response' {} a -> s {httpStatus = a} :: DescribeLocationS3Response)

instance Prelude.NFData DescribeLocationS3Response where
  rnf DescribeLocationS3Response' {..} =
    Prelude.rnf locationArn
      `Prelude.seq` Prelude.rnf s3Config
      `Prelude.seq` Prelude.rnf s3StorageClass
      `Prelude.seq` Prelude.rnf locationUri
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf agentArns
      `Prelude.seq` Prelude.rnf httpStatus
