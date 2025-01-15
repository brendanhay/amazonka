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
-- Module      : Amazonka.DataSync.CreateLocationS3
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an endpoint for an Amazon S3 bucket that DataSync can access for
-- a transfer.
--
-- For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-locations-cli.html#create-location-s3-cli Create an Amazon S3 location>
-- in the /DataSync User Guide/.
module Amazonka.DataSync.CreateLocationS3
  ( -- * Creating a Request
    CreateLocationS3 (..),
    newCreateLocationS3,

    -- * Request Lenses
    createLocationS3_agentArns,
    createLocationS3_s3StorageClass,
    createLocationS3_subdirectory,
    createLocationS3_tags,
    createLocationS3_s3BucketArn,
    createLocationS3_s3Config,

    -- * Destructuring the Response
    CreateLocationS3Response (..),
    newCreateLocationS3Response,

    -- * Response Lenses
    createLocationS3Response_locationArn,
    createLocationS3Response_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | CreateLocationS3Request
--
-- /See:/ 'newCreateLocationS3' smart constructor.
data CreateLocationS3 = CreateLocationS3'
  { -- | If you\'re using DataSync on an Amazon Web Services Outpost, specify the
    -- Amazon Resource Names (ARNs) of the DataSync agents deployed on your
    -- Outpost. For more information about launching a DataSync agent on an
    -- Amazon Web Services Outpost, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/deploy-agents.html#outposts-agent Deploy your DataSync agent on Outposts>.
    agentArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The Amazon S3 storage class that you want to store your files in when
    -- this location is used as a task destination. For buckets in Amazon Web
    -- Services Regions, the storage class defaults to Standard. For buckets on
    -- Outposts, the storage class defaults to Amazon Web Services S3 Outposts.
    --
    -- For more information about S3 storage classes, see
    -- <http://aws.amazon.com/s3/storage-classes/ Amazon S3 Storage Classes>.
    -- Some storage classes have behaviors that can affect your S3 storage
    -- cost. For detailed information, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/create-s3-location.html#using-storage-classes Considerations when working with S3 storage classes in DataSync>.
    s3StorageClass :: Prelude.Maybe S3StorageClass,
    -- | A subdirectory in the Amazon S3 bucket. This subdirectory in Amazon S3
    -- is used to read data from the S3 source location or write data to the S3
    -- destination.
    subdirectory :: Prelude.Maybe Prelude.Text,
    -- | The key-value pair that represents the tag that you want to add to the
    -- location. The value can be an empty string. We recommend using tags to
    -- name your resources.
    tags :: Prelude.Maybe [TagListEntry],
    -- | The ARN of the Amazon S3 bucket. If the bucket is on an Amazon Web
    -- Services Outpost, this must be an access point ARN.
    s3BucketArn :: Prelude.Text,
    s3Config :: S3Config
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLocationS3' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentArns', 'createLocationS3_agentArns' - If you\'re using DataSync on an Amazon Web Services Outpost, specify the
-- Amazon Resource Names (ARNs) of the DataSync agents deployed on your
-- Outpost. For more information about launching a DataSync agent on an
-- Amazon Web Services Outpost, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/deploy-agents.html#outposts-agent Deploy your DataSync agent on Outposts>.
--
-- 's3StorageClass', 'createLocationS3_s3StorageClass' - The Amazon S3 storage class that you want to store your files in when
-- this location is used as a task destination. For buckets in Amazon Web
-- Services Regions, the storage class defaults to Standard. For buckets on
-- Outposts, the storage class defaults to Amazon Web Services S3 Outposts.
--
-- For more information about S3 storage classes, see
-- <http://aws.amazon.com/s3/storage-classes/ Amazon S3 Storage Classes>.
-- Some storage classes have behaviors that can affect your S3 storage
-- cost. For detailed information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-s3-location.html#using-storage-classes Considerations when working with S3 storage classes in DataSync>.
--
-- 'subdirectory', 'createLocationS3_subdirectory' - A subdirectory in the Amazon S3 bucket. This subdirectory in Amazon S3
-- is used to read data from the S3 source location or write data to the S3
-- destination.
--
-- 'tags', 'createLocationS3_tags' - The key-value pair that represents the tag that you want to add to the
-- location. The value can be an empty string. We recommend using tags to
-- name your resources.
--
-- 's3BucketArn', 'createLocationS3_s3BucketArn' - The ARN of the Amazon S3 bucket. If the bucket is on an Amazon Web
-- Services Outpost, this must be an access point ARN.
--
-- 's3Config', 'createLocationS3_s3Config' - Undocumented member.
newCreateLocationS3 ::
  -- | 's3BucketArn'
  Prelude.Text ->
  -- | 's3Config'
  S3Config ->
  CreateLocationS3
newCreateLocationS3 pS3BucketArn_ pS3Config_ =
  CreateLocationS3'
    { agentArns = Prelude.Nothing,
      s3StorageClass = Prelude.Nothing,
      subdirectory = Prelude.Nothing,
      tags = Prelude.Nothing,
      s3BucketArn = pS3BucketArn_,
      s3Config = pS3Config_
    }

-- | If you\'re using DataSync on an Amazon Web Services Outpost, specify the
-- Amazon Resource Names (ARNs) of the DataSync agents deployed on your
-- Outpost. For more information about launching a DataSync agent on an
-- Amazon Web Services Outpost, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/deploy-agents.html#outposts-agent Deploy your DataSync agent on Outposts>.
createLocationS3_agentArns :: Lens.Lens' CreateLocationS3 (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createLocationS3_agentArns = Lens.lens (\CreateLocationS3' {agentArns} -> agentArns) (\s@CreateLocationS3' {} a -> s {agentArns = a} :: CreateLocationS3) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon S3 storage class that you want to store your files in when
-- this location is used as a task destination. For buckets in Amazon Web
-- Services Regions, the storage class defaults to Standard. For buckets on
-- Outposts, the storage class defaults to Amazon Web Services S3 Outposts.
--
-- For more information about S3 storage classes, see
-- <http://aws.amazon.com/s3/storage-classes/ Amazon S3 Storage Classes>.
-- Some storage classes have behaviors that can affect your S3 storage
-- cost. For detailed information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-s3-location.html#using-storage-classes Considerations when working with S3 storage classes in DataSync>.
createLocationS3_s3StorageClass :: Lens.Lens' CreateLocationS3 (Prelude.Maybe S3StorageClass)
createLocationS3_s3StorageClass = Lens.lens (\CreateLocationS3' {s3StorageClass} -> s3StorageClass) (\s@CreateLocationS3' {} a -> s {s3StorageClass = a} :: CreateLocationS3)

-- | A subdirectory in the Amazon S3 bucket. This subdirectory in Amazon S3
-- is used to read data from the S3 source location or write data to the S3
-- destination.
createLocationS3_subdirectory :: Lens.Lens' CreateLocationS3 (Prelude.Maybe Prelude.Text)
createLocationS3_subdirectory = Lens.lens (\CreateLocationS3' {subdirectory} -> subdirectory) (\s@CreateLocationS3' {} a -> s {subdirectory = a} :: CreateLocationS3)

-- | The key-value pair that represents the tag that you want to add to the
-- location. The value can be an empty string. We recommend using tags to
-- name your resources.
createLocationS3_tags :: Lens.Lens' CreateLocationS3 (Prelude.Maybe [TagListEntry])
createLocationS3_tags = Lens.lens (\CreateLocationS3' {tags} -> tags) (\s@CreateLocationS3' {} a -> s {tags = a} :: CreateLocationS3) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the Amazon S3 bucket. If the bucket is on an Amazon Web
-- Services Outpost, this must be an access point ARN.
createLocationS3_s3BucketArn :: Lens.Lens' CreateLocationS3 Prelude.Text
createLocationS3_s3BucketArn = Lens.lens (\CreateLocationS3' {s3BucketArn} -> s3BucketArn) (\s@CreateLocationS3' {} a -> s {s3BucketArn = a} :: CreateLocationS3)

-- | Undocumented member.
createLocationS3_s3Config :: Lens.Lens' CreateLocationS3 S3Config
createLocationS3_s3Config = Lens.lens (\CreateLocationS3' {s3Config} -> s3Config) (\s@CreateLocationS3' {} a -> s {s3Config = a} :: CreateLocationS3)

instance Core.AWSRequest CreateLocationS3 where
  type
    AWSResponse CreateLocationS3 =
      CreateLocationS3Response
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLocationS3Response'
            Prelude.<$> (x Data..?> "LocationArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLocationS3 where
  hashWithSalt _salt CreateLocationS3' {..} =
    _salt
      `Prelude.hashWithSalt` agentArns
      `Prelude.hashWithSalt` s3StorageClass
      `Prelude.hashWithSalt` subdirectory
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` s3BucketArn
      `Prelude.hashWithSalt` s3Config

instance Prelude.NFData CreateLocationS3 where
  rnf CreateLocationS3' {..} =
    Prelude.rnf agentArns `Prelude.seq`
      Prelude.rnf s3StorageClass `Prelude.seq`
        Prelude.rnf subdirectory `Prelude.seq`
          Prelude.rnf tags `Prelude.seq`
            Prelude.rnf s3BucketArn `Prelude.seq`
              Prelude.rnf s3Config

instance Data.ToHeaders CreateLocationS3 where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "FmrsService.CreateLocationS3" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateLocationS3 where
  toJSON CreateLocationS3' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AgentArns" Data..=) Prelude.<$> agentArns,
            ("S3StorageClass" Data..=)
              Prelude.<$> s3StorageClass,
            ("Subdirectory" Data..=) Prelude.<$> subdirectory,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("S3BucketArn" Data..= s3BucketArn),
            Prelude.Just ("S3Config" Data..= s3Config)
          ]
      )

instance Data.ToPath CreateLocationS3 where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateLocationS3 where
  toQuery = Prelude.const Prelude.mempty

-- | CreateLocationS3Response
--
-- /See:/ 'newCreateLocationS3Response' smart constructor.
data CreateLocationS3Response = CreateLocationS3Response'
  { -- | The Amazon Resource Name (ARN) of the source Amazon S3 bucket location
    -- that is created.
    locationArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLocationS3Response' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locationArn', 'createLocationS3Response_locationArn' - The Amazon Resource Name (ARN) of the source Amazon S3 bucket location
-- that is created.
--
-- 'httpStatus', 'createLocationS3Response_httpStatus' - The response's http status code.
newCreateLocationS3Response ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLocationS3Response
newCreateLocationS3Response pHttpStatus_ =
  CreateLocationS3Response'
    { locationArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the source Amazon S3 bucket location
-- that is created.
createLocationS3Response_locationArn :: Lens.Lens' CreateLocationS3Response (Prelude.Maybe Prelude.Text)
createLocationS3Response_locationArn = Lens.lens (\CreateLocationS3Response' {locationArn} -> locationArn) (\s@CreateLocationS3Response' {} a -> s {locationArn = a} :: CreateLocationS3Response)

-- | The response's http status code.
createLocationS3Response_httpStatus :: Lens.Lens' CreateLocationS3Response Prelude.Int
createLocationS3Response_httpStatus = Lens.lens (\CreateLocationS3Response' {httpStatus} -> httpStatus) (\s@CreateLocationS3Response' {} a -> s {httpStatus = a} :: CreateLocationS3Response)

instance Prelude.NFData CreateLocationS3Response where
  rnf CreateLocationS3Response' {..} =
    Prelude.rnf locationArn `Prelude.seq`
      Prelude.rnf httpStatus
