{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Pinpoint.Types.SegmentImportResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.SegmentImportResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.DefinitionFormat
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the import job that created a segment. An
-- import job is a job that creates a user segment by importing endpoint
-- definitions.
--
-- /See:/ 'newSegmentImportResource' smart constructor.
data SegmentImportResource = SegmentImportResource'
  { -- | The number of channel types in the endpoint definitions that were
    -- imported to create the segment.
    channelCounts :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Int),
    -- | The format of the files that were imported to create the segment. Valid
    -- values are: CSV, for comma-separated values format; and, JSON, for
    -- newline-delimited JSON format.
    format :: DefinitionFormat,
    -- | The URL of the Amazon Simple Storage Service (Amazon S3) bucket that the
    -- endpoint definitions were imported from to create the segment.
    s3Url :: Prelude.Text,
    -- | The number of endpoint definitions that were imported successfully to
    -- create the segment.
    size :: Prelude.Int,
    -- | (Deprecated) Your AWS account ID, which you assigned to an external ID
    -- key in an IAM trust policy. Amazon Pinpoint previously used this value
    -- to assume an IAM role when importing endpoint definitions, but we
    -- removed this requirement. We don\'t recommend use of external IDs for
    -- IAM roles that are assumed by Amazon Pinpoint.
    externalId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
    -- (IAM) role that authorized Amazon Pinpoint to access the Amazon S3
    -- location to import endpoint definitions from.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SegmentImportResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelCounts', 'segmentImportResource_channelCounts' - The number of channel types in the endpoint definitions that were
-- imported to create the segment.
--
-- 'format', 'segmentImportResource_format' - The format of the files that were imported to create the segment. Valid
-- values are: CSV, for comma-separated values format; and, JSON, for
-- newline-delimited JSON format.
--
-- 's3Url', 'segmentImportResource_s3Url' - The URL of the Amazon Simple Storage Service (Amazon S3) bucket that the
-- endpoint definitions were imported from to create the segment.
--
-- 'size', 'segmentImportResource_size' - The number of endpoint definitions that were imported successfully to
-- create the segment.
--
-- 'externalId', 'segmentImportResource_externalId' - (Deprecated) Your AWS account ID, which you assigned to an external ID
-- key in an IAM trust policy. Amazon Pinpoint previously used this value
-- to assume an IAM role when importing endpoint definitions, but we
-- removed this requirement. We don\'t recommend use of external IDs for
-- IAM roles that are assumed by Amazon Pinpoint.
--
-- 'roleArn', 'segmentImportResource_roleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that authorized Amazon Pinpoint to access the Amazon S3
-- location to import endpoint definitions from.
newSegmentImportResource ::
  -- | 'format'
  DefinitionFormat ->
  -- | 's3Url'
  Prelude.Text ->
  -- | 'size'
  Prelude.Int ->
  -- | 'externalId'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  SegmentImportResource
newSegmentImportResource
  pFormat_
  pS3Url_
  pSize_
  pExternalId_
  pRoleArn_ =
    SegmentImportResource'
      { channelCounts =
          Prelude.Nothing,
        format = pFormat_,
        s3Url = pS3Url_,
        size = pSize_,
        externalId = pExternalId_,
        roleArn = pRoleArn_
      }

-- | The number of channel types in the endpoint definitions that were
-- imported to create the segment.
segmentImportResource_channelCounts :: Lens.Lens' SegmentImportResource (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Int))
segmentImportResource_channelCounts = Lens.lens (\SegmentImportResource' {channelCounts} -> channelCounts) (\s@SegmentImportResource' {} a -> s {channelCounts = a} :: SegmentImportResource) Prelude.. Lens.mapping Lens.coerced

-- | The format of the files that were imported to create the segment. Valid
-- values are: CSV, for comma-separated values format; and, JSON, for
-- newline-delimited JSON format.
segmentImportResource_format :: Lens.Lens' SegmentImportResource DefinitionFormat
segmentImportResource_format = Lens.lens (\SegmentImportResource' {format} -> format) (\s@SegmentImportResource' {} a -> s {format = a} :: SegmentImportResource)

-- | The URL of the Amazon Simple Storage Service (Amazon S3) bucket that the
-- endpoint definitions were imported from to create the segment.
segmentImportResource_s3Url :: Lens.Lens' SegmentImportResource Prelude.Text
segmentImportResource_s3Url = Lens.lens (\SegmentImportResource' {s3Url} -> s3Url) (\s@SegmentImportResource' {} a -> s {s3Url = a} :: SegmentImportResource)

-- | The number of endpoint definitions that were imported successfully to
-- create the segment.
segmentImportResource_size :: Lens.Lens' SegmentImportResource Prelude.Int
segmentImportResource_size = Lens.lens (\SegmentImportResource' {size} -> size) (\s@SegmentImportResource' {} a -> s {size = a} :: SegmentImportResource)

-- | (Deprecated) Your AWS account ID, which you assigned to an external ID
-- key in an IAM trust policy. Amazon Pinpoint previously used this value
-- to assume an IAM role when importing endpoint definitions, but we
-- removed this requirement. We don\'t recommend use of external IDs for
-- IAM roles that are assumed by Amazon Pinpoint.
segmentImportResource_externalId :: Lens.Lens' SegmentImportResource Prelude.Text
segmentImportResource_externalId = Lens.lens (\SegmentImportResource' {externalId} -> externalId) (\s@SegmentImportResource' {} a -> s {externalId = a} :: SegmentImportResource)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that authorized Amazon Pinpoint to access the Amazon S3
-- location to import endpoint definitions from.
segmentImportResource_roleArn :: Lens.Lens' SegmentImportResource Prelude.Text
segmentImportResource_roleArn = Lens.lens (\SegmentImportResource' {roleArn} -> roleArn) (\s@SegmentImportResource' {} a -> s {roleArn = a} :: SegmentImportResource)

instance Data.FromJSON SegmentImportResource where
  parseJSON =
    Data.withObject
      "SegmentImportResource"
      ( \x ->
          SegmentImportResource'
            Prelude.<$> (x Data..:? "ChannelCounts" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Format")
            Prelude.<*> (x Data..: "S3Url")
            Prelude.<*> (x Data..: "Size")
            Prelude.<*> (x Data..: "ExternalId")
            Prelude.<*> (x Data..: "RoleArn")
      )

instance Prelude.Hashable SegmentImportResource where
  hashWithSalt _salt SegmentImportResource' {..} =
    _salt
      `Prelude.hashWithSalt` channelCounts
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` s3Url
      `Prelude.hashWithSalt` size
      `Prelude.hashWithSalt` externalId
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData SegmentImportResource where
  rnf SegmentImportResource' {..} =
    Prelude.rnf channelCounts `Prelude.seq`
      Prelude.rnf format `Prelude.seq`
        Prelude.rnf s3Url `Prelude.seq`
          Prelude.rnf size `Prelude.seq`
            Prelude.rnf externalId `Prelude.seq`
              Prelude.rnf roleArn
