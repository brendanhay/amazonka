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
-- Module      : Amazonka.Pinpoint.Types.ImportJobRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.ImportJobRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.DefinitionFormat
import qualified Amazonka.Prelude as Prelude

-- | Specifies the settings for a job that imports endpoint definitions from
-- an Amazon Simple Storage Service (Amazon S3) bucket.
--
-- /See:/ 'newImportJobRequest' smart constructor.
data ImportJobRequest = ImportJobRequest'
  { -- | Specifies whether to create a segment that contains the endpoints, when
    -- the endpoint definitions are imported.
    defineSegment :: Prelude.Maybe Prelude.Bool,
    -- | (Deprecated) Your AWS account ID, which you assigned to an external ID
    -- key in an IAM trust policy. Amazon Pinpoint previously used this value
    -- to assume an IAM role when importing endpoint definitions, but we
    -- removed this requirement. We don\'t recommend use of external IDs for
    -- IAM roles that are assumed by Amazon Pinpoint.
    externalId :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether to register the endpoints with Amazon Pinpoint, when
    -- the endpoint definitions are imported.
    registerEndpoints :: Prelude.Maybe Prelude.Bool,
    -- | The identifier for the segment to update or add the imported endpoint
    -- definitions to, if the import job is meant to update an existing
    -- segment.
    segmentId :: Prelude.Maybe Prelude.Text,
    -- | A custom name for the segment that\'s created by the import job, if the
    -- value of the DefineSegment property is true.
    segmentName :: Prelude.Maybe Prelude.Text,
    -- | The format of the files that contain the endpoint definitions to import.
    -- Valid values are: CSV, for comma-separated values format; and, JSON, for
    -- newline-delimited JSON format. If the Amazon S3 location stores multiple
    -- files that use different formats, Amazon Pinpoint imports data only from
    -- the files that use the specified format.
    format :: DefinitionFormat,
    -- | The URL of the Amazon Simple Storage Service (Amazon S3) bucket that
    -- contains the endpoint definitions to import. This location can be a
    -- folder or a single file. If the location is a folder, Amazon Pinpoint
    -- imports endpoint definitions from the files in this location, including
    -- any subfolders that the folder contains.
    --
    -- The URL should be in the following format:
    -- s3:\/\/bucket-name\/folder-name\/file-name. The location can end with
    -- the key for an individual object or a prefix that qualifies multiple
    -- objects.
    s3Url :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
    -- (IAM) role that authorizes Amazon Pinpoint to access the Amazon S3
    -- location to import endpoint definitions from.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportJobRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defineSegment', 'importJobRequest_defineSegment' - Specifies whether to create a segment that contains the endpoints, when
-- the endpoint definitions are imported.
--
-- 'externalId', 'importJobRequest_externalId' - (Deprecated) Your AWS account ID, which you assigned to an external ID
-- key in an IAM trust policy. Amazon Pinpoint previously used this value
-- to assume an IAM role when importing endpoint definitions, but we
-- removed this requirement. We don\'t recommend use of external IDs for
-- IAM roles that are assumed by Amazon Pinpoint.
--
-- 'registerEndpoints', 'importJobRequest_registerEndpoints' - Specifies whether to register the endpoints with Amazon Pinpoint, when
-- the endpoint definitions are imported.
--
-- 'segmentId', 'importJobRequest_segmentId' - The identifier for the segment to update or add the imported endpoint
-- definitions to, if the import job is meant to update an existing
-- segment.
--
-- 'segmentName', 'importJobRequest_segmentName' - A custom name for the segment that\'s created by the import job, if the
-- value of the DefineSegment property is true.
--
-- 'format', 'importJobRequest_format' - The format of the files that contain the endpoint definitions to import.
-- Valid values are: CSV, for comma-separated values format; and, JSON, for
-- newline-delimited JSON format. If the Amazon S3 location stores multiple
-- files that use different formats, Amazon Pinpoint imports data only from
-- the files that use the specified format.
--
-- 's3Url', 'importJobRequest_s3Url' - The URL of the Amazon Simple Storage Service (Amazon S3) bucket that
-- contains the endpoint definitions to import. This location can be a
-- folder or a single file. If the location is a folder, Amazon Pinpoint
-- imports endpoint definitions from the files in this location, including
-- any subfolders that the folder contains.
--
-- The URL should be in the following format:
-- s3:\/\/bucket-name\/folder-name\/file-name. The location can end with
-- the key for an individual object or a prefix that qualifies multiple
-- objects.
--
-- 'roleArn', 'importJobRequest_roleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that authorizes Amazon Pinpoint to access the Amazon S3
-- location to import endpoint definitions from.
newImportJobRequest ::
  -- | 'format'
  DefinitionFormat ->
  -- | 's3Url'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  ImportJobRequest
newImportJobRequest pFormat_ pS3Url_ pRoleArn_ =
  ImportJobRequest'
    { defineSegment = Prelude.Nothing,
      externalId = Prelude.Nothing,
      registerEndpoints = Prelude.Nothing,
      segmentId = Prelude.Nothing,
      segmentName = Prelude.Nothing,
      format = pFormat_,
      s3Url = pS3Url_,
      roleArn = pRoleArn_
    }

-- | Specifies whether to create a segment that contains the endpoints, when
-- the endpoint definitions are imported.
importJobRequest_defineSegment :: Lens.Lens' ImportJobRequest (Prelude.Maybe Prelude.Bool)
importJobRequest_defineSegment = Lens.lens (\ImportJobRequest' {defineSegment} -> defineSegment) (\s@ImportJobRequest' {} a -> s {defineSegment = a} :: ImportJobRequest)

-- | (Deprecated) Your AWS account ID, which you assigned to an external ID
-- key in an IAM trust policy. Amazon Pinpoint previously used this value
-- to assume an IAM role when importing endpoint definitions, but we
-- removed this requirement. We don\'t recommend use of external IDs for
-- IAM roles that are assumed by Amazon Pinpoint.
importJobRequest_externalId :: Lens.Lens' ImportJobRequest (Prelude.Maybe Prelude.Text)
importJobRequest_externalId = Lens.lens (\ImportJobRequest' {externalId} -> externalId) (\s@ImportJobRequest' {} a -> s {externalId = a} :: ImportJobRequest)

-- | Specifies whether to register the endpoints with Amazon Pinpoint, when
-- the endpoint definitions are imported.
importJobRequest_registerEndpoints :: Lens.Lens' ImportJobRequest (Prelude.Maybe Prelude.Bool)
importJobRequest_registerEndpoints = Lens.lens (\ImportJobRequest' {registerEndpoints} -> registerEndpoints) (\s@ImportJobRequest' {} a -> s {registerEndpoints = a} :: ImportJobRequest)

-- | The identifier for the segment to update or add the imported endpoint
-- definitions to, if the import job is meant to update an existing
-- segment.
importJobRequest_segmentId :: Lens.Lens' ImportJobRequest (Prelude.Maybe Prelude.Text)
importJobRequest_segmentId = Lens.lens (\ImportJobRequest' {segmentId} -> segmentId) (\s@ImportJobRequest' {} a -> s {segmentId = a} :: ImportJobRequest)

-- | A custom name for the segment that\'s created by the import job, if the
-- value of the DefineSegment property is true.
importJobRequest_segmentName :: Lens.Lens' ImportJobRequest (Prelude.Maybe Prelude.Text)
importJobRequest_segmentName = Lens.lens (\ImportJobRequest' {segmentName} -> segmentName) (\s@ImportJobRequest' {} a -> s {segmentName = a} :: ImportJobRequest)

-- | The format of the files that contain the endpoint definitions to import.
-- Valid values are: CSV, for comma-separated values format; and, JSON, for
-- newline-delimited JSON format. If the Amazon S3 location stores multiple
-- files that use different formats, Amazon Pinpoint imports data only from
-- the files that use the specified format.
importJobRequest_format :: Lens.Lens' ImportJobRequest DefinitionFormat
importJobRequest_format = Lens.lens (\ImportJobRequest' {format} -> format) (\s@ImportJobRequest' {} a -> s {format = a} :: ImportJobRequest)

-- | The URL of the Amazon Simple Storage Service (Amazon S3) bucket that
-- contains the endpoint definitions to import. This location can be a
-- folder or a single file. If the location is a folder, Amazon Pinpoint
-- imports endpoint definitions from the files in this location, including
-- any subfolders that the folder contains.
--
-- The URL should be in the following format:
-- s3:\/\/bucket-name\/folder-name\/file-name. The location can end with
-- the key for an individual object or a prefix that qualifies multiple
-- objects.
importJobRequest_s3Url :: Lens.Lens' ImportJobRequest Prelude.Text
importJobRequest_s3Url = Lens.lens (\ImportJobRequest' {s3Url} -> s3Url) (\s@ImportJobRequest' {} a -> s {s3Url = a} :: ImportJobRequest)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that authorizes Amazon Pinpoint to access the Amazon S3
-- location to import endpoint definitions from.
importJobRequest_roleArn :: Lens.Lens' ImportJobRequest Prelude.Text
importJobRequest_roleArn = Lens.lens (\ImportJobRequest' {roleArn} -> roleArn) (\s@ImportJobRequest' {} a -> s {roleArn = a} :: ImportJobRequest)

instance Prelude.Hashable ImportJobRequest where
  hashWithSalt _salt ImportJobRequest' {..} =
    _salt
      `Prelude.hashWithSalt` defineSegment
      `Prelude.hashWithSalt` externalId
      `Prelude.hashWithSalt` registerEndpoints
      `Prelude.hashWithSalt` segmentId
      `Prelude.hashWithSalt` segmentName
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` s3Url
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData ImportJobRequest where
  rnf ImportJobRequest' {..} =
    Prelude.rnf defineSegment
      `Prelude.seq` Prelude.rnf externalId
      `Prelude.seq` Prelude.rnf registerEndpoints
      `Prelude.seq` Prelude.rnf segmentId
      `Prelude.seq` Prelude.rnf segmentName
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf s3Url
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToJSON ImportJobRequest where
  toJSON ImportJobRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DefineSegment" Data..=) Prelude.<$> defineSegment,
            ("ExternalId" Data..=) Prelude.<$> externalId,
            ("RegisterEndpoints" Data..=)
              Prelude.<$> registerEndpoints,
            ("SegmentId" Data..=) Prelude.<$> segmentId,
            ("SegmentName" Data..=) Prelude.<$> segmentName,
            Prelude.Just ("Format" Data..= format),
            Prelude.Just ("S3Url" Data..= s3Url),
            Prelude.Just ("RoleArn" Data..= roleArn)
          ]
      )
