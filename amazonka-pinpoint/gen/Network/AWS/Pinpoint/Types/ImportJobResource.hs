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
-- Module      : Network.AWS.Pinpoint.Types.ImportJobResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ImportJobResource where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.DefinitionFormat

-- | Provides information about the resource settings for a job that imports
-- endpoint definitions from one or more files. The files can be stored in
-- an Amazon Simple Storage Service (Amazon S3) bucket or uploaded directly
-- from a computer by using the Amazon Pinpoint console.
--
-- /See:/ 'newImportJobResource' smart constructor.
data ImportJobResource = ImportJobResource'
  { -- | Specifies whether the import job creates a segment that contains the
    -- endpoints, when the endpoint definitions are imported.
    defineSegment :: Core.Maybe Core.Bool,
    -- | The custom name for the segment that\'s created by the import job, if
    -- the value of the DefineSegment property is true.
    segmentName :: Core.Maybe Core.Text,
    -- | Specifies whether the import job registers the endpoints with Amazon
    -- Pinpoint, when the endpoint definitions are imported.
    registerEndpoints :: Core.Maybe Core.Bool,
    -- | The identifier for the segment that the import job updates or adds
    -- endpoint definitions to, if the import job updates an existing segment.
    segmentId :: Core.Maybe Core.Text,
    -- | (Deprecated) Your AWS account ID, which you assigned to an external ID
    -- key in an IAM trust policy. Amazon Pinpoint previously used this value
    -- to assume an IAM role when importing endpoint definitions, but we
    -- removed this requirement. We don\'t recommend use of external IDs for
    -- IAM roles that are assumed by Amazon Pinpoint.
    externalId :: Core.Maybe Core.Text,
    -- | The format of the files that contain the endpoint definitions to import.
    -- Valid values are: CSV, for comma-separated values format; and, JSON, for
    -- newline-delimited JSON format.
    --
    -- If the files are stored in an Amazon S3 location and that location
    -- contains multiple files that use different formats, Amazon Pinpoint
    -- imports data only from the files that use the specified format.
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
    s3Url :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
    -- (IAM) role that authorizes Amazon Pinpoint to access the Amazon S3
    -- location to import endpoint definitions from.
    roleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ImportJobResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defineSegment', 'importJobResource_defineSegment' - Specifies whether the import job creates a segment that contains the
-- endpoints, when the endpoint definitions are imported.
--
-- 'segmentName', 'importJobResource_segmentName' - The custom name for the segment that\'s created by the import job, if
-- the value of the DefineSegment property is true.
--
-- 'registerEndpoints', 'importJobResource_registerEndpoints' - Specifies whether the import job registers the endpoints with Amazon
-- Pinpoint, when the endpoint definitions are imported.
--
-- 'segmentId', 'importJobResource_segmentId' - The identifier for the segment that the import job updates or adds
-- endpoint definitions to, if the import job updates an existing segment.
--
-- 'externalId', 'importJobResource_externalId' - (Deprecated) Your AWS account ID, which you assigned to an external ID
-- key in an IAM trust policy. Amazon Pinpoint previously used this value
-- to assume an IAM role when importing endpoint definitions, but we
-- removed this requirement. We don\'t recommend use of external IDs for
-- IAM roles that are assumed by Amazon Pinpoint.
--
-- 'format', 'importJobResource_format' - The format of the files that contain the endpoint definitions to import.
-- Valid values are: CSV, for comma-separated values format; and, JSON, for
-- newline-delimited JSON format.
--
-- If the files are stored in an Amazon S3 location and that location
-- contains multiple files that use different formats, Amazon Pinpoint
-- imports data only from the files that use the specified format.
--
-- 's3Url', 'importJobResource_s3Url' - The URL of the Amazon Simple Storage Service (Amazon S3) bucket that
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
-- 'roleArn', 'importJobResource_roleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that authorizes Amazon Pinpoint to access the Amazon S3
-- location to import endpoint definitions from.
newImportJobResource ::
  -- | 'format'
  DefinitionFormat ->
  -- | 's3Url'
  Core.Text ->
  -- | 'roleArn'
  Core.Text ->
  ImportJobResource
newImportJobResource pFormat_ pS3Url_ pRoleArn_ =
  ImportJobResource'
    { defineSegment = Core.Nothing,
      segmentName = Core.Nothing,
      registerEndpoints = Core.Nothing,
      segmentId = Core.Nothing,
      externalId = Core.Nothing,
      format = pFormat_,
      s3Url = pS3Url_,
      roleArn = pRoleArn_
    }

-- | Specifies whether the import job creates a segment that contains the
-- endpoints, when the endpoint definitions are imported.
importJobResource_defineSegment :: Lens.Lens' ImportJobResource (Core.Maybe Core.Bool)
importJobResource_defineSegment = Lens.lens (\ImportJobResource' {defineSegment} -> defineSegment) (\s@ImportJobResource' {} a -> s {defineSegment = a} :: ImportJobResource)

-- | The custom name for the segment that\'s created by the import job, if
-- the value of the DefineSegment property is true.
importJobResource_segmentName :: Lens.Lens' ImportJobResource (Core.Maybe Core.Text)
importJobResource_segmentName = Lens.lens (\ImportJobResource' {segmentName} -> segmentName) (\s@ImportJobResource' {} a -> s {segmentName = a} :: ImportJobResource)

-- | Specifies whether the import job registers the endpoints with Amazon
-- Pinpoint, when the endpoint definitions are imported.
importJobResource_registerEndpoints :: Lens.Lens' ImportJobResource (Core.Maybe Core.Bool)
importJobResource_registerEndpoints = Lens.lens (\ImportJobResource' {registerEndpoints} -> registerEndpoints) (\s@ImportJobResource' {} a -> s {registerEndpoints = a} :: ImportJobResource)

-- | The identifier for the segment that the import job updates or adds
-- endpoint definitions to, if the import job updates an existing segment.
importJobResource_segmentId :: Lens.Lens' ImportJobResource (Core.Maybe Core.Text)
importJobResource_segmentId = Lens.lens (\ImportJobResource' {segmentId} -> segmentId) (\s@ImportJobResource' {} a -> s {segmentId = a} :: ImportJobResource)

-- | (Deprecated) Your AWS account ID, which you assigned to an external ID
-- key in an IAM trust policy. Amazon Pinpoint previously used this value
-- to assume an IAM role when importing endpoint definitions, but we
-- removed this requirement. We don\'t recommend use of external IDs for
-- IAM roles that are assumed by Amazon Pinpoint.
importJobResource_externalId :: Lens.Lens' ImportJobResource (Core.Maybe Core.Text)
importJobResource_externalId = Lens.lens (\ImportJobResource' {externalId} -> externalId) (\s@ImportJobResource' {} a -> s {externalId = a} :: ImportJobResource)

-- | The format of the files that contain the endpoint definitions to import.
-- Valid values are: CSV, for comma-separated values format; and, JSON, for
-- newline-delimited JSON format.
--
-- If the files are stored in an Amazon S3 location and that location
-- contains multiple files that use different formats, Amazon Pinpoint
-- imports data only from the files that use the specified format.
importJobResource_format :: Lens.Lens' ImportJobResource DefinitionFormat
importJobResource_format = Lens.lens (\ImportJobResource' {format} -> format) (\s@ImportJobResource' {} a -> s {format = a} :: ImportJobResource)

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
importJobResource_s3Url :: Lens.Lens' ImportJobResource Core.Text
importJobResource_s3Url = Lens.lens (\ImportJobResource' {s3Url} -> s3Url) (\s@ImportJobResource' {} a -> s {s3Url = a} :: ImportJobResource)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that authorizes Amazon Pinpoint to access the Amazon S3
-- location to import endpoint definitions from.
importJobResource_roleArn :: Lens.Lens' ImportJobResource Core.Text
importJobResource_roleArn = Lens.lens (\ImportJobResource' {roleArn} -> roleArn) (\s@ImportJobResource' {} a -> s {roleArn = a} :: ImportJobResource)

instance Core.FromJSON ImportJobResource where
  parseJSON =
    Core.withObject
      "ImportJobResource"
      ( \x ->
          ImportJobResource'
            Core.<$> (x Core..:? "DefineSegment")
            Core.<*> (x Core..:? "SegmentName")
            Core.<*> (x Core..:? "RegisterEndpoints")
            Core.<*> (x Core..:? "SegmentId")
            Core.<*> (x Core..:? "ExternalId")
            Core.<*> (x Core..: "Format")
            Core.<*> (x Core..: "S3Url")
            Core.<*> (x Core..: "RoleArn")
      )

instance Core.Hashable ImportJobResource

instance Core.NFData ImportJobResource
