{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ImportJobRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ImportJobRequest
  ( ImportJobRequest (..),

    -- * Smart constructor
    mkImportJobRequest,

    -- * Lenses
    iFormat,
    iS3Url,
    iRoleArn,
    iDefineSegment,
    iExternalId,
    iRegisterEndpoints,
    iSegmentId,
    iSegmentName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.DefinitionFormat as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the settings for a job that imports endpoint definitions from an Amazon Simple Storage Service (Amazon S3) bucket.
--
-- /See:/ 'mkImportJobRequest' smart constructor.
data ImportJobRequest = ImportJobRequest'
  { -- | The format of the files that contain the endpoint definitions to import. Valid values are: CSV, for comma-separated values format; and, JSON, for newline-delimited JSON format. If the Amazon S3 location stores multiple files that use different formats, Amazon Pinpoint imports data only from the files that use the specified format.
    format :: Types.DefinitionFormat,
    -- | The URL of the Amazon Simple Storage Service (Amazon S3) bucket that contains the endpoint definitions to import. This location can be a folder or a single file. If the location is a folder, Amazon Pinpoint imports endpoint definitions from the files in this location, including any subfolders that the folder contains.
    --
    -- The URL should be in the following format: s3://<replaceable>bucket-name/<replaceable>folder-name/<replaceable>file-name. The location can end with the key for an individual object or a prefix that qualifies multiple objects.
    s3Url :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to access the Amazon S3 location to import endpoint definitions from.
    roleArn :: Core.Text,
    -- | Specifies whether to create a segment that contains the endpoints, when the endpoint definitions are imported.
    defineSegment :: Core.Maybe Core.Bool,
    -- | (Deprecated) Your AWS account ID, which you assigned to an external ID key in an IAM trust policy. Amazon Pinpoint previously used this value to assume an IAM role when importing endpoint definitions, but we removed this requirement. We don't recommend use of external IDs for IAM roles that are assumed by Amazon Pinpoint.
    externalId :: Core.Maybe Core.Text,
    -- | Specifies whether to register the endpoints with Amazon Pinpoint, when the endpoint definitions are imported.
    registerEndpoints :: Core.Maybe Core.Bool,
    -- | The identifier for the segment to update or add the imported endpoint definitions to, if the import job is meant to update an existing segment.
    segmentId :: Core.Maybe Core.Text,
    -- | A custom name for the segment that's created by the import job, if the value of the DefineSegment property is true.
    segmentName :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportJobRequest' value with any optional fields omitted.
mkImportJobRequest ::
  -- | 'format'
  Types.DefinitionFormat ->
  -- | 's3Url'
  Core.Text ->
  -- | 'roleArn'
  Core.Text ->
  ImportJobRequest
mkImportJobRequest format s3Url roleArn =
  ImportJobRequest'
    { format,
      s3Url,
      roleArn,
      defineSegment = Core.Nothing,
      externalId = Core.Nothing,
      registerEndpoints = Core.Nothing,
      segmentId = Core.Nothing,
      segmentName = Core.Nothing
    }

-- | The format of the files that contain the endpoint definitions to import. Valid values are: CSV, for comma-separated values format; and, JSON, for newline-delimited JSON format. If the Amazon S3 location stores multiple files that use different formats, Amazon Pinpoint imports data only from the files that use the specified format.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iFormat :: Lens.Lens' ImportJobRequest Types.DefinitionFormat
iFormat = Lens.field @"format"
{-# DEPRECATED iFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | The URL of the Amazon Simple Storage Service (Amazon S3) bucket that contains the endpoint definitions to import. This location can be a folder or a single file. If the location is a folder, Amazon Pinpoint imports endpoint definitions from the files in this location, including any subfolders that the folder contains.
--
-- The URL should be in the following format: s3://<replaceable>bucket-name/<replaceable>folder-name/<replaceable>file-name. The location can end with the key for an individual object or a prefix that qualifies multiple objects.
--
-- /Note:/ Consider using 's3Url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iS3Url :: Lens.Lens' ImportJobRequest Core.Text
iS3Url = Lens.field @"s3Url"
{-# DEPRECATED iS3Url "Use generic-lens or generic-optics with 's3Url' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to access the Amazon S3 location to import endpoint definitions from.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iRoleArn :: Lens.Lens' ImportJobRequest Core.Text
iRoleArn = Lens.field @"roleArn"
{-# DEPRECATED iRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | Specifies whether to create a segment that contains the endpoints, when the endpoint definitions are imported.
--
-- /Note:/ Consider using 'defineSegment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iDefineSegment :: Lens.Lens' ImportJobRequest (Core.Maybe Core.Bool)
iDefineSegment = Lens.field @"defineSegment"
{-# DEPRECATED iDefineSegment "Use generic-lens or generic-optics with 'defineSegment' instead." #-}

-- | (Deprecated) Your AWS account ID, which you assigned to an external ID key in an IAM trust policy. Amazon Pinpoint previously used this value to assume an IAM role when importing endpoint definitions, but we removed this requirement. We don't recommend use of external IDs for IAM roles that are assumed by Amazon Pinpoint.
--
-- /Note:/ Consider using 'externalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iExternalId :: Lens.Lens' ImportJobRequest (Core.Maybe Core.Text)
iExternalId = Lens.field @"externalId"
{-# DEPRECATED iExternalId "Use generic-lens or generic-optics with 'externalId' instead." #-}

-- | Specifies whether to register the endpoints with Amazon Pinpoint, when the endpoint definitions are imported.
--
-- /Note:/ Consider using 'registerEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iRegisterEndpoints :: Lens.Lens' ImportJobRequest (Core.Maybe Core.Bool)
iRegisterEndpoints = Lens.field @"registerEndpoints"
{-# DEPRECATED iRegisterEndpoints "Use generic-lens or generic-optics with 'registerEndpoints' instead." #-}

-- | The identifier for the segment to update or add the imported endpoint definitions to, if the import job is meant to update an existing segment.
--
-- /Note:/ Consider using 'segmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSegmentId :: Lens.Lens' ImportJobRequest (Core.Maybe Core.Text)
iSegmentId = Lens.field @"segmentId"
{-# DEPRECATED iSegmentId "Use generic-lens or generic-optics with 'segmentId' instead." #-}

-- | A custom name for the segment that's created by the import job, if the value of the DefineSegment property is true.
--
-- /Note:/ Consider using 'segmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSegmentName :: Lens.Lens' ImportJobRequest (Core.Maybe Core.Text)
iSegmentName = Lens.field @"segmentName"
{-# DEPRECATED iSegmentName "Use generic-lens or generic-optics with 'segmentName' instead." #-}

instance Core.FromJSON ImportJobRequest where
  toJSON ImportJobRequest {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Format" Core..= format),
            Core.Just ("S3Url" Core..= s3Url),
            Core.Just ("RoleArn" Core..= roleArn),
            ("DefineSegment" Core..=) Core.<$> defineSegment,
            ("ExternalId" Core..=) Core.<$> externalId,
            ("RegisterEndpoints" Core..=) Core.<$> registerEndpoints,
            ("SegmentId" Core..=) Core.<$> segmentId,
            ("SegmentName" Core..=) Core.<$> segmentName
          ]
      )
