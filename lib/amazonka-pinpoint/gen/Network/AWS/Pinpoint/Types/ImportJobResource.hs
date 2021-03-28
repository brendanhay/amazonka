{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ImportJobResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.ImportJobResource
  ( ImportJobResource (..)
  -- * Smart constructor
  , mkImportJobResource
  -- * Lenses
  , ijrFormat
  , ijrS3Url
  , ijrRoleArn
  , ijrDefineSegment
  , ijrExternalId
  , ijrRegisterEndpoints
  , ijrSegmentId
  , ijrSegmentName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.DefinitionFormat as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about the resource settings for a job that imports endpoint definitions from one or more files. The files can be stored in an Amazon Simple Storage Service (Amazon S3) bucket or uploaded directly from a computer by using the Amazon Pinpoint console.
--
-- /See:/ 'mkImportJobResource' smart constructor.
data ImportJobResource = ImportJobResource'
  { format :: Types.DefinitionFormat
    -- ^ The format of the files that contain the endpoint definitions to import. Valid values are: CSV, for comma-separated values format; and, JSON, for newline-delimited JSON format.
--
-- If the files are stored in an Amazon S3 location and that location contains multiple files that use different formats, Amazon Pinpoint imports data only from the files that use the specified format.
  , s3Url :: Core.Text
    -- ^ The URL of the Amazon Simple Storage Service (Amazon S3) bucket that contains the endpoint definitions to import. This location can be a folder or a single file. If the location is a folder, Amazon Pinpoint imports endpoint definitions from the files in this location, including any subfolders that the folder contains.
--
-- The URL should be in the following format: s3://<replaceable>bucket-name/<replaceable>folder-name/<replaceable>file-name. The location can end with the key for an individual object or a prefix that qualifies multiple objects.
  , roleArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to access the Amazon S3 location to import endpoint definitions from.
  , defineSegment :: Core.Maybe Core.Bool
    -- ^ Specifies whether the import job creates a segment that contains the endpoints, when the endpoint definitions are imported.
  , externalId :: Core.Maybe Core.Text
    -- ^ (Deprecated) Your AWS account ID, which you assigned to an external ID key in an IAM trust policy. Amazon Pinpoint previously used this value to assume an IAM role when importing endpoint definitions, but we removed this requirement. We don't recommend use of external IDs for IAM roles that are assumed by Amazon Pinpoint.
  , registerEndpoints :: Core.Maybe Core.Bool
    -- ^ Specifies whether the import job registers the endpoints with Amazon Pinpoint, when the endpoint definitions are imported.
  , segmentId :: Core.Maybe Core.Text
    -- ^ The identifier for the segment that the import job updates or adds endpoint definitions to, if the import job updates an existing segment.
  , segmentName :: Core.Maybe Core.Text
    -- ^ The custom name for the segment that's created by the import job, if the value of the DefineSegment property is true.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportJobResource' value with any optional fields omitted.
mkImportJobResource
    :: Types.DefinitionFormat -- ^ 'format'
    -> Core.Text -- ^ 's3Url'
    -> Core.Text -- ^ 'roleArn'
    -> ImportJobResource
mkImportJobResource format s3Url roleArn
  = ImportJobResource'{format, s3Url, roleArn,
                       defineSegment = Core.Nothing, externalId = Core.Nothing,
                       registerEndpoints = Core.Nothing, segmentId = Core.Nothing,
                       segmentName = Core.Nothing}

-- | The format of the files that contain the endpoint definitions to import. Valid values are: CSV, for comma-separated values format; and, JSON, for newline-delimited JSON format.
--
-- If the files are stored in an Amazon S3 location and that location contains multiple files that use different formats, Amazon Pinpoint imports data only from the files that use the specified format.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrFormat :: Lens.Lens' ImportJobResource Types.DefinitionFormat
ijrFormat = Lens.field @"format"
{-# INLINEABLE ijrFormat #-}
{-# DEPRECATED format "Use generic-lens or generic-optics with 'format' instead"  #-}

-- | The URL of the Amazon Simple Storage Service (Amazon S3) bucket that contains the endpoint definitions to import. This location can be a folder or a single file. If the location is a folder, Amazon Pinpoint imports endpoint definitions from the files in this location, including any subfolders that the folder contains.
--
-- The URL should be in the following format: s3://<replaceable>bucket-name/<replaceable>folder-name/<replaceable>file-name. The location can end with the key for an individual object or a prefix that qualifies multiple objects.
--
-- /Note:/ Consider using 's3Url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrS3Url :: Lens.Lens' ImportJobResource Core.Text
ijrS3Url = Lens.field @"s3Url"
{-# INLINEABLE ijrS3Url #-}
{-# DEPRECATED s3Url "Use generic-lens or generic-optics with 's3Url' instead"  #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to access the Amazon S3 location to import endpoint definitions from.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrRoleArn :: Lens.Lens' ImportJobResource Core.Text
ijrRoleArn = Lens.field @"roleArn"
{-# INLINEABLE ijrRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | Specifies whether the import job creates a segment that contains the endpoints, when the endpoint definitions are imported.
--
-- /Note:/ Consider using 'defineSegment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrDefineSegment :: Lens.Lens' ImportJobResource (Core.Maybe Core.Bool)
ijrDefineSegment = Lens.field @"defineSegment"
{-# INLINEABLE ijrDefineSegment #-}
{-# DEPRECATED defineSegment "Use generic-lens or generic-optics with 'defineSegment' instead"  #-}

-- | (Deprecated) Your AWS account ID, which you assigned to an external ID key in an IAM trust policy. Amazon Pinpoint previously used this value to assume an IAM role when importing endpoint definitions, but we removed this requirement. We don't recommend use of external IDs for IAM roles that are assumed by Amazon Pinpoint.
--
-- /Note:/ Consider using 'externalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrExternalId :: Lens.Lens' ImportJobResource (Core.Maybe Core.Text)
ijrExternalId = Lens.field @"externalId"
{-# INLINEABLE ijrExternalId #-}
{-# DEPRECATED externalId "Use generic-lens or generic-optics with 'externalId' instead"  #-}

-- | Specifies whether the import job registers the endpoints with Amazon Pinpoint, when the endpoint definitions are imported.
--
-- /Note:/ Consider using 'registerEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrRegisterEndpoints :: Lens.Lens' ImportJobResource (Core.Maybe Core.Bool)
ijrRegisterEndpoints = Lens.field @"registerEndpoints"
{-# INLINEABLE ijrRegisterEndpoints #-}
{-# DEPRECATED registerEndpoints "Use generic-lens or generic-optics with 'registerEndpoints' instead"  #-}

-- | The identifier for the segment that the import job updates or adds endpoint definitions to, if the import job updates an existing segment.
--
-- /Note:/ Consider using 'segmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrSegmentId :: Lens.Lens' ImportJobResource (Core.Maybe Core.Text)
ijrSegmentId = Lens.field @"segmentId"
{-# INLINEABLE ijrSegmentId #-}
{-# DEPRECATED segmentId "Use generic-lens or generic-optics with 'segmentId' instead"  #-}

-- | The custom name for the segment that's created by the import job, if the value of the DefineSegment property is true.
--
-- /Note:/ Consider using 'segmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrSegmentName :: Lens.Lens' ImportJobResource (Core.Maybe Core.Text)
ijrSegmentName = Lens.field @"segmentName"
{-# INLINEABLE ijrSegmentName #-}
{-# DEPRECATED segmentName "Use generic-lens or generic-optics with 'segmentName' instead"  #-}

instance Core.FromJSON ImportJobResource where
        parseJSON
          = Core.withObject "ImportJobResource" Core.$
              \ x ->
                ImportJobResource' Core.<$>
                  (x Core..: "Format") Core.<*> x Core..: "S3Url" Core.<*>
                    x Core..: "RoleArn"
                    Core.<*> x Core..:? "DefineSegment"
                    Core.<*> x Core..:? "ExternalId"
                    Core.<*> x Core..:? "RegisterEndpoints"
                    Core.<*> x Core..:? "SegmentId"
                    Core.<*> x Core..:? "SegmentName"
