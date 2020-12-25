{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ExportJobResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ExportJobResource
  ( ExportJobResource (..),

    -- * Smart constructor
    mkExportJobResource,

    -- * Lenses
    ejrS3UrlPrefix,
    ejrRoleArn,
    ejrSegmentId,
    ejrSegmentVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information about the resource settings for a job that exports endpoint definitions to a file. The file can be added directly to an Amazon Simple Storage Service (Amazon S3) bucket by using the Amazon Pinpoint API or downloaded directly to a computer by using the Amazon Pinpoint console.
--
-- /See:/ 'mkExportJobResource' smart constructor.
data ExportJobResource = ExportJobResource'
  { -- | The URL of the location in an Amazon Simple Storage Service (Amazon S3) bucket where the endpoint definitions were exported to. This location is typically a folder that contains multiple files. The URL should be in the following format: s3://<replaceable>bucket-name/<replaceable>folder-name/.
    s3UrlPrefix :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorized Amazon Pinpoint to access the Amazon S3 location where the endpoint definitions were exported to.
    roleArn :: Core.Text,
    -- | The identifier for the segment that the endpoint definitions were exported from. If this value isn't present, Amazon Pinpoint exported definitions for all the endpoints that are associated with the application.
    segmentId :: Core.Maybe Core.Text,
    -- | The version of the segment that the endpoint definitions were exported from.
    segmentVersion :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExportJobResource' value with any optional fields omitted.
mkExportJobResource ::
  -- | 's3UrlPrefix'
  Core.Text ->
  -- | 'roleArn'
  Core.Text ->
  ExportJobResource
mkExportJobResource s3UrlPrefix roleArn =
  ExportJobResource'
    { s3UrlPrefix,
      roleArn,
      segmentId = Core.Nothing,
      segmentVersion = Core.Nothing
    }

-- | The URL of the location in an Amazon Simple Storage Service (Amazon S3) bucket where the endpoint definitions were exported to. This location is typically a folder that contains multiple files. The URL should be in the following format: s3://<replaceable>bucket-name/<replaceable>folder-name/.
--
-- /Note:/ Consider using 's3UrlPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrS3UrlPrefix :: Lens.Lens' ExportJobResource Core.Text
ejrS3UrlPrefix = Lens.field @"s3UrlPrefix"
{-# DEPRECATED ejrS3UrlPrefix "Use generic-lens or generic-optics with 's3UrlPrefix' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorized Amazon Pinpoint to access the Amazon S3 location where the endpoint definitions were exported to.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrRoleArn :: Lens.Lens' ExportJobResource Core.Text
ejrRoleArn = Lens.field @"roleArn"
{-# DEPRECATED ejrRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | The identifier for the segment that the endpoint definitions were exported from. If this value isn't present, Amazon Pinpoint exported definitions for all the endpoints that are associated with the application.
--
-- /Note:/ Consider using 'segmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrSegmentId :: Lens.Lens' ExportJobResource (Core.Maybe Core.Text)
ejrSegmentId = Lens.field @"segmentId"
{-# DEPRECATED ejrSegmentId "Use generic-lens or generic-optics with 'segmentId' instead." #-}

-- | The version of the segment that the endpoint definitions were exported from.
--
-- /Note:/ Consider using 'segmentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrSegmentVersion :: Lens.Lens' ExportJobResource (Core.Maybe Core.Int)
ejrSegmentVersion = Lens.field @"segmentVersion"
{-# DEPRECATED ejrSegmentVersion "Use generic-lens or generic-optics with 'segmentVersion' instead." #-}

instance Core.FromJSON ExportJobResource where
  parseJSON =
    Core.withObject "ExportJobResource" Core.$
      \x ->
        ExportJobResource'
          Core.<$> (x Core..: "S3UrlPrefix")
          Core.<*> (x Core..: "RoleArn")
          Core.<*> (x Core..:? "SegmentId")
          Core.<*> (x Core..:? "SegmentVersion")
