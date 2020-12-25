{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ExportJobRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ExportJobRequest
  ( ExportJobRequest (..),

    -- * Smart constructor
    mkExportJobRequest,

    -- * Lenses
    eS3UrlPrefix,
    eRoleArn,
    eSegmentId,
    eSegmentVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the settings for a job that exports endpoint definitions to an Amazon Simple Storage Service (Amazon S3) bucket.
--
-- /See:/ 'mkExportJobRequest' smart constructor.
data ExportJobRequest = ExportJobRequest'
  { -- | The URL of the location in an Amazon Simple Storage Service (Amazon S3) bucket where you want to export endpoint definitions to. This location is typically a folder that contains multiple files. The URL should be in the following format: s3://<replaceable>bucket-name/<replaceable>folder-name/.
    s3UrlPrefix :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to access the Amazon S3 location where you want to export endpoint definitions to.
    roleArn :: Core.Text,
    -- | The identifier for the segment to export endpoint definitions from. If you don't specify this value, Amazon Pinpoint exports definitions for all the endpoints that are associated with the application.
    segmentId :: Core.Maybe Core.Text,
    -- | The version of the segment to export endpoint definitions from, if specified.
    segmentVersion :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExportJobRequest' value with any optional fields omitted.
mkExportJobRequest ::
  -- | 's3UrlPrefix'
  Core.Text ->
  -- | 'roleArn'
  Core.Text ->
  ExportJobRequest
mkExportJobRequest s3UrlPrefix roleArn =
  ExportJobRequest'
    { s3UrlPrefix,
      roleArn,
      segmentId = Core.Nothing,
      segmentVersion = Core.Nothing
    }

-- | The URL of the location in an Amazon Simple Storage Service (Amazon S3) bucket where you want to export endpoint definitions to. This location is typically a folder that contains multiple files. The URL should be in the following format: s3://<replaceable>bucket-name/<replaceable>folder-name/.
--
-- /Note:/ Consider using 's3UrlPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eS3UrlPrefix :: Lens.Lens' ExportJobRequest Core.Text
eS3UrlPrefix = Lens.field @"s3UrlPrefix"
{-# DEPRECATED eS3UrlPrefix "Use generic-lens or generic-optics with 's3UrlPrefix' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to access the Amazon S3 location where you want to export endpoint definitions to.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eRoleArn :: Lens.Lens' ExportJobRequest Core.Text
eRoleArn = Lens.field @"roleArn"
{-# DEPRECATED eRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | The identifier for the segment to export endpoint definitions from. If you don't specify this value, Amazon Pinpoint exports definitions for all the endpoints that are associated with the application.
--
-- /Note:/ Consider using 'segmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSegmentId :: Lens.Lens' ExportJobRequest (Core.Maybe Core.Text)
eSegmentId = Lens.field @"segmentId"
{-# DEPRECATED eSegmentId "Use generic-lens or generic-optics with 'segmentId' instead." #-}

-- | The version of the segment to export endpoint definitions from, if specified.
--
-- /Note:/ Consider using 'segmentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSegmentVersion :: Lens.Lens' ExportJobRequest (Core.Maybe Core.Int)
eSegmentVersion = Lens.field @"segmentVersion"
{-# DEPRECATED eSegmentVersion "Use generic-lens or generic-optics with 'segmentVersion' instead." #-}

instance Core.FromJSON ExportJobRequest where
  toJSON ExportJobRequest {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("S3UrlPrefix" Core..= s3UrlPrefix),
            Core.Just ("RoleArn" Core..= roleArn),
            ("SegmentId" Core..=) Core.<$> segmentId,
            ("SegmentVersion" Core..=) Core.<$> segmentVersion
          ]
      )
