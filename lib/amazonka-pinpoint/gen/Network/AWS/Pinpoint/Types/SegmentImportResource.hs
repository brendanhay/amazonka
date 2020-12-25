{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SegmentImportResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SegmentImportResource
  ( SegmentImportResource (..),

    -- * Smart constructor
    mkSegmentImportResource,

    -- * Lenses
    sirFormat,
    sirS3Url,
    sirSize,
    sirExternalId,
    sirRoleArn,
    sirChannelCounts,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.DefinitionFormat as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about the import job that created a segment. An import job is a job that creates a user segment by importing endpoint definitions.
--
-- /See:/ 'mkSegmentImportResource' smart constructor.
data SegmentImportResource = SegmentImportResource'
  { -- | The format of the files that were imported to create the segment. Valid values are: CSV, for comma-separated values format; and, JSON, for newline-delimited JSON format.
    format :: Types.DefinitionFormat,
    -- | The URL of the Amazon Simple Storage Service (Amazon S3) bucket that the endpoint definitions were imported from to create the segment.
    s3Url :: Core.Text,
    -- | The number of endpoint definitions that were imported successfully to create the segment.
    size :: Core.Int,
    -- | (Deprecated) Your AWS account ID, which you assigned to an external ID key in an IAM trust policy. Amazon Pinpoint previously used this value to assume an IAM role when importing endpoint definitions, but we removed this requirement. We don't recommend use of external IDs for IAM roles that are assumed by Amazon Pinpoint.
    externalId :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorized Amazon Pinpoint to access the Amazon S3 location to import endpoint definitions from.
    roleArn :: Core.Text,
    -- | The number of channel types in the endpoint definitions that were imported to create the segment.
    channelCounts :: Core.Maybe (Core.HashMap Core.Text Core.Int)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SegmentImportResource' value with any optional fields omitted.
mkSegmentImportResource ::
  -- | 'format'
  Types.DefinitionFormat ->
  -- | 's3Url'
  Core.Text ->
  -- | 'size'
  Core.Int ->
  -- | 'externalId'
  Core.Text ->
  -- | 'roleArn'
  Core.Text ->
  SegmentImportResource
mkSegmentImportResource format s3Url size externalId roleArn =
  SegmentImportResource'
    { format,
      s3Url,
      size,
      externalId,
      roleArn,
      channelCounts = Core.Nothing
    }

-- | The format of the files that were imported to create the segment. Valid values are: CSV, for comma-separated values format; and, JSON, for newline-delimited JSON format.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirFormat :: Lens.Lens' SegmentImportResource Types.DefinitionFormat
sirFormat = Lens.field @"format"
{-# DEPRECATED sirFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | The URL of the Amazon Simple Storage Service (Amazon S3) bucket that the endpoint definitions were imported from to create the segment.
--
-- /Note:/ Consider using 's3Url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirS3Url :: Lens.Lens' SegmentImportResource Core.Text
sirS3Url = Lens.field @"s3Url"
{-# DEPRECATED sirS3Url "Use generic-lens or generic-optics with 's3Url' instead." #-}

-- | The number of endpoint definitions that were imported successfully to create the segment.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirSize :: Lens.Lens' SegmentImportResource Core.Int
sirSize = Lens.field @"size"
{-# DEPRECATED sirSize "Use generic-lens or generic-optics with 'size' instead." #-}

-- | (Deprecated) Your AWS account ID, which you assigned to an external ID key in an IAM trust policy. Amazon Pinpoint previously used this value to assume an IAM role when importing endpoint definitions, but we removed this requirement. We don't recommend use of external IDs for IAM roles that are assumed by Amazon Pinpoint.
--
-- /Note:/ Consider using 'externalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirExternalId :: Lens.Lens' SegmentImportResource Core.Text
sirExternalId = Lens.field @"externalId"
{-# DEPRECATED sirExternalId "Use generic-lens or generic-optics with 'externalId' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorized Amazon Pinpoint to access the Amazon S3 location to import endpoint definitions from.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirRoleArn :: Lens.Lens' SegmentImportResource Core.Text
sirRoleArn = Lens.field @"roleArn"
{-# DEPRECATED sirRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | The number of channel types in the endpoint definitions that were imported to create the segment.
--
-- /Note:/ Consider using 'channelCounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirChannelCounts :: Lens.Lens' SegmentImportResource (Core.Maybe (Core.HashMap Core.Text Core.Int))
sirChannelCounts = Lens.field @"channelCounts"
{-# DEPRECATED sirChannelCounts "Use generic-lens or generic-optics with 'channelCounts' instead." #-}

instance Core.FromJSON SegmentImportResource where
  parseJSON =
    Core.withObject "SegmentImportResource" Core.$
      \x ->
        SegmentImportResource'
          Core.<$> (x Core..: "Format")
          Core.<*> (x Core..: "S3Url")
          Core.<*> (x Core..: "Size")
          Core.<*> (x Core..: "ExternalId")
          Core.<*> (x Core..: "RoleArn")
          Core.<*> (x Core..:? "ChannelCounts")
