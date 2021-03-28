{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SegmentImportResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.SegmentImportResource
  ( SegmentImportResource (..)
  -- * Smart constructor
  , mkSegmentImportResource
  -- * Lenses
  , sirFormat
  , sirS3Url
  , sirSize
  , sirExternalId
  , sirRoleArn
  , sirChannelCounts
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.DefinitionFormat as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about the import job that created a segment. An import job is a job that creates a user segment by importing endpoint definitions.
--
-- /See:/ 'mkSegmentImportResource' smart constructor.
data SegmentImportResource = SegmentImportResource'
  { format :: Types.DefinitionFormat
    -- ^ The format of the files that were imported to create the segment. Valid values are: CSV, for comma-separated values format; and, JSON, for newline-delimited JSON format.
  , s3Url :: Core.Text
    -- ^ The URL of the Amazon Simple Storage Service (Amazon S3) bucket that the endpoint definitions were imported from to create the segment.
  , size :: Core.Int
    -- ^ The number of endpoint definitions that were imported successfully to create the segment.
  , externalId :: Core.Text
    -- ^ (Deprecated) Your AWS account ID, which you assigned to an external ID key in an IAM trust policy. Amazon Pinpoint previously used this value to assume an IAM role when importing endpoint definitions, but we removed this requirement. We don't recommend use of external IDs for IAM roles that are assumed by Amazon Pinpoint.
  , roleArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorized Amazon Pinpoint to access the Amazon S3 location to import endpoint definitions from.
  , channelCounts :: Core.Maybe (Core.HashMap Core.Text Core.Int)
    -- ^ The number of channel types in the endpoint definitions that were imported to create the segment.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SegmentImportResource' value with any optional fields omitted.
mkSegmentImportResource
    :: Types.DefinitionFormat -- ^ 'format'
    -> Core.Text -- ^ 's3Url'
    -> Core.Int -- ^ 'size'
    -> Core.Text -- ^ 'externalId'
    -> Core.Text -- ^ 'roleArn'
    -> SegmentImportResource
mkSegmentImportResource format s3Url size externalId roleArn
  = SegmentImportResource'{format, s3Url, size, externalId, roleArn,
                           channelCounts = Core.Nothing}

-- | The format of the files that were imported to create the segment. Valid values are: CSV, for comma-separated values format; and, JSON, for newline-delimited JSON format.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirFormat :: Lens.Lens' SegmentImportResource Types.DefinitionFormat
sirFormat = Lens.field @"format"
{-# INLINEABLE sirFormat #-}
{-# DEPRECATED format "Use generic-lens or generic-optics with 'format' instead"  #-}

-- | The URL of the Amazon Simple Storage Service (Amazon S3) bucket that the endpoint definitions were imported from to create the segment.
--
-- /Note:/ Consider using 's3Url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirS3Url :: Lens.Lens' SegmentImportResource Core.Text
sirS3Url = Lens.field @"s3Url"
{-# INLINEABLE sirS3Url #-}
{-# DEPRECATED s3Url "Use generic-lens or generic-optics with 's3Url' instead"  #-}

-- | The number of endpoint definitions that were imported successfully to create the segment.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirSize :: Lens.Lens' SegmentImportResource Core.Int
sirSize = Lens.field @"size"
{-# INLINEABLE sirSize #-}
{-# DEPRECATED size "Use generic-lens or generic-optics with 'size' instead"  #-}

-- | (Deprecated) Your AWS account ID, which you assigned to an external ID key in an IAM trust policy. Amazon Pinpoint previously used this value to assume an IAM role when importing endpoint definitions, but we removed this requirement. We don't recommend use of external IDs for IAM roles that are assumed by Amazon Pinpoint.
--
-- /Note:/ Consider using 'externalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirExternalId :: Lens.Lens' SegmentImportResource Core.Text
sirExternalId = Lens.field @"externalId"
{-# INLINEABLE sirExternalId #-}
{-# DEPRECATED externalId "Use generic-lens or generic-optics with 'externalId' instead"  #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorized Amazon Pinpoint to access the Amazon S3 location to import endpoint definitions from.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirRoleArn :: Lens.Lens' SegmentImportResource Core.Text
sirRoleArn = Lens.field @"roleArn"
{-# INLINEABLE sirRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The number of channel types in the endpoint definitions that were imported to create the segment.
--
-- /Note:/ Consider using 'channelCounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirChannelCounts :: Lens.Lens' SegmentImportResource (Core.Maybe (Core.HashMap Core.Text Core.Int))
sirChannelCounts = Lens.field @"channelCounts"
{-# INLINEABLE sirChannelCounts #-}
{-# DEPRECATED channelCounts "Use generic-lens or generic-optics with 'channelCounts' instead"  #-}

instance Core.FromJSON SegmentImportResource where
        parseJSON
          = Core.withObject "SegmentImportResource" Core.$
              \ x ->
                SegmentImportResource' Core.<$>
                  (x Core..: "Format") Core.<*> x Core..: "S3Url" Core.<*>
                    x Core..: "Size"
                    Core.<*> x Core..: "ExternalId"
                    Core.<*> x Core..: "RoleArn"
                    Core.<*> x Core..:? "ChannelCounts"
