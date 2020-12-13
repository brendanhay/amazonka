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
    sirSize,
    sirFormat,
    sirChannelCounts,
    sirExternalId,
    sirS3URL,
    sirRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.DefinitionFormat
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the import job that created a segment. An import job is a job that creates a user segment by importing endpoint definitions.
--
-- /See:/ 'mkSegmentImportResource' smart constructor.
data SegmentImportResource = SegmentImportResource'
  { -- | The number of endpoint definitions that were imported successfully to create the segment.
    size :: Lude.Int,
    -- | The format of the files that were imported to create the segment. Valid values are: CSV, for comma-separated values format; and, JSON, for newline-delimited JSON format.
    format :: DefinitionFormat,
    -- | The number of channel types in the endpoint definitions that were imported to create the segment.
    channelCounts :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Int)),
    -- | (Deprecated) Your AWS account ID, which you assigned to an external ID key in an IAM trust policy. Amazon Pinpoint previously used this value to assume an IAM role when importing endpoint definitions, but we removed this requirement. We don't recommend use of external IDs for IAM roles that are assumed by Amazon Pinpoint.
    externalId :: Lude.Text,
    -- | The URL of the Amazon Simple Storage Service (Amazon S3) bucket that the endpoint definitions were imported from to create the segment.
    s3URL :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorized Amazon Pinpoint to access the Amazon S3 location to import endpoint definitions from.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SegmentImportResource' with the minimum fields required to make a request.
--
-- * 'size' - The number of endpoint definitions that were imported successfully to create the segment.
-- * 'format' - The format of the files that were imported to create the segment. Valid values are: CSV, for comma-separated values format; and, JSON, for newline-delimited JSON format.
-- * 'channelCounts' - The number of channel types in the endpoint definitions that were imported to create the segment.
-- * 'externalId' - (Deprecated) Your AWS account ID, which you assigned to an external ID key in an IAM trust policy. Amazon Pinpoint previously used this value to assume an IAM role when importing endpoint definitions, but we removed this requirement. We don't recommend use of external IDs for IAM roles that are assumed by Amazon Pinpoint.
-- * 's3URL' - The URL of the Amazon Simple Storage Service (Amazon S3) bucket that the endpoint definitions were imported from to create the segment.
-- * 'roleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorized Amazon Pinpoint to access the Amazon S3 location to import endpoint definitions from.
mkSegmentImportResource ::
  -- | 'size'
  Lude.Int ->
  -- | 'format'
  DefinitionFormat ->
  -- | 'externalId'
  Lude.Text ->
  -- | 's3URL'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  SegmentImportResource
mkSegmentImportResource
  pSize_
  pFormat_
  pExternalId_
  pS3URL_
  pRoleARN_ =
    SegmentImportResource'
      { size = pSize_,
        format = pFormat_,
        channelCounts = Lude.Nothing,
        externalId = pExternalId_,
        s3URL = pS3URL_,
        roleARN = pRoleARN_
      }

-- | The number of endpoint definitions that were imported successfully to create the segment.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirSize :: Lens.Lens' SegmentImportResource Lude.Int
sirSize = Lens.lens (size :: SegmentImportResource -> Lude.Int) (\s a -> s {size = a} :: SegmentImportResource)
{-# DEPRECATED sirSize "Use generic-lens or generic-optics with 'size' instead." #-}

-- | The format of the files that were imported to create the segment. Valid values are: CSV, for comma-separated values format; and, JSON, for newline-delimited JSON format.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirFormat :: Lens.Lens' SegmentImportResource DefinitionFormat
sirFormat = Lens.lens (format :: SegmentImportResource -> DefinitionFormat) (\s a -> s {format = a} :: SegmentImportResource)
{-# DEPRECATED sirFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | The number of channel types in the endpoint definitions that were imported to create the segment.
--
-- /Note:/ Consider using 'channelCounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirChannelCounts :: Lens.Lens' SegmentImportResource (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Int)))
sirChannelCounts = Lens.lens (channelCounts :: SegmentImportResource -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Int))) (\s a -> s {channelCounts = a} :: SegmentImportResource)
{-# DEPRECATED sirChannelCounts "Use generic-lens or generic-optics with 'channelCounts' instead." #-}

-- | (Deprecated) Your AWS account ID, which you assigned to an external ID key in an IAM trust policy. Amazon Pinpoint previously used this value to assume an IAM role when importing endpoint definitions, but we removed this requirement. We don't recommend use of external IDs for IAM roles that are assumed by Amazon Pinpoint.
--
-- /Note:/ Consider using 'externalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirExternalId :: Lens.Lens' SegmentImportResource Lude.Text
sirExternalId = Lens.lens (externalId :: SegmentImportResource -> Lude.Text) (\s a -> s {externalId = a} :: SegmentImportResource)
{-# DEPRECATED sirExternalId "Use generic-lens or generic-optics with 'externalId' instead." #-}

-- | The URL of the Amazon Simple Storage Service (Amazon S3) bucket that the endpoint definitions were imported from to create the segment.
--
-- /Note:/ Consider using 's3URL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirS3URL :: Lens.Lens' SegmentImportResource Lude.Text
sirS3URL = Lens.lens (s3URL :: SegmentImportResource -> Lude.Text) (\s a -> s {s3URL = a} :: SegmentImportResource)
{-# DEPRECATED sirS3URL "Use generic-lens or generic-optics with 's3URL' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorized Amazon Pinpoint to access the Amazon S3 location to import endpoint definitions from.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirRoleARN :: Lens.Lens' SegmentImportResource Lude.Text
sirRoleARN = Lens.lens (roleARN :: SegmentImportResource -> Lude.Text) (\s a -> s {roleARN = a} :: SegmentImportResource)
{-# DEPRECATED sirRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON SegmentImportResource where
  parseJSON =
    Lude.withObject
      "SegmentImportResource"
      ( \x ->
          SegmentImportResource'
            Lude.<$> (x Lude..: "Size")
            Lude.<*> (x Lude..: "Format")
            Lude.<*> (x Lude..:? "ChannelCounts" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "ExternalId")
            Lude.<*> (x Lude..: "S3Url")
            Lude.<*> (x Lude..: "RoleArn")
      )
