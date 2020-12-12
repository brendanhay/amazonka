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
    ejrSegmentId,
    ejrSegmentVersion,
    ejrS3URLPrefix,
    ejrRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the resource settings for a job that exports endpoint definitions to a file. The file can be added directly to an Amazon Simple Storage Service (Amazon S3) bucket by using the Amazon Pinpoint API or downloaded directly to a computer by using the Amazon Pinpoint console.
--
-- /See:/ 'mkExportJobResource' smart constructor.
data ExportJobResource = ExportJobResource'
  { segmentId ::
      Lude.Maybe Lude.Text,
    segmentVersion :: Lude.Maybe Lude.Int,
    s3URLPrefix :: Lude.Text,
    roleARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportJobResource' with the minimum fields required to make a request.
--
-- * 'roleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorized Amazon Pinpoint to access the Amazon S3 location where the endpoint definitions were exported to.
-- * 's3URLPrefix' - The URL of the location in an Amazon Simple Storage Service (Amazon S3) bucket where the endpoint definitions were exported to. This location is typically a folder that contains multiple files. The URL should be in the following format: s3://<replaceable>bucket-name/<replaceable>folder-name/.
-- * 'segmentId' - The identifier for the segment that the endpoint definitions were exported from. If this value isn't present, Amazon Pinpoint exported definitions for all the endpoints that are associated with the application.
-- * 'segmentVersion' - The version of the segment that the endpoint definitions were exported from.
mkExportJobResource ::
  -- | 's3URLPrefix'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  ExportJobResource
mkExportJobResource pS3URLPrefix_ pRoleARN_ =
  ExportJobResource'
    { segmentId = Lude.Nothing,
      segmentVersion = Lude.Nothing,
      s3URLPrefix = pS3URLPrefix_,
      roleARN = pRoleARN_
    }

-- | The identifier for the segment that the endpoint definitions were exported from. If this value isn't present, Amazon Pinpoint exported definitions for all the endpoints that are associated with the application.
--
-- /Note:/ Consider using 'segmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrSegmentId :: Lens.Lens' ExportJobResource (Lude.Maybe Lude.Text)
ejrSegmentId = Lens.lens (segmentId :: ExportJobResource -> Lude.Maybe Lude.Text) (\s a -> s {segmentId = a} :: ExportJobResource)
{-# DEPRECATED ejrSegmentId "Use generic-lens or generic-optics with 'segmentId' instead." #-}

-- | The version of the segment that the endpoint definitions were exported from.
--
-- /Note:/ Consider using 'segmentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrSegmentVersion :: Lens.Lens' ExportJobResource (Lude.Maybe Lude.Int)
ejrSegmentVersion = Lens.lens (segmentVersion :: ExportJobResource -> Lude.Maybe Lude.Int) (\s a -> s {segmentVersion = a} :: ExportJobResource)
{-# DEPRECATED ejrSegmentVersion "Use generic-lens or generic-optics with 'segmentVersion' instead." #-}

-- | The URL of the location in an Amazon Simple Storage Service (Amazon S3) bucket where the endpoint definitions were exported to. This location is typically a folder that contains multiple files. The URL should be in the following format: s3://<replaceable>bucket-name/<replaceable>folder-name/.
--
-- /Note:/ Consider using 's3URLPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrS3URLPrefix :: Lens.Lens' ExportJobResource Lude.Text
ejrS3URLPrefix = Lens.lens (s3URLPrefix :: ExportJobResource -> Lude.Text) (\s a -> s {s3URLPrefix = a} :: ExportJobResource)
{-# DEPRECATED ejrS3URLPrefix "Use generic-lens or generic-optics with 's3URLPrefix' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorized Amazon Pinpoint to access the Amazon S3 location where the endpoint definitions were exported to.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrRoleARN :: Lens.Lens' ExportJobResource Lude.Text
ejrRoleARN = Lens.lens (roleARN :: ExportJobResource -> Lude.Text) (\s a -> s {roleARN = a} :: ExportJobResource)
{-# DEPRECATED ejrRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON ExportJobResource where
  parseJSON =
    Lude.withObject
      "ExportJobResource"
      ( \x ->
          ExportJobResource'
            Lude.<$> (x Lude..:? "SegmentId")
            Lude.<*> (x Lude..:? "SegmentVersion")
            Lude.<*> (x Lude..: "S3UrlPrefix")
            Lude.<*> (x Lude..: "RoleArn")
      )
