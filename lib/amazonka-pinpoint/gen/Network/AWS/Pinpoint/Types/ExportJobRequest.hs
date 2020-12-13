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
    eS3URLPrefix,
    eSegmentId,
    eRoleARN,
    eSegmentVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the settings for a job that exports endpoint definitions to an Amazon Simple Storage Service (Amazon S3) bucket.
--
-- /See:/ 'mkExportJobRequest' smart constructor.
data ExportJobRequest = ExportJobRequest'
  { -- | The URL of the location in an Amazon Simple Storage Service (Amazon S3) bucket where you want to export endpoint definitions to. This location is typically a folder that contains multiple files. The URL should be in the following format: s3://<replaceable>bucket-name/<replaceable>folder-name/.
    s3URLPrefix :: Lude.Text,
    -- | The identifier for the segment to export endpoint definitions from. If you don't specify this value, Amazon Pinpoint exports definitions for all the endpoints that are associated with the application.
    segmentId :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to access the Amazon S3 location where you want to export endpoint definitions to.
    roleARN :: Lude.Text,
    -- | The version of the segment to export endpoint definitions from, if specified.
    segmentVersion :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportJobRequest' with the minimum fields required to make a request.
--
-- * 's3URLPrefix' - The URL of the location in an Amazon Simple Storage Service (Amazon S3) bucket where you want to export endpoint definitions to. This location is typically a folder that contains multiple files. The URL should be in the following format: s3://<replaceable>bucket-name/<replaceable>folder-name/.
-- * 'segmentId' - The identifier for the segment to export endpoint definitions from. If you don't specify this value, Amazon Pinpoint exports definitions for all the endpoints that are associated with the application.
-- * 'roleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to access the Amazon S3 location where you want to export endpoint definitions to.
-- * 'segmentVersion' - The version of the segment to export endpoint definitions from, if specified.
mkExportJobRequest ::
  -- | 's3URLPrefix'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  ExportJobRequest
mkExportJobRequest pS3URLPrefix_ pRoleARN_ =
  ExportJobRequest'
    { s3URLPrefix = pS3URLPrefix_,
      segmentId = Lude.Nothing,
      roleARN = pRoleARN_,
      segmentVersion = Lude.Nothing
    }

-- | The URL of the location in an Amazon Simple Storage Service (Amazon S3) bucket where you want to export endpoint definitions to. This location is typically a folder that contains multiple files. The URL should be in the following format: s3://<replaceable>bucket-name/<replaceable>folder-name/.
--
-- /Note:/ Consider using 's3URLPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eS3URLPrefix :: Lens.Lens' ExportJobRequest Lude.Text
eS3URLPrefix = Lens.lens (s3URLPrefix :: ExportJobRequest -> Lude.Text) (\s a -> s {s3URLPrefix = a} :: ExportJobRequest)
{-# DEPRECATED eS3URLPrefix "Use generic-lens or generic-optics with 's3URLPrefix' instead." #-}

-- | The identifier for the segment to export endpoint definitions from. If you don't specify this value, Amazon Pinpoint exports definitions for all the endpoints that are associated with the application.
--
-- /Note:/ Consider using 'segmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSegmentId :: Lens.Lens' ExportJobRequest (Lude.Maybe Lude.Text)
eSegmentId = Lens.lens (segmentId :: ExportJobRequest -> Lude.Maybe Lude.Text) (\s a -> s {segmentId = a} :: ExportJobRequest)
{-# DEPRECATED eSegmentId "Use generic-lens or generic-optics with 'segmentId' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to access the Amazon S3 location where you want to export endpoint definitions to.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eRoleARN :: Lens.Lens' ExportJobRequest Lude.Text
eRoleARN = Lens.lens (roleARN :: ExportJobRequest -> Lude.Text) (\s a -> s {roleARN = a} :: ExportJobRequest)
{-# DEPRECATED eRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The version of the segment to export endpoint definitions from, if specified.
--
-- /Note:/ Consider using 'segmentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSegmentVersion :: Lens.Lens' ExportJobRequest (Lude.Maybe Lude.Int)
eSegmentVersion = Lens.lens (segmentVersion :: ExportJobRequest -> Lude.Maybe Lude.Int) (\s a -> s {segmentVersion = a} :: ExportJobRequest)
{-# DEPRECATED eSegmentVersion "Use generic-lens or generic-optics with 'segmentVersion' instead." #-}

instance Lude.ToJSON ExportJobRequest where
  toJSON ExportJobRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("S3UrlPrefix" Lude..= s3URLPrefix),
            ("SegmentId" Lude..=) Lude.<$> segmentId,
            Lude.Just ("RoleArn" Lude..= roleARN),
            ("SegmentVersion" Lude..=) Lude.<$> segmentVersion
          ]
      )
