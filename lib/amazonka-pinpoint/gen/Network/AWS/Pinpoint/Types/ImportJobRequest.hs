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
    iSegmentName,
    iDefineSegment,
    iRegisterEndpoints,
    iExternalId,
    iSegmentId,
    iFormat,
    iS3URL,
    iRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.DefinitionFormat
import qualified Network.AWS.Prelude as Lude

-- | Specifies the settings for a job that imports endpoint definitions from an Amazon Simple Storage Service (Amazon S3) bucket.
--
-- /See:/ 'mkImportJobRequest' smart constructor.
data ImportJobRequest = ImportJobRequest'
  { segmentName ::
      Lude.Maybe Lude.Text,
    defineSegment :: Lude.Maybe Lude.Bool,
    registerEndpoints :: Lude.Maybe Lude.Bool,
    externalId :: Lude.Maybe Lude.Text,
    segmentId :: Lude.Maybe Lude.Text,
    format :: DefinitionFormat,
    s3URL :: Lude.Text,
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

-- | Creates a value of 'ImportJobRequest' with the minimum fields required to make a request.
--
-- * 'defineSegment' - Specifies whether to create a segment that contains the endpoints, when the endpoint definitions are imported.
-- * 'externalId' - (Deprecated) Your AWS account ID, which you assigned to an external ID key in an IAM trust policy. Amazon Pinpoint previously used this value to assume an IAM role when importing endpoint definitions, but we removed this requirement. We don't recommend use of external IDs for IAM roles that are assumed by Amazon Pinpoint.
-- * 'format' - The format of the files that contain the endpoint definitions to import. Valid values are: CSV, for comma-separated values format; and, JSON, for newline-delimited JSON format. If the Amazon S3 location stores multiple files that use different formats, Amazon Pinpoint imports data only from the files that use the specified format.
-- * 'registerEndpoints' - Specifies whether to register the endpoints with Amazon Pinpoint, when the endpoint definitions are imported.
-- * 'roleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to access the Amazon S3 location to import endpoint definitions from.
-- * 's3URL' - The URL of the Amazon Simple Storage Service (Amazon S3) bucket that contains the endpoint definitions to import. This location can be a folder or a single file. If the location is a folder, Amazon Pinpoint imports endpoint definitions from the files in this location, including any subfolders that the folder contains.
--
-- The URL should be in the following format: s3://<replaceable>bucket-name/<replaceable>folder-name/<replaceable>file-name. The location can end with the key for an individual object or a prefix that qualifies multiple objects.
-- * 'segmentId' - The identifier for the segment to update or add the imported endpoint definitions to, if the import job is meant to update an existing segment.
-- * 'segmentName' - A custom name for the segment that's created by the import job, if the value of the DefineSegment property is true.
mkImportJobRequest ::
  -- | 'format'
  DefinitionFormat ->
  -- | 's3URL'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  ImportJobRequest
mkImportJobRequest pFormat_ pS3URL_ pRoleARN_ =
  ImportJobRequest'
    { segmentName = Lude.Nothing,
      defineSegment = Lude.Nothing,
      registerEndpoints = Lude.Nothing,
      externalId = Lude.Nothing,
      segmentId = Lude.Nothing,
      format = pFormat_,
      s3URL = pS3URL_,
      roleARN = pRoleARN_
    }

-- | A custom name for the segment that's created by the import job, if the value of the DefineSegment property is true.
--
-- /Note:/ Consider using 'segmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSegmentName :: Lens.Lens' ImportJobRequest (Lude.Maybe Lude.Text)
iSegmentName = Lens.lens (segmentName :: ImportJobRequest -> Lude.Maybe Lude.Text) (\s a -> s {segmentName = a} :: ImportJobRequest)
{-# DEPRECATED iSegmentName "Use generic-lens or generic-optics with 'segmentName' instead." #-}

-- | Specifies whether to create a segment that contains the endpoints, when the endpoint definitions are imported.
--
-- /Note:/ Consider using 'defineSegment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iDefineSegment :: Lens.Lens' ImportJobRequest (Lude.Maybe Lude.Bool)
iDefineSegment = Lens.lens (defineSegment :: ImportJobRequest -> Lude.Maybe Lude.Bool) (\s a -> s {defineSegment = a} :: ImportJobRequest)
{-# DEPRECATED iDefineSegment "Use generic-lens or generic-optics with 'defineSegment' instead." #-}

-- | Specifies whether to register the endpoints with Amazon Pinpoint, when the endpoint definitions are imported.
--
-- /Note:/ Consider using 'registerEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iRegisterEndpoints :: Lens.Lens' ImportJobRequest (Lude.Maybe Lude.Bool)
iRegisterEndpoints = Lens.lens (registerEndpoints :: ImportJobRequest -> Lude.Maybe Lude.Bool) (\s a -> s {registerEndpoints = a} :: ImportJobRequest)
{-# DEPRECATED iRegisterEndpoints "Use generic-lens or generic-optics with 'registerEndpoints' instead." #-}

-- | (Deprecated) Your AWS account ID, which you assigned to an external ID key in an IAM trust policy. Amazon Pinpoint previously used this value to assume an IAM role when importing endpoint definitions, but we removed this requirement. We don't recommend use of external IDs for IAM roles that are assumed by Amazon Pinpoint.
--
-- /Note:/ Consider using 'externalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iExternalId :: Lens.Lens' ImportJobRequest (Lude.Maybe Lude.Text)
iExternalId = Lens.lens (externalId :: ImportJobRequest -> Lude.Maybe Lude.Text) (\s a -> s {externalId = a} :: ImportJobRequest)
{-# DEPRECATED iExternalId "Use generic-lens or generic-optics with 'externalId' instead." #-}

-- | The identifier for the segment to update or add the imported endpoint definitions to, if the import job is meant to update an existing segment.
--
-- /Note:/ Consider using 'segmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSegmentId :: Lens.Lens' ImportJobRequest (Lude.Maybe Lude.Text)
iSegmentId = Lens.lens (segmentId :: ImportJobRequest -> Lude.Maybe Lude.Text) (\s a -> s {segmentId = a} :: ImportJobRequest)
{-# DEPRECATED iSegmentId "Use generic-lens or generic-optics with 'segmentId' instead." #-}

-- | The format of the files that contain the endpoint definitions to import. Valid values are: CSV, for comma-separated values format; and, JSON, for newline-delimited JSON format. If the Amazon S3 location stores multiple files that use different formats, Amazon Pinpoint imports data only from the files that use the specified format.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iFormat :: Lens.Lens' ImportJobRequest DefinitionFormat
iFormat = Lens.lens (format :: ImportJobRequest -> DefinitionFormat) (\s a -> s {format = a} :: ImportJobRequest)
{-# DEPRECATED iFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | The URL of the Amazon Simple Storage Service (Amazon S3) bucket that contains the endpoint definitions to import. This location can be a folder or a single file. If the location is a folder, Amazon Pinpoint imports endpoint definitions from the files in this location, including any subfolders that the folder contains.
--
-- The URL should be in the following format: s3://<replaceable>bucket-name/<replaceable>folder-name/<replaceable>file-name. The location can end with the key for an individual object or a prefix that qualifies multiple objects.
--
-- /Note:/ Consider using 's3URL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iS3URL :: Lens.Lens' ImportJobRequest Lude.Text
iS3URL = Lens.lens (s3URL :: ImportJobRequest -> Lude.Text) (\s a -> s {s3URL = a} :: ImportJobRequest)
{-# DEPRECATED iS3URL "Use generic-lens or generic-optics with 's3URL' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to access the Amazon S3 location to import endpoint definitions from.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iRoleARN :: Lens.Lens' ImportJobRequest Lude.Text
iRoleARN = Lens.lens (roleARN :: ImportJobRequest -> Lude.Text) (\s a -> s {roleARN = a} :: ImportJobRequest)
{-# DEPRECATED iRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.ToJSON ImportJobRequest where
  toJSON ImportJobRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SegmentName" Lude..=) Lude.<$> segmentName,
            ("DefineSegment" Lude..=) Lude.<$> defineSegment,
            ("RegisterEndpoints" Lude..=) Lude.<$> registerEndpoints,
            ("ExternalId" Lude..=) Lude.<$> externalId,
            ("SegmentId" Lude..=) Lude.<$> segmentId,
            Lude.Just ("Format" Lude..= format),
            Lude.Just ("S3Url" Lude..= s3URL),
            Lude.Just ("RoleArn" Lude..= roleARN)
          ]
      )
