{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ImportJobResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ImportJobResource
  ( ImportJobResource (..),

    -- * Smart constructor
    mkImportJobResource,

    -- * Lenses
    ijrSegmentName,
    ijrFormat,
    ijrDefineSegment,
    ijrRegisterEndpoints,
    ijrExternalId,
    ijrS3URL,
    ijrSegmentId,
    ijrRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.DefinitionFormat
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the resource settings for a job that imports endpoint definitions from one or more files. The files can be stored in an Amazon Simple Storage Service (Amazon S3) bucket or uploaded directly from a computer by using the Amazon Pinpoint console.
--
-- /See:/ 'mkImportJobResource' smart constructor.
data ImportJobResource = ImportJobResource'
  { -- | The custom name for the segment that's created by the import job, if the value of the DefineSegment property is true.
    segmentName :: Lude.Maybe Lude.Text,
    -- | The format of the files that contain the endpoint definitions to import. Valid values are: CSV, for comma-separated values format; and, JSON, for newline-delimited JSON format.
    --
    -- If the files are stored in an Amazon S3 location and that location contains multiple files that use different formats, Amazon Pinpoint imports data only from the files that use the specified format.
    format :: DefinitionFormat,
    -- | Specifies whether the import job creates a segment that contains the endpoints, when the endpoint definitions are imported.
    defineSegment :: Lude.Maybe Lude.Bool,
    -- | Specifies whether the import job registers the endpoints with Amazon Pinpoint, when the endpoint definitions are imported.
    registerEndpoints :: Lude.Maybe Lude.Bool,
    -- | (Deprecated) Your AWS account ID, which you assigned to an external ID key in an IAM trust policy. Amazon Pinpoint previously used this value to assume an IAM role when importing endpoint definitions, but we removed this requirement. We don't recommend use of external IDs for IAM roles that are assumed by Amazon Pinpoint.
    externalId :: Lude.Maybe Lude.Text,
    -- | The URL of the Amazon Simple Storage Service (Amazon S3) bucket that contains the endpoint definitions to import. This location can be a folder or a single file. If the location is a folder, Amazon Pinpoint imports endpoint definitions from the files in this location, including any subfolders that the folder contains.
    --
    -- The URL should be in the following format: s3://<replaceable>bucket-name/<replaceable>folder-name/<replaceable>file-name. The location can end with the key for an individual object or a prefix that qualifies multiple objects.
    s3URL :: Lude.Text,
    -- | The identifier for the segment that the import job updates or adds endpoint definitions to, if the import job updates an existing segment.
    segmentId :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to access the Amazon S3 location to import endpoint definitions from.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportJobResource' with the minimum fields required to make a request.
--
-- * 'segmentName' - The custom name for the segment that's created by the import job, if the value of the DefineSegment property is true.
-- * 'format' - The format of the files that contain the endpoint definitions to import. Valid values are: CSV, for comma-separated values format; and, JSON, for newline-delimited JSON format.
--
-- If the files are stored in an Amazon S3 location and that location contains multiple files that use different formats, Amazon Pinpoint imports data only from the files that use the specified format.
-- * 'defineSegment' - Specifies whether the import job creates a segment that contains the endpoints, when the endpoint definitions are imported.
-- * 'registerEndpoints' - Specifies whether the import job registers the endpoints with Amazon Pinpoint, when the endpoint definitions are imported.
-- * 'externalId' - (Deprecated) Your AWS account ID, which you assigned to an external ID key in an IAM trust policy. Amazon Pinpoint previously used this value to assume an IAM role when importing endpoint definitions, but we removed this requirement. We don't recommend use of external IDs for IAM roles that are assumed by Amazon Pinpoint.
-- * 's3URL' - The URL of the Amazon Simple Storage Service (Amazon S3) bucket that contains the endpoint definitions to import. This location can be a folder or a single file. If the location is a folder, Amazon Pinpoint imports endpoint definitions from the files in this location, including any subfolders that the folder contains.
--
-- The URL should be in the following format: s3://<replaceable>bucket-name/<replaceable>folder-name/<replaceable>file-name. The location can end with the key for an individual object or a prefix that qualifies multiple objects.
-- * 'segmentId' - The identifier for the segment that the import job updates or adds endpoint definitions to, if the import job updates an existing segment.
-- * 'roleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to access the Amazon S3 location to import endpoint definitions from.
mkImportJobResource ::
  -- | 'format'
  DefinitionFormat ->
  -- | 's3URL'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  ImportJobResource
mkImportJobResource pFormat_ pS3URL_ pRoleARN_ =
  ImportJobResource'
    { segmentName = Lude.Nothing,
      format = pFormat_,
      defineSegment = Lude.Nothing,
      registerEndpoints = Lude.Nothing,
      externalId = Lude.Nothing,
      s3URL = pS3URL_,
      segmentId = Lude.Nothing,
      roleARN = pRoleARN_
    }

-- | The custom name for the segment that's created by the import job, if the value of the DefineSegment property is true.
--
-- /Note:/ Consider using 'segmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrSegmentName :: Lens.Lens' ImportJobResource (Lude.Maybe Lude.Text)
ijrSegmentName = Lens.lens (segmentName :: ImportJobResource -> Lude.Maybe Lude.Text) (\s a -> s {segmentName = a} :: ImportJobResource)
{-# DEPRECATED ijrSegmentName "Use generic-lens or generic-optics with 'segmentName' instead." #-}

-- | The format of the files that contain the endpoint definitions to import. Valid values are: CSV, for comma-separated values format; and, JSON, for newline-delimited JSON format.
--
-- If the files are stored in an Amazon S3 location and that location contains multiple files that use different formats, Amazon Pinpoint imports data only from the files that use the specified format.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrFormat :: Lens.Lens' ImportJobResource DefinitionFormat
ijrFormat = Lens.lens (format :: ImportJobResource -> DefinitionFormat) (\s a -> s {format = a} :: ImportJobResource)
{-# DEPRECATED ijrFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | Specifies whether the import job creates a segment that contains the endpoints, when the endpoint definitions are imported.
--
-- /Note:/ Consider using 'defineSegment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrDefineSegment :: Lens.Lens' ImportJobResource (Lude.Maybe Lude.Bool)
ijrDefineSegment = Lens.lens (defineSegment :: ImportJobResource -> Lude.Maybe Lude.Bool) (\s a -> s {defineSegment = a} :: ImportJobResource)
{-# DEPRECATED ijrDefineSegment "Use generic-lens or generic-optics with 'defineSegment' instead." #-}

-- | Specifies whether the import job registers the endpoints with Amazon Pinpoint, when the endpoint definitions are imported.
--
-- /Note:/ Consider using 'registerEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrRegisterEndpoints :: Lens.Lens' ImportJobResource (Lude.Maybe Lude.Bool)
ijrRegisterEndpoints = Lens.lens (registerEndpoints :: ImportJobResource -> Lude.Maybe Lude.Bool) (\s a -> s {registerEndpoints = a} :: ImportJobResource)
{-# DEPRECATED ijrRegisterEndpoints "Use generic-lens or generic-optics with 'registerEndpoints' instead." #-}

-- | (Deprecated) Your AWS account ID, which you assigned to an external ID key in an IAM trust policy. Amazon Pinpoint previously used this value to assume an IAM role when importing endpoint definitions, but we removed this requirement. We don't recommend use of external IDs for IAM roles that are assumed by Amazon Pinpoint.
--
-- /Note:/ Consider using 'externalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrExternalId :: Lens.Lens' ImportJobResource (Lude.Maybe Lude.Text)
ijrExternalId = Lens.lens (externalId :: ImportJobResource -> Lude.Maybe Lude.Text) (\s a -> s {externalId = a} :: ImportJobResource)
{-# DEPRECATED ijrExternalId "Use generic-lens or generic-optics with 'externalId' instead." #-}

-- | The URL of the Amazon Simple Storage Service (Amazon S3) bucket that contains the endpoint definitions to import. This location can be a folder or a single file. If the location is a folder, Amazon Pinpoint imports endpoint definitions from the files in this location, including any subfolders that the folder contains.
--
-- The URL should be in the following format: s3://<replaceable>bucket-name/<replaceable>folder-name/<replaceable>file-name. The location can end with the key for an individual object or a prefix that qualifies multiple objects.
--
-- /Note:/ Consider using 's3URL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrS3URL :: Lens.Lens' ImportJobResource Lude.Text
ijrS3URL = Lens.lens (s3URL :: ImportJobResource -> Lude.Text) (\s a -> s {s3URL = a} :: ImportJobResource)
{-# DEPRECATED ijrS3URL "Use generic-lens or generic-optics with 's3URL' instead." #-}

-- | The identifier for the segment that the import job updates or adds endpoint definitions to, if the import job updates an existing segment.
--
-- /Note:/ Consider using 'segmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrSegmentId :: Lens.Lens' ImportJobResource (Lude.Maybe Lude.Text)
ijrSegmentId = Lens.lens (segmentId :: ImportJobResource -> Lude.Maybe Lude.Text) (\s a -> s {segmentId = a} :: ImportJobResource)
{-# DEPRECATED ijrSegmentId "Use generic-lens or generic-optics with 'segmentId' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to access the Amazon S3 location to import endpoint definitions from.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrRoleARN :: Lens.Lens' ImportJobResource Lude.Text
ijrRoleARN = Lens.lens (roleARN :: ImportJobResource -> Lude.Text) (\s a -> s {roleARN = a} :: ImportJobResource)
{-# DEPRECATED ijrRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON ImportJobResource where
  parseJSON =
    Lude.withObject
      "ImportJobResource"
      ( \x ->
          ImportJobResource'
            Lude.<$> (x Lude..:? "SegmentName")
            Lude.<*> (x Lude..: "Format")
            Lude.<*> (x Lude..:? "DefineSegment")
            Lude.<*> (x Lude..:? "RegisterEndpoints")
            Lude.<*> (x Lude..:? "ExternalId")
            Lude.<*> (x Lude..: "S3Url")
            Lude.<*> (x Lude..:? "SegmentId")
            Lude.<*> (x Lude..: "RoleArn")
      )
