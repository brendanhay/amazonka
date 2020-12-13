{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.NeptuneSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.NeptuneSettings
  ( NeptuneSettings (..),

    -- * Smart constructor
    mkNeptuneSettings,

    -- * Lenses
    nsMaxFileSize,
    nsMaxRetryCount,
    nsServiceAccessRoleARN,
    nsS3BucketFolder,
    nsIAMAuthEnabled,
    nsErrorRetryDuration,
    nsS3BucketName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information that defines an Amazon Neptune endpoint.
--
-- /See:/ 'mkNeptuneSettings' smart constructor.
data NeptuneSettings = NeptuneSettings'
  { -- | The maximum size in kilobytes of migrated graph data stored in a .csv file before AWS DMS bulk-loads the data to the Neptune target database. The default is 1,048,576 KB. If the bulk load is successful, AWS DMS clears the bucket, ready to store the next batch of migrated graph data.
    maxFileSize :: Lude.Maybe Lude.Int,
    -- | The number of times for AWS DMS to retry a bulk load of migrated graph data to the Neptune target database before raising an error. The default is 5.
    maxRetryCount :: Lude.Maybe Lude.Int,
    -- | The Amazon Resource Name (ARN) of the service role that you created for the Neptune target endpoint. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Neptune.html#CHAP_Target.Neptune.ServiceRole Creating an IAM Service Role for Accessing Amazon Neptune as a Target> in the /AWS Database Migration Service User Guide. /
    serviceAccessRoleARN :: Lude.Maybe Lude.Text,
    -- | A folder path where you want AWS DMS to store migrated graph data in the S3 bucket specified by @S3BucketName@
    s3BucketFolder :: Lude.Text,
    -- | If you want AWS Identity and Access Management (IAM) authorization enabled for this endpoint, set this parameter to @true@ . Then attach the appropriate IAM policy document to your service role specified by @ServiceAccessRoleArn@ . The default is @false@ .
    iamAuthEnabled :: Lude.Maybe Lude.Bool,
    -- | The number of milliseconds for AWS DMS to wait to retry a bulk-load of migrated graph data to the Neptune target database before raising an error. The default is 250.
    errorRetryDuration :: Lude.Maybe Lude.Int,
    -- | The name of the Amazon S3 bucket where AWS DMS can temporarily store migrated graph data in .csv files before bulk-loading it to the Neptune target database. AWS DMS maps the SQL source data to graph data before storing it in these .csv files.
    s3BucketName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NeptuneSettings' with the minimum fields required to make a request.
--
-- * 'maxFileSize' - The maximum size in kilobytes of migrated graph data stored in a .csv file before AWS DMS bulk-loads the data to the Neptune target database. The default is 1,048,576 KB. If the bulk load is successful, AWS DMS clears the bucket, ready to store the next batch of migrated graph data.
-- * 'maxRetryCount' - The number of times for AWS DMS to retry a bulk load of migrated graph data to the Neptune target database before raising an error. The default is 5.
-- * 'serviceAccessRoleARN' - The Amazon Resource Name (ARN) of the service role that you created for the Neptune target endpoint. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Neptune.html#CHAP_Target.Neptune.ServiceRole Creating an IAM Service Role for Accessing Amazon Neptune as a Target> in the /AWS Database Migration Service User Guide. /
-- * 's3BucketFolder' - A folder path where you want AWS DMS to store migrated graph data in the S3 bucket specified by @S3BucketName@
-- * 'iamAuthEnabled' - If you want AWS Identity and Access Management (IAM) authorization enabled for this endpoint, set this parameter to @true@ . Then attach the appropriate IAM policy document to your service role specified by @ServiceAccessRoleArn@ . The default is @false@ .
-- * 'errorRetryDuration' - The number of milliseconds for AWS DMS to wait to retry a bulk-load of migrated graph data to the Neptune target database before raising an error. The default is 250.
-- * 's3BucketName' - The name of the Amazon S3 bucket where AWS DMS can temporarily store migrated graph data in .csv files before bulk-loading it to the Neptune target database. AWS DMS maps the SQL source data to graph data before storing it in these .csv files.
mkNeptuneSettings ::
  -- | 's3BucketFolder'
  Lude.Text ->
  -- | 's3BucketName'
  Lude.Text ->
  NeptuneSettings
mkNeptuneSettings pS3BucketFolder_ pS3BucketName_ =
  NeptuneSettings'
    { maxFileSize = Lude.Nothing,
      maxRetryCount = Lude.Nothing,
      serviceAccessRoleARN = Lude.Nothing,
      s3BucketFolder = pS3BucketFolder_,
      iamAuthEnabled = Lude.Nothing,
      errorRetryDuration = Lude.Nothing,
      s3BucketName = pS3BucketName_
    }

-- | The maximum size in kilobytes of migrated graph data stored in a .csv file before AWS DMS bulk-loads the data to the Neptune target database. The default is 1,048,576 KB. If the bulk load is successful, AWS DMS clears the bucket, ready to store the next batch of migrated graph data.
--
-- /Note:/ Consider using 'maxFileSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsMaxFileSize :: Lens.Lens' NeptuneSettings (Lude.Maybe Lude.Int)
nsMaxFileSize = Lens.lens (maxFileSize :: NeptuneSettings -> Lude.Maybe Lude.Int) (\s a -> s {maxFileSize = a} :: NeptuneSettings)
{-# DEPRECATED nsMaxFileSize "Use generic-lens or generic-optics with 'maxFileSize' instead." #-}

-- | The number of times for AWS DMS to retry a bulk load of migrated graph data to the Neptune target database before raising an error. The default is 5.
--
-- /Note:/ Consider using 'maxRetryCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsMaxRetryCount :: Lens.Lens' NeptuneSettings (Lude.Maybe Lude.Int)
nsMaxRetryCount = Lens.lens (maxRetryCount :: NeptuneSettings -> Lude.Maybe Lude.Int) (\s a -> s {maxRetryCount = a} :: NeptuneSettings)
{-# DEPRECATED nsMaxRetryCount "Use generic-lens or generic-optics with 'maxRetryCount' instead." #-}

-- | The Amazon Resource Name (ARN) of the service role that you created for the Neptune target endpoint. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Neptune.html#CHAP_Target.Neptune.ServiceRole Creating an IAM Service Role for Accessing Amazon Neptune as a Target> in the /AWS Database Migration Service User Guide. /
--
-- /Note:/ Consider using 'serviceAccessRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsServiceAccessRoleARN :: Lens.Lens' NeptuneSettings (Lude.Maybe Lude.Text)
nsServiceAccessRoleARN = Lens.lens (serviceAccessRoleARN :: NeptuneSettings -> Lude.Maybe Lude.Text) (\s a -> s {serviceAccessRoleARN = a} :: NeptuneSettings)
{-# DEPRECATED nsServiceAccessRoleARN "Use generic-lens or generic-optics with 'serviceAccessRoleARN' instead." #-}

-- | A folder path where you want AWS DMS to store migrated graph data in the S3 bucket specified by @S3BucketName@
--
-- /Note:/ Consider using 's3BucketFolder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsS3BucketFolder :: Lens.Lens' NeptuneSettings Lude.Text
nsS3BucketFolder = Lens.lens (s3BucketFolder :: NeptuneSettings -> Lude.Text) (\s a -> s {s3BucketFolder = a} :: NeptuneSettings)
{-# DEPRECATED nsS3BucketFolder "Use generic-lens or generic-optics with 's3BucketFolder' instead." #-}

-- | If you want AWS Identity and Access Management (IAM) authorization enabled for this endpoint, set this parameter to @true@ . Then attach the appropriate IAM policy document to your service role specified by @ServiceAccessRoleArn@ . The default is @false@ .
--
-- /Note:/ Consider using 'iamAuthEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsIAMAuthEnabled :: Lens.Lens' NeptuneSettings (Lude.Maybe Lude.Bool)
nsIAMAuthEnabled = Lens.lens (iamAuthEnabled :: NeptuneSettings -> Lude.Maybe Lude.Bool) (\s a -> s {iamAuthEnabled = a} :: NeptuneSettings)
{-# DEPRECATED nsIAMAuthEnabled "Use generic-lens or generic-optics with 'iamAuthEnabled' instead." #-}

-- | The number of milliseconds for AWS DMS to wait to retry a bulk-load of migrated graph data to the Neptune target database before raising an error. The default is 250.
--
-- /Note:/ Consider using 'errorRetryDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsErrorRetryDuration :: Lens.Lens' NeptuneSettings (Lude.Maybe Lude.Int)
nsErrorRetryDuration = Lens.lens (errorRetryDuration :: NeptuneSettings -> Lude.Maybe Lude.Int) (\s a -> s {errorRetryDuration = a} :: NeptuneSettings)
{-# DEPRECATED nsErrorRetryDuration "Use generic-lens or generic-optics with 'errorRetryDuration' instead." #-}

-- | The name of the Amazon S3 bucket where AWS DMS can temporarily store migrated graph data in .csv files before bulk-loading it to the Neptune target database. AWS DMS maps the SQL source data to graph data before storing it in these .csv files.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsS3BucketName :: Lens.Lens' NeptuneSettings Lude.Text
nsS3BucketName = Lens.lens (s3BucketName :: NeptuneSettings -> Lude.Text) (\s a -> s {s3BucketName = a} :: NeptuneSettings)
{-# DEPRECATED nsS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

instance Lude.FromJSON NeptuneSettings where
  parseJSON =
    Lude.withObject
      "NeptuneSettings"
      ( \x ->
          NeptuneSettings'
            Lude.<$> (x Lude..:? "MaxFileSize")
            Lude.<*> (x Lude..:? "MaxRetryCount")
            Lude.<*> (x Lude..:? "ServiceAccessRoleArn")
            Lude.<*> (x Lude..: "S3BucketFolder")
            Lude.<*> (x Lude..:? "IamAuthEnabled")
            Lude.<*> (x Lude..:? "ErrorRetryDuration")
            Lude.<*> (x Lude..: "S3BucketName")
      )

instance Lude.ToJSON NeptuneSettings where
  toJSON NeptuneSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MaxFileSize" Lude..=) Lude.<$> maxFileSize,
            ("MaxRetryCount" Lude..=) Lude.<$> maxRetryCount,
            ("ServiceAccessRoleArn" Lude..=) Lude.<$> serviceAccessRoleARN,
            Lude.Just ("S3BucketFolder" Lude..= s3BucketFolder),
            ("IamAuthEnabled" Lude..=) Lude.<$> iamAuthEnabled,
            ("ErrorRetryDuration" Lude..=) Lude.<$> errorRetryDuration,
            Lude.Just ("S3BucketName" Lude..= s3BucketName)
          ]
      )
