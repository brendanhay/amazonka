{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.NeptuneSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.NeptuneSettings
  ( NeptuneSettings (..)
  -- * Smart constructor
  , mkNeptuneSettings
  -- * Lenses
  , nsS3BucketName
  , nsS3BucketFolder
  , nsErrorRetryDuration
  , nsIamAuthEnabled
  , nsMaxFileSize
  , nsMaxRetryCount
  , nsServiceAccessRoleArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information that defines an Amazon Neptune endpoint.
--
-- /See:/ 'mkNeptuneSettings' smart constructor.
data NeptuneSettings = NeptuneSettings'
  { s3BucketName :: Core.Text
    -- ^ The name of the Amazon S3 bucket where AWS DMS can temporarily store migrated graph data in .csv files before bulk-loading it to the Neptune target database. AWS DMS maps the SQL source data to graph data before storing it in these .csv files.
  , s3BucketFolder :: Core.Text
    -- ^ A folder path where you want AWS DMS to store migrated graph data in the S3 bucket specified by @S3BucketName@ 
  , errorRetryDuration :: Core.Maybe Core.Int
    -- ^ The number of milliseconds for AWS DMS to wait to retry a bulk-load of migrated graph data to the Neptune target database before raising an error. The default is 250.
  , iamAuthEnabled :: Core.Maybe Core.Bool
    -- ^ If you want AWS Identity and Access Management (IAM) authorization enabled for this endpoint, set this parameter to @true@ . Then attach the appropriate IAM policy document to your service role specified by @ServiceAccessRoleArn@ . The default is @false@ .
  , maxFileSize :: Core.Maybe Core.Int
    -- ^ The maximum size in kilobytes of migrated graph data stored in a .csv file before AWS DMS bulk-loads the data to the Neptune target database. The default is 1,048,576 KB. If the bulk load is successful, AWS DMS clears the bucket, ready to store the next batch of migrated graph data.
  , maxRetryCount :: Core.Maybe Core.Int
    -- ^ The number of times for AWS DMS to retry a bulk load of migrated graph data to the Neptune target database before raising an error. The default is 5.
  , serviceAccessRoleArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the service role that you created for the Neptune target endpoint. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Neptune.html#CHAP_Target.Neptune.ServiceRole Creating an IAM Service Role for Accessing Amazon Neptune as a Target> in the /AWS Database Migration Service User Guide. / 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NeptuneSettings' value with any optional fields omitted.
mkNeptuneSettings
    :: Core.Text -- ^ 's3BucketName'
    -> Core.Text -- ^ 's3BucketFolder'
    -> NeptuneSettings
mkNeptuneSettings s3BucketName s3BucketFolder
  = NeptuneSettings'{s3BucketName, s3BucketFolder,
                     errorRetryDuration = Core.Nothing, iamAuthEnabled = Core.Nothing,
                     maxFileSize = Core.Nothing, maxRetryCount = Core.Nothing,
                     serviceAccessRoleArn = Core.Nothing}

-- | The name of the Amazon S3 bucket where AWS DMS can temporarily store migrated graph data in .csv files before bulk-loading it to the Neptune target database. AWS DMS maps the SQL source data to graph data before storing it in these .csv files.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsS3BucketName :: Lens.Lens' NeptuneSettings Core.Text
nsS3BucketName = Lens.field @"s3BucketName"
{-# INLINEABLE nsS3BucketName #-}
{-# DEPRECATED s3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead"  #-}

-- | A folder path where you want AWS DMS to store migrated graph data in the S3 bucket specified by @S3BucketName@ 
--
-- /Note:/ Consider using 's3BucketFolder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsS3BucketFolder :: Lens.Lens' NeptuneSettings Core.Text
nsS3BucketFolder = Lens.field @"s3BucketFolder"
{-# INLINEABLE nsS3BucketFolder #-}
{-# DEPRECATED s3BucketFolder "Use generic-lens or generic-optics with 's3BucketFolder' instead"  #-}

-- | The number of milliseconds for AWS DMS to wait to retry a bulk-load of migrated graph data to the Neptune target database before raising an error. The default is 250.
--
-- /Note:/ Consider using 'errorRetryDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsErrorRetryDuration :: Lens.Lens' NeptuneSettings (Core.Maybe Core.Int)
nsErrorRetryDuration = Lens.field @"errorRetryDuration"
{-# INLINEABLE nsErrorRetryDuration #-}
{-# DEPRECATED errorRetryDuration "Use generic-lens or generic-optics with 'errorRetryDuration' instead"  #-}

-- | If you want AWS Identity and Access Management (IAM) authorization enabled for this endpoint, set this parameter to @true@ . Then attach the appropriate IAM policy document to your service role specified by @ServiceAccessRoleArn@ . The default is @false@ .
--
-- /Note:/ Consider using 'iamAuthEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsIamAuthEnabled :: Lens.Lens' NeptuneSettings (Core.Maybe Core.Bool)
nsIamAuthEnabled = Lens.field @"iamAuthEnabled"
{-# INLINEABLE nsIamAuthEnabled #-}
{-# DEPRECATED iamAuthEnabled "Use generic-lens or generic-optics with 'iamAuthEnabled' instead"  #-}

-- | The maximum size in kilobytes of migrated graph data stored in a .csv file before AWS DMS bulk-loads the data to the Neptune target database. The default is 1,048,576 KB. If the bulk load is successful, AWS DMS clears the bucket, ready to store the next batch of migrated graph data.
--
-- /Note:/ Consider using 'maxFileSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsMaxFileSize :: Lens.Lens' NeptuneSettings (Core.Maybe Core.Int)
nsMaxFileSize = Lens.field @"maxFileSize"
{-# INLINEABLE nsMaxFileSize #-}
{-# DEPRECATED maxFileSize "Use generic-lens or generic-optics with 'maxFileSize' instead"  #-}

-- | The number of times for AWS DMS to retry a bulk load of migrated graph data to the Neptune target database before raising an error. The default is 5.
--
-- /Note:/ Consider using 'maxRetryCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsMaxRetryCount :: Lens.Lens' NeptuneSettings (Core.Maybe Core.Int)
nsMaxRetryCount = Lens.field @"maxRetryCount"
{-# INLINEABLE nsMaxRetryCount #-}
{-# DEPRECATED maxRetryCount "Use generic-lens or generic-optics with 'maxRetryCount' instead"  #-}

-- | The Amazon Resource Name (ARN) of the service role that you created for the Neptune target endpoint. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Neptune.html#CHAP_Target.Neptune.ServiceRole Creating an IAM Service Role for Accessing Amazon Neptune as a Target> in the /AWS Database Migration Service User Guide. / 
--
-- /Note:/ Consider using 'serviceAccessRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsServiceAccessRoleArn :: Lens.Lens' NeptuneSettings (Core.Maybe Core.Text)
nsServiceAccessRoleArn = Lens.field @"serviceAccessRoleArn"
{-# INLINEABLE nsServiceAccessRoleArn #-}
{-# DEPRECATED serviceAccessRoleArn "Use generic-lens or generic-optics with 'serviceAccessRoleArn' instead"  #-}

instance Core.FromJSON NeptuneSettings where
        toJSON NeptuneSettings{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("S3BucketName" Core..= s3BucketName),
                  Core.Just ("S3BucketFolder" Core..= s3BucketFolder),
                  ("ErrorRetryDuration" Core..=) Core.<$> errorRetryDuration,
                  ("IamAuthEnabled" Core..=) Core.<$> iamAuthEnabled,
                  ("MaxFileSize" Core..=) Core.<$> maxFileSize,
                  ("MaxRetryCount" Core..=) Core.<$> maxRetryCount,
                  ("ServiceAccessRoleArn" Core..=) Core.<$> serviceAccessRoleArn])

instance Core.FromJSON NeptuneSettings where
        parseJSON
          = Core.withObject "NeptuneSettings" Core.$
              \ x ->
                NeptuneSettings' Core.<$>
                  (x Core..: "S3BucketName") Core.<*> x Core..: "S3BucketFolder"
                    Core.<*> x Core..:? "ErrorRetryDuration"
                    Core.<*> x Core..:? "IamAuthEnabled"
                    Core.<*> x Core..:? "MaxFileSize"
                    Core.<*> x Core..:? "MaxRetryCount"
                    Core.<*> x Core..:? "ServiceAccessRoleArn"
