{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResourceDataSyncS3Destination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ResourceDataSyncS3Destination
  ( ResourceDataSyncS3Destination (..),

    -- * Smart constructor
    mkResourceDataSyncS3Destination,

    -- * Lenses
    rdssdBucketName,
    rdssdSyncFormat,
    rdssdRegion,
    rdssdAWSKMSKeyARN,
    rdssdDestinationDataSharing,
    rdssdPrefix,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.AWSKMSKeyARN as Types
import qualified Network.AWS.SSM.Types.BucketName as Types
import qualified Network.AWS.SSM.Types.Prefix as Types
import qualified Network.AWS.SSM.Types.ResourceDataSyncDestinationDataSharing as Types
import qualified Network.AWS.SSM.Types.ResourceDataSyncS3Format as Types
import qualified Network.AWS.SSM.Types.ResourceDataSyncS3Region as Types

-- | Information about the target S3 bucket for the Resource Data Sync.
--
-- /See:/ 'mkResourceDataSyncS3Destination' smart constructor.
data ResourceDataSyncS3Destination = ResourceDataSyncS3Destination'
  { -- | The name of the S3 bucket where the aggregated data is stored.
    bucketName :: Types.BucketName,
    -- | A supported sync format. The following format is currently supported: JsonSerDe
    syncFormat :: Types.ResourceDataSyncS3Format,
    -- | The AWS Region with the S3 bucket targeted by the Resource Data Sync.
    region :: Types.ResourceDataSyncS3Region,
    -- | The ARN of an encryption key for a destination in Amazon S3. Must belong to the same Region as the destination S3 bucket.
    aWSKMSKeyARN :: Core.Maybe Types.AWSKMSKeyARN,
    -- | Enables destination data sharing. By default, this field is @null@ .
    destinationDataSharing :: Core.Maybe Types.ResourceDataSyncDestinationDataSharing,
    -- | An Amazon S3 prefix for the bucket.
    prefix :: Core.Maybe Types.Prefix
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceDataSyncS3Destination' value with any optional fields omitted.
mkResourceDataSyncS3Destination ::
  -- | 'bucketName'
  Types.BucketName ->
  -- | 'syncFormat'
  Types.ResourceDataSyncS3Format ->
  -- | 'region'
  Types.ResourceDataSyncS3Region ->
  ResourceDataSyncS3Destination
mkResourceDataSyncS3Destination bucketName syncFormat region =
  ResourceDataSyncS3Destination'
    { bucketName,
      syncFormat,
      region,
      aWSKMSKeyARN = Core.Nothing,
      destinationDataSharing = Core.Nothing,
      prefix = Core.Nothing
    }

-- | The name of the S3 bucket where the aggregated data is stored.
--
-- /Note:/ Consider using 'bucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdssdBucketName :: Lens.Lens' ResourceDataSyncS3Destination Types.BucketName
rdssdBucketName = Lens.field @"bucketName"
{-# DEPRECATED rdssdBucketName "Use generic-lens or generic-optics with 'bucketName' instead." #-}

-- | A supported sync format. The following format is currently supported: JsonSerDe
--
-- /Note:/ Consider using 'syncFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdssdSyncFormat :: Lens.Lens' ResourceDataSyncS3Destination Types.ResourceDataSyncS3Format
rdssdSyncFormat = Lens.field @"syncFormat"
{-# DEPRECATED rdssdSyncFormat "Use generic-lens or generic-optics with 'syncFormat' instead." #-}

-- | The AWS Region with the S3 bucket targeted by the Resource Data Sync.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdssdRegion :: Lens.Lens' ResourceDataSyncS3Destination Types.ResourceDataSyncS3Region
rdssdRegion = Lens.field @"region"
{-# DEPRECATED rdssdRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The ARN of an encryption key for a destination in Amazon S3. Must belong to the same Region as the destination S3 bucket.
--
-- /Note:/ Consider using 'aWSKMSKeyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdssdAWSKMSKeyARN :: Lens.Lens' ResourceDataSyncS3Destination (Core.Maybe Types.AWSKMSKeyARN)
rdssdAWSKMSKeyARN = Lens.field @"aWSKMSKeyARN"
{-# DEPRECATED rdssdAWSKMSKeyARN "Use generic-lens or generic-optics with 'aWSKMSKeyARN' instead." #-}

-- | Enables destination data sharing. By default, this field is @null@ .
--
-- /Note:/ Consider using 'destinationDataSharing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdssdDestinationDataSharing :: Lens.Lens' ResourceDataSyncS3Destination (Core.Maybe Types.ResourceDataSyncDestinationDataSharing)
rdssdDestinationDataSharing = Lens.field @"destinationDataSharing"
{-# DEPRECATED rdssdDestinationDataSharing "Use generic-lens or generic-optics with 'destinationDataSharing' instead." #-}

-- | An Amazon S3 prefix for the bucket.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdssdPrefix :: Lens.Lens' ResourceDataSyncS3Destination (Core.Maybe Types.Prefix)
rdssdPrefix = Lens.field @"prefix"
{-# DEPRECATED rdssdPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

instance Core.FromJSON ResourceDataSyncS3Destination where
  toJSON ResourceDataSyncS3Destination {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("BucketName" Core..= bucketName),
            Core.Just ("SyncFormat" Core..= syncFormat),
            Core.Just ("Region" Core..= region),
            ("AWSKMSKeyARN" Core..=) Core.<$> aWSKMSKeyARN,
            ("DestinationDataSharing" Core..=) Core.<$> destinationDataSharing,
            ("Prefix" Core..=) Core.<$> prefix
          ]
      )

instance Core.FromJSON ResourceDataSyncS3Destination where
  parseJSON =
    Core.withObject "ResourceDataSyncS3Destination" Core.$
      \x ->
        ResourceDataSyncS3Destination'
          Core.<$> (x Core..: "BucketName")
          Core.<*> (x Core..: "SyncFormat")
          Core.<*> (x Core..: "Region")
          Core.<*> (x Core..:? "AWSKMSKeyARN")
          Core.<*> (x Core..:? "DestinationDataSharing")
          Core.<*> (x Core..:? "Prefix")
