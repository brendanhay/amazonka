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
    rdssdPrefix,
    rdssdDestinationDataSharing,
    rdssdAWSKMSKeyARN,
    rdssdBucketName,
    rdssdSyncFormat,
    rdssdRegion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.ResourceDataSyncDestinationDataSharing
import Network.AWS.SSM.Types.ResourceDataSyncS3Format

-- | Information about the target S3 bucket for the Resource Data Sync.
--
-- /See:/ 'mkResourceDataSyncS3Destination' smart constructor.
data ResourceDataSyncS3Destination = ResourceDataSyncS3Destination'
  { prefix ::
      Lude.Maybe Lude.Text,
    destinationDataSharing ::
      Lude.Maybe
        ResourceDataSyncDestinationDataSharing,
    awsKMSKeyARN ::
      Lude.Maybe Lude.Text,
    bucketName :: Lude.Text,
    syncFormat ::
      ResourceDataSyncS3Format,
    region :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceDataSyncS3Destination' with the minimum fields required to make a request.
--
-- * 'awsKMSKeyARN' - The ARN of an encryption key for a destination in Amazon S3. Must belong to the same Region as the destination S3 bucket.
-- * 'bucketName' - The name of the S3 bucket where the aggregated data is stored.
-- * 'destinationDataSharing' - Enables destination data sharing. By default, this field is @null@ .
-- * 'prefix' - An Amazon S3 prefix for the bucket.
-- * 'region' - The AWS Region with the S3 bucket targeted by the Resource Data Sync.
-- * 'syncFormat' - A supported sync format. The following format is currently supported: JsonSerDe
mkResourceDataSyncS3Destination ::
  -- | 'bucketName'
  Lude.Text ->
  -- | 'syncFormat'
  ResourceDataSyncS3Format ->
  -- | 'region'
  Lude.Text ->
  ResourceDataSyncS3Destination
mkResourceDataSyncS3Destination pBucketName_ pSyncFormat_ pRegion_ =
  ResourceDataSyncS3Destination'
    { prefix = Lude.Nothing,
      destinationDataSharing = Lude.Nothing,
      awsKMSKeyARN = Lude.Nothing,
      bucketName = pBucketName_,
      syncFormat = pSyncFormat_,
      region = pRegion_
    }

-- | An Amazon S3 prefix for the bucket.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdssdPrefix :: Lens.Lens' ResourceDataSyncS3Destination (Lude.Maybe Lude.Text)
rdssdPrefix = Lens.lens (prefix :: ResourceDataSyncS3Destination -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: ResourceDataSyncS3Destination)
{-# DEPRECATED rdssdPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | Enables destination data sharing. By default, this field is @null@ .
--
-- /Note:/ Consider using 'destinationDataSharing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdssdDestinationDataSharing :: Lens.Lens' ResourceDataSyncS3Destination (Lude.Maybe ResourceDataSyncDestinationDataSharing)
rdssdDestinationDataSharing = Lens.lens (destinationDataSharing :: ResourceDataSyncS3Destination -> Lude.Maybe ResourceDataSyncDestinationDataSharing) (\s a -> s {destinationDataSharing = a} :: ResourceDataSyncS3Destination)
{-# DEPRECATED rdssdDestinationDataSharing "Use generic-lens or generic-optics with 'destinationDataSharing' instead." #-}

-- | The ARN of an encryption key for a destination in Amazon S3. Must belong to the same Region as the destination S3 bucket.
--
-- /Note:/ Consider using 'awsKMSKeyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdssdAWSKMSKeyARN :: Lens.Lens' ResourceDataSyncS3Destination (Lude.Maybe Lude.Text)
rdssdAWSKMSKeyARN = Lens.lens (awsKMSKeyARN :: ResourceDataSyncS3Destination -> Lude.Maybe Lude.Text) (\s a -> s {awsKMSKeyARN = a} :: ResourceDataSyncS3Destination)
{-# DEPRECATED rdssdAWSKMSKeyARN "Use generic-lens or generic-optics with 'awsKMSKeyARN' instead." #-}

-- | The name of the S3 bucket where the aggregated data is stored.
--
-- /Note:/ Consider using 'bucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdssdBucketName :: Lens.Lens' ResourceDataSyncS3Destination Lude.Text
rdssdBucketName = Lens.lens (bucketName :: ResourceDataSyncS3Destination -> Lude.Text) (\s a -> s {bucketName = a} :: ResourceDataSyncS3Destination)
{-# DEPRECATED rdssdBucketName "Use generic-lens or generic-optics with 'bucketName' instead." #-}

-- | A supported sync format. The following format is currently supported: JsonSerDe
--
-- /Note:/ Consider using 'syncFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdssdSyncFormat :: Lens.Lens' ResourceDataSyncS3Destination ResourceDataSyncS3Format
rdssdSyncFormat = Lens.lens (syncFormat :: ResourceDataSyncS3Destination -> ResourceDataSyncS3Format) (\s a -> s {syncFormat = a} :: ResourceDataSyncS3Destination)
{-# DEPRECATED rdssdSyncFormat "Use generic-lens or generic-optics with 'syncFormat' instead." #-}

-- | The AWS Region with the S3 bucket targeted by the Resource Data Sync.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdssdRegion :: Lens.Lens' ResourceDataSyncS3Destination Lude.Text
rdssdRegion = Lens.lens (region :: ResourceDataSyncS3Destination -> Lude.Text) (\s a -> s {region = a} :: ResourceDataSyncS3Destination)
{-# DEPRECATED rdssdRegion "Use generic-lens or generic-optics with 'region' instead." #-}

instance Lude.FromJSON ResourceDataSyncS3Destination where
  parseJSON =
    Lude.withObject
      "ResourceDataSyncS3Destination"
      ( \x ->
          ResourceDataSyncS3Destination'
            Lude.<$> (x Lude..:? "Prefix")
            Lude.<*> (x Lude..:? "DestinationDataSharing")
            Lude.<*> (x Lude..:? "AWSKMSKeyARN")
            Lude.<*> (x Lude..: "BucketName")
            Lude.<*> (x Lude..: "SyncFormat")
            Lude.<*> (x Lude..: "Region")
      )

instance Lude.ToJSON ResourceDataSyncS3Destination where
  toJSON ResourceDataSyncS3Destination' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Prefix" Lude..=) Lude.<$> prefix,
            ("DestinationDataSharing" Lude..=) Lude.<$> destinationDataSharing,
            ("AWSKMSKeyARN" Lude..=) Lude.<$> awsKMSKeyARN,
            Lude.Just ("BucketName" Lude..= bucketName),
            Lude.Just ("SyncFormat" Lude..= syncFormat),
            Lude.Just ("Region" Lude..= region)
          ]
      )
