{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResourceDataSyncS3Destination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ResourceDataSyncS3Destination where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.ResourceDataSyncDestinationDataSharing
import Network.AWS.SSM.Types.ResourceDataSyncS3Format

-- | Information about the target S3 bucket for the Resource Data Sync.
--
-- /See:/ 'newResourceDataSyncS3Destination' smart constructor.
data ResourceDataSyncS3Destination = ResourceDataSyncS3Destination'
  { -- | An Amazon S3 prefix for the bucket.
    prefix :: Core.Maybe Core.Text,
    -- | Enables destination data sharing. By default, this field is @null@.
    destinationDataSharing :: Core.Maybe ResourceDataSyncDestinationDataSharing,
    -- | The ARN of an encryption key for a destination in Amazon S3. Must belong
    -- to the same Region as the destination S3 bucket.
    aWSKMSKeyARN :: Core.Maybe Core.Text,
    -- | The name of the S3 bucket where the aggregated data is stored.
    bucketName :: Core.Text,
    -- | A supported sync format. The following format is currently supported:
    -- JsonSerDe
    syncFormat :: ResourceDataSyncS3Format,
    -- | The AWS Region with the S3 bucket targeted by the Resource Data Sync.
    region :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResourceDataSyncS3Destination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefix', 'resourceDataSyncS3Destination_prefix' - An Amazon S3 prefix for the bucket.
--
-- 'destinationDataSharing', 'resourceDataSyncS3Destination_destinationDataSharing' - Enables destination data sharing. By default, this field is @null@.
--
-- 'aWSKMSKeyARN', 'resourceDataSyncS3Destination_aWSKMSKeyARN' - The ARN of an encryption key for a destination in Amazon S3. Must belong
-- to the same Region as the destination S3 bucket.
--
-- 'bucketName', 'resourceDataSyncS3Destination_bucketName' - The name of the S3 bucket where the aggregated data is stored.
--
-- 'syncFormat', 'resourceDataSyncS3Destination_syncFormat' - A supported sync format. The following format is currently supported:
-- JsonSerDe
--
-- 'region', 'resourceDataSyncS3Destination_region' - The AWS Region with the S3 bucket targeted by the Resource Data Sync.
newResourceDataSyncS3Destination ::
  -- | 'bucketName'
  Core.Text ->
  -- | 'syncFormat'
  ResourceDataSyncS3Format ->
  -- | 'region'
  Core.Text ->
  ResourceDataSyncS3Destination
newResourceDataSyncS3Destination
  pBucketName_
  pSyncFormat_
  pRegion_ =
    ResourceDataSyncS3Destination'
      { prefix =
          Core.Nothing,
        destinationDataSharing = Core.Nothing,
        aWSKMSKeyARN = Core.Nothing,
        bucketName = pBucketName_,
        syncFormat = pSyncFormat_,
        region = pRegion_
      }

-- | An Amazon S3 prefix for the bucket.
resourceDataSyncS3Destination_prefix :: Lens.Lens' ResourceDataSyncS3Destination (Core.Maybe Core.Text)
resourceDataSyncS3Destination_prefix = Lens.lens (\ResourceDataSyncS3Destination' {prefix} -> prefix) (\s@ResourceDataSyncS3Destination' {} a -> s {prefix = a} :: ResourceDataSyncS3Destination)

-- | Enables destination data sharing. By default, this field is @null@.
resourceDataSyncS3Destination_destinationDataSharing :: Lens.Lens' ResourceDataSyncS3Destination (Core.Maybe ResourceDataSyncDestinationDataSharing)
resourceDataSyncS3Destination_destinationDataSharing = Lens.lens (\ResourceDataSyncS3Destination' {destinationDataSharing} -> destinationDataSharing) (\s@ResourceDataSyncS3Destination' {} a -> s {destinationDataSharing = a} :: ResourceDataSyncS3Destination)

-- | The ARN of an encryption key for a destination in Amazon S3. Must belong
-- to the same Region as the destination S3 bucket.
resourceDataSyncS3Destination_aWSKMSKeyARN :: Lens.Lens' ResourceDataSyncS3Destination (Core.Maybe Core.Text)
resourceDataSyncS3Destination_aWSKMSKeyARN = Lens.lens (\ResourceDataSyncS3Destination' {aWSKMSKeyARN} -> aWSKMSKeyARN) (\s@ResourceDataSyncS3Destination' {} a -> s {aWSKMSKeyARN = a} :: ResourceDataSyncS3Destination)

-- | The name of the S3 bucket where the aggregated data is stored.
resourceDataSyncS3Destination_bucketName :: Lens.Lens' ResourceDataSyncS3Destination Core.Text
resourceDataSyncS3Destination_bucketName = Lens.lens (\ResourceDataSyncS3Destination' {bucketName} -> bucketName) (\s@ResourceDataSyncS3Destination' {} a -> s {bucketName = a} :: ResourceDataSyncS3Destination)

-- | A supported sync format. The following format is currently supported:
-- JsonSerDe
resourceDataSyncS3Destination_syncFormat :: Lens.Lens' ResourceDataSyncS3Destination ResourceDataSyncS3Format
resourceDataSyncS3Destination_syncFormat = Lens.lens (\ResourceDataSyncS3Destination' {syncFormat} -> syncFormat) (\s@ResourceDataSyncS3Destination' {} a -> s {syncFormat = a} :: ResourceDataSyncS3Destination)

-- | The AWS Region with the S3 bucket targeted by the Resource Data Sync.
resourceDataSyncS3Destination_region :: Lens.Lens' ResourceDataSyncS3Destination Core.Text
resourceDataSyncS3Destination_region = Lens.lens (\ResourceDataSyncS3Destination' {region} -> region) (\s@ResourceDataSyncS3Destination' {} a -> s {region = a} :: ResourceDataSyncS3Destination)

instance Core.FromJSON ResourceDataSyncS3Destination where
  parseJSON =
    Core.withObject
      "ResourceDataSyncS3Destination"
      ( \x ->
          ResourceDataSyncS3Destination'
            Core.<$> (x Core..:? "Prefix")
            Core.<*> (x Core..:? "DestinationDataSharing")
            Core.<*> (x Core..:? "AWSKMSKeyARN")
            Core.<*> (x Core..: "BucketName")
            Core.<*> (x Core..: "SyncFormat")
            Core.<*> (x Core..: "Region")
      )

instance Core.Hashable ResourceDataSyncS3Destination

instance Core.NFData ResourceDataSyncS3Destination

instance Core.ToJSON ResourceDataSyncS3Destination where
  toJSON ResourceDataSyncS3Destination' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Prefix" Core..=) Core.<$> prefix,
            ("DestinationDataSharing" Core..=)
              Core.<$> destinationDataSharing,
            ("AWSKMSKeyARN" Core..=) Core.<$> aWSKMSKeyARN,
            Core.Just ("BucketName" Core..= bucketName),
            Core.Just ("SyncFormat" Core..= syncFormat),
            Core.Just ("Region" Core..= region)
          ]
      )
