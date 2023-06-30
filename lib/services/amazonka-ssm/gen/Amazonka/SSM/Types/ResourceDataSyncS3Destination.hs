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
-- Module      : Amazonka.SSM.Types.ResourceDataSyncS3Destination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.ResourceDataSyncS3Destination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.ResourceDataSyncDestinationDataSharing
import Amazonka.SSM.Types.ResourceDataSyncS3Format

-- | Information about the target S3 bucket for the resource data sync.
--
-- /See:/ 'newResourceDataSyncS3Destination' smart constructor.
data ResourceDataSyncS3Destination = ResourceDataSyncS3Destination'
  { -- | The ARN of an encryption key for a destination in Amazon S3. Must belong
    -- to the same Region as the destination S3 bucket.
    aWSKMSKeyARN :: Prelude.Maybe Prelude.Text,
    -- | Enables destination data sharing. By default, this field is @null@.
    destinationDataSharing :: Prelude.Maybe ResourceDataSyncDestinationDataSharing,
    -- | An Amazon S3 prefix for the bucket.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | The name of the S3 bucket where the aggregated data is stored.
    bucketName :: Prelude.Text,
    -- | A supported sync format. The following format is currently supported:
    -- JsonSerDe
    syncFormat :: ResourceDataSyncS3Format,
    -- | The Amazon Web Services Region with the S3 bucket targeted by the
    -- resource data sync.
    region :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceDataSyncS3Destination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aWSKMSKeyARN', 'resourceDataSyncS3Destination_aWSKMSKeyARN' - The ARN of an encryption key for a destination in Amazon S3. Must belong
-- to the same Region as the destination S3 bucket.
--
-- 'destinationDataSharing', 'resourceDataSyncS3Destination_destinationDataSharing' - Enables destination data sharing. By default, this field is @null@.
--
-- 'prefix', 'resourceDataSyncS3Destination_prefix' - An Amazon S3 prefix for the bucket.
--
-- 'bucketName', 'resourceDataSyncS3Destination_bucketName' - The name of the S3 bucket where the aggregated data is stored.
--
-- 'syncFormat', 'resourceDataSyncS3Destination_syncFormat' - A supported sync format. The following format is currently supported:
-- JsonSerDe
--
-- 'region', 'resourceDataSyncS3Destination_region' - The Amazon Web Services Region with the S3 bucket targeted by the
-- resource data sync.
newResourceDataSyncS3Destination ::
  -- | 'bucketName'
  Prelude.Text ->
  -- | 'syncFormat'
  ResourceDataSyncS3Format ->
  -- | 'region'
  Prelude.Text ->
  ResourceDataSyncS3Destination
newResourceDataSyncS3Destination
  pBucketName_
  pSyncFormat_
  pRegion_ =
    ResourceDataSyncS3Destination'
      { aWSKMSKeyARN =
          Prelude.Nothing,
        destinationDataSharing = Prelude.Nothing,
        prefix = Prelude.Nothing,
        bucketName = pBucketName_,
        syncFormat = pSyncFormat_,
        region = pRegion_
      }

-- | The ARN of an encryption key for a destination in Amazon S3. Must belong
-- to the same Region as the destination S3 bucket.
resourceDataSyncS3Destination_aWSKMSKeyARN :: Lens.Lens' ResourceDataSyncS3Destination (Prelude.Maybe Prelude.Text)
resourceDataSyncS3Destination_aWSKMSKeyARN = Lens.lens (\ResourceDataSyncS3Destination' {aWSKMSKeyARN} -> aWSKMSKeyARN) (\s@ResourceDataSyncS3Destination' {} a -> s {aWSKMSKeyARN = a} :: ResourceDataSyncS3Destination)

-- | Enables destination data sharing. By default, this field is @null@.
resourceDataSyncS3Destination_destinationDataSharing :: Lens.Lens' ResourceDataSyncS3Destination (Prelude.Maybe ResourceDataSyncDestinationDataSharing)
resourceDataSyncS3Destination_destinationDataSharing = Lens.lens (\ResourceDataSyncS3Destination' {destinationDataSharing} -> destinationDataSharing) (\s@ResourceDataSyncS3Destination' {} a -> s {destinationDataSharing = a} :: ResourceDataSyncS3Destination)

-- | An Amazon S3 prefix for the bucket.
resourceDataSyncS3Destination_prefix :: Lens.Lens' ResourceDataSyncS3Destination (Prelude.Maybe Prelude.Text)
resourceDataSyncS3Destination_prefix = Lens.lens (\ResourceDataSyncS3Destination' {prefix} -> prefix) (\s@ResourceDataSyncS3Destination' {} a -> s {prefix = a} :: ResourceDataSyncS3Destination)

-- | The name of the S3 bucket where the aggregated data is stored.
resourceDataSyncS3Destination_bucketName :: Lens.Lens' ResourceDataSyncS3Destination Prelude.Text
resourceDataSyncS3Destination_bucketName = Lens.lens (\ResourceDataSyncS3Destination' {bucketName} -> bucketName) (\s@ResourceDataSyncS3Destination' {} a -> s {bucketName = a} :: ResourceDataSyncS3Destination)

-- | A supported sync format. The following format is currently supported:
-- JsonSerDe
resourceDataSyncS3Destination_syncFormat :: Lens.Lens' ResourceDataSyncS3Destination ResourceDataSyncS3Format
resourceDataSyncS3Destination_syncFormat = Lens.lens (\ResourceDataSyncS3Destination' {syncFormat} -> syncFormat) (\s@ResourceDataSyncS3Destination' {} a -> s {syncFormat = a} :: ResourceDataSyncS3Destination)

-- | The Amazon Web Services Region with the S3 bucket targeted by the
-- resource data sync.
resourceDataSyncS3Destination_region :: Lens.Lens' ResourceDataSyncS3Destination Prelude.Text
resourceDataSyncS3Destination_region = Lens.lens (\ResourceDataSyncS3Destination' {region} -> region) (\s@ResourceDataSyncS3Destination' {} a -> s {region = a} :: ResourceDataSyncS3Destination)

instance Data.FromJSON ResourceDataSyncS3Destination where
  parseJSON =
    Data.withObject
      "ResourceDataSyncS3Destination"
      ( \x ->
          ResourceDataSyncS3Destination'
            Prelude.<$> (x Data..:? "AWSKMSKeyARN")
            Prelude.<*> (x Data..:? "DestinationDataSharing")
            Prelude.<*> (x Data..:? "Prefix")
            Prelude.<*> (x Data..: "BucketName")
            Prelude.<*> (x Data..: "SyncFormat")
            Prelude.<*> (x Data..: "Region")
      )

instance
  Prelude.Hashable
    ResourceDataSyncS3Destination
  where
  hashWithSalt _salt ResourceDataSyncS3Destination' {..} =
    _salt
      `Prelude.hashWithSalt` aWSKMSKeyARN
      `Prelude.hashWithSalt` destinationDataSharing
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` syncFormat
      `Prelude.hashWithSalt` region

instance Prelude.NFData ResourceDataSyncS3Destination where
  rnf ResourceDataSyncS3Destination' {..} =
    Prelude.rnf aWSKMSKeyARN
      `Prelude.seq` Prelude.rnf destinationDataSharing
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf syncFormat
      `Prelude.seq` Prelude.rnf region

instance Data.ToJSON ResourceDataSyncS3Destination where
  toJSON ResourceDataSyncS3Destination' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AWSKMSKeyARN" Data..=) Prelude.<$> aWSKMSKeyARN,
            ("DestinationDataSharing" Data..=)
              Prelude.<$> destinationDataSharing,
            ("Prefix" Data..=) Prelude.<$> prefix,
            Prelude.Just ("BucketName" Data..= bucketName),
            Prelude.Just ("SyncFormat" Data..= syncFormat),
            Prelude.Just ("Region" Data..= region)
          ]
      )
