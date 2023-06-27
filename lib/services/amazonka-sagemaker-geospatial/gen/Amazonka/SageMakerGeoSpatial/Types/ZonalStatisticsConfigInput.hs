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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.ZonalStatisticsConfigInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.ZonalStatisticsConfigInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.ZonalStatistics

-- | The structure representing input configuration of ZonalStatistics
-- operation.
--
-- /See:/ 'newZonalStatisticsConfigInput' smart constructor.
data ZonalStatisticsConfigInput = ZonalStatisticsConfigInput'
  { -- | Bands used in the operation. If no target bands are specified, it uses
    -- all bands available input.
    targetBands :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The Amazon Resource Name (ARN) or an ID of a Amazon Web Services Key
    -- Management Service (Amazon Web Services KMS) key that Amazon SageMaker
    -- uses to decrypt your output artifacts with Amazon S3 server-side
    -- encryption. The SageMaker execution role must have @kms:GenerateDataKey@
    -- permission.
    --
    -- The @KmsKeyId@ can be any of the following formats:
    --
    -- -   \/\/ KMS Key ID
    --
    --     @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
    --
    -- -   \/\/ Amazon Resource Name (ARN) of a KMS Key
    --
    --     @\"arn:aws:kms:\<region>:\<account>:key\/\<key-id-12ab-34cd-56ef-1234567890ab>\"@
    --
    -- For more information about key identifiers, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-id Key identifiers (KeyID)>
    -- in the Amazon Web Services Key Management Service (Amazon Web Services
    -- KMS) documentation.
    zoneS3PathKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | List of zonal statistics to compute.
    statistics :: Prelude.NonEmpty ZonalStatistics,
    -- | The Amazon S3 path pointing to the GeoJSON containing the polygonal
    -- zones.
    zoneS3Path :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ZonalStatisticsConfigInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetBands', 'zonalStatisticsConfigInput_targetBands' - Bands used in the operation. If no target bands are specified, it uses
-- all bands available input.
--
-- 'zoneS3PathKmsKeyId', 'zonalStatisticsConfigInput_zoneS3PathKmsKeyId' - The Amazon Resource Name (ARN) or an ID of a Amazon Web Services Key
-- Management Service (Amazon Web Services KMS) key that Amazon SageMaker
-- uses to decrypt your output artifacts with Amazon S3 server-side
-- encryption. The SageMaker execution role must have @kms:GenerateDataKey@
-- permission.
--
-- The @KmsKeyId@ can be any of the following formats:
--
-- -   \/\/ KMS Key ID
--
--     @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   \/\/ Amazon Resource Name (ARN) of a KMS Key
--
--     @\"arn:aws:kms:\<region>:\<account>:key\/\<key-id-12ab-34cd-56ef-1234567890ab>\"@
--
-- For more information about key identifiers, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-id Key identifiers (KeyID)>
-- in the Amazon Web Services Key Management Service (Amazon Web Services
-- KMS) documentation.
--
-- 'statistics', 'zonalStatisticsConfigInput_statistics' - List of zonal statistics to compute.
--
-- 'zoneS3Path', 'zonalStatisticsConfigInput_zoneS3Path' - The Amazon S3 path pointing to the GeoJSON containing the polygonal
-- zones.
newZonalStatisticsConfigInput ::
  -- | 'statistics'
  Prelude.NonEmpty ZonalStatistics ->
  -- | 'zoneS3Path'
  Prelude.Text ->
  ZonalStatisticsConfigInput
newZonalStatisticsConfigInput
  pStatistics_
  pZoneS3Path_ =
    ZonalStatisticsConfigInput'
      { targetBands =
          Prelude.Nothing,
        zoneS3PathKmsKeyId = Prelude.Nothing,
        statistics = Lens.coerced Lens.# pStatistics_,
        zoneS3Path = pZoneS3Path_
      }

-- | Bands used in the operation. If no target bands are specified, it uses
-- all bands available input.
zonalStatisticsConfigInput_targetBands :: Lens.Lens' ZonalStatisticsConfigInput (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
zonalStatisticsConfigInput_targetBands = Lens.lens (\ZonalStatisticsConfigInput' {targetBands} -> targetBands) (\s@ZonalStatisticsConfigInput' {} a -> s {targetBands = a} :: ZonalStatisticsConfigInput) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) or an ID of a Amazon Web Services Key
-- Management Service (Amazon Web Services KMS) key that Amazon SageMaker
-- uses to decrypt your output artifacts with Amazon S3 server-side
-- encryption. The SageMaker execution role must have @kms:GenerateDataKey@
-- permission.
--
-- The @KmsKeyId@ can be any of the following formats:
--
-- -   \/\/ KMS Key ID
--
--     @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   \/\/ Amazon Resource Name (ARN) of a KMS Key
--
--     @\"arn:aws:kms:\<region>:\<account>:key\/\<key-id-12ab-34cd-56ef-1234567890ab>\"@
--
-- For more information about key identifiers, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-id Key identifiers (KeyID)>
-- in the Amazon Web Services Key Management Service (Amazon Web Services
-- KMS) documentation.
zonalStatisticsConfigInput_zoneS3PathKmsKeyId :: Lens.Lens' ZonalStatisticsConfigInput (Prelude.Maybe Prelude.Text)
zonalStatisticsConfigInput_zoneS3PathKmsKeyId = Lens.lens (\ZonalStatisticsConfigInput' {zoneS3PathKmsKeyId} -> zoneS3PathKmsKeyId) (\s@ZonalStatisticsConfigInput' {} a -> s {zoneS3PathKmsKeyId = a} :: ZonalStatisticsConfigInput)

-- | List of zonal statistics to compute.
zonalStatisticsConfigInput_statistics :: Lens.Lens' ZonalStatisticsConfigInput (Prelude.NonEmpty ZonalStatistics)
zonalStatisticsConfigInput_statistics = Lens.lens (\ZonalStatisticsConfigInput' {statistics} -> statistics) (\s@ZonalStatisticsConfigInput' {} a -> s {statistics = a} :: ZonalStatisticsConfigInput) Prelude.. Lens.coerced

-- | The Amazon S3 path pointing to the GeoJSON containing the polygonal
-- zones.
zonalStatisticsConfigInput_zoneS3Path :: Lens.Lens' ZonalStatisticsConfigInput Prelude.Text
zonalStatisticsConfigInput_zoneS3Path = Lens.lens (\ZonalStatisticsConfigInput' {zoneS3Path} -> zoneS3Path) (\s@ZonalStatisticsConfigInput' {} a -> s {zoneS3Path = a} :: ZonalStatisticsConfigInput)

instance Data.FromJSON ZonalStatisticsConfigInput where
  parseJSON =
    Data.withObject
      "ZonalStatisticsConfigInput"
      ( \x ->
          ZonalStatisticsConfigInput'
            Prelude.<$> (x Data..:? "TargetBands")
            Prelude.<*> (x Data..:? "ZoneS3PathKmsKeyId")
            Prelude.<*> (x Data..: "Statistics")
            Prelude.<*> (x Data..: "ZoneS3Path")
      )

instance Prelude.Hashable ZonalStatisticsConfigInput where
  hashWithSalt _salt ZonalStatisticsConfigInput' {..} =
    _salt
      `Prelude.hashWithSalt` targetBands
      `Prelude.hashWithSalt` zoneS3PathKmsKeyId
      `Prelude.hashWithSalt` statistics
      `Prelude.hashWithSalt` zoneS3Path

instance Prelude.NFData ZonalStatisticsConfigInput where
  rnf ZonalStatisticsConfigInput' {..} =
    Prelude.rnf targetBands
      `Prelude.seq` Prelude.rnf zoneS3PathKmsKeyId
      `Prelude.seq` Prelude.rnf statistics
      `Prelude.seq` Prelude.rnf zoneS3Path

instance Data.ToJSON ZonalStatisticsConfigInput where
  toJSON ZonalStatisticsConfigInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TargetBands" Data..=) Prelude.<$> targetBands,
            ("ZoneS3PathKmsKeyId" Data..=)
              Prelude.<$> zoneS3PathKmsKeyId,
            Prelude.Just ("Statistics" Data..= statistics),
            Prelude.Just ("ZoneS3Path" Data..= zoneS3Path)
          ]
      )
