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
-- Module      : Amazonka.MacieV2.Types.BucketStatisticsBySensitivity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.BucketStatisticsBySensitivity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.SensitivityAggregations
import qualified Amazonka.Prelude as Prelude

-- | Provides aggregated statistical data for sensitive data discovery
-- metrics that apply to S3 buckets, grouped by bucket sensitivity score
-- (sensitivityScore). If automated sensitive data discovery is currently
-- disabled for your account, the value for each metric is 0.
--
-- /See:/ 'newBucketStatisticsBySensitivity' smart constructor.
data BucketStatisticsBySensitivity = BucketStatisticsBySensitivity'
  { -- | The aggregated statistical data for all buckets that have a sensitivity
    -- score of -1.
    classificationError :: Prelude.Maybe SensitivityAggregations,
    -- | The aggregated statistical data for all buckets that have a sensitivity
    -- score of 50.
    notClassified :: Prelude.Maybe SensitivityAggregations,
    -- | The aggregated statistical data for all buckets that have a sensitivity
    -- score of 0-49.
    notSensitive :: Prelude.Maybe SensitivityAggregations,
    -- | The aggregated statistical data for all buckets that have a sensitivity
    -- score of 51-100.
    sensitive :: Prelude.Maybe SensitivityAggregations
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BucketStatisticsBySensitivity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'classificationError', 'bucketStatisticsBySensitivity_classificationError' - The aggregated statistical data for all buckets that have a sensitivity
-- score of -1.
--
-- 'notClassified', 'bucketStatisticsBySensitivity_notClassified' - The aggregated statistical data for all buckets that have a sensitivity
-- score of 50.
--
-- 'notSensitive', 'bucketStatisticsBySensitivity_notSensitive' - The aggregated statistical data for all buckets that have a sensitivity
-- score of 0-49.
--
-- 'sensitive', 'bucketStatisticsBySensitivity_sensitive' - The aggregated statistical data for all buckets that have a sensitivity
-- score of 51-100.
newBucketStatisticsBySensitivity ::
  BucketStatisticsBySensitivity
newBucketStatisticsBySensitivity =
  BucketStatisticsBySensitivity'
    { classificationError =
        Prelude.Nothing,
      notClassified = Prelude.Nothing,
      notSensitive = Prelude.Nothing,
      sensitive = Prelude.Nothing
    }

-- | The aggregated statistical data for all buckets that have a sensitivity
-- score of -1.
bucketStatisticsBySensitivity_classificationError :: Lens.Lens' BucketStatisticsBySensitivity (Prelude.Maybe SensitivityAggregations)
bucketStatisticsBySensitivity_classificationError = Lens.lens (\BucketStatisticsBySensitivity' {classificationError} -> classificationError) (\s@BucketStatisticsBySensitivity' {} a -> s {classificationError = a} :: BucketStatisticsBySensitivity)

-- | The aggregated statistical data for all buckets that have a sensitivity
-- score of 50.
bucketStatisticsBySensitivity_notClassified :: Lens.Lens' BucketStatisticsBySensitivity (Prelude.Maybe SensitivityAggregations)
bucketStatisticsBySensitivity_notClassified = Lens.lens (\BucketStatisticsBySensitivity' {notClassified} -> notClassified) (\s@BucketStatisticsBySensitivity' {} a -> s {notClassified = a} :: BucketStatisticsBySensitivity)

-- | The aggregated statistical data for all buckets that have a sensitivity
-- score of 0-49.
bucketStatisticsBySensitivity_notSensitive :: Lens.Lens' BucketStatisticsBySensitivity (Prelude.Maybe SensitivityAggregations)
bucketStatisticsBySensitivity_notSensitive = Lens.lens (\BucketStatisticsBySensitivity' {notSensitive} -> notSensitive) (\s@BucketStatisticsBySensitivity' {} a -> s {notSensitive = a} :: BucketStatisticsBySensitivity)

-- | The aggregated statistical data for all buckets that have a sensitivity
-- score of 51-100.
bucketStatisticsBySensitivity_sensitive :: Lens.Lens' BucketStatisticsBySensitivity (Prelude.Maybe SensitivityAggregations)
bucketStatisticsBySensitivity_sensitive = Lens.lens (\BucketStatisticsBySensitivity' {sensitive} -> sensitive) (\s@BucketStatisticsBySensitivity' {} a -> s {sensitive = a} :: BucketStatisticsBySensitivity)

instance Data.FromJSON BucketStatisticsBySensitivity where
  parseJSON =
    Data.withObject
      "BucketStatisticsBySensitivity"
      ( \x ->
          BucketStatisticsBySensitivity'
            Prelude.<$> (x Data..:? "classificationError")
            Prelude.<*> (x Data..:? "notClassified")
            Prelude.<*> (x Data..:? "notSensitive")
            Prelude.<*> (x Data..:? "sensitive")
      )

instance
  Prelude.Hashable
    BucketStatisticsBySensitivity
  where
  hashWithSalt _salt BucketStatisticsBySensitivity' {..} =
    _salt
      `Prelude.hashWithSalt` classificationError
      `Prelude.hashWithSalt` notClassified
      `Prelude.hashWithSalt` notSensitive
      `Prelude.hashWithSalt` sensitive

instance Prelude.NFData BucketStatisticsBySensitivity where
  rnf BucketStatisticsBySensitivity' {..} =
    Prelude.rnf classificationError
      `Prelude.seq` Prelude.rnf notClassified
      `Prelude.seq` Prelude.rnf notSensitive
      `Prelude.seq` Prelude.rnf sensitive
