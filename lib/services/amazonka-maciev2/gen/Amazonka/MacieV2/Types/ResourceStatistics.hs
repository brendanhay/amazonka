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
-- Module      : Amazonka.MacieV2.Types.ResourceStatistics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.ResourceStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides statistical data for sensitive data discovery metrics that
-- apply to an S3 bucket that Amazon Macie monitors and analyzes for your
-- account. The statistics capture the results of automated sensitive data
-- discovery activities that Macie has performed for the bucket. The data
-- is available only if automated sensitive data discovery is currently
-- enabled for your account.
--
-- /See:/ 'newResourceStatistics' smart constructor.
data ResourceStatistics = ResourceStatistics'
  { -- | The total amount of data, in bytes, that Amazon Macie has analyzed in
    -- the bucket.
    totalBytesClassified :: Prelude.Maybe Prelude.Integer,
    -- | The total number of occurrences of sensitive data that Amazon Macie has
    -- found in the bucket\'s objects. This includes occurrences that are
    -- currently suppressed by the sensitivity scoring settings for the bucket
    -- (totalDetectionsSuppressed).
    totalDetections :: Prelude.Maybe Prelude.Integer,
    -- | The total number of occurrences of sensitive data that are currently
    -- suppressed by the sensitivity scoring settings for the bucket. These
    -- represent occurrences of sensitive data that Amazon Macie found in the
    -- bucket\'s objects, but the occurrences were manually suppressed. By
    -- default, suppressed occurrences are excluded from the bucket\'s
    -- sensitivity score.
    totalDetectionsSuppressed :: Prelude.Maybe Prelude.Integer,
    -- | The total number of objects that Amazon Macie has analyzed in the
    -- bucket.
    totalItemsClassified :: Prelude.Maybe Prelude.Integer,
    -- | The total number of the bucket\'s objects that Amazon Macie has found
    -- sensitive data in.
    totalItemsSensitive :: Prelude.Maybe Prelude.Integer,
    -- | The total number of objects that Amazon Macie hasn\'t analyzed in the
    -- bucket due to an error or issue. For example, the object is a malformed
    -- file. This value includes objects that Macie hasn\'t analyzed for
    -- reasons reported by other statistics in the ResourceStatistics object.
    totalItemsSkipped :: Prelude.Maybe Prelude.Integer,
    -- | The total number of objects that Amazon Macie hasn\'t analyzed in the
    -- bucket because the objects are encrypted with a key that Macie isn\'t
    -- allowed to use.
    totalItemsSkippedInvalidEncryption :: Prelude.Maybe Prelude.Integer,
    -- | The total number of objects that Amazon Macie hasn\'t analyzed in the
    -- bucket because the objects are encrypted with an KMS key that was
    -- disabled or deleted.
    totalItemsSkippedInvalidKms :: Prelude.Maybe Prelude.Integer,
    -- | The total number of objects that Amazon Macie hasn\'t analyzed in the
    -- bucket because Macie isn\'t allowed to access the objects.
    totalItemsSkippedPermissionDenied :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'totalBytesClassified', 'resourceStatistics_totalBytesClassified' - The total amount of data, in bytes, that Amazon Macie has analyzed in
-- the bucket.
--
-- 'totalDetections', 'resourceStatistics_totalDetections' - The total number of occurrences of sensitive data that Amazon Macie has
-- found in the bucket\'s objects. This includes occurrences that are
-- currently suppressed by the sensitivity scoring settings for the bucket
-- (totalDetectionsSuppressed).
--
-- 'totalDetectionsSuppressed', 'resourceStatistics_totalDetectionsSuppressed' - The total number of occurrences of sensitive data that are currently
-- suppressed by the sensitivity scoring settings for the bucket. These
-- represent occurrences of sensitive data that Amazon Macie found in the
-- bucket\'s objects, but the occurrences were manually suppressed. By
-- default, suppressed occurrences are excluded from the bucket\'s
-- sensitivity score.
--
-- 'totalItemsClassified', 'resourceStatistics_totalItemsClassified' - The total number of objects that Amazon Macie has analyzed in the
-- bucket.
--
-- 'totalItemsSensitive', 'resourceStatistics_totalItemsSensitive' - The total number of the bucket\'s objects that Amazon Macie has found
-- sensitive data in.
--
-- 'totalItemsSkipped', 'resourceStatistics_totalItemsSkipped' - The total number of objects that Amazon Macie hasn\'t analyzed in the
-- bucket due to an error or issue. For example, the object is a malformed
-- file. This value includes objects that Macie hasn\'t analyzed for
-- reasons reported by other statistics in the ResourceStatistics object.
--
-- 'totalItemsSkippedInvalidEncryption', 'resourceStatistics_totalItemsSkippedInvalidEncryption' - The total number of objects that Amazon Macie hasn\'t analyzed in the
-- bucket because the objects are encrypted with a key that Macie isn\'t
-- allowed to use.
--
-- 'totalItemsSkippedInvalidKms', 'resourceStatistics_totalItemsSkippedInvalidKms' - The total number of objects that Amazon Macie hasn\'t analyzed in the
-- bucket because the objects are encrypted with an KMS key that was
-- disabled or deleted.
--
-- 'totalItemsSkippedPermissionDenied', 'resourceStatistics_totalItemsSkippedPermissionDenied' - The total number of objects that Amazon Macie hasn\'t analyzed in the
-- bucket because Macie isn\'t allowed to access the objects.
newResourceStatistics ::
  ResourceStatistics
newResourceStatistics =
  ResourceStatistics'
    { totalBytesClassified =
        Prelude.Nothing,
      totalDetections = Prelude.Nothing,
      totalDetectionsSuppressed = Prelude.Nothing,
      totalItemsClassified = Prelude.Nothing,
      totalItemsSensitive = Prelude.Nothing,
      totalItemsSkipped = Prelude.Nothing,
      totalItemsSkippedInvalidEncryption = Prelude.Nothing,
      totalItemsSkippedInvalidKms = Prelude.Nothing,
      totalItemsSkippedPermissionDenied = Prelude.Nothing
    }

-- | The total amount of data, in bytes, that Amazon Macie has analyzed in
-- the bucket.
resourceStatistics_totalBytesClassified :: Lens.Lens' ResourceStatistics (Prelude.Maybe Prelude.Integer)
resourceStatistics_totalBytesClassified = Lens.lens (\ResourceStatistics' {totalBytesClassified} -> totalBytesClassified) (\s@ResourceStatistics' {} a -> s {totalBytesClassified = a} :: ResourceStatistics)

-- | The total number of occurrences of sensitive data that Amazon Macie has
-- found in the bucket\'s objects. This includes occurrences that are
-- currently suppressed by the sensitivity scoring settings for the bucket
-- (totalDetectionsSuppressed).
resourceStatistics_totalDetections :: Lens.Lens' ResourceStatistics (Prelude.Maybe Prelude.Integer)
resourceStatistics_totalDetections = Lens.lens (\ResourceStatistics' {totalDetections} -> totalDetections) (\s@ResourceStatistics' {} a -> s {totalDetections = a} :: ResourceStatistics)

-- | The total number of occurrences of sensitive data that are currently
-- suppressed by the sensitivity scoring settings for the bucket. These
-- represent occurrences of sensitive data that Amazon Macie found in the
-- bucket\'s objects, but the occurrences were manually suppressed. By
-- default, suppressed occurrences are excluded from the bucket\'s
-- sensitivity score.
resourceStatistics_totalDetectionsSuppressed :: Lens.Lens' ResourceStatistics (Prelude.Maybe Prelude.Integer)
resourceStatistics_totalDetectionsSuppressed = Lens.lens (\ResourceStatistics' {totalDetectionsSuppressed} -> totalDetectionsSuppressed) (\s@ResourceStatistics' {} a -> s {totalDetectionsSuppressed = a} :: ResourceStatistics)

-- | The total number of objects that Amazon Macie has analyzed in the
-- bucket.
resourceStatistics_totalItemsClassified :: Lens.Lens' ResourceStatistics (Prelude.Maybe Prelude.Integer)
resourceStatistics_totalItemsClassified = Lens.lens (\ResourceStatistics' {totalItemsClassified} -> totalItemsClassified) (\s@ResourceStatistics' {} a -> s {totalItemsClassified = a} :: ResourceStatistics)

-- | The total number of the bucket\'s objects that Amazon Macie has found
-- sensitive data in.
resourceStatistics_totalItemsSensitive :: Lens.Lens' ResourceStatistics (Prelude.Maybe Prelude.Integer)
resourceStatistics_totalItemsSensitive = Lens.lens (\ResourceStatistics' {totalItemsSensitive} -> totalItemsSensitive) (\s@ResourceStatistics' {} a -> s {totalItemsSensitive = a} :: ResourceStatistics)

-- | The total number of objects that Amazon Macie hasn\'t analyzed in the
-- bucket due to an error or issue. For example, the object is a malformed
-- file. This value includes objects that Macie hasn\'t analyzed for
-- reasons reported by other statistics in the ResourceStatistics object.
resourceStatistics_totalItemsSkipped :: Lens.Lens' ResourceStatistics (Prelude.Maybe Prelude.Integer)
resourceStatistics_totalItemsSkipped = Lens.lens (\ResourceStatistics' {totalItemsSkipped} -> totalItemsSkipped) (\s@ResourceStatistics' {} a -> s {totalItemsSkipped = a} :: ResourceStatistics)

-- | The total number of objects that Amazon Macie hasn\'t analyzed in the
-- bucket because the objects are encrypted with a key that Macie isn\'t
-- allowed to use.
resourceStatistics_totalItemsSkippedInvalidEncryption :: Lens.Lens' ResourceStatistics (Prelude.Maybe Prelude.Integer)
resourceStatistics_totalItemsSkippedInvalidEncryption = Lens.lens (\ResourceStatistics' {totalItemsSkippedInvalidEncryption} -> totalItemsSkippedInvalidEncryption) (\s@ResourceStatistics' {} a -> s {totalItemsSkippedInvalidEncryption = a} :: ResourceStatistics)

-- | The total number of objects that Amazon Macie hasn\'t analyzed in the
-- bucket because the objects are encrypted with an KMS key that was
-- disabled or deleted.
resourceStatistics_totalItemsSkippedInvalidKms :: Lens.Lens' ResourceStatistics (Prelude.Maybe Prelude.Integer)
resourceStatistics_totalItemsSkippedInvalidKms = Lens.lens (\ResourceStatistics' {totalItemsSkippedInvalidKms} -> totalItemsSkippedInvalidKms) (\s@ResourceStatistics' {} a -> s {totalItemsSkippedInvalidKms = a} :: ResourceStatistics)

-- | The total number of objects that Amazon Macie hasn\'t analyzed in the
-- bucket because Macie isn\'t allowed to access the objects.
resourceStatistics_totalItemsSkippedPermissionDenied :: Lens.Lens' ResourceStatistics (Prelude.Maybe Prelude.Integer)
resourceStatistics_totalItemsSkippedPermissionDenied = Lens.lens (\ResourceStatistics' {totalItemsSkippedPermissionDenied} -> totalItemsSkippedPermissionDenied) (\s@ResourceStatistics' {} a -> s {totalItemsSkippedPermissionDenied = a} :: ResourceStatistics)

instance Data.FromJSON ResourceStatistics where
  parseJSON =
    Data.withObject
      "ResourceStatistics"
      ( \x ->
          ResourceStatistics'
            Prelude.<$> (x Data..:? "totalBytesClassified")
            Prelude.<*> (x Data..:? "totalDetections")
            Prelude.<*> (x Data..:? "totalDetectionsSuppressed")
            Prelude.<*> (x Data..:? "totalItemsClassified")
            Prelude.<*> (x Data..:? "totalItemsSensitive")
            Prelude.<*> (x Data..:? "totalItemsSkipped")
            Prelude.<*> (x Data..:? "totalItemsSkippedInvalidEncryption")
            Prelude.<*> (x Data..:? "totalItemsSkippedInvalidKms")
            Prelude.<*> (x Data..:? "totalItemsSkippedPermissionDenied")
      )

instance Prelude.Hashable ResourceStatistics where
  hashWithSalt _salt ResourceStatistics' {..} =
    _salt
      `Prelude.hashWithSalt` totalBytesClassified
      `Prelude.hashWithSalt` totalDetections
      `Prelude.hashWithSalt` totalDetectionsSuppressed
      `Prelude.hashWithSalt` totalItemsClassified
      `Prelude.hashWithSalt` totalItemsSensitive
      `Prelude.hashWithSalt` totalItemsSkipped
      `Prelude.hashWithSalt` totalItemsSkippedInvalidEncryption
      `Prelude.hashWithSalt` totalItemsSkippedInvalidKms
      `Prelude.hashWithSalt` totalItemsSkippedPermissionDenied

instance Prelude.NFData ResourceStatistics where
  rnf ResourceStatistics' {..} =
    Prelude.rnf totalBytesClassified
      `Prelude.seq` Prelude.rnf totalDetections
      `Prelude.seq` Prelude.rnf totalDetectionsSuppressed
      `Prelude.seq` Prelude.rnf totalItemsClassified
      `Prelude.seq` Prelude.rnf totalItemsSensitive
      `Prelude.seq` Prelude.rnf totalItemsSkipped
      `Prelude.seq` Prelude.rnf totalItemsSkippedInvalidEncryption
      `Prelude.seq` Prelude.rnf totalItemsSkippedInvalidKms
      `Prelude.seq` Prelude.rnf totalItemsSkippedPermissionDenied
