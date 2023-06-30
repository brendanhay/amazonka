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
-- Module      : Amazonka.S3.Types.Tiering
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.Tiering where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.IntelligentTieringAccessTier

-- | The S3 Intelligent-Tiering storage class is designed to optimize storage
-- costs by automatically moving data to the most cost-effective storage
-- access tier, without additional operational overhead.
--
-- /See:/ 'newTiering' smart constructor.
data Tiering = Tiering'
  { -- | The number of consecutive days of no access after which an object will
    -- be eligible to be transitioned to the corresponding tier. The minimum
    -- number of days specified for Archive Access tier must be at least 90
    -- days and Deep Archive Access tier must be at least 180 days. The maximum
    -- can be up to 2 years (730 days).
    days :: Prelude.Int,
    -- | S3 Intelligent-Tiering access tier. See
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html#sc-dynamic-data-access Storage class for automatically optimizing frequently and infrequently accessed objects>
    -- for a list of access tiers in the S3 Intelligent-Tiering storage class.
    accessTier :: IntelligentTieringAccessTier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Tiering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'days', 'tiering_days' - The number of consecutive days of no access after which an object will
-- be eligible to be transitioned to the corresponding tier. The minimum
-- number of days specified for Archive Access tier must be at least 90
-- days and Deep Archive Access tier must be at least 180 days. The maximum
-- can be up to 2 years (730 days).
--
-- 'accessTier', 'tiering_accessTier' - S3 Intelligent-Tiering access tier. See
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html#sc-dynamic-data-access Storage class for automatically optimizing frequently and infrequently accessed objects>
-- for a list of access tiers in the S3 Intelligent-Tiering storage class.
newTiering ::
  -- | 'days'
  Prelude.Int ->
  -- | 'accessTier'
  IntelligentTieringAccessTier ->
  Tiering
newTiering pDays_ pAccessTier_ =
  Tiering' {days = pDays_, accessTier = pAccessTier_}

-- | The number of consecutive days of no access after which an object will
-- be eligible to be transitioned to the corresponding tier. The minimum
-- number of days specified for Archive Access tier must be at least 90
-- days and Deep Archive Access tier must be at least 180 days. The maximum
-- can be up to 2 years (730 days).
tiering_days :: Lens.Lens' Tiering Prelude.Int
tiering_days = Lens.lens (\Tiering' {days} -> days) (\s@Tiering' {} a -> s {days = a} :: Tiering)

-- | S3 Intelligent-Tiering access tier. See
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html#sc-dynamic-data-access Storage class for automatically optimizing frequently and infrequently accessed objects>
-- for a list of access tiers in the S3 Intelligent-Tiering storage class.
tiering_accessTier :: Lens.Lens' Tiering IntelligentTieringAccessTier
tiering_accessTier = Lens.lens (\Tiering' {accessTier} -> accessTier) (\s@Tiering' {} a -> s {accessTier = a} :: Tiering)

instance Data.FromXML Tiering where
  parseXML x =
    Tiering'
      Prelude.<$> (x Data..@ "Days")
      Prelude.<*> (x Data..@ "AccessTier")

instance Prelude.Hashable Tiering where
  hashWithSalt _salt Tiering' {..} =
    _salt
      `Prelude.hashWithSalt` days
      `Prelude.hashWithSalt` accessTier

instance Prelude.NFData Tiering where
  rnf Tiering' {..} =
    Prelude.rnf days
      `Prelude.seq` Prelude.rnf accessTier

instance Data.ToXML Tiering where
  toXML Tiering' {..} =
    Prelude.mconcat
      [ "Days" Data.@= days,
        "AccessTier" Data.@= accessTier
      ]
