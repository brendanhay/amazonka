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
-- Module      : Amazonka.LookoutEquipment.Types.CountPercent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.CountPercent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Entity that comprises information of count and percentage.
--
-- /See:/ 'newCountPercent' smart constructor.
data CountPercent = CountPercent'
  { -- | Indicates the count of occurences of the given statistic.
    count :: Prelude.Int,
    -- | Indicates the percentage of occurances of the given statistic.
    percentage :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CountPercent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'count', 'countPercent_count' - Indicates the count of occurences of the given statistic.
--
-- 'percentage', 'countPercent_percentage' - Indicates the percentage of occurances of the given statistic.
newCountPercent ::
  -- | 'count'
  Prelude.Int ->
  -- | 'percentage'
  Prelude.Double ->
  CountPercent
newCountPercent pCount_ pPercentage_ =
  CountPercent'
    { count = pCount_,
      percentage = pPercentage_
    }

-- | Indicates the count of occurences of the given statistic.
countPercent_count :: Lens.Lens' CountPercent Prelude.Int
countPercent_count = Lens.lens (\CountPercent' {count} -> count) (\s@CountPercent' {} a -> s {count = a} :: CountPercent)

-- | Indicates the percentage of occurances of the given statistic.
countPercent_percentage :: Lens.Lens' CountPercent Prelude.Double
countPercent_percentage = Lens.lens (\CountPercent' {percentage} -> percentage) (\s@CountPercent' {} a -> s {percentage = a} :: CountPercent)

instance Data.FromJSON CountPercent where
  parseJSON =
    Data.withObject
      "CountPercent"
      ( \x ->
          CountPercent'
            Prelude.<$> (x Data..: "Count")
            Prelude.<*> (x Data..: "Percentage")
      )

instance Prelude.Hashable CountPercent where
  hashWithSalt _salt CountPercent' {..} =
    _salt `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` percentage

instance Prelude.NFData CountPercent where
  rnf CountPercent' {..} =
    Prelude.rnf count
      `Prelude.seq` Prelude.rnf percentage
