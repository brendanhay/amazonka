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
-- Module      : Amazonka.LookoutMetrics.Types.DimensionValueContribution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.DimensionValueContribution where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The severity of a value of a dimension that contributed to an anomaly.
--
-- /See:/ 'newDimensionValueContribution' smart constructor.
data DimensionValueContribution = DimensionValueContribution'
  { -- | The severity score of the value.
    contributionScore :: Prelude.Maybe Prelude.Double,
    -- | The value of the dimension.
    dimensionValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DimensionValueContribution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contributionScore', 'dimensionValueContribution_contributionScore' - The severity score of the value.
--
-- 'dimensionValue', 'dimensionValueContribution_dimensionValue' - The value of the dimension.
newDimensionValueContribution ::
  DimensionValueContribution
newDimensionValueContribution =
  DimensionValueContribution'
    { contributionScore =
        Prelude.Nothing,
      dimensionValue = Prelude.Nothing
    }

-- | The severity score of the value.
dimensionValueContribution_contributionScore :: Lens.Lens' DimensionValueContribution (Prelude.Maybe Prelude.Double)
dimensionValueContribution_contributionScore = Lens.lens (\DimensionValueContribution' {contributionScore} -> contributionScore) (\s@DimensionValueContribution' {} a -> s {contributionScore = a} :: DimensionValueContribution)

-- | The value of the dimension.
dimensionValueContribution_dimensionValue :: Lens.Lens' DimensionValueContribution (Prelude.Maybe Prelude.Text)
dimensionValueContribution_dimensionValue = Lens.lens (\DimensionValueContribution' {dimensionValue} -> dimensionValue) (\s@DimensionValueContribution' {} a -> s {dimensionValue = a} :: DimensionValueContribution)

instance Data.FromJSON DimensionValueContribution where
  parseJSON =
    Data.withObject
      "DimensionValueContribution"
      ( \x ->
          DimensionValueContribution'
            Prelude.<$> (x Data..:? "ContributionScore")
            Prelude.<*> (x Data..:? "DimensionValue")
      )

instance Prelude.Hashable DimensionValueContribution where
  hashWithSalt _salt DimensionValueContribution' {..} =
    _salt
      `Prelude.hashWithSalt` contributionScore
      `Prelude.hashWithSalt` dimensionValue

instance Prelude.NFData DimensionValueContribution where
  rnf DimensionValueContribution' {..} =
    Prelude.rnf contributionScore `Prelude.seq`
      Prelude.rnf dimensionValue
