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
-- Module      : Amazonka.LookoutMetrics.Types.DimensionContribution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.DimensionContribution where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types.DimensionValueContribution
import qualified Amazonka.Prelude as Prelude

-- | Details about a dimension that contributed to an anomaly.
--
-- /See:/ 'newDimensionContribution' smart constructor.
data DimensionContribution = DimensionContribution'
  { -- | The name of the dimension.
    dimensionName :: Prelude.Maybe Prelude.Text,
    -- | A list of dimension values that contributed to the anomaly.
    dimensionValueContributionList :: Prelude.Maybe [DimensionValueContribution]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DimensionContribution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensionName', 'dimensionContribution_dimensionName' - The name of the dimension.
--
-- 'dimensionValueContributionList', 'dimensionContribution_dimensionValueContributionList' - A list of dimension values that contributed to the anomaly.
newDimensionContribution ::
  DimensionContribution
newDimensionContribution =
  DimensionContribution'
    { dimensionName =
        Prelude.Nothing,
      dimensionValueContributionList = Prelude.Nothing
    }

-- | The name of the dimension.
dimensionContribution_dimensionName :: Lens.Lens' DimensionContribution (Prelude.Maybe Prelude.Text)
dimensionContribution_dimensionName = Lens.lens (\DimensionContribution' {dimensionName} -> dimensionName) (\s@DimensionContribution' {} a -> s {dimensionName = a} :: DimensionContribution)

-- | A list of dimension values that contributed to the anomaly.
dimensionContribution_dimensionValueContributionList :: Lens.Lens' DimensionContribution (Prelude.Maybe [DimensionValueContribution])
dimensionContribution_dimensionValueContributionList = Lens.lens (\DimensionContribution' {dimensionValueContributionList} -> dimensionValueContributionList) (\s@DimensionContribution' {} a -> s {dimensionValueContributionList = a} :: DimensionContribution) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DimensionContribution where
  parseJSON =
    Data.withObject
      "DimensionContribution"
      ( \x ->
          DimensionContribution'
            Prelude.<$> (x Data..:? "DimensionName")
            Prelude.<*> ( x Data..:? "DimensionValueContributionList"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable DimensionContribution where
  hashWithSalt _salt DimensionContribution' {..} =
    _salt `Prelude.hashWithSalt` dimensionName
      `Prelude.hashWithSalt` dimensionValueContributionList

instance Prelude.NFData DimensionContribution where
  rnf DimensionContribution' {..} =
    Prelude.rnf dimensionName
      `Prelude.seq` Prelude.rnf dimensionValueContributionList
