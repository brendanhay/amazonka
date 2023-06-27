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
-- Module      : Amazonka.LookoutMetrics.Types.ContributionMatrix
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.ContributionMatrix where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types.DimensionContribution
import qualified Amazonka.Prelude as Prelude

-- | Details about dimensions that contributed to an anomaly.
--
-- /See:/ 'newContributionMatrix' smart constructor.
data ContributionMatrix = ContributionMatrix'
  { -- | A list of contributing dimensions.
    dimensionContributionList :: Prelude.Maybe [DimensionContribution]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContributionMatrix' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensionContributionList', 'contributionMatrix_dimensionContributionList' - A list of contributing dimensions.
newContributionMatrix ::
  ContributionMatrix
newContributionMatrix =
  ContributionMatrix'
    { dimensionContributionList =
        Prelude.Nothing
    }

-- | A list of contributing dimensions.
contributionMatrix_dimensionContributionList :: Lens.Lens' ContributionMatrix (Prelude.Maybe [DimensionContribution])
contributionMatrix_dimensionContributionList = Lens.lens (\ContributionMatrix' {dimensionContributionList} -> dimensionContributionList) (\s@ContributionMatrix' {} a -> s {dimensionContributionList = a} :: ContributionMatrix) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ContributionMatrix where
  parseJSON =
    Data.withObject
      "ContributionMatrix"
      ( \x ->
          ContributionMatrix'
            Prelude.<$> ( x
                            Data..:? "DimensionContributionList"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ContributionMatrix where
  hashWithSalt _salt ContributionMatrix' {..} =
    _salt
      `Prelude.hashWithSalt` dimensionContributionList

instance Prelude.NFData ContributionMatrix where
  rnf ContributionMatrix' {..} =
    Prelude.rnf dimensionContributionList
