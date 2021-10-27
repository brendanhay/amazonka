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
-- Module      : Network.AWS.LookoutMetrics.Types.ContributionMatrix
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LookoutMetrics.Types.ContributionMatrix where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LookoutMetrics.Types.DimensionContribution
import qualified Network.AWS.Prelude as Prelude

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

instance Core.FromJSON ContributionMatrix where
  parseJSON =
    Core.withObject
      "ContributionMatrix"
      ( \x ->
          ContributionMatrix'
            Prelude.<$> ( x Core..:? "DimensionContributionList"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ContributionMatrix

instance Prelude.NFData ContributionMatrix
