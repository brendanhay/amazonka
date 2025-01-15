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
-- Module      : Amazonka.QuickSight.Types.ContributionAnalysisDefault
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ContributionAnalysisDefault where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnIdentifier

-- | The contribution analysis visual display for a line, pie, or bar chart.
--
-- /See:/ 'newContributionAnalysisDefault' smart constructor.
data ContributionAnalysisDefault = ContributionAnalysisDefault'
  { -- | The measure field that is used in the contribution analysis.
    measureFieldId :: Prelude.Text,
    -- | The dimensions columns that are used in the contribution analysis,
    -- usually a list of @ColumnIdentifiers@.
    contributorDimensions :: Prelude.NonEmpty ColumnIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContributionAnalysisDefault' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'measureFieldId', 'contributionAnalysisDefault_measureFieldId' - The measure field that is used in the contribution analysis.
--
-- 'contributorDimensions', 'contributionAnalysisDefault_contributorDimensions' - The dimensions columns that are used in the contribution analysis,
-- usually a list of @ColumnIdentifiers@.
newContributionAnalysisDefault ::
  -- | 'measureFieldId'
  Prelude.Text ->
  -- | 'contributorDimensions'
  Prelude.NonEmpty ColumnIdentifier ->
  ContributionAnalysisDefault
newContributionAnalysisDefault
  pMeasureFieldId_
  pContributorDimensions_ =
    ContributionAnalysisDefault'
      { measureFieldId =
          pMeasureFieldId_,
        contributorDimensions =
          Lens.coerced Lens.# pContributorDimensions_
      }

-- | The measure field that is used in the contribution analysis.
contributionAnalysisDefault_measureFieldId :: Lens.Lens' ContributionAnalysisDefault Prelude.Text
contributionAnalysisDefault_measureFieldId = Lens.lens (\ContributionAnalysisDefault' {measureFieldId} -> measureFieldId) (\s@ContributionAnalysisDefault' {} a -> s {measureFieldId = a} :: ContributionAnalysisDefault)

-- | The dimensions columns that are used in the contribution analysis,
-- usually a list of @ColumnIdentifiers@.
contributionAnalysisDefault_contributorDimensions :: Lens.Lens' ContributionAnalysisDefault (Prelude.NonEmpty ColumnIdentifier)
contributionAnalysisDefault_contributorDimensions = Lens.lens (\ContributionAnalysisDefault' {contributorDimensions} -> contributorDimensions) (\s@ContributionAnalysisDefault' {} a -> s {contributorDimensions = a} :: ContributionAnalysisDefault) Prelude.. Lens.coerced

instance Data.FromJSON ContributionAnalysisDefault where
  parseJSON =
    Data.withObject
      "ContributionAnalysisDefault"
      ( \x ->
          ContributionAnalysisDefault'
            Prelude.<$> (x Data..: "MeasureFieldId")
            Prelude.<*> (x Data..: "ContributorDimensions")
      )

instance Prelude.Hashable ContributionAnalysisDefault where
  hashWithSalt _salt ContributionAnalysisDefault' {..} =
    _salt
      `Prelude.hashWithSalt` measureFieldId
      `Prelude.hashWithSalt` contributorDimensions

instance Prelude.NFData ContributionAnalysisDefault where
  rnf ContributionAnalysisDefault' {..} =
    Prelude.rnf measureFieldId `Prelude.seq`
      Prelude.rnf contributorDimensions

instance Data.ToJSON ContributionAnalysisDefault where
  toJSON ContributionAnalysisDefault' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("MeasureFieldId" Data..= measureFieldId),
            Prelude.Just
              ( "ContributorDimensions"
                  Data..= contributorDimensions
              )
          ]
      )
