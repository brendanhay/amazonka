{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Glue.Types.FindMatchesParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.FindMatchesParameters where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The parameters to configure the find matches transform.
--
-- /See:/ 'newFindMatchesParameters' smart constructor.
data FindMatchesParameters = FindMatchesParameters'
  { -- | The value that is selected when tuning your transform for a balance
    -- between accuracy and cost. A value of 0.5 means that the system balances
    -- accuracy and cost concerns. A value of 1.0 means a bias purely for
    -- accuracy, which typically results in a higher cost, sometimes
    -- substantially higher. A value of 0.0 means a bias purely for cost, which
    -- results in a less accurate @FindMatches@ transform, sometimes with
    -- unacceptable accuracy.
    --
    -- Accuracy measures how well the transform finds true positives and true
    -- negatives. Increasing accuracy requires more machine resources and cost.
    -- But it also results in increased recall.
    --
    -- Cost measures how many compute resources, and thus money, are consumed
    -- to run the transform.
    accuracyCostTradeoff :: Prelude.Maybe Prelude.Double,
    -- | The value to switch on or off to force the output to match the provided
    -- labels from users. If the value is @True@, the @find matches@ transform
    -- forces the output to match the provided labels. The results override the
    -- normal conflation results. If the value is @False@, the @find matches@
    -- transform does not ensure all the labels provided are respected, and the
    -- results rely on the trained model.
    --
    -- Note that setting this value to true may increase the conflation
    -- execution time.
    enforceProvidedLabels :: Prelude.Maybe Prelude.Bool,
    -- | The value selected when tuning your transform for a balance between
    -- precision and recall. A value of 0.5 means no preference; a value of 1.0
    -- means a bias purely for precision, and a value of 0.0 means a bias for
    -- recall. Because this is a tradeoff, choosing values close to 1.0 means
    -- very low recall, and choosing values close to 0.0 results in very low
    -- precision.
    --
    -- The precision metric indicates how often your model is correct when it
    -- predicts a match.
    --
    -- The recall metric indicates that for an actual match, how often your
    -- model predicts the match.
    precisionRecallTradeoff :: Prelude.Maybe Prelude.Double,
    -- | The name of a column that uniquely identifies rows in the source table.
    -- Used to help identify matching records.
    primaryKeyColumnName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FindMatchesParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accuracyCostTradeoff', 'findMatchesParameters_accuracyCostTradeoff' - The value that is selected when tuning your transform for a balance
-- between accuracy and cost. A value of 0.5 means that the system balances
-- accuracy and cost concerns. A value of 1.0 means a bias purely for
-- accuracy, which typically results in a higher cost, sometimes
-- substantially higher. A value of 0.0 means a bias purely for cost, which
-- results in a less accurate @FindMatches@ transform, sometimes with
-- unacceptable accuracy.
--
-- Accuracy measures how well the transform finds true positives and true
-- negatives. Increasing accuracy requires more machine resources and cost.
-- But it also results in increased recall.
--
-- Cost measures how many compute resources, and thus money, are consumed
-- to run the transform.
--
-- 'enforceProvidedLabels', 'findMatchesParameters_enforceProvidedLabels' - The value to switch on or off to force the output to match the provided
-- labels from users. If the value is @True@, the @find matches@ transform
-- forces the output to match the provided labels. The results override the
-- normal conflation results. If the value is @False@, the @find matches@
-- transform does not ensure all the labels provided are respected, and the
-- results rely on the trained model.
--
-- Note that setting this value to true may increase the conflation
-- execution time.
--
-- 'precisionRecallTradeoff', 'findMatchesParameters_precisionRecallTradeoff' - The value selected when tuning your transform for a balance between
-- precision and recall. A value of 0.5 means no preference; a value of 1.0
-- means a bias purely for precision, and a value of 0.0 means a bias for
-- recall. Because this is a tradeoff, choosing values close to 1.0 means
-- very low recall, and choosing values close to 0.0 results in very low
-- precision.
--
-- The precision metric indicates how often your model is correct when it
-- predicts a match.
--
-- The recall metric indicates that for an actual match, how often your
-- model predicts the match.
--
-- 'primaryKeyColumnName', 'findMatchesParameters_primaryKeyColumnName' - The name of a column that uniquely identifies rows in the source table.
-- Used to help identify matching records.
newFindMatchesParameters ::
  FindMatchesParameters
newFindMatchesParameters =
  FindMatchesParameters'
    { accuracyCostTradeoff =
        Prelude.Nothing,
      enforceProvidedLabels = Prelude.Nothing,
      precisionRecallTradeoff = Prelude.Nothing,
      primaryKeyColumnName = Prelude.Nothing
    }

-- | The value that is selected when tuning your transform for a balance
-- between accuracy and cost. A value of 0.5 means that the system balances
-- accuracy and cost concerns. A value of 1.0 means a bias purely for
-- accuracy, which typically results in a higher cost, sometimes
-- substantially higher. A value of 0.0 means a bias purely for cost, which
-- results in a less accurate @FindMatches@ transform, sometimes with
-- unacceptable accuracy.
--
-- Accuracy measures how well the transform finds true positives and true
-- negatives. Increasing accuracy requires more machine resources and cost.
-- But it also results in increased recall.
--
-- Cost measures how many compute resources, and thus money, are consumed
-- to run the transform.
findMatchesParameters_accuracyCostTradeoff :: Lens.Lens' FindMatchesParameters (Prelude.Maybe Prelude.Double)
findMatchesParameters_accuracyCostTradeoff = Lens.lens (\FindMatchesParameters' {accuracyCostTradeoff} -> accuracyCostTradeoff) (\s@FindMatchesParameters' {} a -> s {accuracyCostTradeoff = a} :: FindMatchesParameters)

-- | The value to switch on or off to force the output to match the provided
-- labels from users. If the value is @True@, the @find matches@ transform
-- forces the output to match the provided labels. The results override the
-- normal conflation results. If the value is @False@, the @find matches@
-- transform does not ensure all the labels provided are respected, and the
-- results rely on the trained model.
--
-- Note that setting this value to true may increase the conflation
-- execution time.
findMatchesParameters_enforceProvidedLabels :: Lens.Lens' FindMatchesParameters (Prelude.Maybe Prelude.Bool)
findMatchesParameters_enforceProvidedLabels = Lens.lens (\FindMatchesParameters' {enforceProvidedLabels} -> enforceProvidedLabels) (\s@FindMatchesParameters' {} a -> s {enforceProvidedLabels = a} :: FindMatchesParameters)

-- | The value selected when tuning your transform for a balance between
-- precision and recall. A value of 0.5 means no preference; a value of 1.0
-- means a bias purely for precision, and a value of 0.0 means a bias for
-- recall. Because this is a tradeoff, choosing values close to 1.0 means
-- very low recall, and choosing values close to 0.0 results in very low
-- precision.
--
-- The precision metric indicates how often your model is correct when it
-- predicts a match.
--
-- The recall metric indicates that for an actual match, how often your
-- model predicts the match.
findMatchesParameters_precisionRecallTradeoff :: Lens.Lens' FindMatchesParameters (Prelude.Maybe Prelude.Double)
findMatchesParameters_precisionRecallTradeoff = Lens.lens (\FindMatchesParameters' {precisionRecallTradeoff} -> precisionRecallTradeoff) (\s@FindMatchesParameters' {} a -> s {precisionRecallTradeoff = a} :: FindMatchesParameters)

-- | The name of a column that uniquely identifies rows in the source table.
-- Used to help identify matching records.
findMatchesParameters_primaryKeyColumnName :: Lens.Lens' FindMatchesParameters (Prelude.Maybe Prelude.Text)
findMatchesParameters_primaryKeyColumnName = Lens.lens (\FindMatchesParameters' {primaryKeyColumnName} -> primaryKeyColumnName) (\s@FindMatchesParameters' {} a -> s {primaryKeyColumnName = a} :: FindMatchesParameters)

instance Prelude.FromJSON FindMatchesParameters where
  parseJSON =
    Prelude.withObject
      "FindMatchesParameters"
      ( \x ->
          FindMatchesParameters'
            Prelude.<$> (x Prelude..:? "AccuracyCostTradeoff")
            Prelude.<*> (x Prelude..:? "EnforceProvidedLabels")
            Prelude.<*> (x Prelude..:? "PrecisionRecallTradeoff")
            Prelude.<*> (x Prelude..:? "PrimaryKeyColumnName")
      )

instance Prelude.Hashable FindMatchesParameters

instance Prelude.NFData FindMatchesParameters

instance Prelude.ToJSON FindMatchesParameters where
  toJSON FindMatchesParameters' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AccuracyCostTradeoff" Prelude..=)
              Prelude.<$> accuracyCostTradeoff,
            ("EnforceProvidedLabels" Prelude..=)
              Prelude.<$> enforceProvidedLabels,
            ("PrecisionRecallTradeoff" Prelude..=)
              Prelude.<$> precisionRecallTradeoff,
            ("PrimaryKeyColumnName" Prelude..=)
              Prelude.<$> primaryKeyColumnName
          ]
      )
