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
-- Module      : Amazonka.CodeGuruProfiler.Types.Pattern
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruProfiler.Types.Pattern where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A set of rules used to make a recommendation during an analysis.
--
-- /See:/ 'newPattern' smart constructor.
data Pattern = Pattern'
  { -- | A list of the different counters used to determine if there is a match.
    countersToAggregate :: Prelude.Maybe [Prelude.Text],
    -- | The description of the recommendation. This explains a potential
    -- inefficiency in a profiled application.
    description :: Prelude.Maybe Prelude.Text,
    -- | The universally unique identifier (UUID) of this pattern.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name for this pattern.
    name :: Prelude.Maybe Prelude.Text,
    -- | A string that contains the steps recommended to address the potential
    -- inefficiency.
    resolutionSteps :: Prelude.Maybe Prelude.Text,
    -- | A list of frame names that were searched during the analysis that
    -- generated a recommendation.
    targetFrames :: Prelude.Maybe [[Prelude.Text]],
    -- | The percentage of time an application spends in one method that triggers
    -- a recommendation. The percentage of time is the same as the percentage
    -- of the total gathered sample counts during analysis.
    thresholdPercent :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Pattern' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'countersToAggregate', 'pattern_countersToAggregate' - A list of the different counters used to determine if there is a match.
--
-- 'description', 'pattern_description' - The description of the recommendation. This explains a potential
-- inefficiency in a profiled application.
--
-- 'id', 'pattern_id' - The universally unique identifier (UUID) of this pattern.
--
-- 'name', 'pattern_name' - The name for this pattern.
--
-- 'resolutionSteps', 'pattern_resolutionSteps' - A string that contains the steps recommended to address the potential
-- inefficiency.
--
-- 'targetFrames', 'pattern_targetFrames' - A list of frame names that were searched during the analysis that
-- generated a recommendation.
--
-- 'thresholdPercent', 'pattern_thresholdPercent' - The percentage of time an application spends in one method that triggers
-- a recommendation. The percentage of time is the same as the percentage
-- of the total gathered sample counts during analysis.
newPattern ::
  Pattern
newPattern =
  Pattern'
    { countersToAggregate = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      resolutionSteps = Prelude.Nothing,
      targetFrames = Prelude.Nothing,
      thresholdPercent = Prelude.Nothing
    }

-- | A list of the different counters used to determine if there is a match.
pattern_countersToAggregate :: Lens.Lens' Pattern (Prelude.Maybe [Prelude.Text])
pattern_countersToAggregate = Lens.lens (\Pattern' {countersToAggregate} -> countersToAggregate) (\s@Pattern' {} a -> s {countersToAggregate = a} :: Pattern) Prelude.. Lens.mapping Lens.coerced

-- | The description of the recommendation. This explains a potential
-- inefficiency in a profiled application.
pattern_description :: Lens.Lens' Pattern (Prelude.Maybe Prelude.Text)
pattern_description = Lens.lens (\Pattern' {description} -> description) (\s@Pattern' {} a -> s {description = a} :: Pattern)

-- | The universally unique identifier (UUID) of this pattern.
pattern_id :: Lens.Lens' Pattern (Prelude.Maybe Prelude.Text)
pattern_id = Lens.lens (\Pattern' {id} -> id) (\s@Pattern' {} a -> s {id = a} :: Pattern)

-- | The name for this pattern.
pattern_name :: Lens.Lens' Pattern (Prelude.Maybe Prelude.Text)
pattern_name = Lens.lens (\Pattern' {name} -> name) (\s@Pattern' {} a -> s {name = a} :: Pattern)

-- | A string that contains the steps recommended to address the potential
-- inefficiency.
pattern_resolutionSteps :: Lens.Lens' Pattern (Prelude.Maybe Prelude.Text)
pattern_resolutionSteps = Lens.lens (\Pattern' {resolutionSteps} -> resolutionSteps) (\s@Pattern' {} a -> s {resolutionSteps = a} :: Pattern)

-- | A list of frame names that were searched during the analysis that
-- generated a recommendation.
pattern_targetFrames :: Lens.Lens' Pattern (Prelude.Maybe [[Prelude.Text]])
pattern_targetFrames = Lens.lens (\Pattern' {targetFrames} -> targetFrames) (\s@Pattern' {} a -> s {targetFrames = a} :: Pattern) Prelude.. Lens.mapping Lens.coerced

-- | The percentage of time an application spends in one method that triggers
-- a recommendation. The percentage of time is the same as the percentage
-- of the total gathered sample counts during analysis.
pattern_thresholdPercent :: Lens.Lens' Pattern (Prelude.Maybe Prelude.Double)
pattern_thresholdPercent = Lens.lens (\Pattern' {thresholdPercent} -> thresholdPercent) (\s@Pattern' {} a -> s {thresholdPercent = a} :: Pattern)

instance Data.FromJSON Pattern where
  parseJSON =
    Data.withObject
      "Pattern"
      ( \x ->
          Pattern'
            Prelude.<$> ( x
                            Data..:? "countersToAggregate"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "resolutionSteps")
            Prelude.<*> (x Data..:? "targetFrames" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "thresholdPercent")
      )

instance Prelude.Hashable Pattern where
  hashWithSalt _salt Pattern' {..} =
    _salt
      `Prelude.hashWithSalt` countersToAggregate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` resolutionSteps
      `Prelude.hashWithSalt` targetFrames
      `Prelude.hashWithSalt` thresholdPercent

instance Prelude.NFData Pattern where
  rnf Pattern' {..} =
    Prelude.rnf countersToAggregate `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf id `Prelude.seq`
          Prelude.rnf name `Prelude.seq`
            Prelude.rnf resolutionSteps `Prelude.seq`
              Prelude.rnf targetFrames `Prelude.seq`
                Prelude.rnf thresholdPercent
