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
-- Module      : Amazonka.ResilienceHub.Types.ResiliencyScore
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.ResiliencyScore where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResilienceHub.Types.DisruptionType

-- | The overall resiliency score, returned as an object that includes the
-- disruption score and outage score.
--
-- /See:/ 'newResiliencyScore' smart constructor.
data ResiliencyScore = ResiliencyScore'
  { -- | The disruption score for a valid key.
    disruptionScore :: Prelude.HashMap DisruptionType Prelude.Double,
    -- | The outage score for a valid key.
    score :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResiliencyScore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disruptionScore', 'resiliencyScore_disruptionScore' - The disruption score for a valid key.
--
-- 'score', 'resiliencyScore_score' - The outage score for a valid key.
newResiliencyScore ::
  -- | 'score'
  Prelude.Double ->
  ResiliencyScore
newResiliencyScore pScore_ =
  ResiliencyScore'
    { disruptionScore = Prelude.mempty,
      score = pScore_
    }

-- | The disruption score for a valid key.
resiliencyScore_disruptionScore :: Lens.Lens' ResiliencyScore (Prelude.HashMap DisruptionType Prelude.Double)
resiliencyScore_disruptionScore = Lens.lens (\ResiliencyScore' {disruptionScore} -> disruptionScore) (\s@ResiliencyScore' {} a -> s {disruptionScore = a} :: ResiliencyScore) Prelude.. Lens.coerced

-- | The outage score for a valid key.
resiliencyScore_score :: Lens.Lens' ResiliencyScore Prelude.Double
resiliencyScore_score = Lens.lens (\ResiliencyScore' {score} -> score) (\s@ResiliencyScore' {} a -> s {score = a} :: ResiliencyScore)

instance Data.FromJSON ResiliencyScore where
  parseJSON =
    Data.withObject
      "ResiliencyScore"
      ( \x ->
          ResiliencyScore'
            Prelude.<$> ( x
                            Data..:? "disruptionScore"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "score")
      )

instance Prelude.Hashable ResiliencyScore where
  hashWithSalt _salt ResiliencyScore' {..} =
    _salt
      `Prelude.hashWithSalt` disruptionScore
      `Prelude.hashWithSalt` score

instance Prelude.NFData ResiliencyScore where
  rnf ResiliencyScore' {..} =
    Prelude.rnf disruptionScore
      `Prelude.seq` Prelude.rnf score
