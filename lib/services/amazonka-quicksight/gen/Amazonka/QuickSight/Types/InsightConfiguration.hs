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
-- Module      : Amazonka.QuickSight.Types.InsightConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.InsightConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.Computation
import Amazonka.QuickSight.Types.CustomNarrativeOptions

-- | The configuration of an insight visual.
--
-- /See:/ 'newInsightConfiguration' smart constructor.
data InsightConfiguration = InsightConfiguration'
  { -- | The computations configurations of the insight visual
    computations :: Prelude.Maybe [Computation],
    -- | The custom narrative of the insight visual.
    customNarrative :: Prelude.Maybe CustomNarrativeOptions
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InsightConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'computations', 'insightConfiguration_computations' - The computations configurations of the insight visual
--
-- 'customNarrative', 'insightConfiguration_customNarrative' - The custom narrative of the insight visual.
newInsightConfiguration ::
  InsightConfiguration
newInsightConfiguration =
  InsightConfiguration'
    { computations =
        Prelude.Nothing,
      customNarrative = Prelude.Nothing
    }

-- | The computations configurations of the insight visual
insightConfiguration_computations :: Lens.Lens' InsightConfiguration (Prelude.Maybe [Computation])
insightConfiguration_computations = Lens.lens (\InsightConfiguration' {computations} -> computations) (\s@InsightConfiguration' {} a -> s {computations = a} :: InsightConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The custom narrative of the insight visual.
insightConfiguration_customNarrative :: Lens.Lens' InsightConfiguration (Prelude.Maybe CustomNarrativeOptions)
insightConfiguration_customNarrative = Lens.lens (\InsightConfiguration' {customNarrative} -> customNarrative) (\s@InsightConfiguration' {} a -> s {customNarrative = a} :: InsightConfiguration)

instance Data.FromJSON InsightConfiguration where
  parseJSON =
    Data.withObject
      "InsightConfiguration"
      ( \x ->
          InsightConfiguration'
            Prelude.<$> (x Data..:? "Computations" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "CustomNarrative")
      )

instance Prelude.Hashable InsightConfiguration where
  hashWithSalt _salt InsightConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` computations
      `Prelude.hashWithSalt` customNarrative

instance Prelude.NFData InsightConfiguration where
  rnf InsightConfiguration' {..} =
    Prelude.rnf computations
      `Prelude.seq` Prelude.rnf customNarrative

instance Data.ToJSON InsightConfiguration where
  toJSON InsightConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Computations" Data..=) Prelude.<$> computations,
            ("CustomNarrative" Data..=)
              Prelude.<$> customNarrative
          ]
      )
