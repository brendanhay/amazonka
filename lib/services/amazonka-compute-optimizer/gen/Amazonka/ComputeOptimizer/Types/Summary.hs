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
-- Module      : Amazonka.ComputeOptimizer.Types.Summary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.Summary where

import Amazonka.ComputeOptimizer.Types.Finding
import Amazonka.ComputeOptimizer.Types.ReasonCodeSummary
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The summary of a recommendation.
--
-- /See:/ 'newSummary' smart constructor.
data Summary = Summary'
  { -- | The finding classification of the recommendation.
    name :: Prelude.Maybe Finding,
    -- | An array of objects that summarize a finding reason code.
    reasonCodeSummaries :: Prelude.Maybe [ReasonCodeSummary],
    -- | The value of the recommendation summary.
    value :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Summary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'summary_name' - The finding classification of the recommendation.
--
-- 'reasonCodeSummaries', 'summary_reasonCodeSummaries' - An array of objects that summarize a finding reason code.
--
-- 'value', 'summary_value' - The value of the recommendation summary.
newSummary ::
  Summary
newSummary =
  Summary'
    { name = Prelude.Nothing,
      reasonCodeSummaries = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The finding classification of the recommendation.
summary_name :: Lens.Lens' Summary (Prelude.Maybe Finding)
summary_name = Lens.lens (\Summary' {name} -> name) (\s@Summary' {} a -> s {name = a} :: Summary)

-- | An array of objects that summarize a finding reason code.
summary_reasonCodeSummaries :: Lens.Lens' Summary (Prelude.Maybe [ReasonCodeSummary])
summary_reasonCodeSummaries = Lens.lens (\Summary' {reasonCodeSummaries} -> reasonCodeSummaries) (\s@Summary' {} a -> s {reasonCodeSummaries = a} :: Summary) Prelude.. Lens.mapping Lens.coerced

-- | The value of the recommendation summary.
summary_value :: Lens.Lens' Summary (Prelude.Maybe Prelude.Double)
summary_value = Lens.lens (\Summary' {value} -> value) (\s@Summary' {} a -> s {value = a} :: Summary)

instance Data.FromJSON Summary where
  parseJSON =
    Data.withObject
      "Summary"
      ( \x ->
          Summary'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> ( x Data..:? "reasonCodeSummaries"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable Summary where
  hashWithSalt _salt Summary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` reasonCodeSummaries
      `Prelude.hashWithSalt` value

instance Prelude.NFData Summary where
  rnf Summary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf reasonCodeSummaries
      `Prelude.seq` Prelude.rnf value
