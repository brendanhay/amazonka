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
-- Module      : Amazonka.Pinpoint.Types.MultiConditionalBranch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.MultiConditionalBranch where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.SimpleCondition
import qualified Amazonka.Prelude as Prelude

-- | Specifies a condition to evaluate for an activity path in a journey.
--
-- /See:/ 'newMultiConditionalBranch' smart constructor.
data MultiConditionalBranch = MultiConditionalBranch'
  { -- | The condition to evaluate for the activity path.
    condition :: Prelude.Maybe SimpleCondition,
    -- | The unique identifier for the next activity to perform, after completing
    -- the activity for the path.
    nextActivity :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MultiConditionalBranch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'condition', 'multiConditionalBranch_condition' - The condition to evaluate for the activity path.
--
-- 'nextActivity', 'multiConditionalBranch_nextActivity' - The unique identifier for the next activity to perform, after completing
-- the activity for the path.
newMultiConditionalBranch ::
  MultiConditionalBranch
newMultiConditionalBranch =
  MultiConditionalBranch'
    { condition =
        Prelude.Nothing,
      nextActivity = Prelude.Nothing
    }

-- | The condition to evaluate for the activity path.
multiConditionalBranch_condition :: Lens.Lens' MultiConditionalBranch (Prelude.Maybe SimpleCondition)
multiConditionalBranch_condition = Lens.lens (\MultiConditionalBranch' {condition} -> condition) (\s@MultiConditionalBranch' {} a -> s {condition = a} :: MultiConditionalBranch)

-- | The unique identifier for the next activity to perform, after completing
-- the activity for the path.
multiConditionalBranch_nextActivity :: Lens.Lens' MultiConditionalBranch (Prelude.Maybe Prelude.Text)
multiConditionalBranch_nextActivity = Lens.lens (\MultiConditionalBranch' {nextActivity} -> nextActivity) (\s@MultiConditionalBranch' {} a -> s {nextActivity = a} :: MultiConditionalBranch)

instance Data.FromJSON MultiConditionalBranch where
  parseJSON =
    Data.withObject
      "MultiConditionalBranch"
      ( \x ->
          MultiConditionalBranch'
            Prelude.<$> (x Data..:? "Condition")
            Prelude.<*> (x Data..:? "NextActivity")
      )

instance Prelude.Hashable MultiConditionalBranch where
  hashWithSalt _salt MultiConditionalBranch' {..} =
    _salt `Prelude.hashWithSalt` condition
      `Prelude.hashWithSalt` nextActivity

instance Prelude.NFData MultiConditionalBranch where
  rnf MultiConditionalBranch' {..} =
    Prelude.rnf condition
      `Prelude.seq` Prelude.rnf nextActivity

instance Data.ToJSON MultiConditionalBranch where
  toJSON MultiConditionalBranch' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Condition" Data..=) Prelude.<$> condition,
            ("NextActivity" Data..=) Prelude.<$> nextActivity
          ]
      )
