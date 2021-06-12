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
-- Module      : Network.AWS.Pinpoint.Types.MultiConditionalBranch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.MultiConditionalBranch where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.SimpleCondition

-- | Specifies a condition to evaluate for an activity path in a journey.
--
-- /See:/ 'newMultiConditionalBranch' smart constructor.
data MultiConditionalBranch = MultiConditionalBranch'
  { -- | The condition to evaluate for the activity path.
    condition :: Core.Maybe SimpleCondition,
    -- | The unique identifier for the next activity to perform, after completing
    -- the activity for the path.
    nextActivity :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { condition = Core.Nothing,
      nextActivity = Core.Nothing
    }

-- | The condition to evaluate for the activity path.
multiConditionalBranch_condition :: Lens.Lens' MultiConditionalBranch (Core.Maybe SimpleCondition)
multiConditionalBranch_condition = Lens.lens (\MultiConditionalBranch' {condition} -> condition) (\s@MultiConditionalBranch' {} a -> s {condition = a} :: MultiConditionalBranch)

-- | The unique identifier for the next activity to perform, after completing
-- the activity for the path.
multiConditionalBranch_nextActivity :: Lens.Lens' MultiConditionalBranch (Core.Maybe Core.Text)
multiConditionalBranch_nextActivity = Lens.lens (\MultiConditionalBranch' {nextActivity} -> nextActivity) (\s@MultiConditionalBranch' {} a -> s {nextActivity = a} :: MultiConditionalBranch)

instance Core.FromJSON MultiConditionalBranch where
  parseJSON =
    Core.withObject
      "MultiConditionalBranch"
      ( \x ->
          MultiConditionalBranch'
            Core.<$> (x Core..:? "Condition")
            Core.<*> (x Core..:? "NextActivity")
      )

instance Core.Hashable MultiConditionalBranch

instance Core.NFData MultiConditionalBranch

instance Core.ToJSON MultiConditionalBranch where
  toJSON MultiConditionalBranch' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Condition" Core..=) Core.<$> condition,
            ("NextActivity" Core..=) Core.<$> nextActivity
          ]
      )
