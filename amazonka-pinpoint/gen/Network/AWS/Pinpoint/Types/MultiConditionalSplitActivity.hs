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
-- Module      : Network.AWS.Pinpoint.Types.MultiConditionalSplitActivity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.MultiConditionalSplitActivity where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.MultiConditionalBranch
import Network.AWS.Pinpoint.Types.WaitTime

-- | Specifies the settings for a multivariate split activity in a journey.
-- This type of activity sends participants down one of as many as five
-- paths (including a default /Else/ path) in a journey, based on
-- conditions that you specify.
--
-- To create multivariate split activities that send participants down
-- different paths based on push notification events (such as Open or
-- Received events), your mobile app has to specify the User ID and
-- Endpoint ID values. For more information, see
-- <https://docs.aws.amazon.com/pinpoint/latest/developerguide/integrate.html Integrating Amazon Pinpoint with your application>
-- in the /Amazon Pinpoint Developer Guide/.
--
-- /See:/ 'newMultiConditionalSplitActivity' smart constructor.
data MultiConditionalSplitActivity = MultiConditionalSplitActivity'
  { -- | The unique identifier for the activity to perform for participants who
    -- don\'t meet any of the conditions specified for other paths in the
    -- activity.
    defaultActivity :: Core.Maybe Core.Text,
    -- | The amount of time to wait or the date and time when Amazon Pinpoint
    -- determines whether the conditions are met.
    evaluationWaitTime :: Core.Maybe WaitTime,
    -- | The paths for the activity, including the conditions for entering each
    -- path and the activity to perform for each path.
    branches :: Core.Maybe [MultiConditionalBranch]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MultiConditionalSplitActivity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultActivity', 'multiConditionalSplitActivity_defaultActivity' - The unique identifier for the activity to perform for participants who
-- don\'t meet any of the conditions specified for other paths in the
-- activity.
--
-- 'evaluationWaitTime', 'multiConditionalSplitActivity_evaluationWaitTime' - The amount of time to wait or the date and time when Amazon Pinpoint
-- determines whether the conditions are met.
--
-- 'branches', 'multiConditionalSplitActivity_branches' - The paths for the activity, including the conditions for entering each
-- path and the activity to perform for each path.
newMultiConditionalSplitActivity ::
  MultiConditionalSplitActivity
newMultiConditionalSplitActivity =
  MultiConditionalSplitActivity'
    { defaultActivity =
        Core.Nothing,
      evaluationWaitTime = Core.Nothing,
      branches = Core.Nothing
    }

-- | The unique identifier for the activity to perform for participants who
-- don\'t meet any of the conditions specified for other paths in the
-- activity.
multiConditionalSplitActivity_defaultActivity :: Lens.Lens' MultiConditionalSplitActivity (Core.Maybe Core.Text)
multiConditionalSplitActivity_defaultActivity = Lens.lens (\MultiConditionalSplitActivity' {defaultActivity} -> defaultActivity) (\s@MultiConditionalSplitActivity' {} a -> s {defaultActivity = a} :: MultiConditionalSplitActivity)

-- | The amount of time to wait or the date and time when Amazon Pinpoint
-- determines whether the conditions are met.
multiConditionalSplitActivity_evaluationWaitTime :: Lens.Lens' MultiConditionalSplitActivity (Core.Maybe WaitTime)
multiConditionalSplitActivity_evaluationWaitTime = Lens.lens (\MultiConditionalSplitActivity' {evaluationWaitTime} -> evaluationWaitTime) (\s@MultiConditionalSplitActivity' {} a -> s {evaluationWaitTime = a} :: MultiConditionalSplitActivity)

-- | The paths for the activity, including the conditions for entering each
-- path and the activity to perform for each path.
multiConditionalSplitActivity_branches :: Lens.Lens' MultiConditionalSplitActivity (Core.Maybe [MultiConditionalBranch])
multiConditionalSplitActivity_branches = Lens.lens (\MultiConditionalSplitActivity' {branches} -> branches) (\s@MultiConditionalSplitActivity' {} a -> s {branches = a} :: MultiConditionalSplitActivity) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON MultiConditionalSplitActivity where
  parseJSON =
    Core.withObject
      "MultiConditionalSplitActivity"
      ( \x ->
          MultiConditionalSplitActivity'
            Core.<$> (x Core..:? "DefaultActivity")
            Core.<*> (x Core..:? "EvaluationWaitTime")
            Core.<*> (x Core..:? "Branches" Core..!= Core.mempty)
      )

instance Core.Hashable MultiConditionalSplitActivity

instance Core.NFData MultiConditionalSplitActivity

instance Core.ToJSON MultiConditionalSplitActivity where
  toJSON MultiConditionalSplitActivity' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DefaultActivity" Core..=)
              Core.<$> defaultActivity,
            ("EvaluationWaitTime" Core..=)
              Core.<$> evaluationWaitTime,
            ("Branches" Core..=) Core.<$> branches
          ]
      )
