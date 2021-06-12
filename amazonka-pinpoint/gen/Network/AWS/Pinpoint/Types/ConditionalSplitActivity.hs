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
-- Module      : Network.AWS.Pinpoint.Types.ConditionalSplitActivity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ConditionalSplitActivity where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Condition
import Network.AWS.Pinpoint.Types.WaitTime

-- | Specifies the settings for a yes\/no split activity in a journey. This
-- type of activity sends participants down one of two paths in a journey,
-- based on conditions that you specify.
--
-- To create yes\/no split activities that send participants down different
-- paths based on push notification events (such as Open or Received
-- events), your mobile app has to specify the User ID and Endpoint ID
-- values. For more information, see
-- <https://docs.aws.amazon.com/pinpoint/latest/developerguide/integrate.html Integrating Amazon Pinpoint with your application>
-- in the /Amazon Pinpoint Developer Guide/.
--
-- /See:/ 'newConditionalSplitActivity' smart constructor.
data ConditionalSplitActivity = ConditionalSplitActivity'
  { -- | The unique identifier for the activity to perform if the conditions are
    -- met.
    trueActivity :: Core.Maybe Core.Text,
    -- | The conditions that define the paths for the activity, and the
    -- relationship between the conditions.
    condition :: Core.Maybe Condition,
    -- | The amount of time to wait before determining whether the conditions are
    -- met, or the date and time when Amazon Pinpoint determines whether the
    -- conditions are met.
    evaluationWaitTime :: Core.Maybe WaitTime,
    -- | The unique identifier for the activity to perform if the conditions
    -- aren\'t met.
    falseActivity :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConditionalSplitActivity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trueActivity', 'conditionalSplitActivity_trueActivity' - The unique identifier for the activity to perform if the conditions are
-- met.
--
-- 'condition', 'conditionalSplitActivity_condition' - The conditions that define the paths for the activity, and the
-- relationship between the conditions.
--
-- 'evaluationWaitTime', 'conditionalSplitActivity_evaluationWaitTime' - The amount of time to wait before determining whether the conditions are
-- met, or the date and time when Amazon Pinpoint determines whether the
-- conditions are met.
--
-- 'falseActivity', 'conditionalSplitActivity_falseActivity' - The unique identifier for the activity to perform if the conditions
-- aren\'t met.
newConditionalSplitActivity ::
  ConditionalSplitActivity
newConditionalSplitActivity =
  ConditionalSplitActivity'
    { trueActivity =
        Core.Nothing,
      condition = Core.Nothing,
      evaluationWaitTime = Core.Nothing,
      falseActivity = Core.Nothing
    }

-- | The unique identifier for the activity to perform if the conditions are
-- met.
conditionalSplitActivity_trueActivity :: Lens.Lens' ConditionalSplitActivity (Core.Maybe Core.Text)
conditionalSplitActivity_trueActivity = Lens.lens (\ConditionalSplitActivity' {trueActivity} -> trueActivity) (\s@ConditionalSplitActivity' {} a -> s {trueActivity = a} :: ConditionalSplitActivity)

-- | The conditions that define the paths for the activity, and the
-- relationship between the conditions.
conditionalSplitActivity_condition :: Lens.Lens' ConditionalSplitActivity (Core.Maybe Condition)
conditionalSplitActivity_condition = Lens.lens (\ConditionalSplitActivity' {condition} -> condition) (\s@ConditionalSplitActivity' {} a -> s {condition = a} :: ConditionalSplitActivity)

-- | The amount of time to wait before determining whether the conditions are
-- met, or the date and time when Amazon Pinpoint determines whether the
-- conditions are met.
conditionalSplitActivity_evaluationWaitTime :: Lens.Lens' ConditionalSplitActivity (Core.Maybe WaitTime)
conditionalSplitActivity_evaluationWaitTime = Lens.lens (\ConditionalSplitActivity' {evaluationWaitTime} -> evaluationWaitTime) (\s@ConditionalSplitActivity' {} a -> s {evaluationWaitTime = a} :: ConditionalSplitActivity)

-- | The unique identifier for the activity to perform if the conditions
-- aren\'t met.
conditionalSplitActivity_falseActivity :: Lens.Lens' ConditionalSplitActivity (Core.Maybe Core.Text)
conditionalSplitActivity_falseActivity = Lens.lens (\ConditionalSplitActivity' {falseActivity} -> falseActivity) (\s@ConditionalSplitActivity' {} a -> s {falseActivity = a} :: ConditionalSplitActivity)

instance Core.FromJSON ConditionalSplitActivity where
  parseJSON =
    Core.withObject
      "ConditionalSplitActivity"
      ( \x ->
          ConditionalSplitActivity'
            Core.<$> (x Core..:? "TrueActivity")
            Core.<*> (x Core..:? "Condition")
            Core.<*> (x Core..:? "EvaluationWaitTime")
            Core.<*> (x Core..:? "FalseActivity")
      )

instance Core.Hashable ConditionalSplitActivity

instance Core.NFData ConditionalSplitActivity

instance Core.ToJSON ConditionalSplitActivity where
  toJSON ConditionalSplitActivity' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TrueActivity" Core..=) Core.<$> trueActivity,
            ("Condition" Core..=) Core.<$> condition,
            ("EvaluationWaitTime" Core..=)
              Core.<$> evaluationWaitTime,
            ("FalseActivity" Core..=) Core.<$> falseActivity
          ]
      )
