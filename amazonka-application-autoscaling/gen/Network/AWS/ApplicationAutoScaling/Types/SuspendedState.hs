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
-- Module      : Network.AWS.ApplicationAutoScaling.Types.SuspendedState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.SuspendedState where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies whether the scaling activities for a scalable target are in a
-- suspended state.
--
-- /See:/ 'newSuspendedState' smart constructor.
data SuspendedState = SuspendedState'
  { -- | Whether scheduled scaling is suspended. Set the value to @true@ if you
    -- don\'t want Application Auto Scaling to add or remove capacity by
    -- initiating scheduled actions. The default is @false@.
    scheduledScalingSuspended :: Core.Maybe Core.Bool,
    -- | Whether scale in by a target tracking scaling policy or a step scaling
    -- policy is suspended. Set the value to @true@ if you don\'t want
    -- Application Auto Scaling to remove capacity when a scaling policy is
    -- triggered. The default is @false@.
    dynamicScalingInSuspended :: Core.Maybe Core.Bool,
    -- | Whether scale out by a target tracking scaling policy or a step scaling
    -- policy is suspended. Set the value to @true@ if you don\'t want
    -- Application Auto Scaling to add capacity when a scaling policy is
    -- triggered. The default is @false@.
    dynamicScalingOutSuspended :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SuspendedState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduledScalingSuspended', 'suspendedState_scheduledScalingSuspended' - Whether scheduled scaling is suspended. Set the value to @true@ if you
-- don\'t want Application Auto Scaling to add or remove capacity by
-- initiating scheduled actions. The default is @false@.
--
-- 'dynamicScalingInSuspended', 'suspendedState_dynamicScalingInSuspended' - Whether scale in by a target tracking scaling policy or a step scaling
-- policy is suspended. Set the value to @true@ if you don\'t want
-- Application Auto Scaling to remove capacity when a scaling policy is
-- triggered. The default is @false@.
--
-- 'dynamicScalingOutSuspended', 'suspendedState_dynamicScalingOutSuspended' - Whether scale out by a target tracking scaling policy or a step scaling
-- policy is suspended. Set the value to @true@ if you don\'t want
-- Application Auto Scaling to add capacity when a scaling policy is
-- triggered. The default is @false@.
newSuspendedState ::
  SuspendedState
newSuspendedState =
  SuspendedState'
    { scheduledScalingSuspended =
        Core.Nothing,
      dynamicScalingInSuspended = Core.Nothing,
      dynamicScalingOutSuspended = Core.Nothing
    }

-- | Whether scheduled scaling is suspended. Set the value to @true@ if you
-- don\'t want Application Auto Scaling to add or remove capacity by
-- initiating scheduled actions. The default is @false@.
suspendedState_scheduledScalingSuspended :: Lens.Lens' SuspendedState (Core.Maybe Core.Bool)
suspendedState_scheduledScalingSuspended = Lens.lens (\SuspendedState' {scheduledScalingSuspended} -> scheduledScalingSuspended) (\s@SuspendedState' {} a -> s {scheduledScalingSuspended = a} :: SuspendedState)

-- | Whether scale in by a target tracking scaling policy or a step scaling
-- policy is suspended. Set the value to @true@ if you don\'t want
-- Application Auto Scaling to remove capacity when a scaling policy is
-- triggered. The default is @false@.
suspendedState_dynamicScalingInSuspended :: Lens.Lens' SuspendedState (Core.Maybe Core.Bool)
suspendedState_dynamicScalingInSuspended = Lens.lens (\SuspendedState' {dynamicScalingInSuspended} -> dynamicScalingInSuspended) (\s@SuspendedState' {} a -> s {dynamicScalingInSuspended = a} :: SuspendedState)

-- | Whether scale out by a target tracking scaling policy or a step scaling
-- policy is suspended. Set the value to @true@ if you don\'t want
-- Application Auto Scaling to add capacity when a scaling policy is
-- triggered. The default is @false@.
suspendedState_dynamicScalingOutSuspended :: Lens.Lens' SuspendedState (Core.Maybe Core.Bool)
suspendedState_dynamicScalingOutSuspended = Lens.lens (\SuspendedState' {dynamicScalingOutSuspended} -> dynamicScalingOutSuspended) (\s@SuspendedState' {} a -> s {dynamicScalingOutSuspended = a} :: SuspendedState)

instance Core.FromJSON SuspendedState where
  parseJSON =
    Core.withObject
      "SuspendedState"
      ( \x ->
          SuspendedState'
            Core.<$> (x Core..:? "ScheduledScalingSuspended")
            Core.<*> (x Core..:? "DynamicScalingInSuspended")
            Core.<*> (x Core..:? "DynamicScalingOutSuspended")
      )

instance Core.Hashable SuspendedState

instance Core.NFData SuspendedState

instance Core.ToJSON SuspendedState where
  toJSON SuspendedState' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ScheduledScalingSuspended" Core..=)
              Core.<$> scheduledScalingSuspended,
            ("DynamicScalingInSuspended" Core..=)
              Core.<$> dynamicScalingInSuspended,
            ("DynamicScalingOutSuspended" Core..=)
              Core.<$> dynamicScalingOutSuspended
          ]
      )
