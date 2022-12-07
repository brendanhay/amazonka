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
-- Module      : Amazonka.ApplicationAutoScaling.Types.SuspendedState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationAutoScaling.Types.SuspendedState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies whether the scaling activities for a scalable target are in a
-- suspended state.
--
-- /See:/ 'newSuspendedState' smart constructor.
data SuspendedState = SuspendedState'
  { -- | Whether scale out by a target tracking scaling policy or a step scaling
    -- policy is suspended. Set the value to @true@ if you don\'t want
    -- Application Auto Scaling to add capacity when a scaling policy is
    -- triggered. The default is @false@.
    dynamicScalingOutSuspended :: Prelude.Maybe Prelude.Bool,
    -- | Whether scale in by a target tracking scaling policy or a step scaling
    -- policy is suspended. Set the value to @true@ if you don\'t want
    -- Application Auto Scaling to remove capacity when a scaling policy is
    -- triggered. The default is @false@.
    dynamicScalingInSuspended :: Prelude.Maybe Prelude.Bool,
    -- | Whether scheduled scaling is suspended. Set the value to @true@ if you
    -- don\'t want Application Auto Scaling to add or remove capacity by
    -- initiating scheduled actions. The default is @false@.
    scheduledScalingSuspended :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SuspendedState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dynamicScalingOutSuspended', 'suspendedState_dynamicScalingOutSuspended' - Whether scale out by a target tracking scaling policy or a step scaling
-- policy is suspended. Set the value to @true@ if you don\'t want
-- Application Auto Scaling to add capacity when a scaling policy is
-- triggered. The default is @false@.
--
-- 'dynamicScalingInSuspended', 'suspendedState_dynamicScalingInSuspended' - Whether scale in by a target tracking scaling policy or a step scaling
-- policy is suspended. Set the value to @true@ if you don\'t want
-- Application Auto Scaling to remove capacity when a scaling policy is
-- triggered. The default is @false@.
--
-- 'scheduledScalingSuspended', 'suspendedState_scheduledScalingSuspended' - Whether scheduled scaling is suspended. Set the value to @true@ if you
-- don\'t want Application Auto Scaling to add or remove capacity by
-- initiating scheduled actions. The default is @false@.
newSuspendedState ::
  SuspendedState
newSuspendedState =
  SuspendedState'
    { dynamicScalingOutSuspended =
        Prelude.Nothing,
      dynamicScalingInSuspended = Prelude.Nothing,
      scheduledScalingSuspended = Prelude.Nothing
    }

-- | Whether scale out by a target tracking scaling policy or a step scaling
-- policy is suspended. Set the value to @true@ if you don\'t want
-- Application Auto Scaling to add capacity when a scaling policy is
-- triggered. The default is @false@.
suspendedState_dynamicScalingOutSuspended :: Lens.Lens' SuspendedState (Prelude.Maybe Prelude.Bool)
suspendedState_dynamicScalingOutSuspended = Lens.lens (\SuspendedState' {dynamicScalingOutSuspended} -> dynamicScalingOutSuspended) (\s@SuspendedState' {} a -> s {dynamicScalingOutSuspended = a} :: SuspendedState)

-- | Whether scale in by a target tracking scaling policy or a step scaling
-- policy is suspended. Set the value to @true@ if you don\'t want
-- Application Auto Scaling to remove capacity when a scaling policy is
-- triggered. The default is @false@.
suspendedState_dynamicScalingInSuspended :: Lens.Lens' SuspendedState (Prelude.Maybe Prelude.Bool)
suspendedState_dynamicScalingInSuspended = Lens.lens (\SuspendedState' {dynamicScalingInSuspended} -> dynamicScalingInSuspended) (\s@SuspendedState' {} a -> s {dynamicScalingInSuspended = a} :: SuspendedState)

-- | Whether scheduled scaling is suspended. Set the value to @true@ if you
-- don\'t want Application Auto Scaling to add or remove capacity by
-- initiating scheduled actions. The default is @false@.
suspendedState_scheduledScalingSuspended :: Lens.Lens' SuspendedState (Prelude.Maybe Prelude.Bool)
suspendedState_scheduledScalingSuspended = Lens.lens (\SuspendedState' {scheduledScalingSuspended} -> scheduledScalingSuspended) (\s@SuspendedState' {} a -> s {scheduledScalingSuspended = a} :: SuspendedState)

instance Data.FromJSON SuspendedState where
  parseJSON =
    Data.withObject
      "SuspendedState"
      ( \x ->
          SuspendedState'
            Prelude.<$> (x Data..:? "DynamicScalingOutSuspended")
            Prelude.<*> (x Data..:? "DynamicScalingInSuspended")
            Prelude.<*> (x Data..:? "ScheduledScalingSuspended")
      )

instance Prelude.Hashable SuspendedState where
  hashWithSalt _salt SuspendedState' {..} =
    _salt
      `Prelude.hashWithSalt` dynamicScalingOutSuspended
      `Prelude.hashWithSalt` dynamicScalingInSuspended
      `Prelude.hashWithSalt` scheduledScalingSuspended

instance Prelude.NFData SuspendedState where
  rnf SuspendedState' {..} =
    Prelude.rnf dynamicScalingOutSuspended
      `Prelude.seq` Prelude.rnf dynamicScalingInSuspended
      `Prelude.seq` Prelude.rnf scheduledScalingSuspended

instance Data.ToJSON SuspendedState where
  toJSON SuspendedState' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DynamicScalingOutSuspended" Data..=)
              Prelude.<$> dynamicScalingOutSuspended,
            ("DynamicScalingInSuspended" Data..=)
              Prelude.<$> dynamicScalingInSuspended,
            ("ScheduledScalingSuspended" Data..=)
              Prelude.<$> scheduledScalingSuspended
          ]
      )
