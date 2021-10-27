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
-- Module      : Network.AWS.MediaTailor.Types.ScheduleConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaTailor.Types.ScheduleConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaTailor.Types.Transition
import qualified Network.AWS.Prelude as Prelude

-- | Schedule configuration parameters. A channel must be stopped before
-- changes can be made to the schedule.
--
-- /See:/ 'newScheduleConfiguration' smart constructor.
data ScheduleConfiguration = ScheduleConfiguration'
  { -- | Program transition configurations.
    transition :: Transition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduleConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transition', 'scheduleConfiguration_transition' - Program transition configurations.
newScheduleConfiguration ::
  -- | 'transition'
  Transition ->
  ScheduleConfiguration
newScheduleConfiguration pTransition_ =
  ScheduleConfiguration' {transition = pTransition_}

-- | Program transition configurations.
scheduleConfiguration_transition :: Lens.Lens' ScheduleConfiguration Transition
scheduleConfiguration_transition = Lens.lens (\ScheduleConfiguration' {transition} -> transition) (\s@ScheduleConfiguration' {} a -> s {transition = a} :: ScheduleConfiguration)

instance Prelude.Hashable ScheduleConfiguration

instance Prelude.NFData ScheduleConfiguration

instance Core.ToJSON ScheduleConfiguration where
  toJSON ScheduleConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Transition" Core..= transition)]
      )
