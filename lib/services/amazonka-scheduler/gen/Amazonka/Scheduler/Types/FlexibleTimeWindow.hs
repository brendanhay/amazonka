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
-- Module      : Amazonka.Scheduler.Types.FlexibleTimeWindow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Scheduler.Types.FlexibleTimeWindow where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Scheduler.Types.FlexibleTimeWindowMode

-- | Allows you to configure a time window during which EventBridge Scheduler
-- invokes the schedule.
--
-- /See:/ 'newFlexibleTimeWindow' smart constructor.
data FlexibleTimeWindow = FlexibleTimeWindow'
  { -- | The maximum time window during which a schedule can be invoked.
    maximumWindowInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | Determines whether the schedule is invoked within a flexible time
    -- window.
    mode :: FlexibleTimeWindowMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FlexibleTimeWindow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximumWindowInMinutes', 'flexibleTimeWindow_maximumWindowInMinutes' - The maximum time window during which a schedule can be invoked.
--
-- 'mode', 'flexibleTimeWindow_mode' - Determines whether the schedule is invoked within a flexible time
-- window.
newFlexibleTimeWindow ::
  -- | 'mode'
  FlexibleTimeWindowMode ->
  FlexibleTimeWindow
newFlexibleTimeWindow pMode_ =
  FlexibleTimeWindow'
    { maximumWindowInMinutes =
        Prelude.Nothing,
      mode = pMode_
    }

-- | The maximum time window during which a schedule can be invoked.
flexibleTimeWindow_maximumWindowInMinutes :: Lens.Lens' FlexibleTimeWindow (Prelude.Maybe Prelude.Natural)
flexibleTimeWindow_maximumWindowInMinutes = Lens.lens (\FlexibleTimeWindow' {maximumWindowInMinutes} -> maximumWindowInMinutes) (\s@FlexibleTimeWindow' {} a -> s {maximumWindowInMinutes = a} :: FlexibleTimeWindow)

-- | Determines whether the schedule is invoked within a flexible time
-- window.
flexibleTimeWindow_mode :: Lens.Lens' FlexibleTimeWindow FlexibleTimeWindowMode
flexibleTimeWindow_mode = Lens.lens (\FlexibleTimeWindow' {mode} -> mode) (\s@FlexibleTimeWindow' {} a -> s {mode = a} :: FlexibleTimeWindow)

instance Data.FromJSON FlexibleTimeWindow where
  parseJSON =
    Data.withObject
      "FlexibleTimeWindow"
      ( \x ->
          FlexibleTimeWindow'
            Prelude.<$> (x Data..:? "MaximumWindowInMinutes")
            Prelude.<*> (x Data..: "Mode")
      )

instance Prelude.Hashable FlexibleTimeWindow where
  hashWithSalt _salt FlexibleTimeWindow' {..} =
    _salt
      `Prelude.hashWithSalt` maximumWindowInMinutes
      `Prelude.hashWithSalt` mode

instance Prelude.NFData FlexibleTimeWindow where
  rnf FlexibleTimeWindow' {..} =
    Prelude.rnf maximumWindowInMinutes `Prelude.seq`
      Prelude.rnf mode

instance Data.ToJSON FlexibleTimeWindow where
  toJSON FlexibleTimeWindow' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaximumWindowInMinutes" Data..=)
              Prelude.<$> maximumWindowInMinutes,
            Prelude.Just ("Mode" Data..= mode)
          ]
      )
