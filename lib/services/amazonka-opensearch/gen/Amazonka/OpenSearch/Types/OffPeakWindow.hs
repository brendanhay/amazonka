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
-- Module      : Amazonka.OpenSearch.Types.OffPeakWindow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.OffPeakWindow where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.WindowStartTime
import qualified Amazonka.Prelude as Prelude

-- | A custom 10-hour, low-traffic window during which OpenSearch Service can
-- perform mandatory configuration changes on the domain. These actions can
-- include scheduled service software updates and blue\/green Auto-Tune
-- enhancements. OpenSearch Service will schedule these actions during the
-- window that you specify.
--
-- If you don\'t specify a window start time, it defaults to 10:00 P.M.
-- local time.
--
-- For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/off-peak.html Defining off-peak maintenance windows for Amazon OpenSearch Service>.
--
-- /See:/ 'newOffPeakWindow' smart constructor.
data OffPeakWindow = OffPeakWindow'
  { -- | A custom start time for the off-peak window, in Coordinated Universal
    -- Time (UTC). The window length will always be 10 hours, so you can\'t
    -- specify an end time. For example, if you specify 11:00 P.M. UTC as a
    -- start time, the end time will automatically be set to 9:00 A.M.
    windowStartTime :: Prelude.Maybe WindowStartTime
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OffPeakWindow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'windowStartTime', 'offPeakWindow_windowStartTime' - A custom start time for the off-peak window, in Coordinated Universal
-- Time (UTC). The window length will always be 10 hours, so you can\'t
-- specify an end time. For example, if you specify 11:00 P.M. UTC as a
-- start time, the end time will automatically be set to 9:00 A.M.
newOffPeakWindow ::
  OffPeakWindow
newOffPeakWindow =
  OffPeakWindow' {windowStartTime = Prelude.Nothing}

-- | A custom start time for the off-peak window, in Coordinated Universal
-- Time (UTC). The window length will always be 10 hours, so you can\'t
-- specify an end time. For example, if you specify 11:00 P.M. UTC as a
-- start time, the end time will automatically be set to 9:00 A.M.
offPeakWindow_windowStartTime :: Lens.Lens' OffPeakWindow (Prelude.Maybe WindowStartTime)
offPeakWindow_windowStartTime = Lens.lens (\OffPeakWindow' {windowStartTime} -> windowStartTime) (\s@OffPeakWindow' {} a -> s {windowStartTime = a} :: OffPeakWindow)

instance Data.FromJSON OffPeakWindow where
  parseJSON =
    Data.withObject
      "OffPeakWindow"
      ( \x ->
          OffPeakWindow'
            Prelude.<$> (x Data..:? "WindowStartTime")
      )

instance Prelude.Hashable OffPeakWindow where
  hashWithSalt _salt OffPeakWindow' {..} =
    _salt `Prelude.hashWithSalt` windowStartTime

instance Prelude.NFData OffPeakWindow where
  rnf OffPeakWindow' {..} = Prelude.rnf windowStartTime

instance Data.ToJSON OffPeakWindow where
  toJSON OffPeakWindow' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("WindowStartTime" Data..=)
              Prelude.<$> windowStartTime
          ]
      )
