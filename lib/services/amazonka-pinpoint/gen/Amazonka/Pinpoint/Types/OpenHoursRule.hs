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
-- Module      : Amazonka.Pinpoint.Types.OpenHoursRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.OpenHoursRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | List of OpenHours Rules.
--
-- /See:/ 'newOpenHoursRule' smart constructor.
data OpenHoursRule = OpenHoursRule'
  { -- | Local start time in ISO 8601 format.
    endTime :: Prelude.Maybe Prelude.Text,
    -- | Local start time in ISO 8601 format.
    startTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OpenHoursRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'openHoursRule_endTime' - Local start time in ISO 8601 format.
--
-- 'startTime', 'openHoursRule_startTime' - Local start time in ISO 8601 format.
newOpenHoursRule ::
  OpenHoursRule
newOpenHoursRule =
  OpenHoursRule'
    { endTime = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | Local start time in ISO 8601 format.
openHoursRule_endTime :: Lens.Lens' OpenHoursRule (Prelude.Maybe Prelude.Text)
openHoursRule_endTime = Lens.lens (\OpenHoursRule' {endTime} -> endTime) (\s@OpenHoursRule' {} a -> s {endTime = a} :: OpenHoursRule)

-- | Local start time in ISO 8601 format.
openHoursRule_startTime :: Lens.Lens' OpenHoursRule (Prelude.Maybe Prelude.Text)
openHoursRule_startTime = Lens.lens (\OpenHoursRule' {startTime} -> startTime) (\s@OpenHoursRule' {} a -> s {startTime = a} :: OpenHoursRule)

instance Data.FromJSON OpenHoursRule where
  parseJSON =
    Data.withObject
      "OpenHoursRule"
      ( \x ->
          OpenHoursRule'
            Prelude.<$> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "StartTime")
      )

instance Prelude.Hashable OpenHoursRule where
  hashWithSalt _salt OpenHoursRule' {..} =
    _salt
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData OpenHoursRule where
  rnf OpenHoursRule' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf startTime

instance Data.ToJSON OpenHoursRule where
  toJSON OpenHoursRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EndTime" Data..=) Prelude.<$> endTime,
            ("StartTime" Data..=) Prelude.<$> startTime
          ]
      )
