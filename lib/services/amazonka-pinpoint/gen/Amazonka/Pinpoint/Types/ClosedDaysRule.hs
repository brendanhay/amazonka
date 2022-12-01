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
-- Module      : Amazonka.Pinpoint.Types.ClosedDaysRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.ClosedDaysRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Closed Days Rule. Part of Journey sending schedule.
--
-- /See:/ 'newClosedDaysRule' smart constructor.
data ClosedDaysRule = ClosedDaysRule'
  { -- | Name of the rule.
    name :: Prelude.Maybe Prelude.Text,
    -- | Start Datetime in ISO 8601 format.
    startDateTime :: Prelude.Maybe Prelude.Text,
    -- | End Datetime in ISO 8601 format.
    endDateTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClosedDaysRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'closedDaysRule_name' - Name of the rule.
--
-- 'startDateTime', 'closedDaysRule_startDateTime' - Start Datetime in ISO 8601 format.
--
-- 'endDateTime', 'closedDaysRule_endDateTime' - End Datetime in ISO 8601 format.
newClosedDaysRule ::
  ClosedDaysRule
newClosedDaysRule =
  ClosedDaysRule'
    { name = Prelude.Nothing,
      startDateTime = Prelude.Nothing,
      endDateTime = Prelude.Nothing
    }

-- | Name of the rule.
closedDaysRule_name :: Lens.Lens' ClosedDaysRule (Prelude.Maybe Prelude.Text)
closedDaysRule_name = Lens.lens (\ClosedDaysRule' {name} -> name) (\s@ClosedDaysRule' {} a -> s {name = a} :: ClosedDaysRule)

-- | Start Datetime in ISO 8601 format.
closedDaysRule_startDateTime :: Lens.Lens' ClosedDaysRule (Prelude.Maybe Prelude.Text)
closedDaysRule_startDateTime = Lens.lens (\ClosedDaysRule' {startDateTime} -> startDateTime) (\s@ClosedDaysRule' {} a -> s {startDateTime = a} :: ClosedDaysRule)

-- | End Datetime in ISO 8601 format.
closedDaysRule_endDateTime :: Lens.Lens' ClosedDaysRule (Prelude.Maybe Prelude.Text)
closedDaysRule_endDateTime = Lens.lens (\ClosedDaysRule' {endDateTime} -> endDateTime) (\s@ClosedDaysRule' {} a -> s {endDateTime = a} :: ClosedDaysRule)

instance Core.FromJSON ClosedDaysRule where
  parseJSON =
    Core.withObject
      "ClosedDaysRule"
      ( \x ->
          ClosedDaysRule'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "StartDateTime")
            Prelude.<*> (x Core..:? "EndDateTime")
      )

instance Prelude.Hashable ClosedDaysRule where
  hashWithSalt _salt ClosedDaysRule' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` startDateTime
      `Prelude.hashWithSalt` endDateTime

instance Prelude.NFData ClosedDaysRule where
  rnf ClosedDaysRule' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf startDateTime
      `Prelude.seq` Prelude.rnf endDateTime

instance Core.ToJSON ClosedDaysRule where
  toJSON ClosedDaysRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Name" Core..=) Prelude.<$> name,
            ("StartDateTime" Core..=) Prelude.<$> startDateTime,
            ("EndDateTime" Core..=) Prelude.<$> endDateTime
          ]
      )
