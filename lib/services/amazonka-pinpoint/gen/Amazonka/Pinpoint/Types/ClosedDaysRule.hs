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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.ClosedDaysRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the rule settings for when messages can\'t be sent.
--
-- /See:/ 'newClosedDaysRule' smart constructor.
data ClosedDaysRule = ClosedDaysRule'
  { -- | End DateTime ISO 8601 format
    endDateTime :: Prelude.Maybe Prelude.Text,
    -- | The name of the closed day rule.
    name :: Prelude.Maybe Prelude.Text,
    -- | Start DateTime ISO 8601 format
    startDateTime :: Prelude.Maybe Prelude.Text
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
-- 'endDateTime', 'closedDaysRule_endDateTime' - End DateTime ISO 8601 format
--
-- 'name', 'closedDaysRule_name' - The name of the closed day rule.
--
-- 'startDateTime', 'closedDaysRule_startDateTime' - Start DateTime ISO 8601 format
newClosedDaysRule ::
  ClosedDaysRule
newClosedDaysRule =
  ClosedDaysRule'
    { endDateTime = Prelude.Nothing,
      name = Prelude.Nothing,
      startDateTime = Prelude.Nothing
    }

-- | End DateTime ISO 8601 format
closedDaysRule_endDateTime :: Lens.Lens' ClosedDaysRule (Prelude.Maybe Prelude.Text)
closedDaysRule_endDateTime = Lens.lens (\ClosedDaysRule' {endDateTime} -> endDateTime) (\s@ClosedDaysRule' {} a -> s {endDateTime = a} :: ClosedDaysRule)

-- | The name of the closed day rule.
closedDaysRule_name :: Lens.Lens' ClosedDaysRule (Prelude.Maybe Prelude.Text)
closedDaysRule_name = Lens.lens (\ClosedDaysRule' {name} -> name) (\s@ClosedDaysRule' {} a -> s {name = a} :: ClosedDaysRule)

-- | Start DateTime ISO 8601 format
closedDaysRule_startDateTime :: Lens.Lens' ClosedDaysRule (Prelude.Maybe Prelude.Text)
closedDaysRule_startDateTime = Lens.lens (\ClosedDaysRule' {startDateTime} -> startDateTime) (\s@ClosedDaysRule' {} a -> s {startDateTime = a} :: ClosedDaysRule)

instance Data.FromJSON ClosedDaysRule where
  parseJSON =
    Data.withObject
      "ClosedDaysRule"
      ( \x ->
          ClosedDaysRule'
            Prelude.<$> (x Data..:? "EndDateTime")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "StartDateTime")
      )

instance Prelude.Hashable ClosedDaysRule where
  hashWithSalt _salt ClosedDaysRule' {..} =
    _salt
      `Prelude.hashWithSalt` endDateTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` startDateTime

instance Prelude.NFData ClosedDaysRule where
  rnf ClosedDaysRule' {..} =
    Prelude.rnf endDateTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf startDateTime

instance Data.ToJSON ClosedDaysRule where
  toJSON ClosedDaysRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EndDateTime" Data..=) Prelude.<$> endDateTime,
            ("Name" Data..=) Prelude.<$> name,
            ("StartDateTime" Data..=) Prelude.<$> startDateTime
          ]
      )
