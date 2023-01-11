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
-- Module      : Amazonka.AlexaBusiness.Types.BusinessReportRecurrence
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.BusinessReportRecurrence where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The recurrence of the reports.
--
-- /See:/ 'newBusinessReportRecurrence' smart constructor.
data BusinessReportRecurrence = BusinessReportRecurrence'
  { -- | The start date.
    startDate :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BusinessReportRecurrence' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startDate', 'businessReportRecurrence_startDate' - The start date.
newBusinessReportRecurrence ::
  BusinessReportRecurrence
newBusinessReportRecurrence =
  BusinessReportRecurrence'
    { startDate =
        Prelude.Nothing
    }

-- | The start date.
businessReportRecurrence_startDate :: Lens.Lens' BusinessReportRecurrence (Prelude.Maybe Prelude.Text)
businessReportRecurrence_startDate = Lens.lens (\BusinessReportRecurrence' {startDate} -> startDate) (\s@BusinessReportRecurrence' {} a -> s {startDate = a} :: BusinessReportRecurrence)

instance Data.FromJSON BusinessReportRecurrence where
  parseJSON =
    Data.withObject
      "BusinessReportRecurrence"
      ( \x ->
          BusinessReportRecurrence'
            Prelude.<$> (x Data..:? "StartDate")
      )

instance Prelude.Hashable BusinessReportRecurrence where
  hashWithSalt _salt BusinessReportRecurrence' {..} =
    _salt `Prelude.hashWithSalt` startDate

instance Prelude.NFData BusinessReportRecurrence where
  rnf BusinessReportRecurrence' {..} =
    Prelude.rnf startDate

instance Data.ToJSON BusinessReportRecurrence where
  toJSON BusinessReportRecurrence' {..} =
    Data.object
      ( Prelude.catMaybes
          [("StartDate" Data..=) Prelude.<$> startDate]
      )
