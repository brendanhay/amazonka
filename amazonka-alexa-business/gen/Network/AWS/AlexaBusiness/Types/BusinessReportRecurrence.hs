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
-- Module      : Network.AWS.AlexaBusiness.Types.BusinessReportRecurrence
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.BusinessReportRecurrence where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The recurrence of the reports.
--
-- /See:/ 'newBusinessReportRecurrence' smart constructor.
data BusinessReportRecurrence = BusinessReportRecurrence'
  { -- | The start date.
    startDate :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  BusinessReportRecurrence' {startDate = Core.Nothing}

-- | The start date.
businessReportRecurrence_startDate :: Lens.Lens' BusinessReportRecurrence (Core.Maybe Core.Text)
businessReportRecurrence_startDate = Lens.lens (\BusinessReportRecurrence' {startDate} -> startDate) (\s@BusinessReportRecurrence' {} a -> s {startDate = a} :: BusinessReportRecurrence)

instance Core.FromJSON BusinessReportRecurrence where
  parseJSON =
    Core.withObject
      "BusinessReportRecurrence"
      ( \x ->
          BusinessReportRecurrence'
            Core.<$> (x Core..:? "StartDate")
      )

instance Core.Hashable BusinessReportRecurrence

instance Core.NFData BusinessReportRecurrence

instance Core.ToJSON BusinessReportRecurrence where
  toJSON BusinessReportRecurrence' {..} =
    Core.object
      ( Core.catMaybes
          [("StartDate" Core..=) Core.<$> startDate]
      )
