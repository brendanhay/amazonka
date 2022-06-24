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
-- Module      : Amazonka.Pinpoint.Types.InAppCampaignSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.InAppCampaignSchedule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types.CampaignEventFilter
import Amazonka.Pinpoint.Types.QuietTime
import qualified Amazonka.Prelude as Prelude

-- | Schedule of the campaign.
--
-- /See:/ 'newInAppCampaignSchedule' smart constructor.
data InAppCampaignSchedule = InAppCampaignSchedule'
  { -- | The event filter the SDK has to use to show the in-app message in the
    -- application.
    eventFilter :: Prelude.Maybe CampaignEventFilter,
    -- | The scheduled time after which the in-app message should not be shown.
    -- Timestamp is in ISO 8601 format.
    endDate :: Prelude.Maybe Prelude.Text,
    -- | Time during which the in-app message should not be shown to the user.
    quietTime :: Prelude.Maybe QuietTime
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InAppCampaignSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventFilter', 'inAppCampaignSchedule_eventFilter' - The event filter the SDK has to use to show the in-app message in the
-- application.
--
-- 'endDate', 'inAppCampaignSchedule_endDate' - The scheduled time after which the in-app message should not be shown.
-- Timestamp is in ISO 8601 format.
--
-- 'quietTime', 'inAppCampaignSchedule_quietTime' - Time during which the in-app message should not be shown to the user.
newInAppCampaignSchedule ::
  InAppCampaignSchedule
newInAppCampaignSchedule =
  InAppCampaignSchedule'
    { eventFilter =
        Prelude.Nothing,
      endDate = Prelude.Nothing,
      quietTime = Prelude.Nothing
    }

-- | The event filter the SDK has to use to show the in-app message in the
-- application.
inAppCampaignSchedule_eventFilter :: Lens.Lens' InAppCampaignSchedule (Prelude.Maybe CampaignEventFilter)
inAppCampaignSchedule_eventFilter = Lens.lens (\InAppCampaignSchedule' {eventFilter} -> eventFilter) (\s@InAppCampaignSchedule' {} a -> s {eventFilter = a} :: InAppCampaignSchedule)

-- | The scheduled time after which the in-app message should not be shown.
-- Timestamp is in ISO 8601 format.
inAppCampaignSchedule_endDate :: Lens.Lens' InAppCampaignSchedule (Prelude.Maybe Prelude.Text)
inAppCampaignSchedule_endDate = Lens.lens (\InAppCampaignSchedule' {endDate} -> endDate) (\s@InAppCampaignSchedule' {} a -> s {endDate = a} :: InAppCampaignSchedule)

-- | Time during which the in-app message should not be shown to the user.
inAppCampaignSchedule_quietTime :: Lens.Lens' InAppCampaignSchedule (Prelude.Maybe QuietTime)
inAppCampaignSchedule_quietTime = Lens.lens (\InAppCampaignSchedule' {quietTime} -> quietTime) (\s@InAppCampaignSchedule' {} a -> s {quietTime = a} :: InAppCampaignSchedule)

instance Core.FromJSON InAppCampaignSchedule where
  parseJSON =
    Core.withObject
      "InAppCampaignSchedule"
      ( \x ->
          InAppCampaignSchedule'
            Prelude.<$> (x Core..:? "EventFilter")
            Prelude.<*> (x Core..:? "EndDate")
            Prelude.<*> (x Core..:? "QuietTime")
      )

instance Prelude.Hashable InAppCampaignSchedule where
  hashWithSalt _salt InAppCampaignSchedule' {..} =
    _salt `Prelude.hashWithSalt` eventFilter
      `Prelude.hashWithSalt` endDate
      `Prelude.hashWithSalt` quietTime

instance Prelude.NFData InAppCampaignSchedule where
  rnf InAppCampaignSchedule' {..} =
    Prelude.rnf eventFilter
      `Prelude.seq` Prelude.rnf endDate
      `Prelude.seq` Prelude.rnf quietTime
