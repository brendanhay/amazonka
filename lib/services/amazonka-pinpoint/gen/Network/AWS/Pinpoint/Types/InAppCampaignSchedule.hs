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
-- Module      : Network.AWS.Pinpoint.Types.InAppCampaignSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.InAppCampaignSchedule where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.CampaignEventFilter
import Network.AWS.Pinpoint.Types.QuietTime
import qualified Network.AWS.Prelude as Prelude

-- | Schedule of the campaign.
--
-- /See:/ 'newInAppCampaignSchedule' smart constructor.
data InAppCampaignSchedule = InAppCampaignSchedule'
  { -- | The scheduled time after which the in-app message should not be shown.
    -- Timestamp is in ISO 8601 format.
    endDate :: Prelude.Maybe Prelude.Text,
    -- | Time during which the in-app message should not be shown to the user.
    quietTime :: Prelude.Maybe QuietTime,
    -- | The event filter the SDK has to use to show the in-app message in the
    -- application.
    eventFilter :: Prelude.Maybe CampaignEventFilter
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
-- 'endDate', 'inAppCampaignSchedule_endDate' - The scheduled time after which the in-app message should not be shown.
-- Timestamp is in ISO 8601 format.
--
-- 'quietTime', 'inAppCampaignSchedule_quietTime' - Time during which the in-app message should not be shown to the user.
--
-- 'eventFilter', 'inAppCampaignSchedule_eventFilter' - The event filter the SDK has to use to show the in-app message in the
-- application.
newInAppCampaignSchedule ::
  InAppCampaignSchedule
newInAppCampaignSchedule =
  InAppCampaignSchedule'
    { endDate = Prelude.Nothing,
      quietTime = Prelude.Nothing,
      eventFilter = Prelude.Nothing
    }

-- | The scheduled time after which the in-app message should not be shown.
-- Timestamp is in ISO 8601 format.
inAppCampaignSchedule_endDate :: Lens.Lens' InAppCampaignSchedule (Prelude.Maybe Prelude.Text)
inAppCampaignSchedule_endDate = Lens.lens (\InAppCampaignSchedule' {endDate} -> endDate) (\s@InAppCampaignSchedule' {} a -> s {endDate = a} :: InAppCampaignSchedule)

-- | Time during which the in-app message should not be shown to the user.
inAppCampaignSchedule_quietTime :: Lens.Lens' InAppCampaignSchedule (Prelude.Maybe QuietTime)
inAppCampaignSchedule_quietTime = Lens.lens (\InAppCampaignSchedule' {quietTime} -> quietTime) (\s@InAppCampaignSchedule' {} a -> s {quietTime = a} :: InAppCampaignSchedule)

-- | The event filter the SDK has to use to show the in-app message in the
-- application.
inAppCampaignSchedule_eventFilter :: Lens.Lens' InAppCampaignSchedule (Prelude.Maybe CampaignEventFilter)
inAppCampaignSchedule_eventFilter = Lens.lens (\InAppCampaignSchedule' {eventFilter} -> eventFilter) (\s@InAppCampaignSchedule' {} a -> s {eventFilter = a} :: InAppCampaignSchedule)

instance Core.FromJSON InAppCampaignSchedule where
  parseJSON =
    Core.withObject
      "InAppCampaignSchedule"
      ( \x ->
          InAppCampaignSchedule'
            Prelude.<$> (x Core..:? "EndDate")
            Prelude.<*> (x Core..:? "QuietTime")
            Prelude.<*> (x Core..:? "EventFilter")
      )

instance Prelude.Hashable InAppCampaignSchedule

instance Prelude.NFData InAppCampaignSchedule
