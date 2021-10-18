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
-- Module      : Network.AWS.Pinpoint.Types.InAppMessageCampaign
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.InAppMessageCampaign where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.InAppCampaignSchedule
import Network.AWS.Pinpoint.Types.InAppMessage
import qualified Network.AWS.Prelude as Prelude

-- | Targeted in-app message campaign.
--
-- /See:/ 'newInAppMessageCampaign' smart constructor.
data InAppMessageCampaign = InAppMessageCampaign'
  { -- | Session cap which controls the number of times an in-app message can be
    -- shown to the endpoint during an application session.
    sessionCap :: Prelude.Maybe Prelude.Int,
    -- | Campaign id of the corresponding campaign.
    campaignId :: Prelude.Maybe Prelude.Text,
    -- | In-app message content with all fields required for rendering an in-app
    -- message.
    inAppMessage :: Prelude.Maybe InAppMessage,
    -- | Priority of the in-app message.
    priority :: Prelude.Maybe Prelude.Int,
    -- | Treatment id of the campaign.
    treatmentId :: Prelude.Maybe Prelude.Text,
    -- | Daily cap which controls the number of times any in-app messages can be
    -- shown to the endpoint during a day.
    dailyCap :: Prelude.Maybe Prelude.Int,
    -- | Total cap which controls the number of times an in-app message can be
    -- shown to the endpoint.
    totalCap :: Prelude.Maybe Prelude.Int,
    -- | Schedule of the campaign.
    schedule :: Prelude.Maybe InAppCampaignSchedule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InAppMessageCampaign' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionCap', 'inAppMessageCampaign_sessionCap' - Session cap which controls the number of times an in-app message can be
-- shown to the endpoint during an application session.
--
-- 'campaignId', 'inAppMessageCampaign_campaignId' - Campaign id of the corresponding campaign.
--
-- 'inAppMessage', 'inAppMessageCampaign_inAppMessage' - In-app message content with all fields required for rendering an in-app
-- message.
--
-- 'priority', 'inAppMessageCampaign_priority' - Priority of the in-app message.
--
-- 'treatmentId', 'inAppMessageCampaign_treatmentId' - Treatment id of the campaign.
--
-- 'dailyCap', 'inAppMessageCampaign_dailyCap' - Daily cap which controls the number of times any in-app messages can be
-- shown to the endpoint during a day.
--
-- 'totalCap', 'inAppMessageCampaign_totalCap' - Total cap which controls the number of times an in-app message can be
-- shown to the endpoint.
--
-- 'schedule', 'inAppMessageCampaign_schedule' - Schedule of the campaign.
newInAppMessageCampaign ::
  InAppMessageCampaign
newInAppMessageCampaign =
  InAppMessageCampaign'
    { sessionCap = Prelude.Nothing,
      campaignId = Prelude.Nothing,
      inAppMessage = Prelude.Nothing,
      priority = Prelude.Nothing,
      treatmentId = Prelude.Nothing,
      dailyCap = Prelude.Nothing,
      totalCap = Prelude.Nothing,
      schedule = Prelude.Nothing
    }

-- | Session cap which controls the number of times an in-app message can be
-- shown to the endpoint during an application session.
inAppMessageCampaign_sessionCap :: Lens.Lens' InAppMessageCampaign (Prelude.Maybe Prelude.Int)
inAppMessageCampaign_sessionCap = Lens.lens (\InAppMessageCampaign' {sessionCap} -> sessionCap) (\s@InAppMessageCampaign' {} a -> s {sessionCap = a} :: InAppMessageCampaign)

-- | Campaign id of the corresponding campaign.
inAppMessageCampaign_campaignId :: Lens.Lens' InAppMessageCampaign (Prelude.Maybe Prelude.Text)
inAppMessageCampaign_campaignId = Lens.lens (\InAppMessageCampaign' {campaignId} -> campaignId) (\s@InAppMessageCampaign' {} a -> s {campaignId = a} :: InAppMessageCampaign)

-- | In-app message content with all fields required for rendering an in-app
-- message.
inAppMessageCampaign_inAppMessage :: Lens.Lens' InAppMessageCampaign (Prelude.Maybe InAppMessage)
inAppMessageCampaign_inAppMessage = Lens.lens (\InAppMessageCampaign' {inAppMessage} -> inAppMessage) (\s@InAppMessageCampaign' {} a -> s {inAppMessage = a} :: InAppMessageCampaign)

-- | Priority of the in-app message.
inAppMessageCampaign_priority :: Lens.Lens' InAppMessageCampaign (Prelude.Maybe Prelude.Int)
inAppMessageCampaign_priority = Lens.lens (\InAppMessageCampaign' {priority} -> priority) (\s@InAppMessageCampaign' {} a -> s {priority = a} :: InAppMessageCampaign)

-- | Treatment id of the campaign.
inAppMessageCampaign_treatmentId :: Lens.Lens' InAppMessageCampaign (Prelude.Maybe Prelude.Text)
inAppMessageCampaign_treatmentId = Lens.lens (\InAppMessageCampaign' {treatmentId} -> treatmentId) (\s@InAppMessageCampaign' {} a -> s {treatmentId = a} :: InAppMessageCampaign)

-- | Daily cap which controls the number of times any in-app messages can be
-- shown to the endpoint during a day.
inAppMessageCampaign_dailyCap :: Lens.Lens' InAppMessageCampaign (Prelude.Maybe Prelude.Int)
inAppMessageCampaign_dailyCap = Lens.lens (\InAppMessageCampaign' {dailyCap} -> dailyCap) (\s@InAppMessageCampaign' {} a -> s {dailyCap = a} :: InAppMessageCampaign)

-- | Total cap which controls the number of times an in-app message can be
-- shown to the endpoint.
inAppMessageCampaign_totalCap :: Lens.Lens' InAppMessageCampaign (Prelude.Maybe Prelude.Int)
inAppMessageCampaign_totalCap = Lens.lens (\InAppMessageCampaign' {totalCap} -> totalCap) (\s@InAppMessageCampaign' {} a -> s {totalCap = a} :: InAppMessageCampaign)

-- | Schedule of the campaign.
inAppMessageCampaign_schedule :: Lens.Lens' InAppMessageCampaign (Prelude.Maybe InAppCampaignSchedule)
inAppMessageCampaign_schedule = Lens.lens (\InAppMessageCampaign' {schedule} -> schedule) (\s@InAppMessageCampaign' {} a -> s {schedule = a} :: InAppMessageCampaign)

instance Core.FromJSON InAppMessageCampaign where
  parseJSON =
    Core.withObject
      "InAppMessageCampaign"
      ( \x ->
          InAppMessageCampaign'
            Prelude.<$> (x Core..:? "SessionCap")
            Prelude.<*> (x Core..:? "CampaignId")
            Prelude.<*> (x Core..:? "InAppMessage")
            Prelude.<*> (x Core..:? "Priority")
            Prelude.<*> (x Core..:? "TreatmentId")
            Prelude.<*> (x Core..:? "DailyCap")
            Prelude.<*> (x Core..:? "TotalCap")
            Prelude.<*> (x Core..:? "Schedule")
      )

instance Prelude.Hashable InAppMessageCampaign

instance Prelude.NFData InAppMessageCampaign
