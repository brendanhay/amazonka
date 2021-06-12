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
-- Module      : Network.AWS.Pinpoint.Types.CampaignLimits
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CampaignLimits where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | For a campaign, specifies limits on the messages that the campaign can
-- send. For an application, specifies the default limits for messages that
-- campaigns in the application can send.
--
-- /See:/ 'newCampaignLimits' smart constructor.
data CampaignLimits = CampaignLimits'
  { -- | The maximum number of messages that a campaign can send to a single
    -- endpoint during the course of the campaign. If a campaign recurs, this
    -- setting applies to all runs of the campaign. The maximum value is 100.
    total :: Core.Maybe Core.Int,
    -- | The maximum number of messages that a campaign can send each second. For
    -- an application, this value specifies the default limit for the number of
    -- messages that campaigns can send each second. The minimum value is 50.
    -- The maximum value is 20,000.
    messagesPerSecond :: Core.Maybe Core.Int,
    -- | The maximum number of messages that a campaign can send to a single
    -- endpoint during a 24-hour period. For an application, this value
    -- specifies the default limit for the number of messages that campaigns
    -- and journeys can send to a single endpoint during a 24-hour period. The
    -- maximum value is 100.
    daily :: Core.Maybe Core.Int,
    -- | The maximum amount of time, in seconds, that a campaign can attempt to
    -- deliver a message after the scheduled start time for the campaign. The
    -- minimum value is 60 seconds.
    maximumDuration :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CampaignLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'total', 'campaignLimits_total' - The maximum number of messages that a campaign can send to a single
-- endpoint during the course of the campaign. If a campaign recurs, this
-- setting applies to all runs of the campaign. The maximum value is 100.
--
-- 'messagesPerSecond', 'campaignLimits_messagesPerSecond' - The maximum number of messages that a campaign can send each second. For
-- an application, this value specifies the default limit for the number of
-- messages that campaigns can send each second. The minimum value is 50.
-- The maximum value is 20,000.
--
-- 'daily', 'campaignLimits_daily' - The maximum number of messages that a campaign can send to a single
-- endpoint during a 24-hour period. For an application, this value
-- specifies the default limit for the number of messages that campaigns
-- and journeys can send to a single endpoint during a 24-hour period. The
-- maximum value is 100.
--
-- 'maximumDuration', 'campaignLimits_maximumDuration' - The maximum amount of time, in seconds, that a campaign can attempt to
-- deliver a message after the scheduled start time for the campaign. The
-- minimum value is 60 seconds.
newCampaignLimits ::
  CampaignLimits
newCampaignLimits =
  CampaignLimits'
    { total = Core.Nothing,
      messagesPerSecond = Core.Nothing,
      daily = Core.Nothing,
      maximumDuration = Core.Nothing
    }

-- | The maximum number of messages that a campaign can send to a single
-- endpoint during the course of the campaign. If a campaign recurs, this
-- setting applies to all runs of the campaign. The maximum value is 100.
campaignLimits_total :: Lens.Lens' CampaignLimits (Core.Maybe Core.Int)
campaignLimits_total = Lens.lens (\CampaignLimits' {total} -> total) (\s@CampaignLimits' {} a -> s {total = a} :: CampaignLimits)

-- | The maximum number of messages that a campaign can send each second. For
-- an application, this value specifies the default limit for the number of
-- messages that campaigns can send each second. The minimum value is 50.
-- The maximum value is 20,000.
campaignLimits_messagesPerSecond :: Lens.Lens' CampaignLimits (Core.Maybe Core.Int)
campaignLimits_messagesPerSecond = Lens.lens (\CampaignLimits' {messagesPerSecond} -> messagesPerSecond) (\s@CampaignLimits' {} a -> s {messagesPerSecond = a} :: CampaignLimits)

-- | The maximum number of messages that a campaign can send to a single
-- endpoint during a 24-hour period. For an application, this value
-- specifies the default limit for the number of messages that campaigns
-- and journeys can send to a single endpoint during a 24-hour period. The
-- maximum value is 100.
campaignLimits_daily :: Lens.Lens' CampaignLimits (Core.Maybe Core.Int)
campaignLimits_daily = Lens.lens (\CampaignLimits' {daily} -> daily) (\s@CampaignLimits' {} a -> s {daily = a} :: CampaignLimits)

-- | The maximum amount of time, in seconds, that a campaign can attempt to
-- deliver a message after the scheduled start time for the campaign. The
-- minimum value is 60 seconds.
campaignLimits_maximumDuration :: Lens.Lens' CampaignLimits (Core.Maybe Core.Int)
campaignLimits_maximumDuration = Lens.lens (\CampaignLimits' {maximumDuration} -> maximumDuration) (\s@CampaignLimits' {} a -> s {maximumDuration = a} :: CampaignLimits)

instance Core.FromJSON CampaignLimits where
  parseJSON =
    Core.withObject
      "CampaignLimits"
      ( \x ->
          CampaignLimits'
            Core.<$> (x Core..:? "Total")
            Core.<*> (x Core..:? "MessagesPerSecond")
            Core.<*> (x Core..:? "Daily")
            Core.<*> (x Core..:? "MaximumDuration")
      )

instance Core.Hashable CampaignLimits

instance Core.NFData CampaignLimits

instance Core.ToJSON CampaignLimits where
  toJSON CampaignLimits' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Total" Core..=) Core.<$> total,
            ("MessagesPerSecond" Core..=)
              Core.<$> messagesPerSecond,
            ("Daily" Core..=) Core.<$> daily,
            ("MaximumDuration" Core..=)
              Core.<$> maximumDuration
          ]
      )
