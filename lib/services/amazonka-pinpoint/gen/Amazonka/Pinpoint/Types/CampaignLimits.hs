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
-- Module      : Amazonka.Pinpoint.Types.CampaignLimits
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.CampaignLimits where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | For a campaign, specifies limits on the messages that the campaign can
-- send. For an application, specifies the default limits for messages that
-- campaigns in the application can send.
--
-- /See:/ 'newCampaignLimits' smart constructor.
data CampaignLimits = CampaignLimits'
  { -- | The maximum amount of time, in seconds, that a campaign can attempt to
    -- deliver a message after the scheduled start time for the campaign. The
    -- minimum value is 60 seconds.
    maximumDuration :: Prelude.Maybe Prelude.Int,
    -- | The maximum number of messages that a campaign can send to a single
    -- endpoint during the course of the campaign. If a campaign recurs, this
    -- setting applies to all runs of the campaign. The maximum value is 100.
    total :: Prelude.Maybe Prelude.Int,
    -- | The maximum number of messages that a campaign can send each second. For
    -- an application, this value specifies the default limit for the number of
    -- messages that campaigns can send each second. The minimum value is 50.
    -- The maximum value is 20,000.
    messagesPerSecond :: Prelude.Maybe Prelude.Int,
    -- | The maximum total number of messages that the campaign can send per user
    -- session.
    session :: Prelude.Maybe Prelude.Int,
    -- | The maximum number of messages that a campaign can send to a single
    -- endpoint during a 24-hour period. For an application, this value
    -- specifies the default limit for the number of messages that campaigns
    -- and journeys can send to a single endpoint during a 24-hour period. The
    -- maximum value is 100.
    daily :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CampaignLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximumDuration', 'campaignLimits_maximumDuration' - The maximum amount of time, in seconds, that a campaign can attempt to
-- deliver a message after the scheduled start time for the campaign. The
-- minimum value is 60 seconds.
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
-- 'session', 'campaignLimits_session' - The maximum total number of messages that the campaign can send per user
-- session.
--
-- 'daily', 'campaignLimits_daily' - The maximum number of messages that a campaign can send to a single
-- endpoint during a 24-hour period. For an application, this value
-- specifies the default limit for the number of messages that campaigns
-- and journeys can send to a single endpoint during a 24-hour period. The
-- maximum value is 100.
newCampaignLimits ::
  CampaignLimits
newCampaignLimits =
  CampaignLimits'
    { maximumDuration = Prelude.Nothing,
      total = Prelude.Nothing,
      messagesPerSecond = Prelude.Nothing,
      session = Prelude.Nothing,
      daily = Prelude.Nothing
    }

-- | The maximum amount of time, in seconds, that a campaign can attempt to
-- deliver a message after the scheduled start time for the campaign. The
-- minimum value is 60 seconds.
campaignLimits_maximumDuration :: Lens.Lens' CampaignLimits (Prelude.Maybe Prelude.Int)
campaignLimits_maximumDuration = Lens.lens (\CampaignLimits' {maximumDuration} -> maximumDuration) (\s@CampaignLimits' {} a -> s {maximumDuration = a} :: CampaignLimits)

-- | The maximum number of messages that a campaign can send to a single
-- endpoint during the course of the campaign. If a campaign recurs, this
-- setting applies to all runs of the campaign. The maximum value is 100.
campaignLimits_total :: Lens.Lens' CampaignLimits (Prelude.Maybe Prelude.Int)
campaignLimits_total = Lens.lens (\CampaignLimits' {total} -> total) (\s@CampaignLimits' {} a -> s {total = a} :: CampaignLimits)

-- | The maximum number of messages that a campaign can send each second. For
-- an application, this value specifies the default limit for the number of
-- messages that campaigns can send each second. The minimum value is 50.
-- The maximum value is 20,000.
campaignLimits_messagesPerSecond :: Lens.Lens' CampaignLimits (Prelude.Maybe Prelude.Int)
campaignLimits_messagesPerSecond = Lens.lens (\CampaignLimits' {messagesPerSecond} -> messagesPerSecond) (\s@CampaignLimits' {} a -> s {messagesPerSecond = a} :: CampaignLimits)

-- | The maximum total number of messages that the campaign can send per user
-- session.
campaignLimits_session :: Lens.Lens' CampaignLimits (Prelude.Maybe Prelude.Int)
campaignLimits_session = Lens.lens (\CampaignLimits' {session} -> session) (\s@CampaignLimits' {} a -> s {session = a} :: CampaignLimits)

-- | The maximum number of messages that a campaign can send to a single
-- endpoint during a 24-hour period. For an application, this value
-- specifies the default limit for the number of messages that campaigns
-- and journeys can send to a single endpoint during a 24-hour period. The
-- maximum value is 100.
campaignLimits_daily :: Lens.Lens' CampaignLimits (Prelude.Maybe Prelude.Int)
campaignLimits_daily = Lens.lens (\CampaignLimits' {daily} -> daily) (\s@CampaignLimits' {} a -> s {daily = a} :: CampaignLimits)

instance Core.FromJSON CampaignLimits where
  parseJSON =
    Core.withObject
      "CampaignLimits"
      ( \x ->
          CampaignLimits'
            Prelude.<$> (x Core..:? "MaximumDuration")
            Prelude.<*> (x Core..:? "Total")
            Prelude.<*> (x Core..:? "MessagesPerSecond")
            Prelude.<*> (x Core..:? "Session")
            Prelude.<*> (x Core..:? "Daily")
      )

instance Prelude.Hashable CampaignLimits where
  hashWithSalt _salt CampaignLimits' {..} =
    _salt `Prelude.hashWithSalt` maximumDuration
      `Prelude.hashWithSalt` total
      `Prelude.hashWithSalt` messagesPerSecond
      `Prelude.hashWithSalt` session
      `Prelude.hashWithSalt` daily

instance Prelude.NFData CampaignLimits where
  rnf CampaignLimits' {..} =
    Prelude.rnf maximumDuration
      `Prelude.seq` Prelude.rnf total
      `Prelude.seq` Prelude.rnf messagesPerSecond
      `Prelude.seq` Prelude.rnf session
      `Prelude.seq` Prelude.rnf daily

instance Core.ToJSON CampaignLimits where
  toJSON CampaignLimits' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MaximumDuration" Core..=)
              Prelude.<$> maximumDuration,
            ("Total" Core..=) Prelude.<$> total,
            ("MessagesPerSecond" Core..=)
              Prelude.<$> messagesPerSecond,
            ("Session" Core..=) Prelude.<$> session,
            ("Daily" Core..=) Prelude.<$> daily
          ]
      )
