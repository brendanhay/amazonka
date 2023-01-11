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
-- Module      : Amazonka.Pinpoint.Types.JourneyChannelSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.JourneyChannelSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The channel-specific configurations for the journey.
--
-- /See:/ 'newJourneyChannelSettings' smart constructor.
data JourneyChannelSettings = JourneyChannelSettings'
  { -- | Amazon Resource Name (ARN) of the Connect Campaign.
    connectCampaignArn :: Prelude.Maybe Prelude.Text,
    -- | IAM role ARN to be assumed when invoking Connect campaign execution APIs
    -- for dialing.
    connectCampaignExecutionRoleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JourneyChannelSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectCampaignArn', 'journeyChannelSettings_connectCampaignArn' - Amazon Resource Name (ARN) of the Connect Campaign.
--
-- 'connectCampaignExecutionRoleArn', 'journeyChannelSettings_connectCampaignExecutionRoleArn' - IAM role ARN to be assumed when invoking Connect campaign execution APIs
-- for dialing.
newJourneyChannelSettings ::
  JourneyChannelSettings
newJourneyChannelSettings =
  JourneyChannelSettings'
    { connectCampaignArn =
        Prelude.Nothing,
      connectCampaignExecutionRoleArn = Prelude.Nothing
    }

-- | Amazon Resource Name (ARN) of the Connect Campaign.
journeyChannelSettings_connectCampaignArn :: Lens.Lens' JourneyChannelSettings (Prelude.Maybe Prelude.Text)
journeyChannelSettings_connectCampaignArn = Lens.lens (\JourneyChannelSettings' {connectCampaignArn} -> connectCampaignArn) (\s@JourneyChannelSettings' {} a -> s {connectCampaignArn = a} :: JourneyChannelSettings)

-- | IAM role ARN to be assumed when invoking Connect campaign execution APIs
-- for dialing.
journeyChannelSettings_connectCampaignExecutionRoleArn :: Lens.Lens' JourneyChannelSettings (Prelude.Maybe Prelude.Text)
journeyChannelSettings_connectCampaignExecutionRoleArn = Lens.lens (\JourneyChannelSettings' {connectCampaignExecutionRoleArn} -> connectCampaignExecutionRoleArn) (\s@JourneyChannelSettings' {} a -> s {connectCampaignExecutionRoleArn = a} :: JourneyChannelSettings)

instance Data.FromJSON JourneyChannelSettings where
  parseJSON =
    Data.withObject
      "JourneyChannelSettings"
      ( \x ->
          JourneyChannelSettings'
            Prelude.<$> (x Data..:? "ConnectCampaignArn")
            Prelude.<*> (x Data..:? "ConnectCampaignExecutionRoleArn")
      )

instance Prelude.Hashable JourneyChannelSettings where
  hashWithSalt _salt JourneyChannelSettings' {..} =
    _salt `Prelude.hashWithSalt` connectCampaignArn
      `Prelude.hashWithSalt` connectCampaignExecutionRoleArn

instance Prelude.NFData JourneyChannelSettings where
  rnf JourneyChannelSettings' {..} =
    Prelude.rnf connectCampaignArn
      `Prelude.seq` Prelude.rnf connectCampaignExecutionRoleArn

instance Data.ToJSON JourneyChannelSettings where
  toJSON JourneyChannelSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConnectCampaignArn" Data..=)
              Prelude.<$> connectCampaignArn,
            ("ConnectCampaignExecutionRoleArn" Data..=)
              Prelude.<$> connectCampaignExecutionRoleArn
          ]
      )
