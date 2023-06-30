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
-- Module      : Amazonka.Personalize.Types.Campaign
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.Campaign where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types.CampaignConfig
import Amazonka.Personalize.Types.CampaignUpdateSummary
import qualified Amazonka.Prelude as Prelude

-- | An object that describes the deployment of a solution version. For more
-- information on campaigns, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_CreateCampaign.html CreateCampaign>.
--
-- /See:/ 'newCampaign' smart constructor.
data Campaign = Campaign'
  { -- | The Amazon Resource Name (ARN) of the campaign.
    campaignArn :: Prelude.Maybe Prelude.Text,
    -- | The configuration details of a campaign.
    campaignConfig :: Prelude.Maybe CampaignConfig,
    -- | The date and time (in Unix format) that the campaign was created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | If a campaign fails, the reason behind the failure.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The date and time (in Unix format) that the campaign was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    latestCampaignUpdate :: Prelude.Maybe CampaignUpdateSummary,
    -- | Specifies the requested minimum provisioned transactions
    -- (recommendations) per second.
    minProvisionedTPS :: Prelude.Maybe Prelude.Natural,
    -- | The name of the campaign.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of a specific version of the solution.
    solutionVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the campaign.
    --
    -- A campaign can be in one of the following states:
    --
    -- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
    --
    -- -   DELETE PENDING > DELETE IN_PROGRESS
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Campaign' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'campaignArn', 'campaign_campaignArn' - The Amazon Resource Name (ARN) of the campaign.
--
-- 'campaignConfig', 'campaign_campaignConfig' - The configuration details of a campaign.
--
-- 'creationDateTime', 'campaign_creationDateTime' - The date and time (in Unix format) that the campaign was created.
--
-- 'failureReason', 'campaign_failureReason' - If a campaign fails, the reason behind the failure.
--
-- 'lastUpdatedDateTime', 'campaign_lastUpdatedDateTime' - The date and time (in Unix format) that the campaign was last updated.
--
-- 'latestCampaignUpdate', 'campaign_latestCampaignUpdate' - Undocumented member.
--
-- 'minProvisionedTPS', 'campaign_minProvisionedTPS' - Specifies the requested minimum provisioned transactions
-- (recommendations) per second.
--
-- 'name', 'campaign_name' - The name of the campaign.
--
-- 'solutionVersionArn', 'campaign_solutionVersionArn' - The Amazon Resource Name (ARN) of a specific version of the solution.
--
-- 'status', 'campaign_status' - The status of the campaign.
--
-- A campaign can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   DELETE PENDING > DELETE IN_PROGRESS
newCampaign ::
  Campaign
newCampaign =
  Campaign'
    { campaignArn = Prelude.Nothing,
      campaignConfig = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      latestCampaignUpdate = Prelude.Nothing,
      minProvisionedTPS = Prelude.Nothing,
      name = Prelude.Nothing,
      solutionVersionArn = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the campaign.
campaign_campaignArn :: Lens.Lens' Campaign (Prelude.Maybe Prelude.Text)
campaign_campaignArn = Lens.lens (\Campaign' {campaignArn} -> campaignArn) (\s@Campaign' {} a -> s {campaignArn = a} :: Campaign)

-- | The configuration details of a campaign.
campaign_campaignConfig :: Lens.Lens' Campaign (Prelude.Maybe CampaignConfig)
campaign_campaignConfig = Lens.lens (\Campaign' {campaignConfig} -> campaignConfig) (\s@Campaign' {} a -> s {campaignConfig = a} :: Campaign)

-- | The date and time (in Unix format) that the campaign was created.
campaign_creationDateTime :: Lens.Lens' Campaign (Prelude.Maybe Prelude.UTCTime)
campaign_creationDateTime = Lens.lens (\Campaign' {creationDateTime} -> creationDateTime) (\s@Campaign' {} a -> s {creationDateTime = a} :: Campaign) Prelude.. Lens.mapping Data._Time

-- | If a campaign fails, the reason behind the failure.
campaign_failureReason :: Lens.Lens' Campaign (Prelude.Maybe Prelude.Text)
campaign_failureReason = Lens.lens (\Campaign' {failureReason} -> failureReason) (\s@Campaign' {} a -> s {failureReason = a} :: Campaign)

-- | The date and time (in Unix format) that the campaign was last updated.
campaign_lastUpdatedDateTime :: Lens.Lens' Campaign (Prelude.Maybe Prelude.UTCTime)
campaign_lastUpdatedDateTime = Lens.lens (\Campaign' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@Campaign' {} a -> s {lastUpdatedDateTime = a} :: Campaign) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
campaign_latestCampaignUpdate :: Lens.Lens' Campaign (Prelude.Maybe CampaignUpdateSummary)
campaign_latestCampaignUpdate = Lens.lens (\Campaign' {latestCampaignUpdate} -> latestCampaignUpdate) (\s@Campaign' {} a -> s {latestCampaignUpdate = a} :: Campaign)

-- | Specifies the requested minimum provisioned transactions
-- (recommendations) per second.
campaign_minProvisionedTPS :: Lens.Lens' Campaign (Prelude.Maybe Prelude.Natural)
campaign_minProvisionedTPS = Lens.lens (\Campaign' {minProvisionedTPS} -> minProvisionedTPS) (\s@Campaign' {} a -> s {minProvisionedTPS = a} :: Campaign)

-- | The name of the campaign.
campaign_name :: Lens.Lens' Campaign (Prelude.Maybe Prelude.Text)
campaign_name = Lens.lens (\Campaign' {name} -> name) (\s@Campaign' {} a -> s {name = a} :: Campaign)

-- | The Amazon Resource Name (ARN) of a specific version of the solution.
campaign_solutionVersionArn :: Lens.Lens' Campaign (Prelude.Maybe Prelude.Text)
campaign_solutionVersionArn = Lens.lens (\Campaign' {solutionVersionArn} -> solutionVersionArn) (\s@Campaign' {} a -> s {solutionVersionArn = a} :: Campaign)

-- | The status of the campaign.
--
-- A campaign can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   DELETE PENDING > DELETE IN_PROGRESS
campaign_status :: Lens.Lens' Campaign (Prelude.Maybe Prelude.Text)
campaign_status = Lens.lens (\Campaign' {status} -> status) (\s@Campaign' {} a -> s {status = a} :: Campaign)

instance Data.FromJSON Campaign where
  parseJSON =
    Data.withObject
      "Campaign"
      ( \x ->
          Campaign'
            Prelude.<$> (x Data..:? "campaignArn")
            Prelude.<*> (x Data..:? "campaignConfig")
            Prelude.<*> (x Data..:? "creationDateTime")
            Prelude.<*> (x Data..:? "failureReason")
            Prelude.<*> (x Data..:? "lastUpdatedDateTime")
            Prelude.<*> (x Data..:? "latestCampaignUpdate")
            Prelude.<*> (x Data..:? "minProvisionedTPS")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "solutionVersionArn")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable Campaign where
  hashWithSalt _salt Campaign' {..} =
    _salt
      `Prelude.hashWithSalt` campaignArn
      `Prelude.hashWithSalt` campaignConfig
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` latestCampaignUpdate
      `Prelude.hashWithSalt` minProvisionedTPS
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` solutionVersionArn
      `Prelude.hashWithSalt` status

instance Prelude.NFData Campaign where
  rnf Campaign' {..} =
    Prelude.rnf campaignArn
      `Prelude.seq` Prelude.rnf campaignConfig
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf latestCampaignUpdate
      `Prelude.seq` Prelude.rnf minProvisionedTPS
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf solutionVersionArn
      `Prelude.seq` Prelude.rnf status
