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
-- Module      : Amazonka.Personalize.Types.CampaignUpdateSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.CampaignUpdateSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Personalize.Types.CampaignConfig
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of the properties of a campaign update. For a
-- complete listing, call the
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_DescribeCampaign.html DescribeCampaign>
-- API.
--
-- /See:/ 'newCampaignUpdateSummary' smart constructor.
data CampaignUpdateSummary = CampaignUpdateSummary'
  { -- | The date and time (in Unix time) that the campaign update was created.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    campaignConfig :: Prelude.Maybe CampaignConfig,
    -- | The status of the campaign update.
    --
    -- A campaign update can be in one of the following states:
    --
    -- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
    --
    -- -   DELETE PENDING > DELETE IN_PROGRESS
    status :: Prelude.Maybe Prelude.Text,
    -- | Specifies the requested minimum provisioned transactions
    -- (recommendations) per second that Amazon Personalize will support.
    minProvisionedTPS :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the deployed solution version.
    solutionVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time (in Unix time) that the campaign update was last
    -- updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | If a campaign update fails, the reason behind the failure.
    failureReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CampaignUpdateSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDateTime', 'campaignUpdateSummary_creationDateTime' - The date and time (in Unix time) that the campaign update was created.
--
-- 'campaignConfig', 'campaignUpdateSummary_campaignConfig' - Undocumented member.
--
-- 'status', 'campaignUpdateSummary_status' - The status of the campaign update.
--
-- A campaign update can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   DELETE PENDING > DELETE IN_PROGRESS
--
-- 'minProvisionedTPS', 'campaignUpdateSummary_minProvisionedTPS' - Specifies the requested minimum provisioned transactions
-- (recommendations) per second that Amazon Personalize will support.
--
-- 'solutionVersionArn', 'campaignUpdateSummary_solutionVersionArn' - The Amazon Resource Name (ARN) of the deployed solution version.
--
-- 'lastUpdatedDateTime', 'campaignUpdateSummary_lastUpdatedDateTime' - The date and time (in Unix time) that the campaign update was last
-- updated.
--
-- 'failureReason', 'campaignUpdateSummary_failureReason' - If a campaign update fails, the reason behind the failure.
newCampaignUpdateSummary ::
  CampaignUpdateSummary
newCampaignUpdateSummary =
  CampaignUpdateSummary'
    { creationDateTime =
        Prelude.Nothing,
      campaignConfig = Prelude.Nothing,
      status = Prelude.Nothing,
      minProvisionedTPS = Prelude.Nothing,
      solutionVersionArn = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      failureReason = Prelude.Nothing
    }

-- | The date and time (in Unix time) that the campaign update was created.
campaignUpdateSummary_creationDateTime :: Lens.Lens' CampaignUpdateSummary (Prelude.Maybe Prelude.UTCTime)
campaignUpdateSummary_creationDateTime = Lens.lens (\CampaignUpdateSummary' {creationDateTime} -> creationDateTime) (\s@CampaignUpdateSummary' {} a -> s {creationDateTime = a} :: CampaignUpdateSummary) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
campaignUpdateSummary_campaignConfig :: Lens.Lens' CampaignUpdateSummary (Prelude.Maybe CampaignConfig)
campaignUpdateSummary_campaignConfig = Lens.lens (\CampaignUpdateSummary' {campaignConfig} -> campaignConfig) (\s@CampaignUpdateSummary' {} a -> s {campaignConfig = a} :: CampaignUpdateSummary)

-- | The status of the campaign update.
--
-- A campaign update can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   DELETE PENDING > DELETE IN_PROGRESS
campaignUpdateSummary_status :: Lens.Lens' CampaignUpdateSummary (Prelude.Maybe Prelude.Text)
campaignUpdateSummary_status = Lens.lens (\CampaignUpdateSummary' {status} -> status) (\s@CampaignUpdateSummary' {} a -> s {status = a} :: CampaignUpdateSummary)

-- | Specifies the requested minimum provisioned transactions
-- (recommendations) per second that Amazon Personalize will support.
campaignUpdateSummary_minProvisionedTPS :: Lens.Lens' CampaignUpdateSummary (Prelude.Maybe Prelude.Natural)
campaignUpdateSummary_minProvisionedTPS = Lens.lens (\CampaignUpdateSummary' {minProvisionedTPS} -> minProvisionedTPS) (\s@CampaignUpdateSummary' {} a -> s {minProvisionedTPS = a} :: CampaignUpdateSummary)

-- | The Amazon Resource Name (ARN) of the deployed solution version.
campaignUpdateSummary_solutionVersionArn :: Lens.Lens' CampaignUpdateSummary (Prelude.Maybe Prelude.Text)
campaignUpdateSummary_solutionVersionArn = Lens.lens (\CampaignUpdateSummary' {solutionVersionArn} -> solutionVersionArn) (\s@CampaignUpdateSummary' {} a -> s {solutionVersionArn = a} :: CampaignUpdateSummary)

-- | The date and time (in Unix time) that the campaign update was last
-- updated.
campaignUpdateSummary_lastUpdatedDateTime :: Lens.Lens' CampaignUpdateSummary (Prelude.Maybe Prelude.UTCTime)
campaignUpdateSummary_lastUpdatedDateTime = Lens.lens (\CampaignUpdateSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@CampaignUpdateSummary' {} a -> s {lastUpdatedDateTime = a} :: CampaignUpdateSummary) Prelude.. Lens.mapping Core._Time

-- | If a campaign update fails, the reason behind the failure.
campaignUpdateSummary_failureReason :: Lens.Lens' CampaignUpdateSummary (Prelude.Maybe Prelude.Text)
campaignUpdateSummary_failureReason = Lens.lens (\CampaignUpdateSummary' {failureReason} -> failureReason) (\s@CampaignUpdateSummary' {} a -> s {failureReason = a} :: CampaignUpdateSummary)

instance Core.FromJSON CampaignUpdateSummary where
  parseJSON =
    Core.withObject
      "CampaignUpdateSummary"
      ( \x ->
          CampaignUpdateSummary'
            Prelude.<$> (x Core..:? "creationDateTime")
            Prelude.<*> (x Core..:? "campaignConfig")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "minProvisionedTPS")
            Prelude.<*> (x Core..:? "solutionVersionArn")
            Prelude.<*> (x Core..:? "lastUpdatedDateTime")
            Prelude.<*> (x Core..:? "failureReason")
      )

instance Prelude.Hashable CampaignUpdateSummary where
  hashWithSalt _salt CampaignUpdateSummary' {..} =
    _salt `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` campaignConfig
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` minProvisionedTPS
      `Prelude.hashWithSalt` solutionVersionArn
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` failureReason

instance Prelude.NFData CampaignUpdateSummary where
  rnf CampaignUpdateSummary' {..} =
    Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf campaignConfig
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf minProvisionedTPS
      `Prelude.seq` Prelude.rnf solutionVersionArn
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf failureReason
