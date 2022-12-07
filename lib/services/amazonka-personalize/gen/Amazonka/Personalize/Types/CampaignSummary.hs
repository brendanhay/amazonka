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
-- Module      : Amazonka.Personalize.Types.CampaignSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.CampaignSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of the properties of a campaign. For a complete
-- listing, call the
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_DescribeCampaign.html DescribeCampaign>
-- API.
--
-- /See:/ 'newCampaignSummary' smart constructor.
data CampaignSummary = CampaignSummary'
  { -- | The name of the campaign.
    name :: Prelude.Maybe Prelude.Text,
    -- | The date and time (in Unix time) that the campaign was created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The status of the campaign.
    --
    -- A campaign can be in one of the following states:
    --
    -- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
    --
    -- -   DELETE PENDING > DELETE IN_PROGRESS
    status :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the campaign.
    campaignArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time (in Unix time) that the campaign was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | If a campaign fails, the reason behind the failure.
    failureReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CampaignSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'campaignSummary_name' - The name of the campaign.
--
-- 'creationDateTime', 'campaignSummary_creationDateTime' - The date and time (in Unix time) that the campaign was created.
--
-- 'status', 'campaignSummary_status' - The status of the campaign.
--
-- A campaign can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   DELETE PENDING > DELETE IN_PROGRESS
--
-- 'campaignArn', 'campaignSummary_campaignArn' - The Amazon Resource Name (ARN) of the campaign.
--
-- 'lastUpdatedDateTime', 'campaignSummary_lastUpdatedDateTime' - The date and time (in Unix time) that the campaign was last updated.
--
-- 'failureReason', 'campaignSummary_failureReason' - If a campaign fails, the reason behind the failure.
newCampaignSummary ::
  CampaignSummary
newCampaignSummary =
  CampaignSummary'
    { name = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      status = Prelude.Nothing,
      campaignArn = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      failureReason = Prelude.Nothing
    }

-- | The name of the campaign.
campaignSummary_name :: Lens.Lens' CampaignSummary (Prelude.Maybe Prelude.Text)
campaignSummary_name = Lens.lens (\CampaignSummary' {name} -> name) (\s@CampaignSummary' {} a -> s {name = a} :: CampaignSummary)

-- | The date and time (in Unix time) that the campaign was created.
campaignSummary_creationDateTime :: Lens.Lens' CampaignSummary (Prelude.Maybe Prelude.UTCTime)
campaignSummary_creationDateTime = Lens.lens (\CampaignSummary' {creationDateTime} -> creationDateTime) (\s@CampaignSummary' {} a -> s {creationDateTime = a} :: CampaignSummary) Prelude.. Lens.mapping Data._Time

-- | The status of the campaign.
--
-- A campaign can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   DELETE PENDING > DELETE IN_PROGRESS
campaignSummary_status :: Lens.Lens' CampaignSummary (Prelude.Maybe Prelude.Text)
campaignSummary_status = Lens.lens (\CampaignSummary' {status} -> status) (\s@CampaignSummary' {} a -> s {status = a} :: CampaignSummary)

-- | The Amazon Resource Name (ARN) of the campaign.
campaignSummary_campaignArn :: Lens.Lens' CampaignSummary (Prelude.Maybe Prelude.Text)
campaignSummary_campaignArn = Lens.lens (\CampaignSummary' {campaignArn} -> campaignArn) (\s@CampaignSummary' {} a -> s {campaignArn = a} :: CampaignSummary)

-- | The date and time (in Unix time) that the campaign was last updated.
campaignSummary_lastUpdatedDateTime :: Lens.Lens' CampaignSummary (Prelude.Maybe Prelude.UTCTime)
campaignSummary_lastUpdatedDateTime = Lens.lens (\CampaignSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@CampaignSummary' {} a -> s {lastUpdatedDateTime = a} :: CampaignSummary) Prelude.. Lens.mapping Data._Time

-- | If a campaign fails, the reason behind the failure.
campaignSummary_failureReason :: Lens.Lens' CampaignSummary (Prelude.Maybe Prelude.Text)
campaignSummary_failureReason = Lens.lens (\CampaignSummary' {failureReason} -> failureReason) (\s@CampaignSummary' {} a -> s {failureReason = a} :: CampaignSummary)

instance Data.FromJSON CampaignSummary where
  parseJSON =
    Data.withObject
      "CampaignSummary"
      ( \x ->
          CampaignSummary'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "creationDateTime")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "campaignArn")
            Prelude.<*> (x Data..:? "lastUpdatedDateTime")
            Prelude.<*> (x Data..:? "failureReason")
      )

instance Prelude.Hashable CampaignSummary where
  hashWithSalt _salt CampaignSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` campaignArn
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` failureReason

instance Prelude.NFData CampaignSummary where
  rnf CampaignSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf campaignArn
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf failureReason
