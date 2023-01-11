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
-- Module      : Amazonka.IoTFleetWise.Types.CampaignSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.CampaignSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types.CampaignStatus
import qualified Amazonka.Prelude as Prelude

-- | Information about a campaign.
--
-- You can use the API operation to return this information about multiple
-- created campaigns.
--
-- /See:/ 'newCampaignSummary' smart constructor.
data CampaignSummary = CampaignSummary'
  { -- | The Amazon Resource Name (ARN) of a campaign.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The description of the campaign.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of a campaign.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the signal catalog associated with the campaign.
    signalCatalogArn :: Prelude.Maybe Prelude.Text,
    -- | The state of a campaign. The status can be one of the following:
    --
    -- -   @CREATING@ - Amazon Web Services IoT FleetWise is processing your
    --     request to create the campaign.
    --
    -- -   @WAITING_FOR_APPROVAL@ - After a campaign is created, it enters the
    --     @WAITING_FOR_APPROVAL@ state. To allow Amazon Web Services IoT
    --     FleetWise to deploy the campaign to the target vehicle or fleet, use
    --     the API operation to approve the campaign.
    --
    -- -   @RUNNING@ - The campaign is active.
    --
    -- -   @SUSPENDED@ - The campaign is suspended. To resume the campaign, use
    --     the API operation.
    status :: Prelude.Maybe CampaignStatus,
    -- | The ARN of a vehicle or fleet to which the campaign is deployed.
    targetArn :: Prelude.Maybe Prelude.Text,
    -- | The time the campaign was created.
    creationTime :: Data.POSIX,
    -- | The last time the campaign was modified.
    lastModificationTime :: Data.POSIX
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
-- 'arn', 'campaignSummary_arn' - The Amazon Resource Name (ARN) of a campaign.
--
-- 'description', 'campaignSummary_description' - The description of the campaign.
--
-- 'name', 'campaignSummary_name' - The name of a campaign.
--
-- 'signalCatalogArn', 'campaignSummary_signalCatalogArn' - The ARN of the signal catalog associated with the campaign.
--
-- 'status', 'campaignSummary_status' - The state of a campaign. The status can be one of the following:
--
-- -   @CREATING@ - Amazon Web Services IoT FleetWise is processing your
--     request to create the campaign.
--
-- -   @WAITING_FOR_APPROVAL@ - After a campaign is created, it enters the
--     @WAITING_FOR_APPROVAL@ state. To allow Amazon Web Services IoT
--     FleetWise to deploy the campaign to the target vehicle or fleet, use
--     the API operation to approve the campaign.
--
-- -   @RUNNING@ - The campaign is active.
--
-- -   @SUSPENDED@ - The campaign is suspended. To resume the campaign, use
--     the API operation.
--
-- 'targetArn', 'campaignSummary_targetArn' - The ARN of a vehicle or fleet to which the campaign is deployed.
--
-- 'creationTime', 'campaignSummary_creationTime' - The time the campaign was created.
--
-- 'lastModificationTime', 'campaignSummary_lastModificationTime' - The last time the campaign was modified.
newCampaignSummary ::
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'lastModificationTime'
  Prelude.UTCTime ->
  CampaignSummary
newCampaignSummary
  pCreationTime_
  pLastModificationTime_ =
    CampaignSummary'
      { arn = Prelude.Nothing,
        description = Prelude.Nothing,
        name = Prelude.Nothing,
        signalCatalogArn = Prelude.Nothing,
        status = Prelude.Nothing,
        targetArn = Prelude.Nothing,
        creationTime = Data._Time Lens.# pCreationTime_,
        lastModificationTime =
          Data._Time Lens.# pLastModificationTime_
      }

-- | The Amazon Resource Name (ARN) of a campaign.
campaignSummary_arn :: Lens.Lens' CampaignSummary (Prelude.Maybe Prelude.Text)
campaignSummary_arn = Lens.lens (\CampaignSummary' {arn} -> arn) (\s@CampaignSummary' {} a -> s {arn = a} :: CampaignSummary)

-- | The description of the campaign.
campaignSummary_description :: Lens.Lens' CampaignSummary (Prelude.Maybe Prelude.Text)
campaignSummary_description = Lens.lens (\CampaignSummary' {description} -> description) (\s@CampaignSummary' {} a -> s {description = a} :: CampaignSummary)

-- | The name of a campaign.
campaignSummary_name :: Lens.Lens' CampaignSummary (Prelude.Maybe Prelude.Text)
campaignSummary_name = Lens.lens (\CampaignSummary' {name} -> name) (\s@CampaignSummary' {} a -> s {name = a} :: CampaignSummary)

-- | The ARN of the signal catalog associated with the campaign.
campaignSummary_signalCatalogArn :: Lens.Lens' CampaignSummary (Prelude.Maybe Prelude.Text)
campaignSummary_signalCatalogArn = Lens.lens (\CampaignSummary' {signalCatalogArn} -> signalCatalogArn) (\s@CampaignSummary' {} a -> s {signalCatalogArn = a} :: CampaignSummary)

-- | The state of a campaign. The status can be one of the following:
--
-- -   @CREATING@ - Amazon Web Services IoT FleetWise is processing your
--     request to create the campaign.
--
-- -   @WAITING_FOR_APPROVAL@ - After a campaign is created, it enters the
--     @WAITING_FOR_APPROVAL@ state. To allow Amazon Web Services IoT
--     FleetWise to deploy the campaign to the target vehicle or fleet, use
--     the API operation to approve the campaign.
--
-- -   @RUNNING@ - The campaign is active.
--
-- -   @SUSPENDED@ - The campaign is suspended. To resume the campaign, use
--     the API operation.
campaignSummary_status :: Lens.Lens' CampaignSummary (Prelude.Maybe CampaignStatus)
campaignSummary_status = Lens.lens (\CampaignSummary' {status} -> status) (\s@CampaignSummary' {} a -> s {status = a} :: CampaignSummary)

-- | The ARN of a vehicle or fleet to which the campaign is deployed.
campaignSummary_targetArn :: Lens.Lens' CampaignSummary (Prelude.Maybe Prelude.Text)
campaignSummary_targetArn = Lens.lens (\CampaignSummary' {targetArn} -> targetArn) (\s@CampaignSummary' {} a -> s {targetArn = a} :: CampaignSummary)

-- | The time the campaign was created.
campaignSummary_creationTime :: Lens.Lens' CampaignSummary Prelude.UTCTime
campaignSummary_creationTime = Lens.lens (\CampaignSummary' {creationTime} -> creationTime) (\s@CampaignSummary' {} a -> s {creationTime = a} :: CampaignSummary) Prelude.. Data._Time

-- | The last time the campaign was modified.
campaignSummary_lastModificationTime :: Lens.Lens' CampaignSummary Prelude.UTCTime
campaignSummary_lastModificationTime = Lens.lens (\CampaignSummary' {lastModificationTime} -> lastModificationTime) (\s@CampaignSummary' {} a -> s {lastModificationTime = a} :: CampaignSummary) Prelude.. Data._Time

instance Data.FromJSON CampaignSummary where
  parseJSON =
    Data.withObject
      "CampaignSummary"
      ( \x ->
          CampaignSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "signalCatalogArn")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "targetArn")
            Prelude.<*> (x Data..: "creationTime")
            Prelude.<*> (x Data..: "lastModificationTime")
      )

instance Prelude.Hashable CampaignSummary where
  hashWithSalt _salt CampaignSummary' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` signalCatalogArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` targetArn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastModificationTime

instance Prelude.NFData CampaignSummary where
  rnf CampaignSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf signalCatalogArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf targetArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModificationTime
