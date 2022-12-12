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
-- Module      : Amazonka.MediaPackage.Types.HarvestJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackage.Types.HarvestJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackage.Types.S3Destination
import Amazonka.MediaPackage.Types.Status
import qualified Amazonka.Prelude as Prelude

-- | A HarvestJob resource configuration
--
-- /See:/ 'newHarvestJob' smart constructor.
data HarvestJob = HarvestJob'
  { -- | The Amazon Resource Name (ARN) assigned to the HarvestJob.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Channel that the HarvestJob will harvest from.
    channelId :: Prelude.Maybe Prelude.Text,
    -- | The time the HarvestJob was submitted
    createdAt :: Prelude.Maybe Prelude.Text,
    -- | The end of the time-window which will be harvested.
    endTime :: Prelude.Maybe Prelude.Text,
    -- | The ID of the HarvestJob. The ID must be unique within the region and it
    -- cannot be changed after the HarvestJob is submitted.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ID of the OriginEndpoint that the HarvestJob will harvest from. This
    -- cannot be changed after the HarvestJob is submitted.
    originEndpointId :: Prelude.Maybe Prelude.Text,
    s3Destination :: Prelude.Maybe S3Destination,
    -- | The start of the time-window which will be harvested.
    startTime :: Prelude.Maybe Prelude.Text,
    -- | The current status of the HarvestJob. Consider setting up a CloudWatch
    -- Event to listen for HarvestJobs as they succeed or fail. In the event of
    -- failure, the CloudWatch Event will include an explanation of why the
    -- HarvestJob failed.
    status :: Prelude.Maybe Status
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HarvestJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'harvestJob_arn' - The Amazon Resource Name (ARN) assigned to the HarvestJob.
--
-- 'channelId', 'harvestJob_channelId' - The ID of the Channel that the HarvestJob will harvest from.
--
-- 'createdAt', 'harvestJob_createdAt' - The time the HarvestJob was submitted
--
-- 'endTime', 'harvestJob_endTime' - The end of the time-window which will be harvested.
--
-- 'id', 'harvestJob_id' - The ID of the HarvestJob. The ID must be unique within the region and it
-- cannot be changed after the HarvestJob is submitted.
--
-- 'originEndpointId', 'harvestJob_originEndpointId' - The ID of the OriginEndpoint that the HarvestJob will harvest from. This
-- cannot be changed after the HarvestJob is submitted.
--
-- 's3Destination', 'harvestJob_s3Destination' - Undocumented member.
--
-- 'startTime', 'harvestJob_startTime' - The start of the time-window which will be harvested.
--
-- 'status', 'harvestJob_status' - The current status of the HarvestJob. Consider setting up a CloudWatch
-- Event to listen for HarvestJobs as they succeed or fail. In the event of
-- failure, the CloudWatch Event will include an explanation of why the
-- HarvestJob failed.
newHarvestJob ::
  HarvestJob
newHarvestJob =
  HarvestJob'
    { arn = Prelude.Nothing,
      channelId = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      endTime = Prelude.Nothing,
      id = Prelude.Nothing,
      originEndpointId = Prelude.Nothing,
      s3Destination = Prelude.Nothing,
      startTime = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) assigned to the HarvestJob.
harvestJob_arn :: Lens.Lens' HarvestJob (Prelude.Maybe Prelude.Text)
harvestJob_arn = Lens.lens (\HarvestJob' {arn} -> arn) (\s@HarvestJob' {} a -> s {arn = a} :: HarvestJob)

-- | The ID of the Channel that the HarvestJob will harvest from.
harvestJob_channelId :: Lens.Lens' HarvestJob (Prelude.Maybe Prelude.Text)
harvestJob_channelId = Lens.lens (\HarvestJob' {channelId} -> channelId) (\s@HarvestJob' {} a -> s {channelId = a} :: HarvestJob)

-- | The time the HarvestJob was submitted
harvestJob_createdAt :: Lens.Lens' HarvestJob (Prelude.Maybe Prelude.Text)
harvestJob_createdAt = Lens.lens (\HarvestJob' {createdAt} -> createdAt) (\s@HarvestJob' {} a -> s {createdAt = a} :: HarvestJob)

-- | The end of the time-window which will be harvested.
harvestJob_endTime :: Lens.Lens' HarvestJob (Prelude.Maybe Prelude.Text)
harvestJob_endTime = Lens.lens (\HarvestJob' {endTime} -> endTime) (\s@HarvestJob' {} a -> s {endTime = a} :: HarvestJob)

-- | The ID of the HarvestJob. The ID must be unique within the region and it
-- cannot be changed after the HarvestJob is submitted.
harvestJob_id :: Lens.Lens' HarvestJob (Prelude.Maybe Prelude.Text)
harvestJob_id = Lens.lens (\HarvestJob' {id} -> id) (\s@HarvestJob' {} a -> s {id = a} :: HarvestJob)

-- | The ID of the OriginEndpoint that the HarvestJob will harvest from. This
-- cannot be changed after the HarvestJob is submitted.
harvestJob_originEndpointId :: Lens.Lens' HarvestJob (Prelude.Maybe Prelude.Text)
harvestJob_originEndpointId = Lens.lens (\HarvestJob' {originEndpointId} -> originEndpointId) (\s@HarvestJob' {} a -> s {originEndpointId = a} :: HarvestJob)

-- | Undocumented member.
harvestJob_s3Destination :: Lens.Lens' HarvestJob (Prelude.Maybe S3Destination)
harvestJob_s3Destination = Lens.lens (\HarvestJob' {s3Destination} -> s3Destination) (\s@HarvestJob' {} a -> s {s3Destination = a} :: HarvestJob)

-- | The start of the time-window which will be harvested.
harvestJob_startTime :: Lens.Lens' HarvestJob (Prelude.Maybe Prelude.Text)
harvestJob_startTime = Lens.lens (\HarvestJob' {startTime} -> startTime) (\s@HarvestJob' {} a -> s {startTime = a} :: HarvestJob)

-- | The current status of the HarvestJob. Consider setting up a CloudWatch
-- Event to listen for HarvestJobs as they succeed or fail. In the event of
-- failure, the CloudWatch Event will include an explanation of why the
-- HarvestJob failed.
harvestJob_status :: Lens.Lens' HarvestJob (Prelude.Maybe Status)
harvestJob_status = Lens.lens (\HarvestJob' {status} -> status) (\s@HarvestJob' {} a -> s {status = a} :: HarvestJob)

instance Data.FromJSON HarvestJob where
  parseJSON =
    Data.withObject
      "HarvestJob"
      ( \x ->
          HarvestJob'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "channelId")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "endTime")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "originEndpointId")
            Prelude.<*> (x Data..:? "s3Destination")
            Prelude.<*> (x Data..:? "startTime")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable HarvestJob where
  hashWithSalt _salt HarvestJob' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` channelId
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` originEndpointId
      `Prelude.hashWithSalt` s3Destination
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` status

instance Prelude.NFData HarvestJob where
  rnf HarvestJob' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf channelId
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf originEndpointId
      `Prelude.seq` Prelude.rnf s3Destination
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf status
