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
-- Module      : Network.AWS.MediaPackage.Types.HarvestJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.HarvestJob where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types.S3Destination
import Network.AWS.MediaPackage.Types.Status

-- | A HarvestJob resource configuration
--
-- /See:/ 'newHarvestJob' smart constructor.
data HarvestJob = HarvestJob'
  { -- | The current status of the HarvestJob. Consider setting up a CloudWatch
    -- Event to listen for HarvestJobs as they succeed or fail. In the event of
    -- failure, the CloudWatch Event will include an explanation of why the
    -- HarvestJob failed.
    status :: Core.Maybe Status,
    s3Destination :: Core.Maybe S3Destination,
    -- | The ID of the Channel that the HarvestJob will harvest from.
    channelId :: Core.Maybe Core.Text,
    -- | The start of the time-window which will be harvested.
    startTime :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) assigned to the HarvestJob.
    arn :: Core.Maybe Core.Text,
    -- | The ID of the HarvestJob. The ID must be unique within the region and it
    -- cannot be changed after the HarvestJob is submitted.
    id :: Core.Maybe Core.Text,
    -- | The time the HarvestJob was submitted
    createdAt :: Core.Maybe Core.Text,
    -- | The ID of the OriginEndpoint that the HarvestJob will harvest from. This
    -- cannot be changed after the HarvestJob is submitted.
    originEndpointId :: Core.Maybe Core.Text,
    -- | The end of the time-window which will be harvested.
    endTime :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HarvestJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'harvestJob_status' - The current status of the HarvestJob. Consider setting up a CloudWatch
-- Event to listen for HarvestJobs as they succeed or fail. In the event of
-- failure, the CloudWatch Event will include an explanation of why the
-- HarvestJob failed.
--
-- 's3Destination', 'harvestJob_s3Destination' - Undocumented member.
--
-- 'channelId', 'harvestJob_channelId' - The ID of the Channel that the HarvestJob will harvest from.
--
-- 'startTime', 'harvestJob_startTime' - The start of the time-window which will be harvested.
--
-- 'arn', 'harvestJob_arn' - The Amazon Resource Name (ARN) assigned to the HarvestJob.
--
-- 'id', 'harvestJob_id' - The ID of the HarvestJob. The ID must be unique within the region and it
-- cannot be changed after the HarvestJob is submitted.
--
-- 'createdAt', 'harvestJob_createdAt' - The time the HarvestJob was submitted
--
-- 'originEndpointId', 'harvestJob_originEndpointId' - The ID of the OriginEndpoint that the HarvestJob will harvest from. This
-- cannot be changed after the HarvestJob is submitted.
--
-- 'endTime', 'harvestJob_endTime' - The end of the time-window which will be harvested.
newHarvestJob ::
  HarvestJob
newHarvestJob =
  HarvestJob'
    { status = Core.Nothing,
      s3Destination = Core.Nothing,
      channelId = Core.Nothing,
      startTime = Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      createdAt = Core.Nothing,
      originEndpointId = Core.Nothing,
      endTime = Core.Nothing
    }

-- | The current status of the HarvestJob. Consider setting up a CloudWatch
-- Event to listen for HarvestJobs as they succeed or fail. In the event of
-- failure, the CloudWatch Event will include an explanation of why the
-- HarvestJob failed.
harvestJob_status :: Lens.Lens' HarvestJob (Core.Maybe Status)
harvestJob_status = Lens.lens (\HarvestJob' {status} -> status) (\s@HarvestJob' {} a -> s {status = a} :: HarvestJob)

-- | Undocumented member.
harvestJob_s3Destination :: Lens.Lens' HarvestJob (Core.Maybe S3Destination)
harvestJob_s3Destination = Lens.lens (\HarvestJob' {s3Destination} -> s3Destination) (\s@HarvestJob' {} a -> s {s3Destination = a} :: HarvestJob)

-- | The ID of the Channel that the HarvestJob will harvest from.
harvestJob_channelId :: Lens.Lens' HarvestJob (Core.Maybe Core.Text)
harvestJob_channelId = Lens.lens (\HarvestJob' {channelId} -> channelId) (\s@HarvestJob' {} a -> s {channelId = a} :: HarvestJob)

-- | The start of the time-window which will be harvested.
harvestJob_startTime :: Lens.Lens' HarvestJob (Core.Maybe Core.Text)
harvestJob_startTime = Lens.lens (\HarvestJob' {startTime} -> startTime) (\s@HarvestJob' {} a -> s {startTime = a} :: HarvestJob)

-- | The Amazon Resource Name (ARN) assigned to the HarvestJob.
harvestJob_arn :: Lens.Lens' HarvestJob (Core.Maybe Core.Text)
harvestJob_arn = Lens.lens (\HarvestJob' {arn} -> arn) (\s@HarvestJob' {} a -> s {arn = a} :: HarvestJob)

-- | The ID of the HarvestJob. The ID must be unique within the region and it
-- cannot be changed after the HarvestJob is submitted.
harvestJob_id :: Lens.Lens' HarvestJob (Core.Maybe Core.Text)
harvestJob_id = Lens.lens (\HarvestJob' {id} -> id) (\s@HarvestJob' {} a -> s {id = a} :: HarvestJob)

-- | The time the HarvestJob was submitted
harvestJob_createdAt :: Lens.Lens' HarvestJob (Core.Maybe Core.Text)
harvestJob_createdAt = Lens.lens (\HarvestJob' {createdAt} -> createdAt) (\s@HarvestJob' {} a -> s {createdAt = a} :: HarvestJob)

-- | The ID of the OriginEndpoint that the HarvestJob will harvest from. This
-- cannot be changed after the HarvestJob is submitted.
harvestJob_originEndpointId :: Lens.Lens' HarvestJob (Core.Maybe Core.Text)
harvestJob_originEndpointId = Lens.lens (\HarvestJob' {originEndpointId} -> originEndpointId) (\s@HarvestJob' {} a -> s {originEndpointId = a} :: HarvestJob)

-- | The end of the time-window which will be harvested.
harvestJob_endTime :: Lens.Lens' HarvestJob (Core.Maybe Core.Text)
harvestJob_endTime = Lens.lens (\HarvestJob' {endTime} -> endTime) (\s@HarvestJob' {} a -> s {endTime = a} :: HarvestJob)

instance Core.FromJSON HarvestJob where
  parseJSON =
    Core.withObject
      "HarvestJob"
      ( \x ->
          HarvestJob'
            Core.<$> (x Core..:? "status")
            Core.<*> (x Core..:? "s3Destination")
            Core.<*> (x Core..:? "channelId")
            Core.<*> (x Core..:? "startTime")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "createdAt")
            Core.<*> (x Core..:? "originEndpointId")
            Core.<*> (x Core..:? "endTime")
      )

instance Core.Hashable HarvestJob

instance Core.NFData HarvestJob
