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
-- Module      : Network.AWS.Redshift.Types.DataTransferProgress
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.DataTransferProgress where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal

-- | Describes the status of a cluster while it is in the process of resizing
-- with an incremental resize.
--
-- /See:/ 'newDataTransferProgress' smart constructor.
data DataTransferProgress = DataTransferProgress'
  { -- | Describes the status of the cluster. While the transfer is in progress
    -- the status is @transferringdata@.
    status :: Core.Maybe Core.Text,
    -- | Describes the estimated number of seconds remaining to complete the
    -- transfer.
    estimatedTimeToCompletionInSeconds :: Core.Maybe Core.Integer,
    -- | Describes the total amount of data that has been transfered in MB\'s.
    dataTransferredInMegaBytes :: Core.Maybe Core.Integer,
    -- | Describes the data transfer rate in MB\'s per second.
    currentRateInMegaBytesPerSecond :: Core.Maybe Core.Double,
    -- | Describes the number of seconds that have elapsed during the data
    -- transfer.
    elapsedTimeInSeconds :: Core.Maybe Core.Integer,
    -- | Describes the total amount of data to be transfered in megabytes.
    totalDataInMegaBytes :: Core.Maybe Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DataTransferProgress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'dataTransferProgress_status' - Describes the status of the cluster. While the transfer is in progress
-- the status is @transferringdata@.
--
-- 'estimatedTimeToCompletionInSeconds', 'dataTransferProgress_estimatedTimeToCompletionInSeconds' - Describes the estimated number of seconds remaining to complete the
-- transfer.
--
-- 'dataTransferredInMegaBytes', 'dataTransferProgress_dataTransferredInMegaBytes' - Describes the total amount of data that has been transfered in MB\'s.
--
-- 'currentRateInMegaBytesPerSecond', 'dataTransferProgress_currentRateInMegaBytesPerSecond' - Describes the data transfer rate in MB\'s per second.
--
-- 'elapsedTimeInSeconds', 'dataTransferProgress_elapsedTimeInSeconds' - Describes the number of seconds that have elapsed during the data
-- transfer.
--
-- 'totalDataInMegaBytes', 'dataTransferProgress_totalDataInMegaBytes' - Describes the total amount of data to be transfered in megabytes.
newDataTransferProgress ::
  DataTransferProgress
newDataTransferProgress =
  DataTransferProgress'
    { status = Core.Nothing,
      estimatedTimeToCompletionInSeconds = Core.Nothing,
      dataTransferredInMegaBytes = Core.Nothing,
      currentRateInMegaBytesPerSecond = Core.Nothing,
      elapsedTimeInSeconds = Core.Nothing,
      totalDataInMegaBytes = Core.Nothing
    }

-- | Describes the status of the cluster. While the transfer is in progress
-- the status is @transferringdata@.
dataTransferProgress_status :: Lens.Lens' DataTransferProgress (Core.Maybe Core.Text)
dataTransferProgress_status = Lens.lens (\DataTransferProgress' {status} -> status) (\s@DataTransferProgress' {} a -> s {status = a} :: DataTransferProgress)

-- | Describes the estimated number of seconds remaining to complete the
-- transfer.
dataTransferProgress_estimatedTimeToCompletionInSeconds :: Lens.Lens' DataTransferProgress (Core.Maybe Core.Integer)
dataTransferProgress_estimatedTimeToCompletionInSeconds = Lens.lens (\DataTransferProgress' {estimatedTimeToCompletionInSeconds} -> estimatedTimeToCompletionInSeconds) (\s@DataTransferProgress' {} a -> s {estimatedTimeToCompletionInSeconds = a} :: DataTransferProgress)

-- | Describes the total amount of data that has been transfered in MB\'s.
dataTransferProgress_dataTransferredInMegaBytes :: Lens.Lens' DataTransferProgress (Core.Maybe Core.Integer)
dataTransferProgress_dataTransferredInMegaBytes = Lens.lens (\DataTransferProgress' {dataTransferredInMegaBytes} -> dataTransferredInMegaBytes) (\s@DataTransferProgress' {} a -> s {dataTransferredInMegaBytes = a} :: DataTransferProgress)

-- | Describes the data transfer rate in MB\'s per second.
dataTransferProgress_currentRateInMegaBytesPerSecond :: Lens.Lens' DataTransferProgress (Core.Maybe Core.Double)
dataTransferProgress_currentRateInMegaBytesPerSecond = Lens.lens (\DataTransferProgress' {currentRateInMegaBytesPerSecond} -> currentRateInMegaBytesPerSecond) (\s@DataTransferProgress' {} a -> s {currentRateInMegaBytesPerSecond = a} :: DataTransferProgress)

-- | Describes the number of seconds that have elapsed during the data
-- transfer.
dataTransferProgress_elapsedTimeInSeconds :: Lens.Lens' DataTransferProgress (Core.Maybe Core.Integer)
dataTransferProgress_elapsedTimeInSeconds = Lens.lens (\DataTransferProgress' {elapsedTimeInSeconds} -> elapsedTimeInSeconds) (\s@DataTransferProgress' {} a -> s {elapsedTimeInSeconds = a} :: DataTransferProgress)

-- | Describes the total amount of data to be transfered in megabytes.
dataTransferProgress_totalDataInMegaBytes :: Lens.Lens' DataTransferProgress (Core.Maybe Core.Integer)
dataTransferProgress_totalDataInMegaBytes = Lens.lens (\DataTransferProgress' {totalDataInMegaBytes} -> totalDataInMegaBytes) (\s@DataTransferProgress' {} a -> s {totalDataInMegaBytes = a} :: DataTransferProgress)

instance Core.FromXML DataTransferProgress where
  parseXML x =
    DataTransferProgress'
      Core.<$> (x Core..@? "Status")
      Core.<*> (x Core..@? "EstimatedTimeToCompletionInSeconds")
      Core.<*> (x Core..@? "DataTransferredInMegaBytes")
      Core.<*> (x Core..@? "CurrentRateInMegaBytesPerSecond")
      Core.<*> (x Core..@? "ElapsedTimeInSeconds")
      Core.<*> (x Core..@? "TotalDataInMegaBytes")

instance Core.Hashable DataTransferProgress

instance Core.NFData DataTransferProgress
