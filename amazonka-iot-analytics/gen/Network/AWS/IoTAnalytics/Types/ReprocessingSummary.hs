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
-- Module      : Network.AWS.IoTAnalytics.Types.ReprocessingSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ReprocessingSummary where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types.ReprocessingStatus
import qualified Network.AWS.Lens as Lens

-- | Information about pipeline reprocessing.
--
-- /See:/ 'newReprocessingSummary' smart constructor.
data ReprocessingSummary = ReprocessingSummary'
  { -- | The status of the pipeline reprocessing.
    status :: Core.Maybe ReprocessingStatus,
    -- | The time the pipeline reprocessing was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The @reprocessingId@ returned by @StartPipelineReprocessing@.
    id :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReprocessingSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'reprocessingSummary_status' - The status of the pipeline reprocessing.
--
-- 'creationTime', 'reprocessingSummary_creationTime' - The time the pipeline reprocessing was created.
--
-- 'id', 'reprocessingSummary_id' - The @reprocessingId@ returned by @StartPipelineReprocessing@.
newReprocessingSummary ::
  ReprocessingSummary
newReprocessingSummary =
  ReprocessingSummary'
    { status = Core.Nothing,
      creationTime = Core.Nothing,
      id = Core.Nothing
    }

-- | The status of the pipeline reprocessing.
reprocessingSummary_status :: Lens.Lens' ReprocessingSummary (Core.Maybe ReprocessingStatus)
reprocessingSummary_status = Lens.lens (\ReprocessingSummary' {status} -> status) (\s@ReprocessingSummary' {} a -> s {status = a} :: ReprocessingSummary)

-- | The time the pipeline reprocessing was created.
reprocessingSummary_creationTime :: Lens.Lens' ReprocessingSummary (Core.Maybe Core.UTCTime)
reprocessingSummary_creationTime = Lens.lens (\ReprocessingSummary' {creationTime} -> creationTime) (\s@ReprocessingSummary' {} a -> s {creationTime = a} :: ReprocessingSummary) Core.. Lens.mapping Core._Time

-- | The @reprocessingId@ returned by @StartPipelineReprocessing@.
reprocessingSummary_id :: Lens.Lens' ReprocessingSummary (Core.Maybe Core.Text)
reprocessingSummary_id = Lens.lens (\ReprocessingSummary' {id} -> id) (\s@ReprocessingSummary' {} a -> s {id = a} :: ReprocessingSummary)

instance Core.FromJSON ReprocessingSummary where
  parseJSON =
    Core.withObject
      "ReprocessingSummary"
      ( \x ->
          ReprocessingSummary'
            Core.<$> (x Core..:? "status")
            Core.<*> (x Core..:? "creationTime")
            Core.<*> (x Core..:? "id")
      )

instance Core.Hashable ReprocessingSummary

instance Core.NFData ReprocessingSummary
