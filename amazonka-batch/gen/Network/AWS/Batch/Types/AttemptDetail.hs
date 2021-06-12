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
-- Module      : Network.AWS.Batch.Types.AttemptDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.AttemptDetail where

import Network.AWS.Batch.Types.AttemptContainerDetail
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An object representing a job attempt.
--
-- /See:/ 'newAttemptDetail' smart constructor.
data AttemptDetail = AttemptDetail'
  { -- | Details about the container in this job attempt.
    container :: Core.Maybe AttemptContainerDetail,
    -- | The Unix timestamp (in milliseconds) for when the attempt was started
    -- (when the attempt transitioned from the @STARTING@ state to the
    -- @RUNNING@ state).
    startedAt :: Core.Maybe Core.Integer,
    -- | The Unix timestamp (in milliseconds) for when the attempt was stopped
    -- (when the attempt transitioned from the @RUNNING@ state to a terminal
    -- state, such as @SUCCEEDED@ or @FAILED@).
    stoppedAt :: Core.Maybe Core.Integer,
    -- | A short, human-readable string to provide additional details about the
    -- current status of the job attempt.
    statusReason :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AttemptDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'container', 'attemptDetail_container' - Details about the container in this job attempt.
--
-- 'startedAt', 'attemptDetail_startedAt' - The Unix timestamp (in milliseconds) for when the attempt was started
-- (when the attempt transitioned from the @STARTING@ state to the
-- @RUNNING@ state).
--
-- 'stoppedAt', 'attemptDetail_stoppedAt' - The Unix timestamp (in milliseconds) for when the attempt was stopped
-- (when the attempt transitioned from the @RUNNING@ state to a terminal
-- state, such as @SUCCEEDED@ or @FAILED@).
--
-- 'statusReason', 'attemptDetail_statusReason' - A short, human-readable string to provide additional details about the
-- current status of the job attempt.
newAttemptDetail ::
  AttemptDetail
newAttemptDetail =
  AttemptDetail'
    { container = Core.Nothing,
      startedAt = Core.Nothing,
      stoppedAt = Core.Nothing,
      statusReason = Core.Nothing
    }

-- | Details about the container in this job attempt.
attemptDetail_container :: Lens.Lens' AttemptDetail (Core.Maybe AttemptContainerDetail)
attemptDetail_container = Lens.lens (\AttemptDetail' {container} -> container) (\s@AttemptDetail' {} a -> s {container = a} :: AttemptDetail)

-- | The Unix timestamp (in milliseconds) for when the attempt was started
-- (when the attempt transitioned from the @STARTING@ state to the
-- @RUNNING@ state).
attemptDetail_startedAt :: Lens.Lens' AttemptDetail (Core.Maybe Core.Integer)
attemptDetail_startedAt = Lens.lens (\AttemptDetail' {startedAt} -> startedAt) (\s@AttemptDetail' {} a -> s {startedAt = a} :: AttemptDetail)

-- | The Unix timestamp (in milliseconds) for when the attempt was stopped
-- (when the attempt transitioned from the @RUNNING@ state to a terminal
-- state, such as @SUCCEEDED@ or @FAILED@).
attemptDetail_stoppedAt :: Lens.Lens' AttemptDetail (Core.Maybe Core.Integer)
attemptDetail_stoppedAt = Lens.lens (\AttemptDetail' {stoppedAt} -> stoppedAt) (\s@AttemptDetail' {} a -> s {stoppedAt = a} :: AttemptDetail)

-- | A short, human-readable string to provide additional details about the
-- current status of the job attempt.
attemptDetail_statusReason :: Lens.Lens' AttemptDetail (Core.Maybe Core.Text)
attemptDetail_statusReason = Lens.lens (\AttemptDetail' {statusReason} -> statusReason) (\s@AttemptDetail' {} a -> s {statusReason = a} :: AttemptDetail)

instance Core.FromJSON AttemptDetail where
  parseJSON =
    Core.withObject
      "AttemptDetail"
      ( \x ->
          AttemptDetail'
            Core.<$> (x Core..:? "container")
            Core.<*> (x Core..:? "startedAt")
            Core.<*> (x Core..:? "stoppedAt")
            Core.<*> (x Core..:? "statusReason")
      )

instance Core.Hashable AttemptDetail

instance Core.NFData AttemptDetail
