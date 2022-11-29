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
-- Module      : Amazonka.Batch.Types.AttemptDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.AttemptDetail where

import Amazonka.Batch.Types.AttemptContainerDetail
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a job attempt.
--
-- /See:/ 'newAttemptDetail' smart constructor.
data AttemptDetail = AttemptDetail'
  { -- | A short, human-readable string to provide additional details for the
    -- current status of the job attempt.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | The Unix timestamp (in milliseconds) for when the attempt was started
    -- (when the attempt transitioned from the @STARTING@ state to the
    -- @RUNNING@ state).
    startedAt :: Prelude.Maybe Prelude.Integer,
    -- | The details for the container in this job attempt.
    container :: Prelude.Maybe AttemptContainerDetail,
    -- | The Unix timestamp (in milliseconds) for when the attempt was stopped
    -- (when the attempt transitioned from the @RUNNING@ state to a terminal
    -- state, such as @SUCCEEDED@ or @FAILED@).
    stoppedAt :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttemptDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusReason', 'attemptDetail_statusReason' - A short, human-readable string to provide additional details for the
-- current status of the job attempt.
--
-- 'startedAt', 'attemptDetail_startedAt' - The Unix timestamp (in milliseconds) for when the attempt was started
-- (when the attempt transitioned from the @STARTING@ state to the
-- @RUNNING@ state).
--
-- 'container', 'attemptDetail_container' - The details for the container in this job attempt.
--
-- 'stoppedAt', 'attemptDetail_stoppedAt' - The Unix timestamp (in milliseconds) for when the attempt was stopped
-- (when the attempt transitioned from the @RUNNING@ state to a terminal
-- state, such as @SUCCEEDED@ or @FAILED@).
newAttemptDetail ::
  AttemptDetail
newAttemptDetail =
  AttemptDetail'
    { statusReason = Prelude.Nothing,
      startedAt = Prelude.Nothing,
      container = Prelude.Nothing,
      stoppedAt = Prelude.Nothing
    }

-- | A short, human-readable string to provide additional details for the
-- current status of the job attempt.
attemptDetail_statusReason :: Lens.Lens' AttemptDetail (Prelude.Maybe Prelude.Text)
attemptDetail_statusReason = Lens.lens (\AttemptDetail' {statusReason} -> statusReason) (\s@AttemptDetail' {} a -> s {statusReason = a} :: AttemptDetail)

-- | The Unix timestamp (in milliseconds) for when the attempt was started
-- (when the attempt transitioned from the @STARTING@ state to the
-- @RUNNING@ state).
attemptDetail_startedAt :: Lens.Lens' AttemptDetail (Prelude.Maybe Prelude.Integer)
attemptDetail_startedAt = Lens.lens (\AttemptDetail' {startedAt} -> startedAt) (\s@AttemptDetail' {} a -> s {startedAt = a} :: AttemptDetail)

-- | The details for the container in this job attempt.
attemptDetail_container :: Lens.Lens' AttemptDetail (Prelude.Maybe AttemptContainerDetail)
attemptDetail_container = Lens.lens (\AttemptDetail' {container} -> container) (\s@AttemptDetail' {} a -> s {container = a} :: AttemptDetail)

-- | The Unix timestamp (in milliseconds) for when the attempt was stopped
-- (when the attempt transitioned from the @RUNNING@ state to a terminal
-- state, such as @SUCCEEDED@ or @FAILED@).
attemptDetail_stoppedAt :: Lens.Lens' AttemptDetail (Prelude.Maybe Prelude.Integer)
attemptDetail_stoppedAt = Lens.lens (\AttemptDetail' {stoppedAt} -> stoppedAt) (\s@AttemptDetail' {} a -> s {stoppedAt = a} :: AttemptDetail)

instance Core.FromJSON AttemptDetail where
  parseJSON =
    Core.withObject
      "AttemptDetail"
      ( \x ->
          AttemptDetail'
            Prelude.<$> (x Core..:? "statusReason")
            Prelude.<*> (x Core..:? "startedAt")
            Prelude.<*> (x Core..:? "container")
            Prelude.<*> (x Core..:? "stoppedAt")
      )

instance Prelude.Hashable AttemptDetail where
  hashWithSalt _salt AttemptDetail' {..} =
    _salt `Prelude.hashWithSalt` statusReason
      `Prelude.hashWithSalt` startedAt
      `Prelude.hashWithSalt` container
      `Prelude.hashWithSalt` stoppedAt

instance Prelude.NFData AttemptDetail where
  rnf AttemptDetail' {..} =
    Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf startedAt
      `Prelude.seq` Prelude.rnf container
      `Prelude.seq` Prelude.rnf stoppedAt
