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
-- Module      : Network.AWS.MediaConvert.Types.JobMessages
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.JobMessages where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides messages from the service about jobs that you have already
-- successfully submitted.
--
-- /See:/ 'newJobMessages' smart constructor.
data JobMessages = JobMessages'
  { -- | List of messages that are informational only and don\'t indicate a
    -- problem with your job.
    info :: Core.Maybe [Core.Text],
    -- | List of messages that warn about conditions that might cause your job
    -- not to run or to fail.
    warning :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'JobMessages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'info', 'jobMessages_info' - List of messages that are informational only and don\'t indicate a
-- problem with your job.
--
-- 'warning', 'jobMessages_warning' - List of messages that warn about conditions that might cause your job
-- not to run or to fail.
newJobMessages ::
  JobMessages
newJobMessages =
  JobMessages'
    { info = Core.Nothing,
      warning = Core.Nothing
    }

-- | List of messages that are informational only and don\'t indicate a
-- problem with your job.
jobMessages_info :: Lens.Lens' JobMessages (Core.Maybe [Core.Text])
jobMessages_info = Lens.lens (\JobMessages' {info} -> info) (\s@JobMessages' {} a -> s {info = a} :: JobMessages) Core.. Lens.mapping Lens._Coerce

-- | List of messages that warn about conditions that might cause your job
-- not to run or to fail.
jobMessages_warning :: Lens.Lens' JobMessages (Core.Maybe [Core.Text])
jobMessages_warning = Lens.lens (\JobMessages' {warning} -> warning) (\s@JobMessages' {} a -> s {warning = a} :: JobMessages) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON JobMessages where
  parseJSON =
    Core.withObject
      "JobMessages"
      ( \x ->
          JobMessages'
            Core.<$> (x Core..:? "info" Core..!= Core.mempty)
            Core.<*> (x Core..:? "warning" Core..!= Core.mempty)
      )

instance Core.Hashable JobMessages

instance Core.NFData JobMessages
