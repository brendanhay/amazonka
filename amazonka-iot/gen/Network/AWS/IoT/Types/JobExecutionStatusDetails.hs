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
-- Module      : Network.AWS.IoT.Types.JobExecutionStatusDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.JobExecutionStatusDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Details of the job execution status.
--
-- /See:/ 'newJobExecutionStatusDetails' smart constructor.
data JobExecutionStatusDetails = JobExecutionStatusDetails'
  { -- | The job execution status.
    detailsMap :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'JobExecutionStatusDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detailsMap', 'jobExecutionStatusDetails_detailsMap' - The job execution status.
newJobExecutionStatusDetails ::
  JobExecutionStatusDetails
newJobExecutionStatusDetails =
  JobExecutionStatusDetails'
    { detailsMap =
        Core.Nothing
    }

-- | The job execution status.
jobExecutionStatusDetails_detailsMap :: Lens.Lens' JobExecutionStatusDetails (Core.Maybe (Core.HashMap Core.Text Core.Text))
jobExecutionStatusDetails_detailsMap = Lens.lens (\JobExecutionStatusDetails' {detailsMap} -> detailsMap) (\s@JobExecutionStatusDetails' {} a -> s {detailsMap = a} :: JobExecutionStatusDetails) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON JobExecutionStatusDetails where
  parseJSON =
    Core.withObject
      "JobExecutionStatusDetails"
      ( \x ->
          JobExecutionStatusDetails'
            Core.<$> (x Core..:? "detailsMap" Core..!= Core.mempty)
      )

instance Core.Hashable JobExecutionStatusDetails

instance Core.NFData JobExecutionStatusDetails
