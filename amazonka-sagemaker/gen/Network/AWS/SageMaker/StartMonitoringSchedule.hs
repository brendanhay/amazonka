{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.StartMonitoringSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a previously stopped monitoring schedule.
--
-- By default, when you successfully create a new schedule, the status of a
-- monitoring schedule is @scheduled@.
module Network.AWS.SageMaker.StartMonitoringSchedule
  ( -- * Creating a Request
    StartMonitoringSchedule (..),
    newStartMonitoringSchedule,

    -- * Request Lenses
    startMonitoringSchedule_monitoringScheduleName,

    -- * Destructuring the Response
    StartMonitoringScheduleResponse (..),
    newStartMonitoringScheduleResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newStartMonitoringSchedule' smart constructor.
data StartMonitoringSchedule = StartMonitoringSchedule'
  { -- | The name of the schedule to start.
    monitoringScheduleName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartMonitoringSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monitoringScheduleName', 'startMonitoringSchedule_monitoringScheduleName' - The name of the schedule to start.
newStartMonitoringSchedule ::
  -- | 'monitoringScheduleName'
  Core.Text ->
  StartMonitoringSchedule
newStartMonitoringSchedule pMonitoringScheduleName_ =
  StartMonitoringSchedule'
    { monitoringScheduleName =
        pMonitoringScheduleName_
    }

-- | The name of the schedule to start.
startMonitoringSchedule_monitoringScheduleName :: Lens.Lens' StartMonitoringSchedule Core.Text
startMonitoringSchedule_monitoringScheduleName = Lens.lens (\StartMonitoringSchedule' {monitoringScheduleName} -> monitoringScheduleName) (\s@StartMonitoringSchedule' {} a -> s {monitoringScheduleName = a} :: StartMonitoringSchedule)

instance Core.AWSRequest StartMonitoringSchedule where
  type
    AWSResponse StartMonitoringSchedule =
      StartMonitoringScheduleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      StartMonitoringScheduleResponse'

instance Core.Hashable StartMonitoringSchedule

instance Core.NFData StartMonitoringSchedule

instance Core.ToHeaders StartMonitoringSchedule where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.StartMonitoringSchedule" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartMonitoringSchedule where
  toJSON StartMonitoringSchedule' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "MonitoringScheduleName"
                  Core..= monitoringScheduleName
              )
          ]
      )

instance Core.ToPath StartMonitoringSchedule where
  toPath = Core.const "/"

instance Core.ToQuery StartMonitoringSchedule where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartMonitoringScheduleResponse' smart constructor.
data StartMonitoringScheduleResponse = StartMonitoringScheduleResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartMonitoringScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStartMonitoringScheduleResponse ::
  StartMonitoringScheduleResponse
newStartMonitoringScheduleResponse =
  StartMonitoringScheduleResponse'

instance Core.NFData StartMonitoringScheduleResponse
