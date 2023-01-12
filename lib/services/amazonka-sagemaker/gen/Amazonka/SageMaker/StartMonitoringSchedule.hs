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
-- Module      : Amazonka.SageMaker.StartMonitoringSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a previously stopped monitoring schedule.
--
-- By default, when you successfully create a new schedule, the status of a
-- monitoring schedule is @scheduled@.
module Amazonka.SageMaker.StartMonitoringSchedule
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newStartMonitoringSchedule' smart constructor.
data StartMonitoringSchedule = StartMonitoringSchedule'
  { -- | The name of the schedule to start.
    monitoringScheduleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  StartMonitoringSchedule
newStartMonitoringSchedule pMonitoringScheduleName_ =
  StartMonitoringSchedule'
    { monitoringScheduleName =
        pMonitoringScheduleName_
    }

-- | The name of the schedule to start.
startMonitoringSchedule_monitoringScheduleName :: Lens.Lens' StartMonitoringSchedule Prelude.Text
startMonitoringSchedule_monitoringScheduleName = Lens.lens (\StartMonitoringSchedule' {monitoringScheduleName} -> monitoringScheduleName) (\s@StartMonitoringSchedule' {} a -> s {monitoringScheduleName = a} :: StartMonitoringSchedule)

instance Core.AWSRequest StartMonitoringSchedule where
  type
    AWSResponse StartMonitoringSchedule =
      StartMonitoringScheduleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      StartMonitoringScheduleResponse'

instance Prelude.Hashable StartMonitoringSchedule where
  hashWithSalt _salt StartMonitoringSchedule' {..} =
    _salt `Prelude.hashWithSalt` monitoringScheduleName

instance Prelude.NFData StartMonitoringSchedule where
  rnf StartMonitoringSchedule' {..} =
    Prelude.rnf monitoringScheduleName

instance Data.ToHeaders StartMonitoringSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.StartMonitoringSchedule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartMonitoringSchedule where
  toJSON StartMonitoringSchedule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "MonitoringScheduleName"
                  Data..= monitoringScheduleName
              )
          ]
      )

instance Data.ToPath StartMonitoringSchedule where
  toPath = Prelude.const "/"

instance Data.ToQuery StartMonitoringSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartMonitoringScheduleResponse' smart constructor.
data StartMonitoringScheduleResponse = StartMonitoringScheduleResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartMonitoringScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStartMonitoringScheduleResponse ::
  StartMonitoringScheduleResponse
newStartMonitoringScheduleResponse =
  StartMonitoringScheduleResponse'

instance
  Prelude.NFData
    StartMonitoringScheduleResponse
  where
  rnf _ = ()
