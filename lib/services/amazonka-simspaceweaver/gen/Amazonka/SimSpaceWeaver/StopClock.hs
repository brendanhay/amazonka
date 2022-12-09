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
-- Module      : Amazonka.SimSpaceWeaver.StopClock
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the simulation clock.
module Amazonka.SimSpaceWeaver.StopClock
  ( -- * Creating a Request
    StopClock (..),
    newStopClock,

    -- * Request Lenses
    stopClock_simulation,

    -- * Destructuring the Response
    StopClockResponse (..),
    newStopClockResponse,

    -- * Response Lenses
    stopClockResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SimSpaceWeaver.Types

-- | /See:/ 'newStopClock' smart constructor.
data StopClock = StopClock'
  { -- | The name of the simulation.
    simulation :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopClock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'simulation', 'stopClock_simulation' - The name of the simulation.
newStopClock ::
  -- | 'simulation'
  Prelude.Text ->
  StopClock
newStopClock pSimulation_ =
  StopClock' {simulation = pSimulation_}

-- | The name of the simulation.
stopClock_simulation :: Lens.Lens' StopClock Prelude.Text
stopClock_simulation = Lens.lens (\StopClock' {simulation} -> simulation) (\s@StopClock' {} a -> s {simulation = a} :: StopClock)

instance Core.AWSRequest StopClock where
  type AWSResponse StopClock = StopClockResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopClockResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopClock where
  hashWithSalt _salt StopClock' {..} =
    _salt `Prelude.hashWithSalt` simulation

instance Prelude.NFData StopClock where
  rnf StopClock' {..} = Prelude.rnf simulation

instance Data.ToHeaders StopClock where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopClock where
  toJSON StopClock' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Simulation" Data..= simulation)]
      )

instance Data.ToPath StopClock where
  toPath = Prelude.const "/stopclock"

instance Data.ToQuery StopClock where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopClockResponse' smart constructor.
data StopClockResponse = StopClockResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopClockResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopClockResponse_httpStatus' - The response's http status code.
newStopClockResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopClockResponse
newStopClockResponse pHttpStatus_ =
  StopClockResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
stopClockResponse_httpStatus :: Lens.Lens' StopClockResponse Prelude.Int
stopClockResponse_httpStatus = Lens.lens (\StopClockResponse' {httpStatus} -> httpStatus) (\s@StopClockResponse' {} a -> s {httpStatus = a} :: StopClockResponse)

instance Prelude.NFData StopClockResponse where
  rnf StopClockResponse' {..} = Prelude.rnf httpStatus
