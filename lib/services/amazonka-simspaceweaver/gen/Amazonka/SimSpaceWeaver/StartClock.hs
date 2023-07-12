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
-- Module      : Amazonka.SimSpaceWeaver.StartClock
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the simulation clock.
module Amazonka.SimSpaceWeaver.StartClock
  ( -- * Creating a Request
    StartClock (..),
    newStartClock,

    -- * Request Lenses
    startClock_simulation,

    -- * Destructuring the Response
    StartClockResponse (..),
    newStartClockResponse,

    -- * Response Lenses
    startClockResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SimSpaceWeaver.Types

-- | /See:/ 'newStartClock' smart constructor.
data StartClock = StartClock'
  { -- | The name of the simulation.
    simulation :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartClock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'simulation', 'startClock_simulation' - The name of the simulation.
newStartClock ::
  -- | 'simulation'
  Prelude.Text ->
  StartClock
newStartClock pSimulation_ =
  StartClock' {simulation = pSimulation_}

-- | The name of the simulation.
startClock_simulation :: Lens.Lens' StartClock Prelude.Text
startClock_simulation = Lens.lens (\StartClock' {simulation} -> simulation) (\s@StartClock' {} a -> s {simulation = a} :: StartClock)

instance Core.AWSRequest StartClock where
  type AWSResponse StartClock = StartClockResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartClockResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartClock where
  hashWithSalt _salt StartClock' {..} =
    _salt `Prelude.hashWithSalt` simulation

instance Prelude.NFData StartClock where
  rnf StartClock' {..} = Prelude.rnf simulation

instance Data.ToHeaders StartClock where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartClock where
  toJSON StartClock' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Simulation" Data..= simulation)]
      )

instance Data.ToPath StartClock where
  toPath = Prelude.const "/startclock"

instance Data.ToQuery StartClock where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartClockResponse' smart constructor.
data StartClockResponse = StartClockResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartClockResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startClockResponse_httpStatus' - The response's http status code.
newStartClockResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartClockResponse
newStartClockResponse pHttpStatus_ =
  StartClockResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
startClockResponse_httpStatus :: Lens.Lens' StartClockResponse Prelude.Int
startClockResponse_httpStatus = Lens.lens (\StartClockResponse' {httpStatus} -> httpStatus) (\s@StartClockResponse' {} a -> s {httpStatus = a} :: StartClockResponse)

instance Prelude.NFData StartClockResponse where
  rnf StartClockResponse' {..} = Prelude.rnf httpStatus
