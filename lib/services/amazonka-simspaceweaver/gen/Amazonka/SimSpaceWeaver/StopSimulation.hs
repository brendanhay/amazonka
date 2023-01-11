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
-- Module      : Amazonka.SimSpaceWeaver.StopSimulation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the given simulation.
--
-- You can\'t restart a simulation after you stop it. If you need to
-- restart a simulation, you must stop it, delete it, and start a new
-- instance of it.
module Amazonka.SimSpaceWeaver.StopSimulation
  ( -- * Creating a Request
    StopSimulation (..),
    newStopSimulation,

    -- * Request Lenses
    stopSimulation_simulation,

    -- * Destructuring the Response
    StopSimulationResponse (..),
    newStopSimulationResponse,

    -- * Response Lenses
    stopSimulationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SimSpaceWeaver.Types

-- | /See:/ 'newStopSimulation' smart constructor.
data StopSimulation = StopSimulation'
  { -- | The name of the simulation.
    simulation :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopSimulation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'simulation', 'stopSimulation_simulation' - The name of the simulation.
newStopSimulation ::
  -- | 'simulation'
  Prelude.Text ->
  StopSimulation
newStopSimulation pSimulation_ =
  StopSimulation' {simulation = pSimulation_}

-- | The name of the simulation.
stopSimulation_simulation :: Lens.Lens' StopSimulation Prelude.Text
stopSimulation_simulation = Lens.lens (\StopSimulation' {simulation} -> simulation) (\s@StopSimulation' {} a -> s {simulation = a} :: StopSimulation)

instance Core.AWSRequest StopSimulation where
  type
    AWSResponse StopSimulation =
      StopSimulationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopSimulationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopSimulation where
  hashWithSalt _salt StopSimulation' {..} =
    _salt `Prelude.hashWithSalt` simulation

instance Prelude.NFData StopSimulation where
  rnf StopSimulation' {..} = Prelude.rnf simulation

instance Data.ToHeaders StopSimulation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopSimulation where
  toJSON StopSimulation' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Simulation" Data..= simulation)]
      )

instance Data.ToPath StopSimulation where
  toPath = Prelude.const "/stopsimulation"

instance Data.ToQuery StopSimulation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopSimulationResponse' smart constructor.
data StopSimulationResponse = StopSimulationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopSimulationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopSimulationResponse_httpStatus' - The response's http status code.
newStopSimulationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopSimulationResponse
newStopSimulationResponse pHttpStatus_ =
  StopSimulationResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
stopSimulationResponse_httpStatus :: Lens.Lens' StopSimulationResponse Prelude.Int
stopSimulationResponse_httpStatus = Lens.lens (\StopSimulationResponse' {httpStatus} -> httpStatus) (\s@StopSimulationResponse' {} a -> s {httpStatus = a} :: StopSimulationResponse)

instance Prelude.NFData StopSimulationResponse where
  rnf StopSimulationResponse' {..} =
    Prelude.rnf httpStatus
