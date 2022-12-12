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
-- Module      : Amazonka.SimSpaceWeaver.DeleteSimulation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes all SimSpace Weaver resources assigned to the given simulation.
--
-- Your simulation uses resources in other Amazon Web Services services.
-- This API operation doesn\'t delete resources in other Amazon Web
-- Services services.
module Amazonka.SimSpaceWeaver.DeleteSimulation
  ( -- * Creating a Request
    DeleteSimulation (..),
    newDeleteSimulation,

    -- * Request Lenses
    deleteSimulation_simulation,

    -- * Destructuring the Response
    DeleteSimulationResponse (..),
    newDeleteSimulationResponse,

    -- * Response Lenses
    deleteSimulationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SimSpaceWeaver.Types

-- | /See:/ 'newDeleteSimulation' smart constructor.
data DeleteSimulation = DeleteSimulation'
  { -- | The name of the simulation.
    simulation :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSimulation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'simulation', 'deleteSimulation_simulation' - The name of the simulation.
newDeleteSimulation ::
  -- | 'simulation'
  Prelude.Text ->
  DeleteSimulation
newDeleteSimulation pSimulation_ =
  DeleteSimulation' {simulation = pSimulation_}

-- | The name of the simulation.
deleteSimulation_simulation :: Lens.Lens' DeleteSimulation Prelude.Text
deleteSimulation_simulation = Lens.lens (\DeleteSimulation' {simulation} -> simulation) (\s@DeleteSimulation' {} a -> s {simulation = a} :: DeleteSimulation)

instance Core.AWSRequest DeleteSimulation where
  type
    AWSResponse DeleteSimulation =
      DeleteSimulationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSimulationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSimulation where
  hashWithSalt _salt DeleteSimulation' {..} =
    _salt `Prelude.hashWithSalt` simulation

instance Prelude.NFData DeleteSimulation where
  rnf DeleteSimulation' {..} = Prelude.rnf simulation

instance Data.ToHeaders DeleteSimulation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteSimulation where
  toPath = Prelude.const "/deletesimulation"

instance Data.ToQuery DeleteSimulation where
  toQuery DeleteSimulation' {..} =
    Prelude.mconcat ["simulation" Data.=: simulation]

-- | /See:/ 'newDeleteSimulationResponse' smart constructor.
data DeleteSimulationResponse = DeleteSimulationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSimulationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSimulationResponse_httpStatus' - The response's http status code.
newDeleteSimulationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSimulationResponse
newDeleteSimulationResponse pHttpStatus_ =
  DeleteSimulationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteSimulationResponse_httpStatus :: Lens.Lens' DeleteSimulationResponse Prelude.Int
deleteSimulationResponse_httpStatus = Lens.lens (\DeleteSimulationResponse' {httpStatus} -> httpStatus) (\s@DeleteSimulationResponse' {} a -> s {httpStatus = a} :: DeleteSimulationResponse)

instance Prelude.NFData DeleteSimulationResponse where
  rnf DeleteSimulationResponse' {..} =
    Prelude.rnf httpStatus
