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
-- Module      : Amazonka.GameLift.DeregisterCompute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a compute resource from the specified fleet. Deregister your
-- compute resources before you delete the compute.
module Amazonka.GameLift.DeregisterCompute
  ( -- * Creating a Request
    DeregisterCompute (..),
    newDeregisterCompute,

    -- * Request Lenses
    deregisterCompute_fleetId,
    deregisterCompute_computeName,

    -- * Destructuring the Response
    DeregisterComputeResponse (..),
    newDeregisterComputeResponse,

    -- * Response Lenses
    deregisterComputeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeregisterCompute' smart constructor.
data DeregisterCompute = DeregisterCompute'
  { -- | >A unique identifier for the fleet the compute resource is registered
    -- to.
    fleetId :: Prelude.Text,
    -- | The name of the compute resource you want to delete.
    computeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterCompute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'deregisterCompute_fleetId' - >A unique identifier for the fleet the compute resource is registered
-- to.
--
-- 'computeName', 'deregisterCompute_computeName' - The name of the compute resource you want to delete.
newDeregisterCompute ::
  -- | 'fleetId'
  Prelude.Text ->
  -- | 'computeName'
  Prelude.Text ->
  DeregisterCompute
newDeregisterCompute pFleetId_ pComputeName_ =
  DeregisterCompute'
    { fleetId = pFleetId_,
      computeName = pComputeName_
    }

-- | >A unique identifier for the fleet the compute resource is registered
-- to.
deregisterCompute_fleetId :: Lens.Lens' DeregisterCompute Prelude.Text
deregisterCompute_fleetId = Lens.lens (\DeregisterCompute' {fleetId} -> fleetId) (\s@DeregisterCompute' {} a -> s {fleetId = a} :: DeregisterCompute)

-- | The name of the compute resource you want to delete.
deregisterCompute_computeName :: Lens.Lens' DeregisterCompute Prelude.Text
deregisterCompute_computeName = Lens.lens (\DeregisterCompute' {computeName} -> computeName) (\s@DeregisterCompute' {} a -> s {computeName = a} :: DeregisterCompute)

instance Core.AWSRequest DeregisterCompute where
  type
    AWSResponse DeregisterCompute =
      DeregisterComputeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeregisterComputeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeregisterCompute where
  hashWithSalt _salt DeregisterCompute' {..} =
    _salt `Prelude.hashWithSalt` fleetId
      `Prelude.hashWithSalt` computeName

instance Prelude.NFData DeregisterCompute where
  rnf DeregisterCompute' {..} =
    Prelude.rnf fleetId
      `Prelude.seq` Prelude.rnf computeName

instance Data.ToHeaders DeregisterCompute where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("GameLift.DeregisterCompute" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeregisterCompute where
  toJSON DeregisterCompute' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FleetId" Data..= fleetId),
            Prelude.Just ("ComputeName" Data..= computeName)
          ]
      )

instance Data.ToPath DeregisterCompute where
  toPath = Prelude.const "/"

instance Data.ToQuery DeregisterCompute where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterComputeResponse' smart constructor.
data DeregisterComputeResponse = DeregisterComputeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterComputeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deregisterComputeResponse_httpStatus' - The response's http status code.
newDeregisterComputeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeregisterComputeResponse
newDeregisterComputeResponse pHttpStatus_ =
  DeregisterComputeResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deregisterComputeResponse_httpStatus :: Lens.Lens' DeregisterComputeResponse Prelude.Int
deregisterComputeResponse_httpStatus = Lens.lens (\DeregisterComputeResponse' {httpStatus} -> httpStatus) (\s@DeregisterComputeResponse' {} a -> s {httpStatus = a} :: DeregisterComputeResponse)

instance Prelude.NFData DeregisterComputeResponse where
  rnf DeregisterComputeResponse' {..} =
    Prelude.rnf httpStatus
