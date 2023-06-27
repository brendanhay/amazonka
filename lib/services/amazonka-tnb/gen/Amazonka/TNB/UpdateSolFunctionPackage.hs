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
-- Module      : Amazonka.TNB.UpdateSolFunctionPackage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the operational state of function package.
--
-- A function package is a .zip file in CSAR (Cloud Service Archive) format
-- that contains a network function (an ETSI standard telecommunication
-- application) and function package descriptor that uses the TOSCA
-- standard to describe how the network functions should run on your
-- network.
module Amazonka.TNB.UpdateSolFunctionPackage
  ( -- * Creating a Request
    UpdateSolFunctionPackage (..),
    newUpdateSolFunctionPackage,

    -- * Request Lenses
    updateSolFunctionPackage_operationalState,
    updateSolFunctionPackage_vnfPkgId,

    -- * Destructuring the Response
    UpdateSolFunctionPackageResponse (..),
    newUpdateSolFunctionPackageResponse,

    -- * Response Lenses
    updateSolFunctionPackageResponse_httpStatus,
    updateSolFunctionPackageResponse_operationalState,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TNB.Types

-- | /See:/ 'newUpdateSolFunctionPackage' smart constructor.
data UpdateSolFunctionPackage = UpdateSolFunctionPackage'
  { -- | Operational state of the function package.
    operationalState :: OperationalState,
    -- | ID of the function package.
    vnfPkgId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSolFunctionPackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationalState', 'updateSolFunctionPackage_operationalState' - Operational state of the function package.
--
-- 'vnfPkgId', 'updateSolFunctionPackage_vnfPkgId' - ID of the function package.
newUpdateSolFunctionPackage ::
  -- | 'operationalState'
  OperationalState ->
  -- | 'vnfPkgId'
  Prelude.Text ->
  UpdateSolFunctionPackage
newUpdateSolFunctionPackage
  pOperationalState_
  pVnfPkgId_ =
    UpdateSolFunctionPackage'
      { operationalState =
          pOperationalState_,
        vnfPkgId = pVnfPkgId_
      }

-- | Operational state of the function package.
updateSolFunctionPackage_operationalState :: Lens.Lens' UpdateSolFunctionPackage OperationalState
updateSolFunctionPackage_operationalState = Lens.lens (\UpdateSolFunctionPackage' {operationalState} -> operationalState) (\s@UpdateSolFunctionPackage' {} a -> s {operationalState = a} :: UpdateSolFunctionPackage)

-- | ID of the function package.
updateSolFunctionPackage_vnfPkgId :: Lens.Lens' UpdateSolFunctionPackage Prelude.Text
updateSolFunctionPackage_vnfPkgId = Lens.lens (\UpdateSolFunctionPackage' {vnfPkgId} -> vnfPkgId) (\s@UpdateSolFunctionPackage' {} a -> s {vnfPkgId = a} :: UpdateSolFunctionPackage)

instance Core.AWSRequest UpdateSolFunctionPackage where
  type
    AWSResponse UpdateSolFunctionPackage =
      UpdateSolFunctionPackageResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSolFunctionPackageResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "operationalState")
      )

instance Prelude.Hashable UpdateSolFunctionPackage where
  hashWithSalt _salt UpdateSolFunctionPackage' {..} =
    _salt
      `Prelude.hashWithSalt` operationalState
      `Prelude.hashWithSalt` vnfPkgId

instance Prelude.NFData UpdateSolFunctionPackage where
  rnf UpdateSolFunctionPackage' {..} =
    Prelude.rnf operationalState
      `Prelude.seq` Prelude.rnf vnfPkgId

instance Data.ToHeaders UpdateSolFunctionPackage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateSolFunctionPackage where
  toJSON UpdateSolFunctionPackage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("operationalState" Data..= operationalState)
          ]
      )

instance Data.ToPath UpdateSolFunctionPackage where
  toPath UpdateSolFunctionPackage' {..} =
    Prelude.mconcat
      ["/sol/vnfpkgm/v1/vnf_packages/", Data.toBS vnfPkgId]

instance Data.ToQuery UpdateSolFunctionPackage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSolFunctionPackageResponse' smart constructor.
data UpdateSolFunctionPackageResponse = UpdateSolFunctionPackageResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Operational state of the function package.
    operationalState :: OperationalState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSolFunctionPackageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateSolFunctionPackageResponse_httpStatus' - The response's http status code.
--
-- 'operationalState', 'updateSolFunctionPackageResponse_operationalState' - Operational state of the function package.
newUpdateSolFunctionPackageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'operationalState'
  OperationalState ->
  UpdateSolFunctionPackageResponse
newUpdateSolFunctionPackageResponse
  pHttpStatus_
  pOperationalState_ =
    UpdateSolFunctionPackageResponse'
      { httpStatus =
          pHttpStatus_,
        operationalState = pOperationalState_
      }

-- | The response's http status code.
updateSolFunctionPackageResponse_httpStatus :: Lens.Lens' UpdateSolFunctionPackageResponse Prelude.Int
updateSolFunctionPackageResponse_httpStatus = Lens.lens (\UpdateSolFunctionPackageResponse' {httpStatus} -> httpStatus) (\s@UpdateSolFunctionPackageResponse' {} a -> s {httpStatus = a} :: UpdateSolFunctionPackageResponse)

-- | Operational state of the function package.
updateSolFunctionPackageResponse_operationalState :: Lens.Lens' UpdateSolFunctionPackageResponse OperationalState
updateSolFunctionPackageResponse_operationalState = Lens.lens (\UpdateSolFunctionPackageResponse' {operationalState} -> operationalState) (\s@UpdateSolFunctionPackageResponse' {} a -> s {operationalState = a} :: UpdateSolFunctionPackageResponse)

instance
  Prelude.NFData
    UpdateSolFunctionPackageResponse
  where
  rnf UpdateSolFunctionPackageResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf operationalState
