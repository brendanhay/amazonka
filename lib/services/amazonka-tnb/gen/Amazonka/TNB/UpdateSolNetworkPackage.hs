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
-- Module      : Amazonka.TNB.UpdateSolNetworkPackage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the operational state of a network package.
--
-- A network package is a .zip file in CSAR (Cloud Service Archive) format
-- defines the function packages you want to deploy and the Amazon Web
-- Services infrastructure you want to deploy them on.
--
-- A network service descriptor is a .yaml file in a network package that
-- uses the TOSCA standard to describe the network functions you want to
-- deploy and the Amazon Web Services infrastructure you want to deploy the
-- network functions on.
module Amazonka.TNB.UpdateSolNetworkPackage
  ( -- * Creating a Request
    UpdateSolNetworkPackage (..),
    newUpdateSolNetworkPackage,

    -- * Request Lenses
    updateSolNetworkPackage_nsdInfoId,
    updateSolNetworkPackage_nsdOperationalState,

    -- * Destructuring the Response
    UpdateSolNetworkPackageResponse (..),
    newUpdateSolNetworkPackageResponse,

    -- * Response Lenses
    updateSolNetworkPackageResponse_httpStatus,
    updateSolNetworkPackageResponse_nsdOperationalState,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TNB.Types

-- | /See:/ 'newUpdateSolNetworkPackage' smart constructor.
data UpdateSolNetworkPackage = UpdateSolNetworkPackage'
  { -- | ID of the network service descriptor in the network package.
    nsdInfoId :: Prelude.Text,
    -- | Operational state of the network service descriptor in the network
    -- package.
    nsdOperationalState :: NsdOperationalState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSolNetworkPackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nsdInfoId', 'updateSolNetworkPackage_nsdInfoId' - ID of the network service descriptor in the network package.
--
-- 'nsdOperationalState', 'updateSolNetworkPackage_nsdOperationalState' - Operational state of the network service descriptor in the network
-- package.
newUpdateSolNetworkPackage ::
  -- | 'nsdInfoId'
  Prelude.Text ->
  -- | 'nsdOperationalState'
  NsdOperationalState ->
  UpdateSolNetworkPackage
newUpdateSolNetworkPackage
  pNsdInfoId_
  pNsdOperationalState_ =
    UpdateSolNetworkPackage'
      { nsdInfoId = pNsdInfoId_,
        nsdOperationalState = pNsdOperationalState_
      }

-- | ID of the network service descriptor in the network package.
updateSolNetworkPackage_nsdInfoId :: Lens.Lens' UpdateSolNetworkPackage Prelude.Text
updateSolNetworkPackage_nsdInfoId = Lens.lens (\UpdateSolNetworkPackage' {nsdInfoId} -> nsdInfoId) (\s@UpdateSolNetworkPackage' {} a -> s {nsdInfoId = a} :: UpdateSolNetworkPackage)

-- | Operational state of the network service descriptor in the network
-- package.
updateSolNetworkPackage_nsdOperationalState :: Lens.Lens' UpdateSolNetworkPackage NsdOperationalState
updateSolNetworkPackage_nsdOperationalState = Lens.lens (\UpdateSolNetworkPackage' {nsdOperationalState} -> nsdOperationalState) (\s@UpdateSolNetworkPackage' {} a -> s {nsdOperationalState = a} :: UpdateSolNetworkPackage)

instance Core.AWSRequest UpdateSolNetworkPackage where
  type
    AWSResponse UpdateSolNetworkPackage =
      UpdateSolNetworkPackageResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSolNetworkPackageResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "nsdOperationalState")
      )

instance Prelude.Hashable UpdateSolNetworkPackage where
  hashWithSalt _salt UpdateSolNetworkPackage' {..} =
    _salt
      `Prelude.hashWithSalt` nsdInfoId
      `Prelude.hashWithSalt` nsdOperationalState

instance Prelude.NFData UpdateSolNetworkPackage where
  rnf UpdateSolNetworkPackage' {..} =
    Prelude.rnf nsdInfoId
      `Prelude.seq` Prelude.rnf nsdOperationalState

instance Data.ToHeaders UpdateSolNetworkPackage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateSolNetworkPackage where
  toJSON UpdateSolNetworkPackage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("nsdOperationalState" Data..= nsdOperationalState)
          ]
      )

instance Data.ToPath UpdateSolNetworkPackage where
  toPath UpdateSolNetworkPackage' {..} =
    Prelude.mconcat
      ["/sol/nsd/v1/ns_descriptors/", Data.toBS nsdInfoId]

instance Data.ToQuery UpdateSolNetworkPackage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSolNetworkPackageResponse' smart constructor.
data UpdateSolNetworkPackageResponse = UpdateSolNetworkPackageResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Operational state of the network service descriptor in the network
    -- package.
    nsdOperationalState :: NsdOperationalState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSolNetworkPackageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateSolNetworkPackageResponse_httpStatus' - The response's http status code.
--
-- 'nsdOperationalState', 'updateSolNetworkPackageResponse_nsdOperationalState' - Operational state of the network service descriptor in the network
-- package.
newUpdateSolNetworkPackageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'nsdOperationalState'
  NsdOperationalState ->
  UpdateSolNetworkPackageResponse
newUpdateSolNetworkPackageResponse
  pHttpStatus_
  pNsdOperationalState_ =
    UpdateSolNetworkPackageResponse'
      { httpStatus =
          pHttpStatus_,
        nsdOperationalState =
          pNsdOperationalState_
      }

-- | The response's http status code.
updateSolNetworkPackageResponse_httpStatus :: Lens.Lens' UpdateSolNetworkPackageResponse Prelude.Int
updateSolNetworkPackageResponse_httpStatus = Lens.lens (\UpdateSolNetworkPackageResponse' {httpStatus} -> httpStatus) (\s@UpdateSolNetworkPackageResponse' {} a -> s {httpStatus = a} :: UpdateSolNetworkPackageResponse)

-- | Operational state of the network service descriptor in the network
-- package.
updateSolNetworkPackageResponse_nsdOperationalState :: Lens.Lens' UpdateSolNetworkPackageResponse NsdOperationalState
updateSolNetworkPackageResponse_nsdOperationalState = Lens.lens (\UpdateSolNetworkPackageResponse' {nsdOperationalState} -> nsdOperationalState) (\s@UpdateSolNetworkPackageResponse' {} a -> s {nsdOperationalState = a} :: UpdateSolNetworkPackageResponse)

instance
  Prelude.NFData
    UpdateSolNetworkPackageResponse
  where
  rnf UpdateSolNetworkPackageResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nsdOperationalState
