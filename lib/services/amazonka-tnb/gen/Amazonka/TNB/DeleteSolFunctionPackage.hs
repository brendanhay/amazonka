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
-- Module      : Amazonka.TNB.DeleteSolFunctionPackage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a function package.
--
-- A function package is a .zip file in CSAR (Cloud Service Archive) format
-- that contains a network function (an ETSI standard telecommunication
-- application) and function package descriptor that uses the TOSCA
-- standard to describe how the network functions should run on your
-- network.
--
-- To delete a function package, the package must be in a disabled state.
-- To disable a function package, see
-- <https://docs.aws.amazon.com/tnb/latest/APIReference/API_UpdateSolFunctionPackage.html UpdateSolFunctionPackage>.
module Amazonka.TNB.DeleteSolFunctionPackage
  ( -- * Creating a Request
    DeleteSolFunctionPackage (..),
    newDeleteSolFunctionPackage,

    -- * Request Lenses
    deleteSolFunctionPackage_vnfPkgId,

    -- * Destructuring the Response
    DeleteSolFunctionPackageResponse (..),
    newDeleteSolFunctionPackageResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TNB.Types

-- | /See:/ 'newDeleteSolFunctionPackage' smart constructor.
data DeleteSolFunctionPackage = DeleteSolFunctionPackage'
  { -- | ID of the function package.
    vnfPkgId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSolFunctionPackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vnfPkgId', 'deleteSolFunctionPackage_vnfPkgId' - ID of the function package.
newDeleteSolFunctionPackage ::
  -- | 'vnfPkgId'
  Prelude.Text ->
  DeleteSolFunctionPackage
newDeleteSolFunctionPackage pVnfPkgId_ =
  DeleteSolFunctionPackage' {vnfPkgId = pVnfPkgId_}

-- | ID of the function package.
deleteSolFunctionPackage_vnfPkgId :: Lens.Lens' DeleteSolFunctionPackage Prelude.Text
deleteSolFunctionPackage_vnfPkgId = Lens.lens (\DeleteSolFunctionPackage' {vnfPkgId} -> vnfPkgId) (\s@DeleteSolFunctionPackage' {} a -> s {vnfPkgId = a} :: DeleteSolFunctionPackage)

instance Core.AWSRequest DeleteSolFunctionPackage where
  type
    AWSResponse DeleteSolFunctionPackage =
      DeleteSolFunctionPackageResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteSolFunctionPackageResponse'

instance Prelude.Hashable DeleteSolFunctionPackage where
  hashWithSalt _salt DeleteSolFunctionPackage' {..} =
    _salt `Prelude.hashWithSalt` vnfPkgId

instance Prelude.NFData DeleteSolFunctionPackage where
  rnf DeleteSolFunctionPackage' {..} =
    Prelude.rnf vnfPkgId

instance Data.ToHeaders DeleteSolFunctionPackage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteSolFunctionPackage where
  toPath DeleteSolFunctionPackage' {..} =
    Prelude.mconcat
      ["/sol/vnfpkgm/v1/vnf_packages/", Data.toBS vnfPkgId]

instance Data.ToQuery DeleteSolFunctionPackage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSolFunctionPackageResponse' smart constructor.
data DeleteSolFunctionPackageResponse = DeleteSolFunctionPackageResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSolFunctionPackageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteSolFunctionPackageResponse ::
  DeleteSolFunctionPackageResponse
newDeleteSolFunctionPackageResponse =
  DeleteSolFunctionPackageResponse'

instance
  Prelude.NFData
    DeleteSolFunctionPackageResponse
  where
  rnf _ = ()
