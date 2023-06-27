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
-- Module      : Amazonka.TNB.DeleteSolNetworkPackage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes network package.
--
-- A network package is a .zip file in CSAR (Cloud Service Archive) format
-- defines the function packages you want to deploy and the Amazon Web
-- Services infrastructure you want to deploy them on.
--
-- To delete a network package, the package must be in a disable state. To
-- disable a network package, see
-- <https://docs.aws.amazon.com/tnb/latest/APIReference/API_UpdateSolNetworkPackage.html UpdateSolNetworkPackage>.
module Amazonka.TNB.DeleteSolNetworkPackage
  ( -- * Creating a Request
    DeleteSolNetworkPackage (..),
    newDeleteSolNetworkPackage,

    -- * Request Lenses
    deleteSolNetworkPackage_nsdInfoId,

    -- * Destructuring the Response
    DeleteSolNetworkPackageResponse (..),
    newDeleteSolNetworkPackageResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TNB.Types

-- | /See:/ 'newDeleteSolNetworkPackage' smart constructor.
data DeleteSolNetworkPackage = DeleteSolNetworkPackage'
  { -- | ID of the network service descriptor in the network package.
    nsdInfoId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSolNetworkPackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nsdInfoId', 'deleteSolNetworkPackage_nsdInfoId' - ID of the network service descriptor in the network package.
newDeleteSolNetworkPackage ::
  -- | 'nsdInfoId'
  Prelude.Text ->
  DeleteSolNetworkPackage
newDeleteSolNetworkPackage pNsdInfoId_ =
  DeleteSolNetworkPackage' {nsdInfoId = pNsdInfoId_}

-- | ID of the network service descriptor in the network package.
deleteSolNetworkPackage_nsdInfoId :: Lens.Lens' DeleteSolNetworkPackage Prelude.Text
deleteSolNetworkPackage_nsdInfoId = Lens.lens (\DeleteSolNetworkPackage' {nsdInfoId} -> nsdInfoId) (\s@DeleteSolNetworkPackage' {} a -> s {nsdInfoId = a} :: DeleteSolNetworkPackage)

instance Core.AWSRequest DeleteSolNetworkPackage where
  type
    AWSResponse DeleteSolNetworkPackage =
      DeleteSolNetworkPackageResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteSolNetworkPackageResponse'

instance Prelude.Hashable DeleteSolNetworkPackage where
  hashWithSalt _salt DeleteSolNetworkPackage' {..} =
    _salt `Prelude.hashWithSalt` nsdInfoId

instance Prelude.NFData DeleteSolNetworkPackage where
  rnf DeleteSolNetworkPackage' {..} =
    Prelude.rnf nsdInfoId

instance Data.ToHeaders DeleteSolNetworkPackage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteSolNetworkPackage where
  toPath DeleteSolNetworkPackage' {..} =
    Prelude.mconcat
      ["/sol/nsd/v1/ns_descriptors/", Data.toBS nsdInfoId]

instance Data.ToQuery DeleteSolNetworkPackage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSolNetworkPackageResponse' smart constructor.
data DeleteSolNetworkPackageResponse = DeleteSolNetworkPackageResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSolNetworkPackageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteSolNetworkPackageResponse ::
  DeleteSolNetworkPackageResponse
newDeleteSolNetworkPackageResponse =
  DeleteSolNetworkPackageResponse'

instance
  Prelude.NFData
    DeleteSolNetworkPackageResponse
  where
  rnf _ = ()
