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
-- Module      : Amazonka.NetworkManager.DeleteGlobalNetwork
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing global network. You must first delete all global
-- network objects (devices, links, and sites), deregister all transit
-- gateways, and delete any core networks.
module Amazonka.NetworkManager.DeleteGlobalNetwork
  ( -- * Creating a Request
    DeleteGlobalNetwork (..),
    newDeleteGlobalNetwork,

    -- * Request Lenses
    deleteGlobalNetwork_globalNetworkId,

    -- * Destructuring the Response
    DeleteGlobalNetworkResponse (..),
    newDeleteGlobalNetworkResponse,

    -- * Response Lenses
    deleteGlobalNetworkResponse_globalNetwork,
    deleteGlobalNetworkResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteGlobalNetwork' smart constructor.
data DeleteGlobalNetwork = DeleteGlobalNetwork'
  { -- | The ID of the global network.
    globalNetworkId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteGlobalNetwork' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalNetworkId', 'deleteGlobalNetwork_globalNetworkId' - The ID of the global network.
newDeleteGlobalNetwork ::
  -- | 'globalNetworkId'
  Prelude.Text ->
  DeleteGlobalNetwork
newDeleteGlobalNetwork pGlobalNetworkId_ =
  DeleteGlobalNetwork'
    { globalNetworkId =
        pGlobalNetworkId_
    }

-- | The ID of the global network.
deleteGlobalNetwork_globalNetworkId :: Lens.Lens' DeleteGlobalNetwork Prelude.Text
deleteGlobalNetwork_globalNetworkId = Lens.lens (\DeleteGlobalNetwork' {globalNetworkId} -> globalNetworkId) (\s@DeleteGlobalNetwork' {} a -> s {globalNetworkId = a} :: DeleteGlobalNetwork)

instance Core.AWSRequest DeleteGlobalNetwork where
  type
    AWSResponse DeleteGlobalNetwork =
      DeleteGlobalNetworkResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteGlobalNetworkResponse'
            Prelude.<$> (x Data..?> "GlobalNetwork")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteGlobalNetwork where
  hashWithSalt _salt DeleteGlobalNetwork' {..} =
    _salt `Prelude.hashWithSalt` globalNetworkId

instance Prelude.NFData DeleteGlobalNetwork where
  rnf DeleteGlobalNetwork' {..} =
    Prelude.rnf globalNetworkId

instance Data.ToHeaders DeleteGlobalNetwork where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteGlobalNetwork where
  toPath DeleteGlobalNetwork' {..} =
    Prelude.mconcat
      ["/global-networks/", Data.toBS globalNetworkId]

instance Data.ToQuery DeleteGlobalNetwork where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteGlobalNetworkResponse' smart constructor.
data DeleteGlobalNetworkResponse = DeleteGlobalNetworkResponse'
  { -- | Information about the global network.
    globalNetwork :: Prelude.Maybe GlobalNetwork,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteGlobalNetworkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalNetwork', 'deleteGlobalNetworkResponse_globalNetwork' - Information about the global network.
--
-- 'httpStatus', 'deleteGlobalNetworkResponse_httpStatus' - The response's http status code.
newDeleteGlobalNetworkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteGlobalNetworkResponse
newDeleteGlobalNetworkResponse pHttpStatus_ =
  DeleteGlobalNetworkResponse'
    { globalNetwork =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the global network.
deleteGlobalNetworkResponse_globalNetwork :: Lens.Lens' DeleteGlobalNetworkResponse (Prelude.Maybe GlobalNetwork)
deleteGlobalNetworkResponse_globalNetwork = Lens.lens (\DeleteGlobalNetworkResponse' {globalNetwork} -> globalNetwork) (\s@DeleteGlobalNetworkResponse' {} a -> s {globalNetwork = a} :: DeleteGlobalNetworkResponse)

-- | The response's http status code.
deleteGlobalNetworkResponse_httpStatus :: Lens.Lens' DeleteGlobalNetworkResponse Prelude.Int
deleteGlobalNetworkResponse_httpStatus = Lens.lens (\DeleteGlobalNetworkResponse' {httpStatus} -> httpStatus) (\s@DeleteGlobalNetworkResponse' {} a -> s {httpStatus = a} :: DeleteGlobalNetworkResponse)

instance Prelude.NFData DeleteGlobalNetworkResponse where
  rnf DeleteGlobalNetworkResponse' {..} =
    Prelude.rnf globalNetwork `Prelude.seq`
      Prelude.rnf httpStatus
