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
-- Module      : Amazonka.NetworkManager.DeleteCoreNetwork
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a core network along with all core network policies. This can
-- only be done if there are no attachments on a core network.
module Amazonka.NetworkManager.DeleteCoreNetwork
  ( -- * Creating a Request
    DeleteCoreNetwork (..),
    newDeleteCoreNetwork,

    -- * Request Lenses
    deleteCoreNetwork_coreNetworkId,

    -- * Destructuring the Response
    DeleteCoreNetworkResponse (..),
    newDeleteCoreNetworkResponse,

    -- * Response Lenses
    deleteCoreNetworkResponse_coreNetwork,
    deleteCoreNetworkResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteCoreNetwork' smart constructor.
data DeleteCoreNetwork = DeleteCoreNetwork'
  { -- | The network ID of the deleted core network.
    coreNetworkId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCoreNetwork' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreNetworkId', 'deleteCoreNetwork_coreNetworkId' - The network ID of the deleted core network.
newDeleteCoreNetwork ::
  -- | 'coreNetworkId'
  Prelude.Text ->
  DeleteCoreNetwork
newDeleteCoreNetwork pCoreNetworkId_ =
  DeleteCoreNetwork' {coreNetworkId = pCoreNetworkId_}

-- | The network ID of the deleted core network.
deleteCoreNetwork_coreNetworkId :: Lens.Lens' DeleteCoreNetwork Prelude.Text
deleteCoreNetwork_coreNetworkId = Lens.lens (\DeleteCoreNetwork' {coreNetworkId} -> coreNetworkId) (\s@DeleteCoreNetwork' {} a -> s {coreNetworkId = a} :: DeleteCoreNetwork)

instance Core.AWSRequest DeleteCoreNetwork where
  type
    AWSResponse DeleteCoreNetwork =
      DeleteCoreNetworkResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteCoreNetworkResponse'
            Prelude.<$> (x Data..?> "CoreNetwork")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCoreNetwork where
  hashWithSalt _salt DeleteCoreNetwork' {..} =
    _salt `Prelude.hashWithSalt` coreNetworkId

instance Prelude.NFData DeleteCoreNetwork where
  rnf DeleteCoreNetwork' {..} =
    Prelude.rnf coreNetworkId

instance Data.ToHeaders DeleteCoreNetwork where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteCoreNetwork where
  toPath DeleteCoreNetwork' {..} =
    Prelude.mconcat
      ["/core-networks/", Data.toBS coreNetworkId]

instance Data.ToQuery DeleteCoreNetwork where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCoreNetworkResponse' smart constructor.
data DeleteCoreNetworkResponse = DeleteCoreNetworkResponse'
  { -- | Information about the deleted core network.
    coreNetwork :: Prelude.Maybe CoreNetwork,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCoreNetworkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreNetwork', 'deleteCoreNetworkResponse_coreNetwork' - Information about the deleted core network.
--
-- 'httpStatus', 'deleteCoreNetworkResponse_httpStatus' - The response's http status code.
newDeleteCoreNetworkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteCoreNetworkResponse
newDeleteCoreNetworkResponse pHttpStatus_ =
  DeleteCoreNetworkResponse'
    { coreNetwork =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the deleted core network.
deleteCoreNetworkResponse_coreNetwork :: Lens.Lens' DeleteCoreNetworkResponse (Prelude.Maybe CoreNetwork)
deleteCoreNetworkResponse_coreNetwork = Lens.lens (\DeleteCoreNetworkResponse' {coreNetwork} -> coreNetwork) (\s@DeleteCoreNetworkResponse' {} a -> s {coreNetwork = a} :: DeleteCoreNetworkResponse)

-- | The response's http status code.
deleteCoreNetworkResponse_httpStatus :: Lens.Lens' DeleteCoreNetworkResponse Prelude.Int
deleteCoreNetworkResponse_httpStatus = Lens.lens (\DeleteCoreNetworkResponse' {httpStatus} -> httpStatus) (\s@DeleteCoreNetworkResponse' {} a -> s {httpStatus = a} :: DeleteCoreNetworkResponse)

instance Prelude.NFData DeleteCoreNetworkResponse where
  rnf DeleteCoreNetworkResponse' {..} =
    Prelude.rnf coreNetwork `Prelude.seq`
      Prelude.rnf httpStatus
