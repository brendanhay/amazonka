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
-- Module      : Amazonka.NetworkManager.UpdateCoreNetwork
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the description of a core network.
module Amazonka.NetworkManager.UpdateCoreNetwork
  ( -- * Creating a Request
    UpdateCoreNetwork (..),
    newUpdateCoreNetwork,

    -- * Request Lenses
    updateCoreNetwork_description,
    updateCoreNetwork_coreNetworkId,

    -- * Destructuring the Response
    UpdateCoreNetworkResponse (..),
    newUpdateCoreNetworkResponse,

    -- * Response Lenses
    updateCoreNetworkResponse_coreNetwork,
    updateCoreNetworkResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateCoreNetwork' smart constructor.
data UpdateCoreNetwork = UpdateCoreNetwork'
  { -- | The description of the update.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of a core network.
    coreNetworkId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCoreNetwork' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateCoreNetwork_description' - The description of the update.
--
-- 'coreNetworkId', 'updateCoreNetwork_coreNetworkId' - The ID of a core network.
newUpdateCoreNetwork ::
  -- | 'coreNetworkId'
  Prelude.Text ->
  UpdateCoreNetwork
newUpdateCoreNetwork pCoreNetworkId_ =
  UpdateCoreNetwork'
    { description = Prelude.Nothing,
      coreNetworkId = pCoreNetworkId_
    }

-- | The description of the update.
updateCoreNetwork_description :: Lens.Lens' UpdateCoreNetwork (Prelude.Maybe Prelude.Text)
updateCoreNetwork_description = Lens.lens (\UpdateCoreNetwork' {description} -> description) (\s@UpdateCoreNetwork' {} a -> s {description = a} :: UpdateCoreNetwork)

-- | The ID of a core network.
updateCoreNetwork_coreNetworkId :: Lens.Lens' UpdateCoreNetwork Prelude.Text
updateCoreNetwork_coreNetworkId = Lens.lens (\UpdateCoreNetwork' {coreNetworkId} -> coreNetworkId) (\s@UpdateCoreNetwork' {} a -> s {coreNetworkId = a} :: UpdateCoreNetwork)

instance Core.AWSRequest UpdateCoreNetwork where
  type
    AWSResponse UpdateCoreNetwork =
      UpdateCoreNetworkResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateCoreNetworkResponse'
            Prelude.<$> (x Data..?> "CoreNetwork")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateCoreNetwork where
  hashWithSalt _salt UpdateCoreNetwork' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` coreNetworkId

instance Prelude.NFData UpdateCoreNetwork where
  rnf UpdateCoreNetwork' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf coreNetworkId

instance Data.ToHeaders UpdateCoreNetwork where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateCoreNetwork where
  toJSON UpdateCoreNetwork' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Description" Data..=) Prelude.<$> description]
      )

instance Data.ToPath UpdateCoreNetwork where
  toPath UpdateCoreNetwork' {..} =
    Prelude.mconcat
      ["/core-networks/", Data.toBS coreNetworkId]

instance Data.ToQuery UpdateCoreNetwork where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateCoreNetworkResponse' smart constructor.
data UpdateCoreNetworkResponse = UpdateCoreNetworkResponse'
  { -- | Returns information about a core network update.
    coreNetwork :: Prelude.Maybe CoreNetwork,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCoreNetworkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreNetwork', 'updateCoreNetworkResponse_coreNetwork' - Returns information about a core network update.
--
-- 'httpStatus', 'updateCoreNetworkResponse_httpStatus' - The response's http status code.
newUpdateCoreNetworkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateCoreNetworkResponse
newUpdateCoreNetworkResponse pHttpStatus_ =
  UpdateCoreNetworkResponse'
    { coreNetwork =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns information about a core network update.
updateCoreNetworkResponse_coreNetwork :: Lens.Lens' UpdateCoreNetworkResponse (Prelude.Maybe CoreNetwork)
updateCoreNetworkResponse_coreNetwork = Lens.lens (\UpdateCoreNetworkResponse' {coreNetwork} -> coreNetwork) (\s@UpdateCoreNetworkResponse' {} a -> s {coreNetwork = a} :: UpdateCoreNetworkResponse)

-- | The response's http status code.
updateCoreNetworkResponse_httpStatus :: Lens.Lens' UpdateCoreNetworkResponse Prelude.Int
updateCoreNetworkResponse_httpStatus = Lens.lens (\UpdateCoreNetworkResponse' {httpStatus} -> httpStatus) (\s@UpdateCoreNetworkResponse' {} a -> s {httpStatus = a} :: UpdateCoreNetworkResponse)

instance Prelude.NFData UpdateCoreNetworkResponse where
  rnf UpdateCoreNetworkResponse' {..} =
    Prelude.rnf coreNetwork
      `Prelude.seq` Prelude.rnf httpStatus
