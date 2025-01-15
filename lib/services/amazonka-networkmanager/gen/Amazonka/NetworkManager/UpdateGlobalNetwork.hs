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
-- Module      : Amazonka.NetworkManager.UpdateGlobalNetwork
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing global network. To remove information for any of the
-- parameters, specify an empty string.
module Amazonka.NetworkManager.UpdateGlobalNetwork
  ( -- * Creating a Request
    UpdateGlobalNetwork (..),
    newUpdateGlobalNetwork,

    -- * Request Lenses
    updateGlobalNetwork_description,
    updateGlobalNetwork_globalNetworkId,

    -- * Destructuring the Response
    UpdateGlobalNetworkResponse (..),
    newUpdateGlobalNetworkResponse,

    -- * Response Lenses
    updateGlobalNetworkResponse_globalNetwork,
    updateGlobalNetworkResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateGlobalNetwork' smart constructor.
data UpdateGlobalNetwork = UpdateGlobalNetwork'
  { -- | A description of the global network.
    --
    -- Constraints: Maximum length of 256 characters.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of your global network.
    globalNetworkId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGlobalNetwork' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateGlobalNetwork_description' - A description of the global network.
--
-- Constraints: Maximum length of 256 characters.
--
-- 'globalNetworkId', 'updateGlobalNetwork_globalNetworkId' - The ID of your global network.
newUpdateGlobalNetwork ::
  -- | 'globalNetworkId'
  Prelude.Text ->
  UpdateGlobalNetwork
newUpdateGlobalNetwork pGlobalNetworkId_ =
  UpdateGlobalNetwork'
    { description = Prelude.Nothing,
      globalNetworkId = pGlobalNetworkId_
    }

-- | A description of the global network.
--
-- Constraints: Maximum length of 256 characters.
updateGlobalNetwork_description :: Lens.Lens' UpdateGlobalNetwork (Prelude.Maybe Prelude.Text)
updateGlobalNetwork_description = Lens.lens (\UpdateGlobalNetwork' {description} -> description) (\s@UpdateGlobalNetwork' {} a -> s {description = a} :: UpdateGlobalNetwork)

-- | The ID of your global network.
updateGlobalNetwork_globalNetworkId :: Lens.Lens' UpdateGlobalNetwork Prelude.Text
updateGlobalNetwork_globalNetworkId = Lens.lens (\UpdateGlobalNetwork' {globalNetworkId} -> globalNetworkId) (\s@UpdateGlobalNetwork' {} a -> s {globalNetworkId = a} :: UpdateGlobalNetwork)

instance Core.AWSRequest UpdateGlobalNetwork where
  type
    AWSResponse UpdateGlobalNetwork =
      UpdateGlobalNetworkResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGlobalNetworkResponse'
            Prelude.<$> (x Data..?> "GlobalNetwork")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateGlobalNetwork where
  hashWithSalt _salt UpdateGlobalNetwork' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` globalNetworkId

instance Prelude.NFData UpdateGlobalNetwork where
  rnf UpdateGlobalNetwork' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf globalNetworkId

instance Data.ToHeaders UpdateGlobalNetwork where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateGlobalNetwork where
  toJSON UpdateGlobalNetwork' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Description" Data..=) Prelude.<$> description]
      )

instance Data.ToPath UpdateGlobalNetwork where
  toPath UpdateGlobalNetwork' {..} =
    Prelude.mconcat
      ["/global-networks/", Data.toBS globalNetworkId]

instance Data.ToQuery UpdateGlobalNetwork where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateGlobalNetworkResponse' smart constructor.
data UpdateGlobalNetworkResponse = UpdateGlobalNetworkResponse'
  { -- | Information about the global network object.
    globalNetwork :: Prelude.Maybe GlobalNetwork,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGlobalNetworkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalNetwork', 'updateGlobalNetworkResponse_globalNetwork' - Information about the global network object.
--
-- 'httpStatus', 'updateGlobalNetworkResponse_httpStatus' - The response's http status code.
newUpdateGlobalNetworkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateGlobalNetworkResponse
newUpdateGlobalNetworkResponse pHttpStatus_ =
  UpdateGlobalNetworkResponse'
    { globalNetwork =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the global network object.
updateGlobalNetworkResponse_globalNetwork :: Lens.Lens' UpdateGlobalNetworkResponse (Prelude.Maybe GlobalNetwork)
updateGlobalNetworkResponse_globalNetwork = Lens.lens (\UpdateGlobalNetworkResponse' {globalNetwork} -> globalNetwork) (\s@UpdateGlobalNetworkResponse' {} a -> s {globalNetwork = a} :: UpdateGlobalNetworkResponse)

-- | The response's http status code.
updateGlobalNetworkResponse_httpStatus :: Lens.Lens' UpdateGlobalNetworkResponse Prelude.Int
updateGlobalNetworkResponse_httpStatus = Lens.lens (\UpdateGlobalNetworkResponse' {httpStatus} -> httpStatus) (\s@UpdateGlobalNetworkResponse' {} a -> s {httpStatus = a} :: UpdateGlobalNetworkResponse)

instance Prelude.NFData UpdateGlobalNetworkResponse where
  rnf UpdateGlobalNetworkResponse' {..} =
    Prelude.rnf globalNetwork `Prelude.seq`
      Prelude.rnf httpStatus
