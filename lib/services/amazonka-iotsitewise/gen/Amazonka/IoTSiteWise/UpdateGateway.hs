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
-- Module      : Amazonka.IoTSiteWise.UpdateGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a gateway\'s name.
module Amazonka.IoTSiteWise.UpdateGateway
  ( -- * Creating a Request
    UpdateGateway (..),
    newUpdateGateway,

    -- * Request Lenses
    updateGateway_gatewayId,
    updateGateway_gatewayName,

    -- * Destructuring the Response
    UpdateGatewayResponse (..),
    newUpdateGatewayResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateGateway' smart constructor.
data UpdateGateway = UpdateGateway'
  { -- | The ID of the gateway to update.
    gatewayId :: Prelude.Text,
    -- | A unique, friendly name for the gateway.
    gatewayName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayId', 'updateGateway_gatewayId' - The ID of the gateway to update.
--
-- 'gatewayName', 'updateGateway_gatewayName' - A unique, friendly name for the gateway.
newUpdateGateway ::
  -- | 'gatewayId'
  Prelude.Text ->
  -- | 'gatewayName'
  Prelude.Text ->
  UpdateGateway
newUpdateGateway pGatewayId_ pGatewayName_ =
  UpdateGateway'
    { gatewayId = pGatewayId_,
      gatewayName = pGatewayName_
    }

-- | The ID of the gateway to update.
updateGateway_gatewayId :: Lens.Lens' UpdateGateway Prelude.Text
updateGateway_gatewayId = Lens.lens (\UpdateGateway' {gatewayId} -> gatewayId) (\s@UpdateGateway' {} a -> s {gatewayId = a} :: UpdateGateway)

-- | A unique, friendly name for the gateway.
updateGateway_gatewayName :: Lens.Lens' UpdateGateway Prelude.Text
updateGateway_gatewayName = Lens.lens (\UpdateGateway' {gatewayName} -> gatewayName) (\s@UpdateGateway' {} a -> s {gatewayName = a} :: UpdateGateway)

instance Core.AWSRequest UpdateGateway where
  type
    AWSResponse UpdateGateway =
      UpdateGatewayResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateGatewayResponse'

instance Prelude.Hashable UpdateGateway where
  hashWithSalt _salt UpdateGateway' {..} =
    _salt
      `Prelude.hashWithSalt` gatewayId
      `Prelude.hashWithSalt` gatewayName

instance Prelude.NFData UpdateGateway where
  rnf UpdateGateway' {..} =
    Prelude.rnf gatewayId `Prelude.seq`
      Prelude.rnf gatewayName

instance Data.ToHeaders UpdateGateway where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateGateway where
  toJSON UpdateGateway' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("gatewayName" Data..= gatewayName)]
      )

instance Data.ToPath UpdateGateway where
  toPath UpdateGateway' {..} =
    Prelude.mconcat
      ["/20200301/gateways/", Data.toBS gatewayId]

instance Data.ToQuery UpdateGateway where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateGatewayResponse' smart constructor.
data UpdateGatewayResponse = UpdateGatewayResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateGatewayResponse ::
  UpdateGatewayResponse
newUpdateGatewayResponse = UpdateGatewayResponse'

instance Prelude.NFData UpdateGatewayResponse where
  rnf _ = ()
