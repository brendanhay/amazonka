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
-- Module      : Amazonka.MediaConnect.UpdateGatewayInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration of an existing Gateway Instance.
module Amazonka.MediaConnect.UpdateGatewayInstance
  ( -- * Creating a Request
    UpdateGatewayInstance (..),
    newUpdateGatewayInstance,

    -- * Request Lenses
    updateGatewayInstance_bridgePlacement,
    updateGatewayInstance_gatewayInstanceArn,

    -- * Destructuring the Response
    UpdateGatewayInstanceResponse (..),
    newUpdateGatewayInstanceResponse,

    -- * Response Lenses
    updateGatewayInstanceResponse_bridgePlacement,
    updateGatewayInstanceResponse_gatewayInstanceArn,
    updateGatewayInstanceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to update gateway instance state.
--
-- /See:/ 'newUpdateGatewayInstance' smart constructor.
data UpdateGatewayInstance = UpdateGatewayInstance'
  { -- | The availability of the instance to host new bridges. The
    -- bridgePlacement property can be LOCKED or AVAILABLE. If it is LOCKED, no
    -- new bridges can be deployed to this instance. If it is AVAILABLE, new
    -- bridges can be added to this instance.
    bridgePlacement :: Prelude.Maybe BridgePlacement,
    -- | The Amazon Resource Name (ARN) of the instance that you want to update.
    gatewayInstanceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGatewayInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bridgePlacement', 'updateGatewayInstance_bridgePlacement' - The availability of the instance to host new bridges. The
-- bridgePlacement property can be LOCKED or AVAILABLE. If it is LOCKED, no
-- new bridges can be deployed to this instance. If it is AVAILABLE, new
-- bridges can be added to this instance.
--
-- 'gatewayInstanceArn', 'updateGatewayInstance_gatewayInstanceArn' - The Amazon Resource Name (ARN) of the instance that you want to update.
newUpdateGatewayInstance ::
  -- | 'gatewayInstanceArn'
  Prelude.Text ->
  UpdateGatewayInstance
newUpdateGatewayInstance pGatewayInstanceArn_ =
  UpdateGatewayInstance'
    { bridgePlacement =
        Prelude.Nothing,
      gatewayInstanceArn = pGatewayInstanceArn_
    }

-- | The availability of the instance to host new bridges. The
-- bridgePlacement property can be LOCKED or AVAILABLE. If it is LOCKED, no
-- new bridges can be deployed to this instance. If it is AVAILABLE, new
-- bridges can be added to this instance.
updateGatewayInstance_bridgePlacement :: Lens.Lens' UpdateGatewayInstance (Prelude.Maybe BridgePlacement)
updateGatewayInstance_bridgePlacement = Lens.lens (\UpdateGatewayInstance' {bridgePlacement} -> bridgePlacement) (\s@UpdateGatewayInstance' {} a -> s {bridgePlacement = a} :: UpdateGatewayInstance)

-- | The Amazon Resource Name (ARN) of the instance that you want to update.
updateGatewayInstance_gatewayInstanceArn :: Lens.Lens' UpdateGatewayInstance Prelude.Text
updateGatewayInstance_gatewayInstanceArn = Lens.lens (\UpdateGatewayInstance' {gatewayInstanceArn} -> gatewayInstanceArn) (\s@UpdateGatewayInstance' {} a -> s {gatewayInstanceArn = a} :: UpdateGatewayInstance)

instance Core.AWSRequest UpdateGatewayInstance where
  type
    AWSResponse UpdateGatewayInstance =
      UpdateGatewayInstanceResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGatewayInstanceResponse'
            Prelude.<$> (x Data..?> "bridgePlacement")
            Prelude.<*> (x Data..?> "gatewayInstanceArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateGatewayInstance where
  hashWithSalt _salt UpdateGatewayInstance' {..} =
    _salt
      `Prelude.hashWithSalt` bridgePlacement
      `Prelude.hashWithSalt` gatewayInstanceArn

instance Prelude.NFData UpdateGatewayInstance where
  rnf UpdateGatewayInstance' {..} =
    Prelude.rnf bridgePlacement
      `Prelude.seq` Prelude.rnf gatewayInstanceArn

instance Data.ToHeaders UpdateGatewayInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateGatewayInstance where
  toJSON UpdateGatewayInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bridgePlacement" Data..=)
              Prelude.<$> bridgePlacement
          ]
      )

instance Data.ToPath UpdateGatewayInstance where
  toPath UpdateGatewayInstance' {..} =
    Prelude.mconcat
      [ "/v1/gateway-instances/",
        Data.toBS gatewayInstanceArn
      ]

instance Data.ToQuery UpdateGatewayInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateGatewayInstanceResponse' smart constructor.
data UpdateGatewayInstanceResponse = UpdateGatewayInstanceResponse'
  { -- | The availability of the instance to host new bridges. The
    -- bridgePlacement property can be LOCKED or AVAILABLE. If it is LOCKED, no
    -- new bridges can be deployed to this instance. If it is AVAILABLE, new
    -- bridges can be added to this instance.
    bridgePlacement :: Prelude.Maybe BridgePlacement,
    -- | The Amazon Resource Name (ARN) of the instance.
    gatewayInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGatewayInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bridgePlacement', 'updateGatewayInstanceResponse_bridgePlacement' - The availability of the instance to host new bridges. The
-- bridgePlacement property can be LOCKED or AVAILABLE. If it is LOCKED, no
-- new bridges can be deployed to this instance. If it is AVAILABLE, new
-- bridges can be added to this instance.
--
-- 'gatewayInstanceArn', 'updateGatewayInstanceResponse_gatewayInstanceArn' - The Amazon Resource Name (ARN) of the instance.
--
-- 'httpStatus', 'updateGatewayInstanceResponse_httpStatus' - The response's http status code.
newUpdateGatewayInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateGatewayInstanceResponse
newUpdateGatewayInstanceResponse pHttpStatus_ =
  UpdateGatewayInstanceResponse'
    { bridgePlacement =
        Prelude.Nothing,
      gatewayInstanceArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The availability of the instance to host new bridges. The
-- bridgePlacement property can be LOCKED or AVAILABLE. If it is LOCKED, no
-- new bridges can be deployed to this instance. If it is AVAILABLE, new
-- bridges can be added to this instance.
updateGatewayInstanceResponse_bridgePlacement :: Lens.Lens' UpdateGatewayInstanceResponse (Prelude.Maybe BridgePlacement)
updateGatewayInstanceResponse_bridgePlacement = Lens.lens (\UpdateGatewayInstanceResponse' {bridgePlacement} -> bridgePlacement) (\s@UpdateGatewayInstanceResponse' {} a -> s {bridgePlacement = a} :: UpdateGatewayInstanceResponse)

-- | The Amazon Resource Name (ARN) of the instance.
updateGatewayInstanceResponse_gatewayInstanceArn :: Lens.Lens' UpdateGatewayInstanceResponse (Prelude.Maybe Prelude.Text)
updateGatewayInstanceResponse_gatewayInstanceArn = Lens.lens (\UpdateGatewayInstanceResponse' {gatewayInstanceArn} -> gatewayInstanceArn) (\s@UpdateGatewayInstanceResponse' {} a -> s {gatewayInstanceArn = a} :: UpdateGatewayInstanceResponse)

-- | The response's http status code.
updateGatewayInstanceResponse_httpStatus :: Lens.Lens' UpdateGatewayInstanceResponse Prelude.Int
updateGatewayInstanceResponse_httpStatus = Lens.lens (\UpdateGatewayInstanceResponse' {httpStatus} -> httpStatus) (\s@UpdateGatewayInstanceResponse' {} a -> s {httpStatus = a} :: UpdateGatewayInstanceResponse)

instance Prelude.NFData UpdateGatewayInstanceResponse where
  rnf UpdateGatewayInstanceResponse' {..} =
    Prelude.rnf bridgePlacement
      `Prelude.seq` Prelude.rnf gatewayInstanceArn
      `Prelude.seq` Prelude.rnf httpStatus
