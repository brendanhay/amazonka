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
-- Module      : Amazonka.AlexaBusiness.UpdateGateway
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the details of a gateway. If any optional field is not provided,
-- the existing corresponding value is left unmodified.
module Amazonka.AlexaBusiness.UpdateGateway
  ( -- * Creating a Request
    UpdateGateway (..),
    newUpdateGateway,

    -- * Request Lenses
    updateGateway_name,
    updateGateway_description,
    updateGateway_softwareVersion,
    updateGateway_gatewayArn,

    -- * Destructuring the Response
    UpdateGatewayResponse (..),
    newUpdateGatewayResponse,

    -- * Response Lenses
    updateGatewayResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateGateway' smart constructor.
data UpdateGateway = UpdateGateway'
  { -- | The updated name of the gateway.
    name :: Prelude.Maybe Prelude.Text,
    -- | The updated description of the gateway.
    description :: Prelude.Maybe Prelude.Text,
    -- | The updated software version of the gateway. The gateway automatically
    -- updates its software version during normal operation.
    softwareVersion :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the gateway to update.
    gatewayArn :: Prelude.Text
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
-- 'name', 'updateGateway_name' - The updated name of the gateway.
--
-- 'description', 'updateGateway_description' - The updated description of the gateway.
--
-- 'softwareVersion', 'updateGateway_softwareVersion' - The updated software version of the gateway. The gateway automatically
-- updates its software version during normal operation.
--
-- 'gatewayArn', 'updateGateway_gatewayArn' - The ARN of the gateway to update.
newUpdateGateway ::
  -- | 'gatewayArn'
  Prelude.Text ->
  UpdateGateway
newUpdateGateway pGatewayArn_ =
  UpdateGateway'
    { name = Prelude.Nothing,
      description = Prelude.Nothing,
      softwareVersion = Prelude.Nothing,
      gatewayArn = pGatewayArn_
    }

-- | The updated name of the gateway.
updateGateway_name :: Lens.Lens' UpdateGateway (Prelude.Maybe Prelude.Text)
updateGateway_name = Lens.lens (\UpdateGateway' {name} -> name) (\s@UpdateGateway' {} a -> s {name = a} :: UpdateGateway)

-- | The updated description of the gateway.
updateGateway_description :: Lens.Lens' UpdateGateway (Prelude.Maybe Prelude.Text)
updateGateway_description = Lens.lens (\UpdateGateway' {description} -> description) (\s@UpdateGateway' {} a -> s {description = a} :: UpdateGateway)

-- | The updated software version of the gateway. The gateway automatically
-- updates its software version during normal operation.
updateGateway_softwareVersion :: Lens.Lens' UpdateGateway (Prelude.Maybe Prelude.Text)
updateGateway_softwareVersion = Lens.lens (\UpdateGateway' {softwareVersion} -> softwareVersion) (\s@UpdateGateway' {} a -> s {softwareVersion = a} :: UpdateGateway)

-- | The ARN of the gateway to update.
updateGateway_gatewayArn :: Lens.Lens' UpdateGateway Prelude.Text
updateGateway_gatewayArn = Lens.lens (\UpdateGateway' {gatewayArn} -> gatewayArn) (\s@UpdateGateway' {} a -> s {gatewayArn = a} :: UpdateGateway)

instance Core.AWSRequest UpdateGateway where
  type
    AWSResponse UpdateGateway =
      UpdateGatewayResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateGatewayResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateGateway where
  hashWithSalt _salt UpdateGateway' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` softwareVersion
      `Prelude.hashWithSalt` gatewayArn

instance Prelude.NFData UpdateGateway where
  rnf UpdateGateway' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf softwareVersion
      `Prelude.seq` Prelude.rnf gatewayArn

instance Core.ToHeaders UpdateGateway where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.UpdateGateway" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateGateway where
  toJSON UpdateGateway' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Name" Core..=) Prelude.<$> name,
            ("Description" Core..=) Prelude.<$> description,
            ("SoftwareVersion" Core..=)
              Prelude.<$> softwareVersion,
            Prelude.Just ("GatewayArn" Core..= gatewayArn)
          ]
      )

instance Core.ToPath UpdateGateway where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateGateway where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateGatewayResponse' smart constructor.
data UpdateGatewayResponse = UpdateGatewayResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateGatewayResponse_httpStatus' - The response's http status code.
newUpdateGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateGatewayResponse
newUpdateGatewayResponse pHttpStatus_ =
  UpdateGatewayResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateGatewayResponse_httpStatus :: Lens.Lens' UpdateGatewayResponse Prelude.Int
updateGatewayResponse_httpStatus = Lens.lens (\UpdateGatewayResponse' {httpStatus} -> httpStatus) (\s@UpdateGatewayResponse' {} a -> s {httpStatus = a} :: UpdateGatewayResponse)

instance Prelude.NFData UpdateGatewayResponse where
  rnf UpdateGatewayResponse' {..} =
    Prelude.rnf httpStatus
