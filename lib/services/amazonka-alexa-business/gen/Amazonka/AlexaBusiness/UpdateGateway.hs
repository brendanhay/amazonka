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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    updateGateway_description,
    updateGateway_name,
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateGateway' smart constructor.
data UpdateGateway = UpdateGateway'
  { -- | The updated description of the gateway.
    description :: Prelude.Maybe Prelude.Text,
    -- | The updated name of the gateway.
    name :: Prelude.Maybe Prelude.Text,
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
-- 'description', 'updateGateway_description' - The updated description of the gateway.
--
-- 'name', 'updateGateway_name' - The updated name of the gateway.
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
    { description = Prelude.Nothing,
      name = Prelude.Nothing,
      softwareVersion = Prelude.Nothing,
      gatewayArn = pGatewayArn_
    }

-- | The updated description of the gateway.
updateGateway_description :: Lens.Lens' UpdateGateway (Prelude.Maybe Prelude.Text)
updateGateway_description = Lens.lens (\UpdateGateway' {description} -> description) (\s@UpdateGateway' {} a -> s {description = a} :: UpdateGateway)

-- | The updated name of the gateway.
updateGateway_name :: Lens.Lens' UpdateGateway (Prelude.Maybe Prelude.Text)
updateGateway_name = Lens.lens (\UpdateGateway' {name} -> name) (\s@UpdateGateway' {} a -> s {name = a} :: UpdateGateway)

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
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` softwareVersion
      `Prelude.hashWithSalt` gatewayArn

instance Prelude.NFData UpdateGateway where
  rnf UpdateGateway' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf softwareVersion `Prelude.seq`
          Prelude.rnf gatewayArn

instance Data.ToHeaders UpdateGateway where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.UpdateGateway" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateGateway where
  toJSON UpdateGateway' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Name" Data..=) Prelude.<$> name,
            ("SoftwareVersion" Data..=)
              Prelude.<$> softwareVersion,
            Prelude.Just ("GatewayArn" Data..= gatewayArn)
          ]
      )

instance Data.ToPath UpdateGateway where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateGateway where
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
