{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.AlexaBusiness.UpdateGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the details of a gateway. If any optional field is not provided,
-- the existing corresponding value is left unmodified.
module Network.AWS.AlexaBusiness.UpdateGateway
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

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest UpdateGateway where
  type Rs UpdateGateway = UpdateGatewayResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateGatewayResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateGateway

instance Prelude.NFData UpdateGateway

instance Prelude.ToHeaders UpdateGateway where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.UpdateGateway" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateGateway where
  toJSON UpdateGateway' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Name" Prelude..=) Prelude.<$> name,
            ("Description" Prelude..=) Prelude.<$> description,
            ("SoftwareVersion" Prelude..=)
              Prelude.<$> softwareVersion,
            Prelude.Just ("GatewayArn" Prelude..= gatewayArn)
          ]
      )

instance Prelude.ToPath UpdateGateway where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateGateway where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateGatewayResponse' smart constructor.
data UpdateGatewayResponse = UpdateGatewayResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData UpdateGatewayResponse
