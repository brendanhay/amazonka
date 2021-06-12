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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateGateway' smart constructor.
data UpdateGateway = UpdateGateway'
  { -- | The updated name of the gateway.
    name :: Core.Maybe Core.Text,
    -- | The updated description of the gateway.
    description :: Core.Maybe Core.Text,
    -- | The updated software version of the gateway. The gateway automatically
    -- updates its software version during normal operation.
    softwareVersion :: Core.Maybe Core.Text,
    -- | The ARN of the gateway to update.
    gatewayArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  UpdateGateway
newUpdateGateway pGatewayArn_ =
  UpdateGateway'
    { name = Core.Nothing,
      description = Core.Nothing,
      softwareVersion = Core.Nothing,
      gatewayArn = pGatewayArn_
    }

-- | The updated name of the gateway.
updateGateway_name :: Lens.Lens' UpdateGateway (Core.Maybe Core.Text)
updateGateway_name = Lens.lens (\UpdateGateway' {name} -> name) (\s@UpdateGateway' {} a -> s {name = a} :: UpdateGateway)

-- | The updated description of the gateway.
updateGateway_description :: Lens.Lens' UpdateGateway (Core.Maybe Core.Text)
updateGateway_description = Lens.lens (\UpdateGateway' {description} -> description) (\s@UpdateGateway' {} a -> s {description = a} :: UpdateGateway)

-- | The updated software version of the gateway. The gateway automatically
-- updates its software version during normal operation.
updateGateway_softwareVersion :: Lens.Lens' UpdateGateway (Core.Maybe Core.Text)
updateGateway_softwareVersion = Lens.lens (\UpdateGateway' {softwareVersion} -> softwareVersion) (\s@UpdateGateway' {} a -> s {softwareVersion = a} :: UpdateGateway)

-- | The ARN of the gateway to update.
updateGateway_gatewayArn :: Lens.Lens' UpdateGateway Core.Text
updateGateway_gatewayArn = Lens.lens (\UpdateGateway' {gatewayArn} -> gatewayArn) (\s@UpdateGateway' {} a -> s {gatewayArn = a} :: UpdateGateway)

instance Core.AWSRequest UpdateGateway where
  type
    AWSResponse UpdateGateway =
      UpdateGatewayResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateGatewayResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateGateway

instance Core.NFData UpdateGateway

instance Core.ToHeaders UpdateGateway where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.UpdateGateway" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateGateway where
  toJSON UpdateGateway' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Name" Core..=) Core.<$> name,
            ("Description" Core..=) Core.<$> description,
            ("SoftwareVersion" Core..=) Core.<$> softwareVersion,
            Core.Just ("GatewayArn" Core..= gatewayArn)
          ]
      )

instance Core.ToPath UpdateGateway where
  toPath = Core.const "/"

instance Core.ToQuery UpdateGateway where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateGatewayResponse' smart constructor.
data UpdateGatewayResponse = UpdateGatewayResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateGatewayResponse
newUpdateGatewayResponse pHttpStatus_ =
  UpdateGatewayResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateGatewayResponse_httpStatus :: Lens.Lens' UpdateGatewayResponse Core.Int
updateGatewayResponse_httpStatus = Lens.lens (\UpdateGatewayResponse' {httpStatus} -> httpStatus) (\s@UpdateGatewayResponse' {} a -> s {httpStatus = a} :: UpdateGatewayResponse)

instance Core.NFData UpdateGatewayResponse
