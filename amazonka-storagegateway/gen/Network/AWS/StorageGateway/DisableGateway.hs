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
-- Module      : Network.AWS.StorageGateway.DisableGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables a tape gateway when the gateway is no longer functioning. For
-- example, if your gateway VM is damaged, you can disable the gateway so
-- you can recover virtual tapes.
--
-- Use this operation for a tape gateway that is not reachable or not
-- functioning. This operation is only supported in the tape gateway type.
--
-- After a gateway is disabled, it cannot be enabled.
module Network.AWS.StorageGateway.DisableGateway
  ( -- * Creating a Request
    DisableGateway (..),
    newDisableGateway,

    -- * Request Lenses
    disableGateway_gatewayARN,

    -- * Destructuring the Response
    DisableGatewayResponse (..),
    newDisableGatewayResponse,

    -- * Response Lenses
    disableGatewayResponse_gatewayARN,
    disableGatewayResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | DisableGatewayInput
--
-- /See:/ 'newDisableGateway' smart constructor.
data DisableGateway = DisableGateway'
  { gatewayARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisableGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'disableGateway_gatewayARN' - Undocumented member.
newDisableGateway ::
  -- | 'gatewayARN'
  Core.Text ->
  DisableGateway
newDisableGateway pGatewayARN_ =
  DisableGateway' {gatewayARN = pGatewayARN_}

-- | Undocumented member.
disableGateway_gatewayARN :: Lens.Lens' DisableGateway Core.Text
disableGateway_gatewayARN = Lens.lens (\DisableGateway' {gatewayARN} -> gatewayARN) (\s@DisableGateway' {} a -> s {gatewayARN = a} :: DisableGateway)

instance Core.AWSRequest DisableGateway where
  type
    AWSResponse DisableGateway =
      DisableGatewayResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DisableGatewayResponse'
            Core.<$> (x Core..?> "GatewayARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DisableGateway

instance Core.NFData DisableGateway

instance Core.ToHeaders DisableGateway where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.DisableGateway" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DisableGateway where
  toJSON DisableGateway' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("GatewayARN" Core..= gatewayARN)]
      )

instance Core.ToPath DisableGateway where
  toPath = Core.const "/"

instance Core.ToQuery DisableGateway where
  toQuery = Core.const Core.mempty

-- | DisableGatewayOutput
--
-- /See:/ 'newDisableGatewayResponse' smart constructor.
data DisableGatewayResponse = DisableGatewayResponse'
  { -- | The unique Amazon Resource Name (ARN) of the disabled gateway.
    gatewayARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisableGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'disableGatewayResponse_gatewayARN' - The unique Amazon Resource Name (ARN) of the disabled gateway.
--
-- 'httpStatus', 'disableGatewayResponse_httpStatus' - The response's http status code.
newDisableGatewayResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DisableGatewayResponse
newDisableGatewayResponse pHttpStatus_ =
  DisableGatewayResponse'
    { gatewayARN = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique Amazon Resource Name (ARN) of the disabled gateway.
disableGatewayResponse_gatewayARN :: Lens.Lens' DisableGatewayResponse (Core.Maybe Core.Text)
disableGatewayResponse_gatewayARN = Lens.lens (\DisableGatewayResponse' {gatewayARN} -> gatewayARN) (\s@DisableGatewayResponse' {} a -> s {gatewayARN = a} :: DisableGatewayResponse)

-- | The response's http status code.
disableGatewayResponse_httpStatus :: Lens.Lens' DisableGatewayResponse Core.Int
disableGatewayResponse_httpStatus = Lens.lens (\DisableGatewayResponse' {httpStatus} -> httpStatus) (\s@DisableGatewayResponse' {} a -> s {httpStatus = a} :: DisableGatewayResponse)

instance Core.NFData DisableGatewayResponse
