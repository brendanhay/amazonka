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
-- Module      : Network.AWS.StorageGateway.StartGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a gateway that you previously shut down (see ShutdownGateway).
-- After the gateway starts, you can then make other API calls, your
-- applications can read from or write to the gateway\'s storage volumes
-- and you will be able to take snapshot backups.
--
-- When you make a request, you will get a 200 OK success response
-- immediately. However, it might take some time for the gateway to be
-- ready. You should call DescribeGatewayInformation and check the status
-- before making any additional API calls. For more information, see
-- ActivateGateway.
--
-- To specify which gateway to start, use the Amazon Resource Name (ARN) of
-- the gateway in your request.
module Network.AWS.StorageGateway.StartGateway
  ( -- * Creating a Request
    StartGateway (..),
    newStartGateway,

    -- * Request Lenses
    startGateway_gatewayARN,

    -- * Destructuring the Response
    StartGatewayResponse (..),
    newStartGatewayResponse,

    -- * Response Lenses
    startGatewayResponse_gatewayARN,
    startGatewayResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway
-- to start.
--
-- /See:/ 'newStartGateway' smart constructor.
data StartGateway = StartGateway'
  { gatewayARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'startGateway_gatewayARN' - Undocumented member.
newStartGateway ::
  -- | 'gatewayARN'
  Core.Text ->
  StartGateway
newStartGateway pGatewayARN_ =
  StartGateway' {gatewayARN = pGatewayARN_}

-- | Undocumented member.
startGateway_gatewayARN :: Lens.Lens' StartGateway Core.Text
startGateway_gatewayARN = Lens.lens (\StartGateway' {gatewayARN} -> gatewayARN) (\s@StartGateway' {} a -> s {gatewayARN = a} :: StartGateway)

instance Core.AWSRequest StartGateway where
  type AWSResponse StartGateway = StartGatewayResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartGatewayResponse'
            Core.<$> (x Core..?> "GatewayARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartGateway

instance Core.NFData StartGateway

instance Core.ToHeaders StartGateway where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.StartGateway" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartGateway where
  toJSON StartGateway' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("GatewayARN" Core..= gatewayARN)]
      )

instance Core.ToPath StartGateway where
  toPath = Core.const "/"

instance Core.ToQuery StartGateway where
  toQuery = Core.const Core.mempty

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway
-- that was restarted.
--
-- /See:/ 'newStartGatewayResponse' smart constructor.
data StartGatewayResponse = StartGatewayResponse'
  { gatewayARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'startGatewayResponse_gatewayARN' - Undocumented member.
--
-- 'httpStatus', 'startGatewayResponse_httpStatus' - The response's http status code.
newStartGatewayResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartGatewayResponse
newStartGatewayResponse pHttpStatus_ =
  StartGatewayResponse'
    { gatewayARN = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
startGatewayResponse_gatewayARN :: Lens.Lens' StartGatewayResponse (Core.Maybe Core.Text)
startGatewayResponse_gatewayARN = Lens.lens (\StartGatewayResponse' {gatewayARN} -> gatewayARN) (\s@StartGatewayResponse' {} a -> s {gatewayARN = a} :: StartGatewayResponse)

-- | The response's http status code.
startGatewayResponse_httpStatus :: Lens.Lens' StartGatewayResponse Core.Int
startGatewayResponse_httpStatus = Lens.lens (\StartGatewayResponse' {httpStatus} -> httpStatus) (\s@StartGatewayResponse' {} a -> s {httpStatus = a} :: StartGatewayResponse)

instance Core.NFData StartGatewayResponse
