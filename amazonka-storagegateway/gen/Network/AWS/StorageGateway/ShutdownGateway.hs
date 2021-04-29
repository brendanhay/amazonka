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
-- Module      : Network.AWS.StorageGateway.ShutdownGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shuts down a gateway. To specify which gateway to shut down, use the
-- Amazon Resource Name (ARN) of the gateway in the body of your request.
--
-- The operation shuts down the gateway service component running in the
-- gateway\'s virtual machine (VM) and not the host VM.
--
-- If you want to shut down the VM, it is recommended that you first shut
-- down the gateway component in the VM to avoid unpredictable conditions.
--
-- After the gateway is shutdown, you cannot call any other API except
-- StartGateway, DescribeGatewayInformation, and ListGateways. For more
-- information, see ActivateGateway. Your applications cannot read from or
-- write to the gateway\'s storage volumes, and there are no snapshots
-- taken.
--
-- When you make a shutdown request, you will get a @200 OK@ success
-- response immediately. However, it might take some time for the gateway
-- to shut down. You can call the DescribeGatewayInformation API to check
-- the status. For more information, see ActivateGateway.
--
-- If do not intend to use the gateway again, you must delete the gateway
-- (using DeleteGateway) to no longer pay software charges associated with
-- the gateway.
module Network.AWS.StorageGateway.ShutdownGateway
  ( -- * Creating a Request
    ShutdownGateway (..),
    newShutdownGateway,

    -- * Request Lenses
    shutdownGateway_gatewayARN,

    -- * Destructuring the Response
    ShutdownGatewayResponse (..),
    newShutdownGatewayResponse,

    -- * Response Lenses
    shutdownGatewayResponse_gatewayARN,
    shutdownGatewayResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway
-- to shut down.
--
-- /See:/ 'newShutdownGateway' smart constructor.
data ShutdownGateway = ShutdownGateway'
  { gatewayARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ShutdownGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'shutdownGateway_gatewayARN' - Undocumented member.
newShutdownGateway ::
  -- | 'gatewayARN'
  Prelude.Text ->
  ShutdownGateway
newShutdownGateway pGatewayARN_ =
  ShutdownGateway' {gatewayARN = pGatewayARN_}

-- | Undocumented member.
shutdownGateway_gatewayARN :: Lens.Lens' ShutdownGateway Prelude.Text
shutdownGateway_gatewayARN = Lens.lens (\ShutdownGateway' {gatewayARN} -> gatewayARN) (\s@ShutdownGateway' {} a -> s {gatewayARN = a} :: ShutdownGateway)

instance Prelude.AWSRequest ShutdownGateway where
  type Rs ShutdownGateway = ShutdownGatewayResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ShutdownGatewayResponse'
            Prelude.<$> (x Prelude..?> "GatewayARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ShutdownGateway

instance Prelude.NFData ShutdownGateway

instance Prelude.ToHeaders ShutdownGateway where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StorageGateway_20130630.ShutdownGateway" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ShutdownGateway where
  toJSON ShutdownGateway' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("GatewayARN" Prelude..= gatewayARN)]
      )

instance Prelude.ToPath ShutdownGateway where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ShutdownGateway where
  toQuery = Prelude.const Prelude.mempty

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway
-- that was shut down.
--
-- /See:/ 'newShutdownGatewayResponse' smart constructor.
data ShutdownGatewayResponse = ShutdownGatewayResponse'
  { gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ShutdownGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'shutdownGatewayResponse_gatewayARN' - Undocumented member.
--
-- 'httpStatus', 'shutdownGatewayResponse_httpStatus' - The response's http status code.
newShutdownGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ShutdownGatewayResponse
newShutdownGatewayResponse pHttpStatus_ =
  ShutdownGatewayResponse'
    { gatewayARN =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
shutdownGatewayResponse_gatewayARN :: Lens.Lens' ShutdownGatewayResponse (Prelude.Maybe Prelude.Text)
shutdownGatewayResponse_gatewayARN = Lens.lens (\ShutdownGatewayResponse' {gatewayARN} -> gatewayARN) (\s@ShutdownGatewayResponse' {} a -> s {gatewayARN = a} :: ShutdownGatewayResponse)

-- | The response's http status code.
shutdownGatewayResponse_httpStatus :: Lens.Lens' ShutdownGatewayResponse Prelude.Int
shutdownGatewayResponse_httpStatus = Lens.lens (\ShutdownGatewayResponse' {httpStatus} -> httpStatus) (\s@ShutdownGatewayResponse' {} a -> s {httpStatus = a} :: ShutdownGatewayResponse)

instance Prelude.NFData ShutdownGatewayResponse
