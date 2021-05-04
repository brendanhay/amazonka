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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | DisableGatewayInput
--
-- /See:/ 'newDisableGateway' smart constructor.
data DisableGateway = DisableGateway'
  { gatewayARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DisableGateway
newDisableGateway pGatewayARN_ =
  DisableGateway' {gatewayARN = pGatewayARN_}

-- | Undocumented member.
disableGateway_gatewayARN :: Lens.Lens' DisableGateway Prelude.Text
disableGateway_gatewayARN = Lens.lens (\DisableGateway' {gatewayARN} -> gatewayARN) (\s@DisableGateway' {} a -> s {gatewayARN = a} :: DisableGateway)

instance Prelude.AWSRequest DisableGateway where
  type Rs DisableGateway = DisableGatewayResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DisableGatewayResponse'
            Prelude.<$> (x Prelude..?> "GatewayARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisableGateway

instance Prelude.NFData DisableGateway

instance Prelude.ToHeaders DisableGateway where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StorageGateway_20130630.DisableGateway" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DisableGateway where
  toJSON DisableGateway' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("GatewayARN" Prelude..= gatewayARN)]
      )

instance Prelude.ToPath DisableGateway where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DisableGateway where
  toQuery = Prelude.const Prelude.mempty

-- | DisableGatewayOutput
--
-- /See:/ 'newDisableGatewayResponse' smart constructor.
data DisableGatewayResponse = DisableGatewayResponse'
  { -- | The unique Amazon Resource Name (ARN) of the disabled gateway.
    gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DisableGatewayResponse
newDisableGatewayResponse pHttpStatus_ =
  DisableGatewayResponse'
    { gatewayARN =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique Amazon Resource Name (ARN) of the disabled gateway.
disableGatewayResponse_gatewayARN :: Lens.Lens' DisableGatewayResponse (Prelude.Maybe Prelude.Text)
disableGatewayResponse_gatewayARN = Lens.lens (\DisableGatewayResponse' {gatewayARN} -> gatewayARN) (\s@DisableGatewayResponse' {} a -> s {gatewayARN = a} :: DisableGatewayResponse)

-- | The response's http status code.
disableGatewayResponse_httpStatus :: Lens.Lens' DisableGatewayResponse Prelude.Int
disableGatewayResponse_httpStatus = Lens.lens (\DisableGatewayResponse' {httpStatus} -> httpStatus) (\s@DisableGatewayResponse' {} a -> s {httpStatus = a} :: DisableGatewayResponse)

instance Prelude.NFData DisableGatewayResponse
