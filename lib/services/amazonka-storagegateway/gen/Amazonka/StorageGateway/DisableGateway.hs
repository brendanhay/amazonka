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
-- Module      : Amazonka.StorageGateway.DisableGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.StorageGateway.DisableGateway
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | DisableGatewayInput
--
-- /See:/ 'newDisableGateway' smart constructor.
data DisableGateway = DisableGateway'
  { gatewayARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DisableGateway where
  type
    AWSResponse DisableGateway =
      DisableGatewayResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisableGatewayResponse'
            Prelude.<$> (x Data..?> "GatewayARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisableGateway where
  hashWithSalt _salt DisableGateway' {..} =
    _salt `Prelude.hashWithSalt` gatewayARN

instance Prelude.NFData DisableGateway where
  rnf DisableGateway' {..} = Prelude.rnf gatewayARN

instance Data.ToHeaders DisableGateway where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.DisableGateway" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisableGateway where
  toJSON DisableGateway' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("GatewayARN" Data..= gatewayARN)]
      )

instance Data.ToPath DisableGateway where
  toPath = Prelude.const "/"

instance Data.ToQuery DisableGateway where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData DisableGatewayResponse where
  rnf DisableGatewayResponse' {..} =
    Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf httpStatus
