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
-- Module      : Amazonka.StorageGateway.UpdateSMBSecurityStrategy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the SMB security strategy on a file gateway. This action is only
-- supported in file gateways.
--
-- This API is called Security level in the User Guide.
--
-- A higher security level can affect performance of the gateway.
module Amazonka.StorageGateway.UpdateSMBSecurityStrategy
  ( -- * Creating a Request
    UpdateSMBSecurityStrategy (..),
    newUpdateSMBSecurityStrategy,

    -- * Request Lenses
    updateSMBSecurityStrategy_gatewayARN,
    updateSMBSecurityStrategy_sMBSecurityStrategy,

    -- * Destructuring the Response
    UpdateSMBSecurityStrategyResponse (..),
    newUpdateSMBSecurityStrategyResponse,

    -- * Response Lenses
    updateSMBSecurityStrategyResponse_gatewayARN,
    updateSMBSecurityStrategyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | /See:/ 'newUpdateSMBSecurityStrategy' smart constructor.
data UpdateSMBSecurityStrategy = UpdateSMBSecurityStrategy'
  { gatewayARN :: Prelude.Text,
    -- | Specifies the type of security strategy.
    --
    -- ClientSpecified: if you use this option, requests are established based
    -- on what is negotiated by the client. This option is recommended when you
    -- want to maximize compatibility across different clients in your
    -- environment. Supported only in S3 File Gateway.
    --
    -- MandatorySigning: if you use this option, file gateway only allows
    -- connections from SMBv2 or SMBv3 clients that have signing enabled. This
    -- option works with SMB clients on Microsoft Windows Vista, Windows Server
    -- 2008 or newer.
    --
    -- MandatoryEncryption: if you use this option, file gateway only allows
    -- connections from SMBv3 clients that have encryption enabled. This option
    -- is highly recommended for environments that handle sensitive data. This
    -- option works with SMB clients on Microsoft Windows 8, Windows Server
    -- 2012 or newer.
    sMBSecurityStrategy :: SMBSecurityStrategy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSMBSecurityStrategy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'updateSMBSecurityStrategy_gatewayARN' - Undocumented member.
--
-- 'sMBSecurityStrategy', 'updateSMBSecurityStrategy_sMBSecurityStrategy' - Specifies the type of security strategy.
--
-- ClientSpecified: if you use this option, requests are established based
-- on what is negotiated by the client. This option is recommended when you
-- want to maximize compatibility across different clients in your
-- environment. Supported only in S3 File Gateway.
--
-- MandatorySigning: if you use this option, file gateway only allows
-- connections from SMBv2 or SMBv3 clients that have signing enabled. This
-- option works with SMB clients on Microsoft Windows Vista, Windows Server
-- 2008 or newer.
--
-- MandatoryEncryption: if you use this option, file gateway only allows
-- connections from SMBv3 clients that have encryption enabled. This option
-- is highly recommended for environments that handle sensitive data. This
-- option works with SMB clients on Microsoft Windows 8, Windows Server
-- 2012 or newer.
newUpdateSMBSecurityStrategy ::
  -- | 'gatewayARN'
  Prelude.Text ->
  -- | 'sMBSecurityStrategy'
  SMBSecurityStrategy ->
  UpdateSMBSecurityStrategy
newUpdateSMBSecurityStrategy
  pGatewayARN_
  pSMBSecurityStrategy_ =
    UpdateSMBSecurityStrategy'
      { gatewayARN =
          pGatewayARN_,
        sMBSecurityStrategy = pSMBSecurityStrategy_
      }

-- | Undocumented member.
updateSMBSecurityStrategy_gatewayARN :: Lens.Lens' UpdateSMBSecurityStrategy Prelude.Text
updateSMBSecurityStrategy_gatewayARN = Lens.lens (\UpdateSMBSecurityStrategy' {gatewayARN} -> gatewayARN) (\s@UpdateSMBSecurityStrategy' {} a -> s {gatewayARN = a} :: UpdateSMBSecurityStrategy)

-- | Specifies the type of security strategy.
--
-- ClientSpecified: if you use this option, requests are established based
-- on what is negotiated by the client. This option is recommended when you
-- want to maximize compatibility across different clients in your
-- environment. Supported only in S3 File Gateway.
--
-- MandatorySigning: if you use this option, file gateway only allows
-- connections from SMBv2 or SMBv3 clients that have signing enabled. This
-- option works with SMB clients on Microsoft Windows Vista, Windows Server
-- 2008 or newer.
--
-- MandatoryEncryption: if you use this option, file gateway only allows
-- connections from SMBv3 clients that have encryption enabled. This option
-- is highly recommended for environments that handle sensitive data. This
-- option works with SMB clients on Microsoft Windows 8, Windows Server
-- 2012 or newer.
updateSMBSecurityStrategy_sMBSecurityStrategy :: Lens.Lens' UpdateSMBSecurityStrategy SMBSecurityStrategy
updateSMBSecurityStrategy_sMBSecurityStrategy = Lens.lens (\UpdateSMBSecurityStrategy' {sMBSecurityStrategy} -> sMBSecurityStrategy) (\s@UpdateSMBSecurityStrategy' {} a -> s {sMBSecurityStrategy = a} :: UpdateSMBSecurityStrategy)

instance Core.AWSRequest UpdateSMBSecurityStrategy where
  type
    AWSResponse UpdateSMBSecurityStrategy =
      UpdateSMBSecurityStrategyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSMBSecurityStrategyResponse'
            Prelude.<$> (x Core..?> "GatewayARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSMBSecurityStrategy where
  hashWithSalt _salt UpdateSMBSecurityStrategy' {..} =
    _salt `Prelude.hashWithSalt` gatewayARN
      `Prelude.hashWithSalt` sMBSecurityStrategy

instance Prelude.NFData UpdateSMBSecurityStrategy where
  rnf UpdateSMBSecurityStrategy' {..} =
    Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf sMBSecurityStrategy

instance Core.ToHeaders UpdateSMBSecurityStrategy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.UpdateSMBSecurityStrategy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateSMBSecurityStrategy where
  toJSON UpdateSMBSecurityStrategy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("GatewayARN" Core..= gatewayARN),
            Prelude.Just
              ("SMBSecurityStrategy" Core..= sMBSecurityStrategy)
          ]
      )

instance Core.ToPath UpdateSMBSecurityStrategy where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateSMBSecurityStrategy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSMBSecurityStrategyResponse' smart constructor.
data UpdateSMBSecurityStrategyResponse = UpdateSMBSecurityStrategyResponse'
  { gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSMBSecurityStrategyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'updateSMBSecurityStrategyResponse_gatewayARN' - Undocumented member.
--
-- 'httpStatus', 'updateSMBSecurityStrategyResponse_httpStatus' - The response's http status code.
newUpdateSMBSecurityStrategyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSMBSecurityStrategyResponse
newUpdateSMBSecurityStrategyResponse pHttpStatus_ =
  UpdateSMBSecurityStrategyResponse'
    { gatewayARN =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateSMBSecurityStrategyResponse_gatewayARN :: Lens.Lens' UpdateSMBSecurityStrategyResponse (Prelude.Maybe Prelude.Text)
updateSMBSecurityStrategyResponse_gatewayARN = Lens.lens (\UpdateSMBSecurityStrategyResponse' {gatewayARN} -> gatewayARN) (\s@UpdateSMBSecurityStrategyResponse' {} a -> s {gatewayARN = a} :: UpdateSMBSecurityStrategyResponse)

-- | The response's http status code.
updateSMBSecurityStrategyResponse_httpStatus :: Lens.Lens' UpdateSMBSecurityStrategyResponse Prelude.Int
updateSMBSecurityStrategyResponse_httpStatus = Lens.lens (\UpdateSMBSecurityStrategyResponse' {httpStatus} -> httpStatus) (\s@UpdateSMBSecurityStrategyResponse' {} a -> s {httpStatus = a} :: UpdateSMBSecurityStrategyResponse)

instance
  Prelude.NFData
    UpdateSMBSecurityStrategyResponse
  where
  rnf UpdateSMBSecurityStrategyResponse' {..} =
    Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf httpStatus
