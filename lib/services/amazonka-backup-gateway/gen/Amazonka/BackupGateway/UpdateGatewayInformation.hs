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
-- Module      : Amazonka.BackupGateway.UpdateGatewayInformation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a gateway\'s name. Specify which gateway to update using the
-- Amazon Resource Name (ARN) of the gateway in your request.
module Amazonka.BackupGateway.UpdateGatewayInformation
  ( -- * Creating a Request
    UpdateGatewayInformation (..),
    newUpdateGatewayInformation,

    -- * Request Lenses
    updateGatewayInformation_gatewayDisplayName,
    updateGatewayInformation_gatewayArn,

    -- * Destructuring the Response
    UpdateGatewayInformationResponse (..),
    newUpdateGatewayInformationResponse,

    -- * Response Lenses
    updateGatewayInformationResponse_gatewayArn,
    updateGatewayInformationResponse_httpStatus,
  )
where

import Amazonka.BackupGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateGatewayInformation' smart constructor.
data UpdateGatewayInformation = UpdateGatewayInformation'
  { -- | The updated display name of the gateway.
    gatewayDisplayName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the gateway to update.
    gatewayArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGatewayInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayDisplayName', 'updateGatewayInformation_gatewayDisplayName' - The updated display name of the gateway.
--
-- 'gatewayArn', 'updateGatewayInformation_gatewayArn' - The Amazon Resource Name (ARN) of the gateway to update.
newUpdateGatewayInformation ::
  -- | 'gatewayArn'
  Prelude.Text ->
  UpdateGatewayInformation
newUpdateGatewayInformation pGatewayArn_ =
  UpdateGatewayInformation'
    { gatewayDisplayName =
        Prelude.Nothing,
      gatewayArn = pGatewayArn_
    }

-- | The updated display name of the gateway.
updateGatewayInformation_gatewayDisplayName :: Lens.Lens' UpdateGatewayInformation (Prelude.Maybe Prelude.Text)
updateGatewayInformation_gatewayDisplayName = Lens.lens (\UpdateGatewayInformation' {gatewayDisplayName} -> gatewayDisplayName) (\s@UpdateGatewayInformation' {} a -> s {gatewayDisplayName = a} :: UpdateGatewayInformation)

-- | The Amazon Resource Name (ARN) of the gateway to update.
updateGatewayInformation_gatewayArn :: Lens.Lens' UpdateGatewayInformation Prelude.Text
updateGatewayInformation_gatewayArn = Lens.lens (\UpdateGatewayInformation' {gatewayArn} -> gatewayArn) (\s@UpdateGatewayInformation' {} a -> s {gatewayArn = a} :: UpdateGatewayInformation)

instance Core.AWSRequest UpdateGatewayInformation where
  type
    AWSResponse UpdateGatewayInformation =
      UpdateGatewayInformationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGatewayInformationResponse'
            Prelude.<$> (x Core..?> "GatewayArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateGatewayInformation where
  hashWithSalt _salt UpdateGatewayInformation' {..} =
    _salt `Prelude.hashWithSalt` gatewayDisplayName
      `Prelude.hashWithSalt` gatewayArn

instance Prelude.NFData UpdateGatewayInformation where
  rnf UpdateGatewayInformation' {..} =
    Prelude.rnf gatewayDisplayName
      `Prelude.seq` Prelude.rnf gatewayArn

instance Core.ToHeaders UpdateGatewayInformation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "BackupOnPremises_v20210101.UpdateGatewayInformation" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateGatewayInformation where
  toJSON UpdateGatewayInformation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("GatewayDisplayName" Core..=)
              Prelude.<$> gatewayDisplayName,
            Prelude.Just ("GatewayArn" Core..= gatewayArn)
          ]
      )

instance Core.ToPath UpdateGatewayInformation where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateGatewayInformation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateGatewayInformationResponse' smart constructor.
data UpdateGatewayInformationResponse = UpdateGatewayInformationResponse'
  { -- | The Amazon Resource Name (ARN) of the gateway you updated.
    gatewayArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGatewayInformationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayArn', 'updateGatewayInformationResponse_gatewayArn' - The Amazon Resource Name (ARN) of the gateway you updated.
--
-- 'httpStatus', 'updateGatewayInformationResponse_httpStatus' - The response's http status code.
newUpdateGatewayInformationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateGatewayInformationResponse
newUpdateGatewayInformationResponse pHttpStatus_ =
  UpdateGatewayInformationResponse'
    { gatewayArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the gateway you updated.
updateGatewayInformationResponse_gatewayArn :: Lens.Lens' UpdateGatewayInformationResponse (Prelude.Maybe Prelude.Text)
updateGatewayInformationResponse_gatewayArn = Lens.lens (\UpdateGatewayInformationResponse' {gatewayArn} -> gatewayArn) (\s@UpdateGatewayInformationResponse' {} a -> s {gatewayArn = a} :: UpdateGatewayInformationResponse)

-- | The response's http status code.
updateGatewayInformationResponse_httpStatus :: Lens.Lens' UpdateGatewayInformationResponse Prelude.Int
updateGatewayInformationResponse_httpStatus = Lens.lens (\UpdateGatewayInformationResponse' {httpStatus} -> httpStatus) (\s@UpdateGatewayInformationResponse' {} a -> s {httpStatus = a} :: UpdateGatewayInformationResponse)

instance
  Prelude.NFData
    UpdateGatewayInformationResponse
  where
  rnf UpdateGatewayInformationResponse' {..} =
    Prelude.rnf gatewayArn
      `Prelude.seq` Prelude.rnf httpStatus
