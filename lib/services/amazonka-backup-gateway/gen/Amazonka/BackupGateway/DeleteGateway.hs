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
-- Module      : Amazonka.BackupGateway.DeleteGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a backup gateway.
module Amazonka.BackupGateway.DeleteGateway
  ( -- * Creating a Request
    DeleteGateway (..),
    newDeleteGateway,

    -- * Request Lenses
    deleteGateway_gatewayArn,

    -- * Destructuring the Response
    DeleteGatewayResponse (..),
    newDeleteGatewayResponse,

    -- * Response Lenses
    deleteGatewayResponse_gatewayArn,
    deleteGatewayResponse_httpStatus,
  )
where

import Amazonka.BackupGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteGateway' smart constructor.
data DeleteGateway = DeleteGateway'
  { -- | The Amazon Resource Name (ARN) of the gateway to delete.
    gatewayArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayArn', 'deleteGateway_gatewayArn' - The Amazon Resource Name (ARN) of the gateway to delete.
newDeleteGateway ::
  -- | 'gatewayArn'
  Prelude.Text ->
  DeleteGateway
newDeleteGateway pGatewayArn_ =
  DeleteGateway' {gatewayArn = pGatewayArn_}

-- | The Amazon Resource Name (ARN) of the gateway to delete.
deleteGateway_gatewayArn :: Lens.Lens' DeleteGateway Prelude.Text
deleteGateway_gatewayArn = Lens.lens (\DeleteGateway' {gatewayArn} -> gatewayArn) (\s@DeleteGateway' {} a -> s {gatewayArn = a} :: DeleteGateway)

instance Core.AWSRequest DeleteGateway where
  type
    AWSResponse DeleteGateway =
      DeleteGatewayResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteGatewayResponse'
            Prelude.<$> (x Data..?> "GatewayArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteGateway where
  hashWithSalt _salt DeleteGateway' {..} =
    _salt `Prelude.hashWithSalt` gatewayArn

instance Prelude.NFData DeleteGateway where
  rnf DeleteGateway' {..} = Prelude.rnf gatewayArn

instance Data.ToHeaders DeleteGateway where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "BackupOnPremises_v20210101.DeleteGateway" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteGateway where
  toJSON DeleteGateway' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("GatewayArn" Data..= gatewayArn)]
      )

instance Data.ToPath DeleteGateway where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteGateway where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteGatewayResponse' smart constructor.
data DeleteGatewayResponse = DeleteGatewayResponse'
  { -- | The Amazon Resource Name (ARN) of the gateway you deleted.
    gatewayArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayArn', 'deleteGatewayResponse_gatewayArn' - The Amazon Resource Name (ARN) of the gateway you deleted.
--
-- 'httpStatus', 'deleteGatewayResponse_httpStatus' - The response's http status code.
newDeleteGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteGatewayResponse
newDeleteGatewayResponse pHttpStatus_ =
  DeleteGatewayResponse'
    { gatewayArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the gateway you deleted.
deleteGatewayResponse_gatewayArn :: Lens.Lens' DeleteGatewayResponse (Prelude.Maybe Prelude.Text)
deleteGatewayResponse_gatewayArn = Lens.lens (\DeleteGatewayResponse' {gatewayArn} -> gatewayArn) (\s@DeleteGatewayResponse' {} a -> s {gatewayArn = a} :: DeleteGatewayResponse)

-- | The response's http status code.
deleteGatewayResponse_httpStatus :: Lens.Lens' DeleteGatewayResponse Prelude.Int
deleteGatewayResponse_httpStatus = Lens.lens (\DeleteGatewayResponse' {httpStatus} -> httpStatus) (\s@DeleteGatewayResponse' {} a -> s {httpStatus = a} :: DeleteGatewayResponse)

instance Prelude.NFData DeleteGatewayResponse where
  rnf DeleteGatewayResponse' {..} =
    Prelude.rnf gatewayArn `Prelude.seq`
      Prelude.rnf httpStatus
