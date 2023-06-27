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
-- Module      : Amazonka.Kafka.DeleteVpcConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a MSK VPC connection.
module Amazonka.Kafka.DeleteVpcConnection
  ( -- * Creating a Request
    DeleteVpcConnection (..),
    newDeleteVpcConnection,

    -- * Request Lenses
    deleteVpcConnection_arn,

    -- * Destructuring the Response
    DeleteVpcConnectionResponse (..),
    newDeleteVpcConnectionResponse,

    -- * Response Lenses
    deleteVpcConnectionResponse_state,
    deleteVpcConnectionResponse_vpcConnectionArn,
    deleteVpcConnectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVpcConnection' smart constructor.
data DeleteVpcConnection = DeleteVpcConnection'
  { -- | The Amazon Resource Name (ARN) that uniquely identifies an MSK VPC
    -- connection.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVpcConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteVpcConnection_arn' - The Amazon Resource Name (ARN) that uniquely identifies an MSK VPC
-- connection.
newDeleteVpcConnection ::
  -- | 'arn'
  Prelude.Text ->
  DeleteVpcConnection
newDeleteVpcConnection pArn_ =
  DeleteVpcConnection' {arn = pArn_}

-- | The Amazon Resource Name (ARN) that uniquely identifies an MSK VPC
-- connection.
deleteVpcConnection_arn :: Lens.Lens' DeleteVpcConnection Prelude.Text
deleteVpcConnection_arn = Lens.lens (\DeleteVpcConnection' {arn} -> arn) (\s@DeleteVpcConnection' {} a -> s {arn = a} :: DeleteVpcConnection)

instance Core.AWSRequest DeleteVpcConnection where
  type
    AWSResponse DeleteVpcConnection =
      DeleteVpcConnectionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteVpcConnectionResponse'
            Prelude.<$> (x Data..?> "state")
            Prelude.<*> (x Data..?> "vpcConnectionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteVpcConnection where
  hashWithSalt _salt DeleteVpcConnection' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData DeleteVpcConnection where
  rnf DeleteVpcConnection' {..} = Prelude.rnf arn

instance Data.ToHeaders DeleteVpcConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteVpcConnection where
  toPath DeleteVpcConnection' {..} =
    Prelude.mconcat
      ["/v1/vpc-connection/", Data.toBS arn]

instance Data.ToQuery DeleteVpcConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVpcConnectionResponse' smart constructor.
data DeleteVpcConnectionResponse = DeleteVpcConnectionResponse'
  { -- | The state of the VPC connection.
    state :: Prelude.Maybe VpcConnectionState,
    -- | The Amazon Resource Name (ARN) that uniquely identifies an MSK VPC
    -- connection.
    vpcConnectionArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVpcConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'deleteVpcConnectionResponse_state' - The state of the VPC connection.
--
-- 'vpcConnectionArn', 'deleteVpcConnectionResponse_vpcConnectionArn' - The Amazon Resource Name (ARN) that uniquely identifies an MSK VPC
-- connection.
--
-- 'httpStatus', 'deleteVpcConnectionResponse_httpStatus' - The response's http status code.
newDeleteVpcConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteVpcConnectionResponse
newDeleteVpcConnectionResponse pHttpStatus_ =
  DeleteVpcConnectionResponse'
    { state =
        Prelude.Nothing,
      vpcConnectionArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The state of the VPC connection.
deleteVpcConnectionResponse_state :: Lens.Lens' DeleteVpcConnectionResponse (Prelude.Maybe VpcConnectionState)
deleteVpcConnectionResponse_state = Lens.lens (\DeleteVpcConnectionResponse' {state} -> state) (\s@DeleteVpcConnectionResponse' {} a -> s {state = a} :: DeleteVpcConnectionResponse)

-- | The Amazon Resource Name (ARN) that uniquely identifies an MSK VPC
-- connection.
deleteVpcConnectionResponse_vpcConnectionArn :: Lens.Lens' DeleteVpcConnectionResponse (Prelude.Maybe Prelude.Text)
deleteVpcConnectionResponse_vpcConnectionArn = Lens.lens (\DeleteVpcConnectionResponse' {vpcConnectionArn} -> vpcConnectionArn) (\s@DeleteVpcConnectionResponse' {} a -> s {vpcConnectionArn = a} :: DeleteVpcConnectionResponse)

-- | The response's http status code.
deleteVpcConnectionResponse_httpStatus :: Lens.Lens' DeleteVpcConnectionResponse Prelude.Int
deleteVpcConnectionResponse_httpStatus = Lens.lens (\DeleteVpcConnectionResponse' {httpStatus} -> httpStatus) (\s@DeleteVpcConnectionResponse' {} a -> s {httpStatus = a} :: DeleteVpcConnectionResponse)

instance Prelude.NFData DeleteVpcConnectionResponse where
  rnf DeleteVpcConnectionResponse' {..} =
    Prelude.rnf state
      `Prelude.seq` Prelude.rnf vpcConnectionArn
      `Prelude.seq` Prelude.rnf httpStatus
