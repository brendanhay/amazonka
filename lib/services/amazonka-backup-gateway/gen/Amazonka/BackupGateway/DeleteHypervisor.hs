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
-- Module      : Amazonka.BackupGateway.DeleteHypervisor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a hypervisor.
module Amazonka.BackupGateway.DeleteHypervisor
  ( -- * Creating a Request
    DeleteHypervisor (..),
    newDeleteHypervisor,

    -- * Request Lenses
    deleteHypervisor_hypervisorArn,

    -- * Destructuring the Response
    DeleteHypervisorResponse (..),
    newDeleteHypervisorResponse,

    -- * Response Lenses
    deleteHypervisorResponse_hypervisorArn,
    deleteHypervisorResponse_httpStatus,
  )
where

import Amazonka.BackupGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteHypervisor' smart constructor.
data DeleteHypervisor = DeleteHypervisor'
  { -- | The Amazon Resource Name (ARN) of the hypervisor to delete.
    hypervisorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteHypervisor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hypervisorArn', 'deleteHypervisor_hypervisorArn' - The Amazon Resource Name (ARN) of the hypervisor to delete.
newDeleteHypervisor ::
  -- | 'hypervisorArn'
  Prelude.Text ->
  DeleteHypervisor
newDeleteHypervisor pHypervisorArn_ =
  DeleteHypervisor' {hypervisorArn = pHypervisorArn_}

-- | The Amazon Resource Name (ARN) of the hypervisor to delete.
deleteHypervisor_hypervisorArn :: Lens.Lens' DeleteHypervisor Prelude.Text
deleteHypervisor_hypervisorArn = Lens.lens (\DeleteHypervisor' {hypervisorArn} -> hypervisorArn) (\s@DeleteHypervisor' {} a -> s {hypervisorArn = a} :: DeleteHypervisor)

instance Core.AWSRequest DeleteHypervisor where
  type
    AWSResponse DeleteHypervisor =
      DeleteHypervisorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteHypervisorResponse'
            Prelude.<$> (x Data..?> "HypervisorArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteHypervisor where
  hashWithSalt _salt DeleteHypervisor' {..} =
    _salt `Prelude.hashWithSalt` hypervisorArn

instance Prelude.NFData DeleteHypervisor where
  rnf DeleteHypervisor' {..} = Prelude.rnf hypervisorArn

instance Data.ToHeaders DeleteHypervisor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "BackupOnPremises_v20210101.DeleteHypervisor" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteHypervisor where
  toJSON DeleteHypervisor' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("HypervisorArn" Data..= hypervisorArn)
          ]
      )

instance Data.ToPath DeleteHypervisor where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteHypervisor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteHypervisorResponse' smart constructor.
data DeleteHypervisorResponse = DeleteHypervisorResponse'
  { -- | The Amazon Resource Name (ARN) of the hypervisor you deleted.
    hypervisorArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteHypervisorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hypervisorArn', 'deleteHypervisorResponse_hypervisorArn' - The Amazon Resource Name (ARN) of the hypervisor you deleted.
--
-- 'httpStatus', 'deleteHypervisorResponse_httpStatus' - The response's http status code.
newDeleteHypervisorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteHypervisorResponse
newDeleteHypervisorResponse pHttpStatus_ =
  DeleteHypervisorResponse'
    { hypervisorArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the hypervisor you deleted.
deleteHypervisorResponse_hypervisorArn :: Lens.Lens' DeleteHypervisorResponse (Prelude.Maybe Prelude.Text)
deleteHypervisorResponse_hypervisorArn = Lens.lens (\DeleteHypervisorResponse' {hypervisorArn} -> hypervisorArn) (\s@DeleteHypervisorResponse' {} a -> s {hypervisorArn = a} :: DeleteHypervisorResponse)

-- | The response's http status code.
deleteHypervisorResponse_httpStatus :: Lens.Lens' DeleteHypervisorResponse Prelude.Int
deleteHypervisorResponse_httpStatus = Lens.lens (\DeleteHypervisorResponse' {httpStatus} -> httpStatus) (\s@DeleteHypervisorResponse' {} a -> s {httpStatus = a} :: DeleteHypervisorResponse)

instance Prelude.NFData DeleteHypervisorResponse where
  rnf DeleteHypervisorResponse' {..} =
    Prelude.rnf hypervisorArn
      `Prelude.seq` Prelude.rnf httpStatus
