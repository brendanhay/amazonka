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
-- Module      : Amazonka.BackupGateway.StartVirtualMachinesMetadataSync
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action sends a request to sync metadata across the specified
-- virtual machines.
module Amazonka.BackupGateway.StartVirtualMachinesMetadataSync
  ( -- * Creating a Request
    StartVirtualMachinesMetadataSync (..),
    newStartVirtualMachinesMetadataSync,

    -- * Request Lenses
    startVirtualMachinesMetadataSync_hypervisorArn,

    -- * Destructuring the Response
    StartVirtualMachinesMetadataSyncResponse (..),
    newStartVirtualMachinesMetadataSyncResponse,

    -- * Response Lenses
    startVirtualMachinesMetadataSyncResponse_hypervisorArn,
    startVirtualMachinesMetadataSyncResponse_httpStatus,
  )
where

import Amazonka.BackupGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartVirtualMachinesMetadataSync' smart constructor.
data StartVirtualMachinesMetadataSync = StartVirtualMachinesMetadataSync'
  { -- | The Amazon Resource Name (ARN) of the hypervisor.
    hypervisorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartVirtualMachinesMetadataSync' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hypervisorArn', 'startVirtualMachinesMetadataSync_hypervisorArn' - The Amazon Resource Name (ARN) of the hypervisor.
newStartVirtualMachinesMetadataSync ::
  -- | 'hypervisorArn'
  Prelude.Text ->
  StartVirtualMachinesMetadataSync
newStartVirtualMachinesMetadataSync pHypervisorArn_ =
  StartVirtualMachinesMetadataSync'
    { hypervisorArn =
        pHypervisorArn_
    }

-- | The Amazon Resource Name (ARN) of the hypervisor.
startVirtualMachinesMetadataSync_hypervisorArn :: Lens.Lens' StartVirtualMachinesMetadataSync Prelude.Text
startVirtualMachinesMetadataSync_hypervisorArn = Lens.lens (\StartVirtualMachinesMetadataSync' {hypervisorArn} -> hypervisorArn) (\s@StartVirtualMachinesMetadataSync' {} a -> s {hypervisorArn = a} :: StartVirtualMachinesMetadataSync)

instance
  Core.AWSRequest
    StartVirtualMachinesMetadataSync
  where
  type
    AWSResponse StartVirtualMachinesMetadataSync =
      StartVirtualMachinesMetadataSyncResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartVirtualMachinesMetadataSyncResponse'
            Prelude.<$> (x Data..?> "HypervisorArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartVirtualMachinesMetadataSync
  where
  hashWithSalt
    _salt
    StartVirtualMachinesMetadataSync' {..} =
      _salt `Prelude.hashWithSalt` hypervisorArn

instance
  Prelude.NFData
    StartVirtualMachinesMetadataSync
  where
  rnf StartVirtualMachinesMetadataSync' {..} =
    Prelude.rnf hypervisorArn

instance
  Data.ToHeaders
    StartVirtualMachinesMetadataSync
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "BackupOnPremises_v20210101.StartVirtualMachinesMetadataSync" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartVirtualMachinesMetadataSync where
  toJSON StartVirtualMachinesMetadataSync' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("HypervisorArn" Data..= hypervisorArn)
          ]
      )

instance Data.ToPath StartVirtualMachinesMetadataSync where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    StartVirtualMachinesMetadataSync
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartVirtualMachinesMetadataSyncResponse' smart constructor.
data StartVirtualMachinesMetadataSyncResponse = StartVirtualMachinesMetadataSyncResponse'
  { -- | The Amazon Resource Name (ARN) of the hypervisor.
    hypervisorArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartVirtualMachinesMetadataSyncResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hypervisorArn', 'startVirtualMachinesMetadataSyncResponse_hypervisorArn' - The Amazon Resource Name (ARN) of the hypervisor.
--
-- 'httpStatus', 'startVirtualMachinesMetadataSyncResponse_httpStatus' - The response's http status code.
newStartVirtualMachinesMetadataSyncResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartVirtualMachinesMetadataSyncResponse
newStartVirtualMachinesMetadataSyncResponse
  pHttpStatus_ =
    StartVirtualMachinesMetadataSyncResponse'
      { hypervisorArn =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The Amazon Resource Name (ARN) of the hypervisor.
startVirtualMachinesMetadataSyncResponse_hypervisorArn :: Lens.Lens' StartVirtualMachinesMetadataSyncResponse (Prelude.Maybe Prelude.Text)
startVirtualMachinesMetadataSyncResponse_hypervisorArn = Lens.lens (\StartVirtualMachinesMetadataSyncResponse' {hypervisorArn} -> hypervisorArn) (\s@StartVirtualMachinesMetadataSyncResponse' {} a -> s {hypervisorArn = a} :: StartVirtualMachinesMetadataSyncResponse)

-- | The response's http status code.
startVirtualMachinesMetadataSyncResponse_httpStatus :: Lens.Lens' StartVirtualMachinesMetadataSyncResponse Prelude.Int
startVirtualMachinesMetadataSyncResponse_httpStatus = Lens.lens (\StartVirtualMachinesMetadataSyncResponse' {httpStatus} -> httpStatus) (\s@StartVirtualMachinesMetadataSyncResponse' {} a -> s {httpStatus = a} :: StartVirtualMachinesMetadataSyncResponse)

instance
  Prelude.NFData
    StartVirtualMachinesMetadataSyncResponse
  where
  rnf StartVirtualMachinesMetadataSyncResponse' {..} =
    Prelude.rnf hypervisorArn `Prelude.seq`
      Prelude.rnf httpStatus
