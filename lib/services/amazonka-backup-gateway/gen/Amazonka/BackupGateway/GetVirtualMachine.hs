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
-- Module      : Amazonka.BackupGateway.GetVirtualMachine
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- By providing the ARN (Amazon Resource Name), this API returns the
-- virtual machine.
module Amazonka.BackupGateway.GetVirtualMachine
  ( -- * Creating a Request
    GetVirtualMachine (..),
    newGetVirtualMachine,

    -- * Request Lenses
    getVirtualMachine_resourceArn,

    -- * Destructuring the Response
    GetVirtualMachineResponse (..),
    newGetVirtualMachineResponse,

    -- * Response Lenses
    getVirtualMachineResponse_virtualMachine,
    getVirtualMachineResponse_httpStatus,
  )
where

import Amazonka.BackupGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetVirtualMachine' smart constructor.
data GetVirtualMachine = GetVirtualMachine'
  { -- | The Amazon Resource Name (ARN) of the virtual machine.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVirtualMachine' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'getVirtualMachine_resourceArn' - The Amazon Resource Name (ARN) of the virtual machine.
newGetVirtualMachine ::
  -- | 'resourceArn'
  Prelude.Text ->
  GetVirtualMachine
newGetVirtualMachine pResourceArn_ =
  GetVirtualMachine' {resourceArn = pResourceArn_}

-- | The Amazon Resource Name (ARN) of the virtual machine.
getVirtualMachine_resourceArn :: Lens.Lens' GetVirtualMachine Prelude.Text
getVirtualMachine_resourceArn = Lens.lens (\GetVirtualMachine' {resourceArn} -> resourceArn) (\s@GetVirtualMachine' {} a -> s {resourceArn = a} :: GetVirtualMachine)

instance Core.AWSRequest GetVirtualMachine where
  type
    AWSResponse GetVirtualMachine =
      GetVirtualMachineResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetVirtualMachineResponse'
            Prelude.<$> (x Data..?> "VirtualMachine")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetVirtualMachine where
  hashWithSalt _salt GetVirtualMachine' {..} =
    _salt `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData GetVirtualMachine where
  rnf GetVirtualMachine' {..} = Prelude.rnf resourceArn

instance Data.ToHeaders GetVirtualMachine where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "BackupOnPremises_v20210101.GetVirtualMachine" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetVirtualMachine where
  toJSON GetVirtualMachine' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ResourceArn" Data..= resourceArn)]
      )

instance Data.ToPath GetVirtualMachine where
  toPath = Prelude.const "/"

instance Data.ToQuery GetVirtualMachine where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetVirtualMachineResponse' smart constructor.
data GetVirtualMachineResponse = GetVirtualMachineResponse'
  { -- | This object contains the basic attributes of @VirtualMachine@ contained
    -- by the output of @GetVirtualMachine@
    virtualMachine :: Prelude.Maybe VirtualMachineDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVirtualMachineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'virtualMachine', 'getVirtualMachineResponse_virtualMachine' - This object contains the basic attributes of @VirtualMachine@ contained
-- by the output of @GetVirtualMachine@
--
-- 'httpStatus', 'getVirtualMachineResponse_httpStatus' - The response's http status code.
newGetVirtualMachineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetVirtualMachineResponse
newGetVirtualMachineResponse pHttpStatus_ =
  GetVirtualMachineResponse'
    { virtualMachine =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | This object contains the basic attributes of @VirtualMachine@ contained
-- by the output of @GetVirtualMachine@
getVirtualMachineResponse_virtualMachine :: Lens.Lens' GetVirtualMachineResponse (Prelude.Maybe VirtualMachineDetails)
getVirtualMachineResponse_virtualMachine = Lens.lens (\GetVirtualMachineResponse' {virtualMachine} -> virtualMachine) (\s@GetVirtualMachineResponse' {} a -> s {virtualMachine = a} :: GetVirtualMachineResponse)

-- | The response's http status code.
getVirtualMachineResponse_httpStatus :: Lens.Lens' GetVirtualMachineResponse Prelude.Int
getVirtualMachineResponse_httpStatus = Lens.lens (\GetVirtualMachineResponse' {httpStatus} -> httpStatus) (\s@GetVirtualMachineResponse' {} a -> s {httpStatus = a} :: GetVirtualMachineResponse)

instance Prelude.NFData GetVirtualMachineResponse where
  rnf GetVirtualMachineResponse' {..} =
    Prelude.rnf virtualMachine
      `Prelude.seq` Prelude.rnf httpStatus
