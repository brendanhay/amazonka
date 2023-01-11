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
-- Module      : Amazonka.BackupGateway.GetHypervisor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action requests information about the specified hypervisor to which
-- the gateway will connect. A hypervisor is hardware, software, or
-- firmware that creates and manages virtual machines, and allocates
-- resources to them.
module Amazonka.BackupGateway.GetHypervisor
  ( -- * Creating a Request
    GetHypervisor (..),
    newGetHypervisor,

    -- * Request Lenses
    getHypervisor_hypervisorArn,

    -- * Destructuring the Response
    GetHypervisorResponse (..),
    newGetHypervisorResponse,

    -- * Response Lenses
    getHypervisorResponse_hypervisor,
    getHypervisorResponse_httpStatus,
  )
where

import Amazonka.BackupGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetHypervisor' smart constructor.
data GetHypervisor = GetHypervisor'
  { -- | The Amazon Resource Name (ARN) of the hypervisor.
    hypervisorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetHypervisor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hypervisorArn', 'getHypervisor_hypervisorArn' - The Amazon Resource Name (ARN) of the hypervisor.
newGetHypervisor ::
  -- | 'hypervisorArn'
  Prelude.Text ->
  GetHypervisor
newGetHypervisor pHypervisorArn_ =
  GetHypervisor' {hypervisorArn = pHypervisorArn_}

-- | The Amazon Resource Name (ARN) of the hypervisor.
getHypervisor_hypervisorArn :: Lens.Lens' GetHypervisor Prelude.Text
getHypervisor_hypervisorArn = Lens.lens (\GetHypervisor' {hypervisorArn} -> hypervisorArn) (\s@GetHypervisor' {} a -> s {hypervisorArn = a} :: GetHypervisor)

instance Core.AWSRequest GetHypervisor where
  type
    AWSResponse GetHypervisor =
      GetHypervisorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetHypervisorResponse'
            Prelude.<$> (x Data..?> "Hypervisor")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetHypervisor where
  hashWithSalt _salt GetHypervisor' {..} =
    _salt `Prelude.hashWithSalt` hypervisorArn

instance Prelude.NFData GetHypervisor where
  rnf GetHypervisor' {..} = Prelude.rnf hypervisorArn

instance Data.ToHeaders GetHypervisor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "BackupOnPremises_v20210101.GetHypervisor" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetHypervisor where
  toJSON GetHypervisor' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("HypervisorArn" Data..= hypervisorArn)
          ]
      )

instance Data.ToPath GetHypervisor where
  toPath = Prelude.const "/"

instance Data.ToQuery GetHypervisor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetHypervisorResponse' smart constructor.
data GetHypervisorResponse = GetHypervisorResponse'
  { -- | Details about the requested hypervisor.
    hypervisor :: Prelude.Maybe HypervisorDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetHypervisorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hypervisor', 'getHypervisorResponse_hypervisor' - Details about the requested hypervisor.
--
-- 'httpStatus', 'getHypervisorResponse_httpStatus' - The response's http status code.
newGetHypervisorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetHypervisorResponse
newGetHypervisorResponse pHttpStatus_ =
  GetHypervisorResponse'
    { hypervisor =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about the requested hypervisor.
getHypervisorResponse_hypervisor :: Lens.Lens' GetHypervisorResponse (Prelude.Maybe HypervisorDetails)
getHypervisorResponse_hypervisor = Lens.lens (\GetHypervisorResponse' {hypervisor} -> hypervisor) (\s@GetHypervisorResponse' {} a -> s {hypervisor = a} :: GetHypervisorResponse)

-- | The response's http status code.
getHypervisorResponse_httpStatus :: Lens.Lens' GetHypervisorResponse Prelude.Int
getHypervisorResponse_httpStatus = Lens.lens (\GetHypervisorResponse' {httpStatus} -> httpStatus) (\s@GetHypervisorResponse' {} a -> s {httpStatus = a} :: GetHypervisorResponse)

instance Prelude.NFData GetHypervisorResponse where
  rnf GetHypervisorResponse' {..} =
    Prelude.rnf hypervisor
      `Prelude.seq` Prelude.rnf httpStatus
