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
-- Module      : Amazonka.SESV2.GetDedicatedIpPool
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve information about the dedicated pool.
module Amazonka.SESV2.GetDedicatedIpPool
  ( -- * Creating a Request
    GetDedicatedIpPool (..),
    newGetDedicatedIpPool,

    -- * Request Lenses
    getDedicatedIpPool_poolName,

    -- * Destructuring the Response
    GetDedicatedIpPoolResponse (..),
    newGetDedicatedIpPoolResponse,

    -- * Response Lenses
    getDedicatedIpPoolResponse_dedicatedIpPool,
    getDedicatedIpPoolResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | A request to obtain more information about a dedicated IP pool.
--
-- /See:/ 'newGetDedicatedIpPool' smart constructor.
data GetDedicatedIpPool = GetDedicatedIpPool'
  { -- | The name of the dedicated IP pool to retrieve.
    poolName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDedicatedIpPool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'poolName', 'getDedicatedIpPool_poolName' - The name of the dedicated IP pool to retrieve.
newGetDedicatedIpPool ::
  -- | 'poolName'
  Prelude.Text ->
  GetDedicatedIpPool
newGetDedicatedIpPool pPoolName_ =
  GetDedicatedIpPool' {poolName = pPoolName_}

-- | The name of the dedicated IP pool to retrieve.
getDedicatedIpPool_poolName :: Lens.Lens' GetDedicatedIpPool Prelude.Text
getDedicatedIpPool_poolName = Lens.lens (\GetDedicatedIpPool' {poolName} -> poolName) (\s@GetDedicatedIpPool' {} a -> s {poolName = a} :: GetDedicatedIpPool)

instance Core.AWSRequest GetDedicatedIpPool where
  type
    AWSResponse GetDedicatedIpPool =
      GetDedicatedIpPoolResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDedicatedIpPoolResponse'
            Prelude.<$> (x Data..?> "DedicatedIpPool")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDedicatedIpPool where
  hashWithSalt _salt GetDedicatedIpPool' {..} =
    _salt `Prelude.hashWithSalt` poolName

instance Prelude.NFData GetDedicatedIpPool where
  rnf GetDedicatedIpPool' {..} = Prelude.rnf poolName

instance Data.ToHeaders GetDedicatedIpPool where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetDedicatedIpPool where
  toPath GetDedicatedIpPool' {..} =
    Prelude.mconcat
      ["/v2/email/dedicated-ip-pools/", Data.toBS poolName]

instance Data.ToQuery GetDedicatedIpPool where
  toQuery = Prelude.const Prelude.mempty

-- | The following element is returned by the service.
--
-- /See:/ 'newGetDedicatedIpPoolResponse' smart constructor.
data GetDedicatedIpPoolResponse = GetDedicatedIpPoolResponse'
  { -- | An object that contains information about a dedicated IP pool.
    dedicatedIpPool :: Prelude.Maybe DedicatedIpPool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDedicatedIpPoolResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dedicatedIpPool', 'getDedicatedIpPoolResponse_dedicatedIpPool' - An object that contains information about a dedicated IP pool.
--
-- 'httpStatus', 'getDedicatedIpPoolResponse_httpStatus' - The response's http status code.
newGetDedicatedIpPoolResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDedicatedIpPoolResponse
newGetDedicatedIpPoolResponse pHttpStatus_ =
  GetDedicatedIpPoolResponse'
    { dedicatedIpPool =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains information about a dedicated IP pool.
getDedicatedIpPoolResponse_dedicatedIpPool :: Lens.Lens' GetDedicatedIpPoolResponse (Prelude.Maybe DedicatedIpPool)
getDedicatedIpPoolResponse_dedicatedIpPool = Lens.lens (\GetDedicatedIpPoolResponse' {dedicatedIpPool} -> dedicatedIpPool) (\s@GetDedicatedIpPoolResponse' {} a -> s {dedicatedIpPool = a} :: GetDedicatedIpPoolResponse)

-- | The response's http status code.
getDedicatedIpPoolResponse_httpStatus :: Lens.Lens' GetDedicatedIpPoolResponse Prelude.Int
getDedicatedIpPoolResponse_httpStatus = Lens.lens (\GetDedicatedIpPoolResponse' {httpStatus} -> httpStatus) (\s@GetDedicatedIpPoolResponse' {} a -> s {httpStatus = a} :: GetDedicatedIpPoolResponse)

instance Prelude.NFData GetDedicatedIpPoolResponse where
  rnf GetDedicatedIpPoolResponse' {..} =
    Prelude.rnf dedicatedIpPool
      `Prelude.seq` Prelude.rnf httpStatus
