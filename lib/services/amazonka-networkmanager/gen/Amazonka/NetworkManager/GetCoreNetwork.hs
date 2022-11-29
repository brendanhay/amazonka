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
-- Module      : Amazonka.NetworkManager.GetCoreNetwork
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the LIVE policy for a core network.
module Amazonka.NetworkManager.GetCoreNetwork
  ( -- * Creating a Request
    GetCoreNetwork (..),
    newGetCoreNetwork,

    -- * Request Lenses
    getCoreNetwork_coreNetworkId,

    -- * Destructuring the Response
    GetCoreNetworkResponse (..),
    newGetCoreNetworkResponse,

    -- * Response Lenses
    getCoreNetworkResponse_coreNetwork,
    getCoreNetworkResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCoreNetwork' smart constructor.
data GetCoreNetwork = GetCoreNetwork'
  { -- | The ID of a core network.
    coreNetworkId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCoreNetwork' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreNetworkId', 'getCoreNetwork_coreNetworkId' - The ID of a core network.
newGetCoreNetwork ::
  -- | 'coreNetworkId'
  Prelude.Text ->
  GetCoreNetwork
newGetCoreNetwork pCoreNetworkId_ =
  GetCoreNetwork' {coreNetworkId = pCoreNetworkId_}

-- | The ID of a core network.
getCoreNetwork_coreNetworkId :: Lens.Lens' GetCoreNetwork Prelude.Text
getCoreNetwork_coreNetworkId = Lens.lens (\GetCoreNetwork' {coreNetworkId} -> coreNetworkId) (\s@GetCoreNetwork' {} a -> s {coreNetworkId = a} :: GetCoreNetwork)

instance Core.AWSRequest GetCoreNetwork where
  type
    AWSResponse GetCoreNetwork =
      GetCoreNetworkResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCoreNetworkResponse'
            Prelude.<$> (x Core..?> "CoreNetwork")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCoreNetwork where
  hashWithSalt _salt GetCoreNetwork' {..} =
    _salt `Prelude.hashWithSalt` coreNetworkId

instance Prelude.NFData GetCoreNetwork where
  rnf GetCoreNetwork' {..} = Prelude.rnf coreNetworkId

instance Core.ToHeaders GetCoreNetwork where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetCoreNetwork where
  toPath GetCoreNetwork' {..} =
    Prelude.mconcat
      ["/core-networks/", Core.toBS coreNetworkId]

instance Core.ToQuery GetCoreNetwork where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCoreNetworkResponse' smart constructor.
data GetCoreNetworkResponse = GetCoreNetworkResponse'
  { -- | Details about a core network.
    coreNetwork :: Prelude.Maybe CoreNetwork,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCoreNetworkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreNetwork', 'getCoreNetworkResponse_coreNetwork' - Details about a core network.
--
-- 'httpStatus', 'getCoreNetworkResponse_httpStatus' - The response's http status code.
newGetCoreNetworkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCoreNetworkResponse
newGetCoreNetworkResponse pHttpStatus_ =
  GetCoreNetworkResponse'
    { coreNetwork =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about a core network.
getCoreNetworkResponse_coreNetwork :: Lens.Lens' GetCoreNetworkResponse (Prelude.Maybe CoreNetwork)
getCoreNetworkResponse_coreNetwork = Lens.lens (\GetCoreNetworkResponse' {coreNetwork} -> coreNetwork) (\s@GetCoreNetworkResponse' {} a -> s {coreNetwork = a} :: GetCoreNetworkResponse)

-- | The response's http status code.
getCoreNetworkResponse_httpStatus :: Lens.Lens' GetCoreNetworkResponse Prelude.Int
getCoreNetworkResponse_httpStatus = Lens.lens (\GetCoreNetworkResponse' {httpStatus} -> httpStatus) (\s@GetCoreNetworkResponse' {} a -> s {httpStatus = a} :: GetCoreNetworkResponse)

instance Prelude.NFData GetCoreNetworkResponse where
  rnf GetCoreNetworkResponse' {..} =
    Prelude.rnf coreNetwork
      `Prelude.seq` Prelude.rnf httpStatus
