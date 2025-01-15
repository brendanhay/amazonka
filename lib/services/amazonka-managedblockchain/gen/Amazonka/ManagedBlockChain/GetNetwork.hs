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
-- Module      : Amazonka.ManagedBlockChain.GetNetwork
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed information about a network.
--
-- Applies to Hyperledger Fabric and Ethereum.
module Amazonka.ManagedBlockChain.GetNetwork
  ( -- * Creating a Request
    GetNetwork (..),
    newGetNetwork,

    -- * Request Lenses
    getNetwork_networkId,

    -- * Destructuring the Response
    GetNetworkResponse (..),
    newGetNetworkResponse,

    -- * Response Lenses
    getNetworkResponse_network,
    getNetworkResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ManagedBlockChain.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetNetwork' smart constructor.
data GetNetwork = GetNetwork'
  { -- | The unique identifier of the network to get information about.
    networkId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNetwork' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkId', 'getNetwork_networkId' - The unique identifier of the network to get information about.
newGetNetwork ::
  -- | 'networkId'
  Prelude.Text ->
  GetNetwork
newGetNetwork pNetworkId_ =
  GetNetwork' {networkId = pNetworkId_}

-- | The unique identifier of the network to get information about.
getNetwork_networkId :: Lens.Lens' GetNetwork Prelude.Text
getNetwork_networkId = Lens.lens (\GetNetwork' {networkId} -> networkId) (\s@GetNetwork' {} a -> s {networkId = a} :: GetNetwork)

instance Core.AWSRequest GetNetwork where
  type AWSResponse GetNetwork = GetNetworkResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetNetworkResponse'
            Prelude.<$> (x Data..?> "Network")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetNetwork where
  hashWithSalt _salt GetNetwork' {..} =
    _salt `Prelude.hashWithSalt` networkId

instance Prelude.NFData GetNetwork where
  rnf GetNetwork' {..} = Prelude.rnf networkId

instance Data.ToHeaders GetNetwork where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetNetwork where
  toPath GetNetwork' {..} =
    Prelude.mconcat ["/networks/", Data.toBS networkId]

instance Data.ToQuery GetNetwork where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetNetworkResponse' smart constructor.
data GetNetworkResponse = GetNetworkResponse'
  { -- | An object containing network configuration parameters.
    network :: Prelude.Maybe Network,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNetworkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'network', 'getNetworkResponse_network' - An object containing network configuration parameters.
--
-- 'httpStatus', 'getNetworkResponse_httpStatus' - The response's http status code.
newGetNetworkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetNetworkResponse
newGetNetworkResponse pHttpStatus_ =
  GetNetworkResponse'
    { network = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object containing network configuration parameters.
getNetworkResponse_network :: Lens.Lens' GetNetworkResponse (Prelude.Maybe Network)
getNetworkResponse_network = Lens.lens (\GetNetworkResponse' {network} -> network) (\s@GetNetworkResponse' {} a -> s {network = a} :: GetNetworkResponse)

-- | The response's http status code.
getNetworkResponse_httpStatus :: Lens.Lens' GetNetworkResponse Prelude.Int
getNetworkResponse_httpStatus = Lens.lens (\GetNetworkResponse' {httpStatus} -> httpStatus) (\s@GetNetworkResponse' {} a -> s {httpStatus = a} :: GetNetworkResponse)

instance Prelude.NFData GetNetworkResponse where
  rnf GetNetworkResponse' {..} =
    Prelude.rnf network `Prelude.seq`
      Prelude.rnf httpStatus
