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
-- Module      : Amazonka.PrivateNetworks.GetNetwork
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the specified network.
module Amazonka.PrivateNetworks.GetNetwork
  ( -- * Creating a Request
    GetNetwork (..),
    newGetNetwork,

    -- * Request Lenses
    getNetwork_networkArn,

    -- * Destructuring the Response
    GetNetworkResponse (..),
    newGetNetworkResponse,

    -- * Response Lenses
    getNetworkResponse_tags,
    getNetworkResponse_httpStatus,
    getNetworkResponse_network,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.PrivateNetworks.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetNetwork' smart constructor.
data GetNetwork = GetNetwork'
  { -- | The Amazon Resource Name (ARN) of the network.
    networkArn :: Prelude.Text
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
-- 'networkArn', 'getNetwork_networkArn' - The Amazon Resource Name (ARN) of the network.
newGetNetwork ::
  -- | 'networkArn'
  Prelude.Text ->
  GetNetwork
newGetNetwork pNetworkArn_ =
  GetNetwork' {networkArn = pNetworkArn_}

-- | The Amazon Resource Name (ARN) of the network.
getNetwork_networkArn :: Lens.Lens' GetNetwork Prelude.Text
getNetwork_networkArn = Lens.lens (\GetNetwork' {networkArn} -> networkArn) (\s@GetNetwork' {} a -> s {networkArn = a} :: GetNetwork)

instance Core.AWSRequest GetNetwork where
  type AWSResponse GetNetwork = GetNetworkResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetNetworkResponse'
            Prelude.<$> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "network")
      )

instance Prelude.Hashable GetNetwork where
  hashWithSalt _salt GetNetwork' {..} =
    _salt `Prelude.hashWithSalt` networkArn

instance Prelude.NFData GetNetwork where
  rnf GetNetwork' {..} = Prelude.rnf networkArn

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
    Prelude.mconcat
      ["/v1/networks/", Data.toBS networkArn]

instance Data.ToQuery GetNetwork where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetNetworkResponse' smart constructor.
data GetNetworkResponse = GetNetworkResponse'
  { -- | The network tags.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the network.
    network :: Network
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNetworkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'getNetworkResponse_tags' - The network tags.
--
-- 'httpStatus', 'getNetworkResponse_httpStatus' - The response's http status code.
--
-- 'network', 'getNetworkResponse_network' - Information about the network.
newGetNetworkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'network'
  Network ->
  GetNetworkResponse
newGetNetworkResponse pHttpStatus_ pNetwork_ =
  GetNetworkResponse'
    { tags = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      network = pNetwork_
    }

-- | The network tags.
getNetworkResponse_tags :: Lens.Lens' GetNetworkResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getNetworkResponse_tags = Lens.lens (\GetNetworkResponse' {tags} -> tags) (\s@GetNetworkResponse' {} a -> s {tags = a} :: GetNetworkResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The response's http status code.
getNetworkResponse_httpStatus :: Lens.Lens' GetNetworkResponse Prelude.Int
getNetworkResponse_httpStatus = Lens.lens (\GetNetworkResponse' {httpStatus} -> httpStatus) (\s@GetNetworkResponse' {} a -> s {httpStatus = a} :: GetNetworkResponse)

-- | Information about the network.
getNetworkResponse_network :: Lens.Lens' GetNetworkResponse Network
getNetworkResponse_network = Lens.lens (\GetNetworkResponse' {network} -> network) (\s@GetNetworkResponse' {} a -> s {network = a} :: GetNetworkResponse)

instance Prelude.NFData GetNetworkResponse where
  rnf GetNetworkResponse' {..} =
    Prelude.rnf tags `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf network
