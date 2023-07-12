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
-- Module      : Amazonka.PrivateNetworks.GetNetworkResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the specified network resource.
module Amazonka.PrivateNetworks.GetNetworkResource
  ( -- * Creating a Request
    GetNetworkResource (..),
    newGetNetworkResource,

    -- * Request Lenses
    getNetworkResource_networkResourceArn,

    -- * Destructuring the Response
    GetNetworkResourceResponse (..),
    newGetNetworkResourceResponse,

    -- * Response Lenses
    getNetworkResourceResponse_tags,
    getNetworkResourceResponse_httpStatus,
    getNetworkResourceResponse_networkResource,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.PrivateNetworks.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetNetworkResource' smart constructor.
data GetNetworkResource = GetNetworkResource'
  { -- | The Amazon Resource Name (ARN) of the network resource.
    networkResourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNetworkResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkResourceArn', 'getNetworkResource_networkResourceArn' - The Amazon Resource Name (ARN) of the network resource.
newGetNetworkResource ::
  -- | 'networkResourceArn'
  Prelude.Text ->
  GetNetworkResource
newGetNetworkResource pNetworkResourceArn_ =
  GetNetworkResource'
    { networkResourceArn =
        pNetworkResourceArn_
    }

-- | The Amazon Resource Name (ARN) of the network resource.
getNetworkResource_networkResourceArn :: Lens.Lens' GetNetworkResource Prelude.Text
getNetworkResource_networkResourceArn = Lens.lens (\GetNetworkResource' {networkResourceArn} -> networkResourceArn) (\s@GetNetworkResource' {} a -> s {networkResourceArn = a} :: GetNetworkResource)

instance Core.AWSRequest GetNetworkResource where
  type
    AWSResponse GetNetworkResource =
      GetNetworkResourceResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetNetworkResourceResponse'
            Prelude.<$> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "networkResource")
      )

instance Prelude.Hashable GetNetworkResource where
  hashWithSalt _salt GetNetworkResource' {..} =
    _salt `Prelude.hashWithSalt` networkResourceArn

instance Prelude.NFData GetNetworkResource where
  rnf GetNetworkResource' {..} =
    Prelude.rnf networkResourceArn

instance Data.ToHeaders GetNetworkResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetNetworkResource where
  toPath GetNetworkResource' {..} =
    Prelude.mconcat
      [ "/v1/network-resources/",
        Data.toBS networkResourceArn
      ]

instance Data.ToQuery GetNetworkResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetNetworkResourceResponse' smart constructor.
data GetNetworkResourceResponse = GetNetworkResourceResponse'
  { -- | The network resource tags.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the network resource.
    networkResource :: NetworkResource
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNetworkResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'getNetworkResourceResponse_tags' - The network resource tags.
--
-- 'httpStatus', 'getNetworkResourceResponse_httpStatus' - The response's http status code.
--
-- 'networkResource', 'getNetworkResourceResponse_networkResource' - Information about the network resource.
newGetNetworkResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'networkResource'
  NetworkResource ->
  GetNetworkResourceResponse
newGetNetworkResourceResponse
  pHttpStatus_
  pNetworkResource_ =
    GetNetworkResourceResponse'
      { tags = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        networkResource = pNetworkResource_
      }

-- | The network resource tags.
getNetworkResourceResponse_tags :: Lens.Lens' GetNetworkResourceResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getNetworkResourceResponse_tags = Lens.lens (\GetNetworkResourceResponse' {tags} -> tags) (\s@GetNetworkResourceResponse' {} a -> s {tags = a} :: GetNetworkResourceResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The response's http status code.
getNetworkResourceResponse_httpStatus :: Lens.Lens' GetNetworkResourceResponse Prelude.Int
getNetworkResourceResponse_httpStatus = Lens.lens (\GetNetworkResourceResponse' {httpStatus} -> httpStatus) (\s@GetNetworkResourceResponse' {} a -> s {httpStatus = a} :: GetNetworkResourceResponse)

-- | Information about the network resource.
getNetworkResourceResponse_networkResource :: Lens.Lens' GetNetworkResourceResponse NetworkResource
getNetworkResourceResponse_networkResource = Lens.lens (\GetNetworkResourceResponse' {networkResource} -> networkResource) (\s@GetNetworkResourceResponse' {} a -> s {networkResource = a} :: GetNetworkResourceResponse)

instance Prelude.NFData GetNetworkResourceResponse where
  rnf GetNetworkResourceResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf networkResource
