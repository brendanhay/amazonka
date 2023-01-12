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
-- Module      : Amazonka.PrivateNetworks.GetNetworkSite
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the specified network site.
module Amazonka.PrivateNetworks.GetNetworkSite
  ( -- * Creating a Request
    GetNetworkSite (..),
    newGetNetworkSite,

    -- * Request Lenses
    getNetworkSite_networkSiteArn,

    -- * Destructuring the Response
    GetNetworkSiteResponse (..),
    newGetNetworkSiteResponse,

    -- * Response Lenses
    getNetworkSiteResponse_networkSite,
    getNetworkSiteResponse_tags,
    getNetworkSiteResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.PrivateNetworks.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetNetworkSite' smart constructor.
data GetNetworkSite = GetNetworkSite'
  { -- | The Amazon Resource Name (ARN) of the network site.
    networkSiteArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNetworkSite' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkSiteArn', 'getNetworkSite_networkSiteArn' - The Amazon Resource Name (ARN) of the network site.
newGetNetworkSite ::
  -- | 'networkSiteArn'
  Prelude.Text ->
  GetNetworkSite
newGetNetworkSite pNetworkSiteArn_ =
  GetNetworkSite' {networkSiteArn = pNetworkSiteArn_}

-- | The Amazon Resource Name (ARN) of the network site.
getNetworkSite_networkSiteArn :: Lens.Lens' GetNetworkSite Prelude.Text
getNetworkSite_networkSiteArn = Lens.lens (\GetNetworkSite' {networkSiteArn} -> networkSiteArn) (\s@GetNetworkSite' {} a -> s {networkSiteArn = a} :: GetNetworkSite)

instance Core.AWSRequest GetNetworkSite where
  type
    AWSResponse GetNetworkSite =
      GetNetworkSiteResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetNetworkSiteResponse'
            Prelude.<$> (x Data..?> "networkSite")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetNetworkSite where
  hashWithSalt _salt GetNetworkSite' {..} =
    _salt `Prelude.hashWithSalt` networkSiteArn

instance Prelude.NFData GetNetworkSite where
  rnf GetNetworkSite' {..} = Prelude.rnf networkSiteArn

instance Data.ToHeaders GetNetworkSite where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetNetworkSite where
  toPath GetNetworkSite' {..} =
    Prelude.mconcat
      ["/v1/network-sites/", Data.toBS networkSiteArn]

instance Data.ToQuery GetNetworkSite where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetNetworkSiteResponse' smart constructor.
data GetNetworkSiteResponse = GetNetworkSiteResponse'
  { -- | Information about the network site.
    networkSite :: Prelude.Maybe NetworkSite,
    -- | The network site tags.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNetworkSiteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkSite', 'getNetworkSiteResponse_networkSite' - Information about the network site.
--
-- 'tags', 'getNetworkSiteResponse_tags' - The network site tags.
--
-- 'httpStatus', 'getNetworkSiteResponse_httpStatus' - The response's http status code.
newGetNetworkSiteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetNetworkSiteResponse
newGetNetworkSiteResponse pHttpStatus_ =
  GetNetworkSiteResponse'
    { networkSite =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the network site.
getNetworkSiteResponse_networkSite :: Lens.Lens' GetNetworkSiteResponse (Prelude.Maybe NetworkSite)
getNetworkSiteResponse_networkSite = Lens.lens (\GetNetworkSiteResponse' {networkSite} -> networkSite) (\s@GetNetworkSiteResponse' {} a -> s {networkSite = a} :: GetNetworkSiteResponse)

-- | The network site tags.
getNetworkSiteResponse_tags :: Lens.Lens' GetNetworkSiteResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getNetworkSiteResponse_tags = Lens.lens (\GetNetworkSiteResponse' {tags} -> tags) (\s@GetNetworkSiteResponse' {} a -> s {tags = a} :: GetNetworkSiteResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The response's http status code.
getNetworkSiteResponse_httpStatus :: Lens.Lens' GetNetworkSiteResponse Prelude.Int
getNetworkSiteResponse_httpStatus = Lens.lens (\GetNetworkSiteResponse' {httpStatus} -> httpStatus) (\s@GetNetworkSiteResponse' {} a -> s {httpStatus = a} :: GetNetworkSiteResponse)

instance Prelude.NFData GetNetworkSiteResponse where
  rnf GetNetworkSiteResponse' {..} =
    Prelude.rnf networkSite
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
