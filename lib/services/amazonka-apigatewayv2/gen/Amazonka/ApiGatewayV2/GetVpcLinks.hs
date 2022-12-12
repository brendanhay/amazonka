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
-- Module      : Amazonka.ApiGatewayV2.GetVpcLinks
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a collection of VPC links.
module Amazonka.ApiGatewayV2.GetVpcLinks
  ( -- * Creating a Request
    GetVpcLinks (..),
    newGetVpcLinks,

    -- * Request Lenses
    getVpcLinks_maxResults,
    getVpcLinks_nextToken,

    -- * Destructuring the Response
    GetVpcLinksResponse (..),
    newGetVpcLinksResponse,

    -- * Response Lenses
    getVpcLinksResponse_items,
    getVpcLinksResponse_nextToken,
    getVpcLinksResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetVpcLinks' smart constructor.
data GetVpcLinks = GetVpcLinks'
  { -- | The maximum number of elements to be returned for this resource.
    maxResults :: Prelude.Maybe Prelude.Text,
    -- | The next page of elements from this collection. Not valid for the last
    -- element of the collection.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVpcLinks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getVpcLinks_maxResults' - The maximum number of elements to be returned for this resource.
--
-- 'nextToken', 'getVpcLinks_nextToken' - The next page of elements from this collection. Not valid for the last
-- element of the collection.
newGetVpcLinks ::
  GetVpcLinks
newGetVpcLinks =
  GetVpcLinks'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of elements to be returned for this resource.
getVpcLinks_maxResults :: Lens.Lens' GetVpcLinks (Prelude.Maybe Prelude.Text)
getVpcLinks_maxResults = Lens.lens (\GetVpcLinks' {maxResults} -> maxResults) (\s@GetVpcLinks' {} a -> s {maxResults = a} :: GetVpcLinks)

-- | The next page of elements from this collection. Not valid for the last
-- element of the collection.
getVpcLinks_nextToken :: Lens.Lens' GetVpcLinks (Prelude.Maybe Prelude.Text)
getVpcLinks_nextToken = Lens.lens (\GetVpcLinks' {nextToken} -> nextToken) (\s@GetVpcLinks' {} a -> s {nextToken = a} :: GetVpcLinks)

instance Core.AWSRequest GetVpcLinks where
  type AWSResponse GetVpcLinks = GetVpcLinksResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetVpcLinksResponse'
            Prelude.<$> (x Data..?> "items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetVpcLinks where
  hashWithSalt _salt GetVpcLinks' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData GetVpcLinks where
  rnf GetVpcLinks' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders GetVpcLinks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetVpcLinks where
  toPath = Prelude.const "/v2/vpclinks"

instance Data.ToQuery GetVpcLinks where
  toQuery GetVpcLinks' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newGetVpcLinksResponse' smart constructor.
data GetVpcLinksResponse = GetVpcLinksResponse'
  { -- | A collection of VPC links.
    items :: Prelude.Maybe [VpcLink],
    -- | The next page of elements from this collection. Not valid for the last
    -- element of the collection.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVpcLinksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'getVpcLinksResponse_items' - A collection of VPC links.
--
-- 'nextToken', 'getVpcLinksResponse_nextToken' - The next page of elements from this collection. Not valid for the last
-- element of the collection.
--
-- 'httpStatus', 'getVpcLinksResponse_httpStatus' - The response's http status code.
newGetVpcLinksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetVpcLinksResponse
newGetVpcLinksResponse pHttpStatus_ =
  GetVpcLinksResponse'
    { items = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of VPC links.
getVpcLinksResponse_items :: Lens.Lens' GetVpcLinksResponse (Prelude.Maybe [VpcLink])
getVpcLinksResponse_items = Lens.lens (\GetVpcLinksResponse' {items} -> items) (\s@GetVpcLinksResponse' {} a -> s {items = a} :: GetVpcLinksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next page of elements from this collection. Not valid for the last
-- element of the collection.
getVpcLinksResponse_nextToken :: Lens.Lens' GetVpcLinksResponse (Prelude.Maybe Prelude.Text)
getVpcLinksResponse_nextToken = Lens.lens (\GetVpcLinksResponse' {nextToken} -> nextToken) (\s@GetVpcLinksResponse' {} a -> s {nextToken = a} :: GetVpcLinksResponse)

-- | The response's http status code.
getVpcLinksResponse_httpStatus :: Lens.Lens' GetVpcLinksResponse Prelude.Int
getVpcLinksResponse_httpStatus = Lens.lens (\GetVpcLinksResponse' {httpStatus} -> httpStatus) (\s@GetVpcLinksResponse' {} a -> s {httpStatus = a} :: GetVpcLinksResponse)

instance Prelude.NFData GetVpcLinksResponse where
  rnf GetVpcLinksResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
