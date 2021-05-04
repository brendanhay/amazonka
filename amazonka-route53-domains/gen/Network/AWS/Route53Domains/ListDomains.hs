{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Route53Domains.ListDomains
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns all the domain names registered with Amazon Route
-- 53 for the current AWS account.
--
-- This operation returns paginated results.
module Network.AWS.Route53Domains.ListDomains
  ( -- * Creating a Request
    ListDomains (..),
    newListDomains,

    -- * Request Lenses
    listDomains_maxItems,
    listDomains_marker,

    -- * Destructuring the Response
    ListDomainsResponse (..),
    newListDomainsResponse,

    -- * Response Lenses
    listDomainsResponse_nextPageMarker,
    listDomainsResponse_httpStatus,
    listDomainsResponse_domains,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53Domains.Types

-- | The ListDomains request includes the following elements.
--
-- /See:/ 'newListDomains' smart constructor.
data ListDomains = ListDomains'
  { -- | Number of domains to be returned.
    --
    -- Default: 20
    maxItems :: Prelude.Maybe Prelude.Int,
    -- | For an initial request for a list of domains, omit this element. If the
    -- number of domains that are associated with the current AWS account is
    -- greater than the value that you specified for @MaxItems@, you can use
    -- @Marker@ to return additional domains. Get the value of @NextPageMarker@
    -- from the previous response, and submit another request that includes the
    -- value of @NextPageMarker@ in the @Marker@ element.
    --
    -- Constraints: The marker must match the value specified in the previous
    -- request.
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListDomains' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxItems', 'listDomains_maxItems' - Number of domains to be returned.
--
-- Default: 20
--
-- 'marker', 'listDomains_marker' - For an initial request for a list of domains, omit this element. If the
-- number of domains that are associated with the current AWS account is
-- greater than the value that you specified for @MaxItems@, you can use
-- @Marker@ to return additional domains. Get the value of @NextPageMarker@
-- from the previous response, and submit another request that includes the
-- value of @NextPageMarker@ in the @Marker@ element.
--
-- Constraints: The marker must match the value specified in the previous
-- request.
newListDomains ::
  ListDomains
newListDomains =
  ListDomains'
    { maxItems = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | Number of domains to be returned.
--
-- Default: 20
listDomains_maxItems :: Lens.Lens' ListDomains (Prelude.Maybe Prelude.Int)
listDomains_maxItems = Lens.lens (\ListDomains' {maxItems} -> maxItems) (\s@ListDomains' {} a -> s {maxItems = a} :: ListDomains)

-- | For an initial request for a list of domains, omit this element. If the
-- number of domains that are associated with the current AWS account is
-- greater than the value that you specified for @MaxItems@, you can use
-- @Marker@ to return additional domains. Get the value of @NextPageMarker@
-- from the previous response, and submit another request that includes the
-- value of @NextPageMarker@ in the @Marker@ element.
--
-- Constraints: The marker must match the value specified in the previous
-- request.
listDomains_marker :: Lens.Lens' ListDomains (Prelude.Maybe Prelude.Text)
listDomains_marker = Lens.lens (\ListDomains' {marker} -> marker) (\s@ListDomains' {} a -> s {marker = a} :: ListDomains)

instance Pager.AWSPager ListDomains where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listDomainsResponse_nextPageMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop (rs Lens.^. listDomainsResponse_domains) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listDomains_marker
          Lens..~ rs
          Lens.^? listDomainsResponse_nextPageMarker
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListDomains where
  type Rs ListDomains = ListDomainsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDomainsResponse'
            Prelude.<$> (x Prelude..?> "NextPageMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..?> "Domains" Prelude..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListDomains

instance Prelude.NFData ListDomains

instance Prelude.ToHeaders ListDomains where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Route53Domains_v20140515.ListDomains" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListDomains where
  toJSON ListDomains' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("MaxItems" Prelude..=) Prelude.<$> maxItems,
            ("Marker" Prelude..=) Prelude.<$> marker
          ]
      )

instance Prelude.ToPath ListDomains where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListDomains where
  toQuery = Prelude.const Prelude.mempty

-- | The ListDomains response includes the following elements.
--
-- /See:/ 'newListDomainsResponse' smart constructor.
data ListDomainsResponse = ListDomainsResponse'
  { -- | If there are more domains than you specified for @MaxItems@ in the
    -- request, submit another request and include the value of
    -- @NextPageMarker@ in the value of @Marker@.
    nextPageMarker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A summary of domains.
    domains :: [DomainSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListDomainsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageMarker', 'listDomainsResponse_nextPageMarker' - If there are more domains than you specified for @MaxItems@ in the
-- request, submit another request and include the value of
-- @NextPageMarker@ in the value of @Marker@.
--
-- 'httpStatus', 'listDomainsResponse_httpStatus' - The response's http status code.
--
-- 'domains', 'listDomainsResponse_domains' - A summary of domains.
newListDomainsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDomainsResponse
newListDomainsResponse pHttpStatus_ =
  ListDomainsResponse'
    { nextPageMarker =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      domains = Prelude.mempty
    }

-- | If there are more domains than you specified for @MaxItems@ in the
-- request, submit another request and include the value of
-- @NextPageMarker@ in the value of @Marker@.
listDomainsResponse_nextPageMarker :: Lens.Lens' ListDomainsResponse (Prelude.Maybe Prelude.Text)
listDomainsResponse_nextPageMarker = Lens.lens (\ListDomainsResponse' {nextPageMarker} -> nextPageMarker) (\s@ListDomainsResponse' {} a -> s {nextPageMarker = a} :: ListDomainsResponse)

-- | The response's http status code.
listDomainsResponse_httpStatus :: Lens.Lens' ListDomainsResponse Prelude.Int
listDomainsResponse_httpStatus = Lens.lens (\ListDomainsResponse' {httpStatus} -> httpStatus) (\s@ListDomainsResponse' {} a -> s {httpStatus = a} :: ListDomainsResponse)

-- | A summary of domains.
listDomainsResponse_domains :: Lens.Lens' ListDomainsResponse [DomainSummary]
listDomainsResponse_domains = Lens.lens (\ListDomainsResponse' {domains} -> domains) (\s@ListDomainsResponse' {} a -> s {domains = a} :: ListDomainsResponse) Prelude.. Prelude._Coerce

instance Prelude.NFData ListDomainsResponse
