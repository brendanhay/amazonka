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
-- Module      : Network.AWS.SDB.ListDomains
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListDomains@ operation lists all domains associated with the Access
-- Key ID. It returns domain names up to the limit set by
-- <#MaxNumberOfDomains MaxNumberOfDomains>. A <#NextToken NextToken> is
-- returned if there are more than @MaxNumberOfDomains@ domains. Calling
-- @ListDomains@ successive times with the @NextToken@ provided by the
-- operation returns up to @MaxNumberOfDomains@ more domain names with each
-- successive operation call.
--
-- This operation returns paginated results.
module Network.AWS.SDB.ListDomains
  ( -- * Creating a Request
    ListDomains (..),
    newListDomains,

    -- * Request Lenses
    listDomains_nextToken,
    listDomains_maxNumberOfDomains,

    -- * Destructuring the Response
    ListDomainsResponse (..),
    newListDomainsResponse,

    -- * Response Lenses
    listDomainsResponse_nextToken,
    listDomainsResponse_domainNames,
    listDomainsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SDB.Types

-- | /See:/ 'newListDomains' smart constructor.
data ListDomains = ListDomains'
  { -- | A string informing Amazon SimpleDB where to start the next list of
    -- domain names.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of domain names you want returned. The range is 1 to
    -- 100. The default setting is 100.
    maxNumberOfDomains :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDomains' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDomains_nextToken' - A string informing Amazon SimpleDB where to start the next list of
-- domain names.
--
-- 'maxNumberOfDomains', 'listDomains_maxNumberOfDomains' - The maximum number of domain names you want returned. The range is 1 to
-- 100. The default setting is 100.
newListDomains ::
  ListDomains
newListDomains =
  ListDomains'
    { nextToken = Core.Nothing,
      maxNumberOfDomains = Core.Nothing
    }

-- | A string informing Amazon SimpleDB where to start the next list of
-- domain names.
listDomains_nextToken :: Lens.Lens' ListDomains (Core.Maybe Core.Text)
listDomains_nextToken = Lens.lens (\ListDomains' {nextToken} -> nextToken) (\s@ListDomains' {} a -> s {nextToken = a} :: ListDomains)

-- | The maximum number of domain names you want returned. The range is 1 to
-- 100. The default setting is 100.
listDomains_maxNumberOfDomains :: Lens.Lens' ListDomains (Core.Maybe Core.Int)
listDomains_maxNumberOfDomains = Lens.lens (\ListDomains' {maxNumberOfDomains} -> maxNumberOfDomains) (\s@ListDomains' {} a -> s {maxNumberOfDomains = a} :: ListDomains)

instance Core.AWSPager ListDomains where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDomainsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listDomainsResponse_domainNames Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listDomains_nextToken
          Lens..~ rs
          Lens.^? listDomainsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListDomains where
  type AWSResponse ListDomains = ListDomainsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListDomainsResult"
      ( \s h x ->
          ListDomainsResponse'
            Core.<$> (x Core..@? "NextToken")
            Core.<*> (Core.may (Core.parseXMLList "DomainName") x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListDomains

instance Core.NFData ListDomains

instance Core.ToHeaders ListDomains where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListDomains where
  toPath = Core.const "/"

instance Core.ToQuery ListDomains where
  toQuery ListDomains' {..} =
    Core.mconcat
      [ "Action" Core.=: ("ListDomains" :: Core.ByteString),
        "Version" Core.=: ("2009-04-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "MaxNumberOfDomains" Core.=: maxNumberOfDomains
      ]

-- | /See:/ 'newListDomainsResponse' smart constructor.
data ListDomainsResponse = ListDomainsResponse'
  { -- | An opaque token indicating that there are more domains than the
    -- specified @MaxNumberOfDomains@ still available.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of domain names that match the expression.
    domainNames :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDomainsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDomainsResponse_nextToken' - An opaque token indicating that there are more domains than the
-- specified @MaxNumberOfDomains@ still available.
--
-- 'domainNames', 'listDomainsResponse_domainNames' - A list of domain names that match the expression.
--
-- 'httpStatus', 'listDomainsResponse_httpStatus' - The response's http status code.
newListDomainsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListDomainsResponse
newListDomainsResponse pHttpStatus_ =
  ListDomainsResponse'
    { nextToken = Core.Nothing,
      domainNames = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An opaque token indicating that there are more domains than the
-- specified @MaxNumberOfDomains@ still available.
listDomainsResponse_nextToken :: Lens.Lens' ListDomainsResponse (Core.Maybe Core.Text)
listDomainsResponse_nextToken = Lens.lens (\ListDomainsResponse' {nextToken} -> nextToken) (\s@ListDomainsResponse' {} a -> s {nextToken = a} :: ListDomainsResponse)

-- | A list of domain names that match the expression.
listDomainsResponse_domainNames :: Lens.Lens' ListDomainsResponse (Core.Maybe [Core.Text])
listDomainsResponse_domainNames = Lens.lens (\ListDomainsResponse' {domainNames} -> domainNames) (\s@ListDomainsResponse' {} a -> s {domainNames = a} :: ListDomainsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listDomainsResponse_httpStatus :: Lens.Lens' ListDomainsResponse Core.Int
listDomainsResponse_httpStatus = Lens.lens (\ListDomainsResponse' {httpStatus} -> httpStatus) (\s@ListDomainsResponse' {} a -> s {httpStatus = a} :: ListDomainsResponse)

instance Core.NFData ListDomainsResponse
