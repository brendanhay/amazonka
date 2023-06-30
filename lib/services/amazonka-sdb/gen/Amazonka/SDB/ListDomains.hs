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
-- Module      : Amazonka.SDB.ListDomains
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.SDB.ListDomains
  ( -- * Creating a Request
    ListDomains (..),
    newListDomains,

    -- * Request Lenses
    listDomains_maxNumberOfDomains,
    listDomains_nextToken,

    -- * Destructuring the Response
    ListDomainsResponse (..),
    newListDomainsResponse,

    -- * Response Lenses
    listDomainsResponse_domainNames,
    listDomainsResponse_nextToken,
    listDomainsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SDB.Types

-- | /See:/ 'newListDomains' smart constructor.
data ListDomains = ListDomains'
  { -- | The maximum number of domain names you want returned. The range is 1 to
    -- 100. The default setting is 100.
    maxNumberOfDomains :: Prelude.Maybe Prelude.Int,
    -- | A string informing Amazon SimpleDB where to start the next list of
    -- domain names.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDomains' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxNumberOfDomains', 'listDomains_maxNumberOfDomains' - The maximum number of domain names you want returned. The range is 1 to
-- 100. The default setting is 100.
--
-- 'nextToken', 'listDomains_nextToken' - A string informing Amazon SimpleDB where to start the next list of
-- domain names.
newListDomains ::
  ListDomains
newListDomains =
  ListDomains'
    { maxNumberOfDomains = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of domain names you want returned. The range is 1 to
-- 100. The default setting is 100.
listDomains_maxNumberOfDomains :: Lens.Lens' ListDomains (Prelude.Maybe Prelude.Int)
listDomains_maxNumberOfDomains = Lens.lens (\ListDomains' {maxNumberOfDomains} -> maxNumberOfDomains) (\s@ListDomains' {} a -> s {maxNumberOfDomains = a} :: ListDomains)

-- | A string informing Amazon SimpleDB where to start the next list of
-- domain names.
listDomains_nextToken :: Lens.Lens' ListDomains (Prelude.Maybe Prelude.Text)
listDomains_nextToken = Lens.lens (\ListDomains' {nextToken} -> nextToken) (\s@ListDomains' {} a -> s {nextToken = a} :: ListDomains)

instance Core.AWSPager ListDomains where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDomainsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDomainsResponse_domainNames
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listDomains_nextToken
          Lens..~ rs
          Lens.^? listDomainsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListDomains where
  type AWSResponse ListDomains = ListDomainsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListDomainsResult"
      ( \s h x ->
          ListDomainsResponse'
            Prelude.<$> (Core.may (Data.parseXMLList "DomainName") x)
            Prelude.<*> (x Data..@? "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDomains where
  hashWithSalt _salt ListDomains' {..} =
    _salt
      `Prelude.hashWithSalt` maxNumberOfDomains
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListDomains where
  rnf ListDomains' {..} =
    Prelude.rnf maxNumberOfDomains
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListDomains where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListDomains where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDomains where
  toQuery ListDomains' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListDomains" :: Prelude.ByteString),
        "Version"
          Data.=: ("2009-04-15" :: Prelude.ByteString),
        "MaxNumberOfDomains" Data.=: maxNumberOfDomains,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListDomainsResponse' smart constructor.
data ListDomainsResponse = ListDomainsResponse'
  { -- | A list of domain names that match the expression.
    domainNames :: Prelude.Maybe [Prelude.Text],
    -- | An opaque token indicating that there are more domains than the
    -- specified @MaxNumberOfDomains@ still available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDomainsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainNames', 'listDomainsResponse_domainNames' - A list of domain names that match the expression.
--
-- 'nextToken', 'listDomainsResponse_nextToken' - An opaque token indicating that there are more domains than the
-- specified @MaxNumberOfDomains@ still available.
--
-- 'httpStatus', 'listDomainsResponse_httpStatus' - The response's http status code.
newListDomainsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDomainsResponse
newListDomainsResponse pHttpStatus_ =
  ListDomainsResponse'
    { domainNames = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of domain names that match the expression.
listDomainsResponse_domainNames :: Lens.Lens' ListDomainsResponse (Prelude.Maybe [Prelude.Text])
listDomainsResponse_domainNames = Lens.lens (\ListDomainsResponse' {domainNames} -> domainNames) (\s@ListDomainsResponse' {} a -> s {domainNames = a} :: ListDomainsResponse) Prelude.. Lens.mapping Lens.coerced

-- | An opaque token indicating that there are more domains than the
-- specified @MaxNumberOfDomains@ still available.
listDomainsResponse_nextToken :: Lens.Lens' ListDomainsResponse (Prelude.Maybe Prelude.Text)
listDomainsResponse_nextToken = Lens.lens (\ListDomainsResponse' {nextToken} -> nextToken) (\s@ListDomainsResponse' {} a -> s {nextToken = a} :: ListDomainsResponse)

-- | The response's http status code.
listDomainsResponse_httpStatus :: Lens.Lens' ListDomainsResponse Prelude.Int
listDomainsResponse_httpStatus = Lens.lens (\ListDomainsResponse' {httpStatus} -> httpStatus) (\s@ListDomainsResponse' {} a -> s {httpStatus = a} :: ListDomainsResponse)

instance Prelude.NFData ListDomainsResponse where
  rnf ListDomainsResponse' {..} =
    Prelude.rnf domainNames
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
