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
-- Module      : Network.AWS.ElasticSearch.DescribeElasticsearchDomains
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns domain configuration information about the specified
-- Elasticsearch domains, including the domain ID, domain endpoint, and
-- domain ARN.
module Network.AWS.ElasticSearch.DescribeElasticsearchDomains
  ( -- * Creating a Request
    DescribeElasticsearchDomains (..),
    newDescribeElasticsearchDomains,

    -- * Request Lenses
    describeElasticsearchDomains_domainNames,

    -- * Destructuring the Response
    DescribeElasticsearchDomainsResponse (..),
    newDescribeElasticsearchDomainsResponse,

    -- * Response Lenses
    describeElasticsearchDomainsResponse_httpStatus,
    describeElasticsearchDomainsResponse_domainStatusList,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @DescribeElasticsearchDomains@
-- operation. By default, the API returns the status of all Elasticsearch
-- domains.
--
-- /See:/ 'newDescribeElasticsearchDomains' smart constructor.
data DescribeElasticsearchDomains = DescribeElasticsearchDomains'
  { -- | The Elasticsearch domains for which you want information.
    domainNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeElasticsearchDomains' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainNames', 'describeElasticsearchDomains_domainNames' - The Elasticsearch domains for which you want information.
newDescribeElasticsearchDomains ::
  DescribeElasticsearchDomains
newDescribeElasticsearchDomains =
  DescribeElasticsearchDomains'
    { domainNames =
        Prelude.mempty
    }

-- | The Elasticsearch domains for which you want information.
describeElasticsearchDomains_domainNames :: Lens.Lens' DescribeElasticsearchDomains [Prelude.Text]
describeElasticsearchDomains_domainNames = Lens.lens (\DescribeElasticsearchDomains' {domainNames} -> domainNames) (\s@DescribeElasticsearchDomains' {} a -> s {domainNames = a} :: DescribeElasticsearchDomains) Prelude.. Lens._Coerce

instance Core.AWSRequest DescribeElasticsearchDomains where
  type
    AWSResponse DescribeElasticsearchDomains =
      DescribeElasticsearchDomainsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeElasticsearchDomainsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "DomainStatusList"
                            Core..!@ Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    DescribeElasticsearchDomains

instance Prelude.NFData DescribeElasticsearchDomains

instance Core.ToHeaders DescribeElasticsearchDomains where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON DescribeElasticsearchDomains where
  toJSON DescribeElasticsearchDomains' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("DomainNames" Core..= domainNames)]
      )

instance Core.ToPath DescribeElasticsearchDomains where
  toPath = Prelude.const "/2015-01-01/es/domain-info"

instance Core.ToQuery DescribeElasticsearchDomains where
  toQuery = Prelude.const Prelude.mempty

-- | The result of a @DescribeElasticsearchDomains@ request. Contains the
-- status of the specified domains or all domains owned by the account.
--
-- /See:/ 'newDescribeElasticsearchDomainsResponse' smart constructor.
data DescribeElasticsearchDomainsResponse = DescribeElasticsearchDomainsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The status of the domains requested in the
    -- @DescribeElasticsearchDomains@ request.
    domainStatusList :: [ElasticsearchDomainStatus]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeElasticsearchDomainsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeElasticsearchDomainsResponse_httpStatus' - The response's http status code.
--
-- 'domainStatusList', 'describeElasticsearchDomainsResponse_domainStatusList' - The status of the domains requested in the
-- @DescribeElasticsearchDomains@ request.
newDescribeElasticsearchDomainsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeElasticsearchDomainsResponse
newDescribeElasticsearchDomainsResponse pHttpStatus_ =
  DescribeElasticsearchDomainsResponse'
    { httpStatus =
        pHttpStatus_,
      domainStatusList = Prelude.mempty
    }

-- | The response's http status code.
describeElasticsearchDomainsResponse_httpStatus :: Lens.Lens' DescribeElasticsearchDomainsResponse Prelude.Int
describeElasticsearchDomainsResponse_httpStatus = Lens.lens (\DescribeElasticsearchDomainsResponse' {httpStatus} -> httpStatus) (\s@DescribeElasticsearchDomainsResponse' {} a -> s {httpStatus = a} :: DescribeElasticsearchDomainsResponse)

-- | The status of the domains requested in the
-- @DescribeElasticsearchDomains@ request.
describeElasticsearchDomainsResponse_domainStatusList :: Lens.Lens' DescribeElasticsearchDomainsResponse [ElasticsearchDomainStatus]
describeElasticsearchDomainsResponse_domainStatusList = Lens.lens (\DescribeElasticsearchDomainsResponse' {domainStatusList} -> domainStatusList) (\s@DescribeElasticsearchDomainsResponse' {} a -> s {domainStatusList = a} :: DescribeElasticsearchDomainsResponse) Prelude.. Lens._Coerce

instance
  Prelude.NFData
    DescribeElasticsearchDomainsResponse
