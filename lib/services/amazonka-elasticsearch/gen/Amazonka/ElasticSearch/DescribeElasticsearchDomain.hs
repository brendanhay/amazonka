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
-- Module      : Amazonka.ElasticSearch.DescribeElasticsearchDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns domain configuration information about the specified
-- Elasticsearch domain, including the domain ID, domain endpoint, and
-- domain ARN.
module Amazonka.ElasticSearch.DescribeElasticsearchDomain
  ( -- * Creating a Request
    DescribeElasticsearchDomain (..),
    newDescribeElasticsearchDomain,

    -- * Request Lenses
    describeElasticsearchDomain_domainName,

    -- * Destructuring the Response
    DescribeElasticsearchDomainResponse (..),
    newDescribeElasticsearchDomainResponse,

    -- * Response Lenses
    describeElasticsearchDomainResponse_httpStatus,
    describeElasticsearchDomainResponse_domainStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @DescribeElasticsearchDomain@
-- operation.
--
-- /See:/ 'newDescribeElasticsearchDomain' smart constructor.
data DescribeElasticsearchDomain = DescribeElasticsearchDomain'
  { -- | The name of the Elasticsearch domain for which you want information.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeElasticsearchDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'describeElasticsearchDomain_domainName' - The name of the Elasticsearch domain for which you want information.
newDescribeElasticsearchDomain ::
  -- | 'domainName'
  Prelude.Text ->
  DescribeElasticsearchDomain
newDescribeElasticsearchDomain pDomainName_ =
  DescribeElasticsearchDomain'
    { domainName =
        pDomainName_
    }

-- | The name of the Elasticsearch domain for which you want information.
describeElasticsearchDomain_domainName :: Lens.Lens' DescribeElasticsearchDomain Prelude.Text
describeElasticsearchDomain_domainName = Lens.lens (\DescribeElasticsearchDomain' {domainName} -> domainName) (\s@DescribeElasticsearchDomain' {} a -> s {domainName = a} :: DescribeElasticsearchDomain)

instance Core.AWSRequest DescribeElasticsearchDomain where
  type
    AWSResponse DescribeElasticsearchDomain =
      DescribeElasticsearchDomainResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeElasticsearchDomainResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "DomainStatus")
      )

instance Prelude.Hashable DescribeElasticsearchDomain where
  hashWithSalt _salt DescribeElasticsearchDomain' {..} =
    _salt `Prelude.hashWithSalt` domainName

instance Prelude.NFData DescribeElasticsearchDomain where
  rnf DescribeElasticsearchDomain' {..} =
    Prelude.rnf domainName

instance Data.ToHeaders DescribeElasticsearchDomain where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeElasticsearchDomain where
  toPath DescribeElasticsearchDomain' {..} =
    Prelude.mconcat
      ["/2015-01-01/es/domain/", Data.toBS domainName]

instance Data.ToQuery DescribeElasticsearchDomain where
  toQuery = Prelude.const Prelude.mempty

-- | The result of a @DescribeElasticsearchDomain@ request. Contains the
-- status of the domain specified in the request.
--
-- /See:/ 'newDescribeElasticsearchDomainResponse' smart constructor.
data DescribeElasticsearchDomainResponse = DescribeElasticsearchDomainResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The current status of the Elasticsearch domain.
    domainStatus :: ElasticsearchDomainStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeElasticsearchDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeElasticsearchDomainResponse_httpStatus' - The response's http status code.
--
-- 'domainStatus', 'describeElasticsearchDomainResponse_domainStatus' - The current status of the Elasticsearch domain.
newDescribeElasticsearchDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'domainStatus'
  ElasticsearchDomainStatus ->
  DescribeElasticsearchDomainResponse
newDescribeElasticsearchDomainResponse
  pHttpStatus_
  pDomainStatus_ =
    DescribeElasticsearchDomainResponse'
      { httpStatus =
          pHttpStatus_,
        domainStatus = pDomainStatus_
      }

-- | The response's http status code.
describeElasticsearchDomainResponse_httpStatus :: Lens.Lens' DescribeElasticsearchDomainResponse Prelude.Int
describeElasticsearchDomainResponse_httpStatus = Lens.lens (\DescribeElasticsearchDomainResponse' {httpStatus} -> httpStatus) (\s@DescribeElasticsearchDomainResponse' {} a -> s {httpStatus = a} :: DescribeElasticsearchDomainResponse)

-- | The current status of the Elasticsearch domain.
describeElasticsearchDomainResponse_domainStatus :: Lens.Lens' DescribeElasticsearchDomainResponse ElasticsearchDomainStatus
describeElasticsearchDomainResponse_domainStatus = Lens.lens (\DescribeElasticsearchDomainResponse' {domainStatus} -> domainStatus) (\s@DescribeElasticsearchDomainResponse' {} a -> s {domainStatus = a} :: DescribeElasticsearchDomainResponse)

instance
  Prelude.NFData
    DescribeElasticsearchDomainResponse
  where
  rnf DescribeElasticsearchDomainResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf domainStatus
