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
-- Module      : Amazonka.OpenSearch.DescribeDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the domain configuration for the specified Amazon OpenSearch
-- Service domain, including the domain ID, domain service endpoint, and
-- domain ARN.
module Amazonka.OpenSearch.DescribeDomain
  ( -- * Creating a Request
    DescribeDomain (..),
    newDescribeDomain,

    -- * Request Lenses
    describeDomain_domainName,

    -- * Destructuring the Response
    DescribeDomainResponse (..),
    newDescribeDomainResponse,

    -- * Response Lenses
    describeDomainResponse_httpStatus,
    describeDomainResponse_domainStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @DescribeDomain@ operation.
--
-- /See:/ 'newDescribeDomain' smart constructor.
data DescribeDomain = DescribeDomain'
  { -- | The name of the domain that you want information about.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'describeDomain_domainName' - The name of the domain that you want information about.
newDescribeDomain ::
  -- | 'domainName'
  Prelude.Text ->
  DescribeDomain
newDescribeDomain pDomainName_ =
  DescribeDomain' {domainName = pDomainName_}

-- | The name of the domain that you want information about.
describeDomain_domainName :: Lens.Lens' DescribeDomain Prelude.Text
describeDomain_domainName = Lens.lens (\DescribeDomain' {domainName} -> domainName) (\s@DescribeDomain' {} a -> s {domainName = a} :: DescribeDomain)

instance Core.AWSRequest DescribeDomain where
  type
    AWSResponse DescribeDomain =
      DescribeDomainResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDomainResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "DomainStatus")
      )

instance Prelude.Hashable DescribeDomain where
  hashWithSalt _salt DescribeDomain' {..} =
    _salt `Prelude.hashWithSalt` domainName

instance Prelude.NFData DescribeDomain where
  rnf DescribeDomain' {..} = Prelude.rnf domainName

instance Data.ToHeaders DescribeDomain where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeDomain where
  toPath DescribeDomain' {..} =
    Prelude.mconcat
      [ "/2021-01-01/opensearch/domain/",
        Data.toBS domainName
      ]

instance Data.ToQuery DescribeDomain where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the status of the domain specified in the request.
--
-- /See:/ 'newDescribeDomainResponse' smart constructor.
data DescribeDomainResponse = DescribeDomainResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | List that contains the status of each specified OpenSearch Service
    -- domain.
    domainStatus :: DomainStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeDomainResponse_httpStatus' - The response's http status code.
--
-- 'domainStatus', 'describeDomainResponse_domainStatus' - List that contains the status of each specified OpenSearch Service
-- domain.
newDescribeDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'domainStatus'
  DomainStatus ->
  DescribeDomainResponse
newDescribeDomainResponse pHttpStatus_ pDomainStatus_ =
  DescribeDomainResponse'
    { httpStatus = pHttpStatus_,
      domainStatus = pDomainStatus_
    }

-- | The response's http status code.
describeDomainResponse_httpStatus :: Lens.Lens' DescribeDomainResponse Prelude.Int
describeDomainResponse_httpStatus = Lens.lens (\DescribeDomainResponse' {httpStatus} -> httpStatus) (\s@DescribeDomainResponse' {} a -> s {httpStatus = a} :: DescribeDomainResponse)

-- | List that contains the status of each specified OpenSearch Service
-- domain.
describeDomainResponse_domainStatus :: Lens.Lens' DescribeDomainResponse DomainStatus
describeDomainResponse_domainStatus = Lens.lens (\DescribeDomainResponse' {domainStatus} -> domainStatus) (\s@DescribeDomainResponse' {} a -> s {domainStatus = a} :: DescribeDomainResponse)

instance Prelude.NFData DescribeDomainResponse where
  rnf DescribeDomainResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf domainStatus
