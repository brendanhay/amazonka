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
-- Module      : Network.AWS.CloudSearch.DescribeDomains
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the search domains owned by this account. Can be
-- limited to specific domains. Shows all domains by default. To get the
-- number of searchable documents in a domain, use the console or submit a
-- @matchall@ request to your domain\'s search endpoint:
-- @q=matchall&amp;q.parser=structured&amp;size=0@. For more information,
-- see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-domain-info.html Getting Information about a Search Domain>
-- in the /Amazon CloudSearch Developer Guide/.
module Network.AWS.CloudSearch.DescribeDomains
  ( -- * Creating a Request
    DescribeDomains (..),
    newDescribeDomains,

    -- * Request Lenses
    describeDomains_domainNames,

    -- * Destructuring the Response
    DescribeDomainsResponse (..),
    newDescribeDomainsResponse,

    -- * Response Lenses
    describeDomainsResponse_httpStatus,
    describeDomainsResponse_domainStatusList,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @DescribeDomains@ operation. By
-- default shows the status of all domains. To restrict the response to
-- particular domains, specify the names of the domains you want to
-- describe.
--
-- /See:/ 'newDescribeDomains' smart constructor.
data DescribeDomains = DescribeDomains'
  { -- | The names of the domains you want to include in the response.
    domainNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDomains' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainNames', 'describeDomains_domainNames' - The names of the domains you want to include in the response.
newDescribeDomains ::
  DescribeDomains
newDescribeDomains =
  DescribeDomains' {domainNames = Prelude.Nothing}

-- | The names of the domains you want to include in the response.
describeDomains_domainNames :: Lens.Lens' DescribeDomains (Prelude.Maybe [Prelude.Text])
describeDomains_domainNames = Lens.lens (\DescribeDomains' {domainNames} -> domainNames) (\s@DescribeDomains' {} a -> s {domainNames = a} :: DescribeDomains) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSRequest DescribeDomains where
  type
    AWSResponse DescribeDomains =
      DescribeDomainsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeDomainsResult"
      ( \s h x ->
          DescribeDomainsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..@? "DomainStatusList"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.parseXMLList "member"
                        )
      )

instance Prelude.Hashable DescribeDomains

instance Prelude.NFData DescribeDomains

instance Core.ToHeaders DescribeDomains where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeDomains where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeDomains where
  toQuery DescribeDomains' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeDomains" :: Prelude.ByteString),
        "Version"
          Core.=: ("2013-01-01" :: Prelude.ByteString),
        "DomainNames"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> domainNames)
      ]

-- | The result of a @DescribeDomains@ request. Contains the status of the
-- domains specified in the request or all domains owned by the account.
--
-- /See:/ 'newDescribeDomainsResponse' smart constructor.
data DescribeDomainsResponse = DescribeDomainsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    domainStatusList :: [DomainStatus]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDomainsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeDomainsResponse_httpStatus' - The response's http status code.
--
-- 'domainStatusList', 'describeDomainsResponse_domainStatusList' - Undocumented member.
newDescribeDomainsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDomainsResponse
newDescribeDomainsResponse pHttpStatus_ =
  DescribeDomainsResponse'
    { httpStatus = pHttpStatus_,
      domainStatusList = Prelude.mempty
    }

-- | The response's http status code.
describeDomainsResponse_httpStatus :: Lens.Lens' DescribeDomainsResponse Prelude.Int
describeDomainsResponse_httpStatus = Lens.lens (\DescribeDomainsResponse' {httpStatus} -> httpStatus) (\s@DescribeDomainsResponse' {} a -> s {httpStatus = a} :: DescribeDomainsResponse)

-- | Undocumented member.
describeDomainsResponse_domainStatusList :: Lens.Lens' DescribeDomainsResponse [DomainStatus]
describeDomainsResponse_domainStatusList = Lens.lens (\DescribeDomainsResponse' {domainStatusList} -> domainStatusList) (\s@DescribeDomainsResponse' {} a -> s {domainStatusList = a} :: DescribeDomainsResponse) Prelude.. Lens._Coerce

instance Prelude.NFData DescribeDomainsResponse
