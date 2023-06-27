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
-- Module      : Amazonka.OpenSearch.DescribeDomainNodes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about domain and nodes, including data nodes, master
-- nodes, ultrawarm nodes, Availability Zone(s), standby nodes, node
-- configurations, and node states.
module Amazonka.OpenSearch.DescribeDomainNodes
  ( -- * Creating a Request
    DescribeDomainNodes (..),
    newDescribeDomainNodes,

    -- * Request Lenses
    describeDomainNodes_domainName,

    -- * Destructuring the Response
    DescribeDomainNodesResponse (..),
    newDescribeDomainNodesResponse,

    -- * Response Lenses
    describeDomainNodesResponse_domainNodesStatusList,
    describeDomainNodesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @DescribeDomainNodes@ operation.
--
-- /See:/ 'newDescribeDomainNodes' smart constructor.
data DescribeDomainNodes = DescribeDomainNodes'
  { -- | The name of the domain.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDomainNodes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'describeDomainNodes_domainName' - The name of the domain.
newDescribeDomainNodes ::
  -- | 'domainName'
  Prelude.Text ->
  DescribeDomainNodes
newDescribeDomainNodes pDomainName_ =
  DescribeDomainNodes' {domainName = pDomainName_}

-- | The name of the domain.
describeDomainNodes_domainName :: Lens.Lens' DescribeDomainNodes Prelude.Text
describeDomainNodes_domainName = Lens.lens (\DescribeDomainNodes' {domainName} -> domainName) (\s@DescribeDomainNodes' {} a -> s {domainName = a} :: DescribeDomainNodes)

instance Core.AWSRequest DescribeDomainNodes where
  type
    AWSResponse DescribeDomainNodes =
      DescribeDomainNodesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDomainNodesResponse'
            Prelude.<$> ( x
                            Data..?> "DomainNodesStatusList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDomainNodes where
  hashWithSalt _salt DescribeDomainNodes' {..} =
    _salt `Prelude.hashWithSalt` domainName

instance Prelude.NFData DescribeDomainNodes where
  rnf DescribeDomainNodes' {..} = Prelude.rnf domainName

instance Data.ToHeaders DescribeDomainNodes where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeDomainNodes where
  toPath DescribeDomainNodes' {..} =
    Prelude.mconcat
      [ "/2021-01-01/opensearch/domain/",
        Data.toBS domainName,
        "/nodes"
      ]

instance Data.ToQuery DescribeDomainNodes where
  toQuery = Prelude.const Prelude.mempty

-- | The result of a @DescribeDomainNodes@ request. Contains information
-- about the nodes on the requested domain.
--
-- /See:/ 'newDescribeDomainNodesResponse' smart constructor.
data DescribeDomainNodesResponse = DescribeDomainNodesResponse'
  { -- | Contains nodes information list @DomainNodesStatusList@ with details
    -- about the all nodes on the requested domain.
    domainNodesStatusList :: Prelude.Maybe [DomainNodesStatus],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDomainNodesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainNodesStatusList', 'describeDomainNodesResponse_domainNodesStatusList' - Contains nodes information list @DomainNodesStatusList@ with details
-- about the all nodes on the requested domain.
--
-- 'httpStatus', 'describeDomainNodesResponse_httpStatus' - The response's http status code.
newDescribeDomainNodesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDomainNodesResponse
newDescribeDomainNodesResponse pHttpStatus_ =
  DescribeDomainNodesResponse'
    { domainNodesStatusList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains nodes information list @DomainNodesStatusList@ with details
-- about the all nodes on the requested domain.
describeDomainNodesResponse_domainNodesStatusList :: Lens.Lens' DescribeDomainNodesResponse (Prelude.Maybe [DomainNodesStatus])
describeDomainNodesResponse_domainNodesStatusList = Lens.lens (\DescribeDomainNodesResponse' {domainNodesStatusList} -> domainNodesStatusList) (\s@DescribeDomainNodesResponse' {} a -> s {domainNodesStatusList = a} :: DescribeDomainNodesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeDomainNodesResponse_httpStatus :: Lens.Lens' DescribeDomainNodesResponse Prelude.Int
describeDomainNodesResponse_httpStatus = Lens.lens (\DescribeDomainNodesResponse' {httpStatus} -> httpStatus) (\s@DescribeDomainNodesResponse' {} a -> s {httpStatus = a} :: DescribeDomainNodesResponse)

instance Prelude.NFData DescribeDomainNodesResponse where
  rnf DescribeDomainNodesResponse' {..} =
    Prelude.rnf domainNodesStatusList
      `Prelude.seq` Prelude.rnf httpStatus
