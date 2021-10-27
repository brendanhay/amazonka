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
-- Module      : Network.AWS.OpenSearch.DescribeInstanceTypeLimits
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe the limits for a given instance type and OpenSearch or
-- Elasticsearch version. When modifying an existing domain, specify the
-- @ DomainName @ to see which limits you can modify.
module Network.AWS.OpenSearch.DescribeInstanceTypeLimits
  ( -- * Creating a Request
    DescribeInstanceTypeLimits (..),
    newDescribeInstanceTypeLimits,

    -- * Request Lenses
    describeInstanceTypeLimits_domainName,
    describeInstanceTypeLimits_instanceType,
    describeInstanceTypeLimits_engineVersion,

    -- * Destructuring the Response
    DescribeInstanceTypeLimitsResponse (..),
    newDescribeInstanceTypeLimitsResponse,

    -- * Response Lenses
    describeInstanceTypeLimitsResponse_limitsByRole,
    describeInstanceTypeLimitsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpenSearch.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @ DescribeInstanceTypeLimits @
-- operation.
--
-- /See:/ 'newDescribeInstanceTypeLimits' smart constructor.
data DescribeInstanceTypeLimits = DescribeInstanceTypeLimits'
  { -- | The name of the domain you want to modify. Only include this value if
    -- you\'re querying OpenSearch @ Limits @ for an existing domain.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The instance type for an OpenSearch cluster for which OpenSearch
    -- @ Limits @ are needed.
    instanceType :: OpenSearchPartitionInstanceType,
    -- | Version of OpenSearch for which @ Limits @ are needed.
    engineVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstanceTypeLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'describeInstanceTypeLimits_domainName' - The name of the domain you want to modify. Only include this value if
-- you\'re querying OpenSearch @ Limits @ for an existing domain.
--
-- 'instanceType', 'describeInstanceTypeLimits_instanceType' - The instance type for an OpenSearch cluster for which OpenSearch
-- @ Limits @ are needed.
--
-- 'engineVersion', 'describeInstanceTypeLimits_engineVersion' - Version of OpenSearch for which @ Limits @ are needed.
newDescribeInstanceTypeLimits ::
  -- | 'instanceType'
  OpenSearchPartitionInstanceType ->
  -- | 'engineVersion'
  Prelude.Text ->
  DescribeInstanceTypeLimits
newDescribeInstanceTypeLimits
  pInstanceType_
  pEngineVersion_ =
    DescribeInstanceTypeLimits'
      { domainName =
          Prelude.Nothing,
        instanceType = pInstanceType_,
        engineVersion = pEngineVersion_
      }

-- | The name of the domain you want to modify. Only include this value if
-- you\'re querying OpenSearch @ Limits @ for an existing domain.
describeInstanceTypeLimits_domainName :: Lens.Lens' DescribeInstanceTypeLimits (Prelude.Maybe Prelude.Text)
describeInstanceTypeLimits_domainName = Lens.lens (\DescribeInstanceTypeLimits' {domainName} -> domainName) (\s@DescribeInstanceTypeLimits' {} a -> s {domainName = a} :: DescribeInstanceTypeLimits)

-- | The instance type for an OpenSearch cluster for which OpenSearch
-- @ Limits @ are needed.
describeInstanceTypeLimits_instanceType :: Lens.Lens' DescribeInstanceTypeLimits OpenSearchPartitionInstanceType
describeInstanceTypeLimits_instanceType = Lens.lens (\DescribeInstanceTypeLimits' {instanceType} -> instanceType) (\s@DescribeInstanceTypeLimits' {} a -> s {instanceType = a} :: DescribeInstanceTypeLimits)

-- | Version of OpenSearch for which @ Limits @ are needed.
describeInstanceTypeLimits_engineVersion :: Lens.Lens' DescribeInstanceTypeLimits Prelude.Text
describeInstanceTypeLimits_engineVersion = Lens.lens (\DescribeInstanceTypeLimits' {engineVersion} -> engineVersion) (\s@DescribeInstanceTypeLimits' {} a -> s {engineVersion = a} :: DescribeInstanceTypeLimits)

instance Core.AWSRequest DescribeInstanceTypeLimits where
  type
    AWSResponse DescribeInstanceTypeLimits =
      DescribeInstanceTypeLimitsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInstanceTypeLimitsResponse'
            Prelude.<$> (x Core..?> "LimitsByRole" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeInstanceTypeLimits

instance Prelude.NFData DescribeInstanceTypeLimits

instance Core.ToHeaders DescribeInstanceTypeLimits where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeInstanceTypeLimits where
  toPath DescribeInstanceTypeLimits' {..} =
    Prelude.mconcat
      [ "/2021-01-01/opensearch/instanceTypeLimits/",
        Core.toBS engineVersion,
        "/",
        Core.toBS instanceType
      ]

instance Core.ToQuery DescribeInstanceTypeLimits where
  toQuery DescribeInstanceTypeLimits' {..} =
    Prelude.mconcat ["domainName" Core.=: domainName]

-- | Container for the parameters received from the
-- @ DescribeInstanceTypeLimits @ operation.
--
-- /See:/ 'newDescribeInstanceTypeLimitsResponse' smart constructor.
data DescribeInstanceTypeLimitsResponse = DescribeInstanceTypeLimitsResponse'
  { limitsByRole :: Prelude.Maybe (Prelude.HashMap Prelude.Text Limits),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstanceTypeLimitsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limitsByRole', 'describeInstanceTypeLimitsResponse_limitsByRole' - Undocumented member.
--
-- 'httpStatus', 'describeInstanceTypeLimitsResponse_httpStatus' - The response's http status code.
newDescribeInstanceTypeLimitsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeInstanceTypeLimitsResponse
newDescribeInstanceTypeLimitsResponse pHttpStatus_ =
  DescribeInstanceTypeLimitsResponse'
    { limitsByRole =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeInstanceTypeLimitsResponse_limitsByRole :: Lens.Lens' DescribeInstanceTypeLimitsResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Limits))
describeInstanceTypeLimitsResponse_limitsByRole = Lens.lens (\DescribeInstanceTypeLimitsResponse' {limitsByRole} -> limitsByRole) (\s@DescribeInstanceTypeLimitsResponse' {} a -> s {limitsByRole = a} :: DescribeInstanceTypeLimitsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeInstanceTypeLimitsResponse_httpStatus :: Lens.Lens' DescribeInstanceTypeLimitsResponse Prelude.Int
describeInstanceTypeLimitsResponse_httpStatus = Lens.lens (\DescribeInstanceTypeLimitsResponse' {httpStatus} -> httpStatus) (\s@DescribeInstanceTypeLimitsResponse' {} a -> s {httpStatus = a} :: DescribeInstanceTypeLimitsResponse)

instance
  Prelude.NFData
    DescribeInstanceTypeLimitsResponse
