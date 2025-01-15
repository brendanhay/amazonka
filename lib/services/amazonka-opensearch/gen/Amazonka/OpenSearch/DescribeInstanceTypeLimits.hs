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
-- Module      : Amazonka.OpenSearch.DescribeInstanceTypeLimits
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the instance count, storage, and master node limits for a
-- given OpenSearch or Elasticsearch version and instance type.
module Amazonka.OpenSearch.DescribeInstanceTypeLimits
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @DescribeInstanceTypeLimits@
-- operation.
--
-- /See:/ 'newDescribeInstanceTypeLimits' smart constructor.
data DescribeInstanceTypeLimits = DescribeInstanceTypeLimits'
  { -- | The name of the domain. Only specify if you need the limits for an
    -- existing domain.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The OpenSearch Service instance type for which you need limit
    -- information.
    instanceType :: OpenSearchPartitionInstanceType,
    -- | Version of OpenSearch or Elasticsearch, in the format Elasticsearch_X.Y
    -- or OpenSearch_X.Y. Defaults to the latest version of OpenSearch.
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
-- 'domainName', 'describeInstanceTypeLimits_domainName' - The name of the domain. Only specify if you need the limits for an
-- existing domain.
--
-- 'instanceType', 'describeInstanceTypeLimits_instanceType' - The OpenSearch Service instance type for which you need limit
-- information.
--
-- 'engineVersion', 'describeInstanceTypeLimits_engineVersion' - Version of OpenSearch or Elasticsearch, in the format Elasticsearch_X.Y
-- or OpenSearch_X.Y. Defaults to the latest version of OpenSearch.
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

-- | The name of the domain. Only specify if you need the limits for an
-- existing domain.
describeInstanceTypeLimits_domainName :: Lens.Lens' DescribeInstanceTypeLimits (Prelude.Maybe Prelude.Text)
describeInstanceTypeLimits_domainName = Lens.lens (\DescribeInstanceTypeLimits' {domainName} -> domainName) (\s@DescribeInstanceTypeLimits' {} a -> s {domainName = a} :: DescribeInstanceTypeLimits)

-- | The OpenSearch Service instance type for which you need limit
-- information.
describeInstanceTypeLimits_instanceType :: Lens.Lens' DescribeInstanceTypeLimits OpenSearchPartitionInstanceType
describeInstanceTypeLimits_instanceType = Lens.lens (\DescribeInstanceTypeLimits' {instanceType} -> instanceType) (\s@DescribeInstanceTypeLimits' {} a -> s {instanceType = a} :: DescribeInstanceTypeLimits)

-- | Version of OpenSearch or Elasticsearch, in the format Elasticsearch_X.Y
-- or OpenSearch_X.Y. Defaults to the latest version of OpenSearch.
describeInstanceTypeLimits_engineVersion :: Lens.Lens' DescribeInstanceTypeLimits Prelude.Text
describeInstanceTypeLimits_engineVersion = Lens.lens (\DescribeInstanceTypeLimits' {engineVersion} -> engineVersion) (\s@DescribeInstanceTypeLimits' {} a -> s {engineVersion = a} :: DescribeInstanceTypeLimits)

instance Core.AWSRequest DescribeInstanceTypeLimits where
  type
    AWSResponse DescribeInstanceTypeLimits =
      DescribeInstanceTypeLimitsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInstanceTypeLimitsResponse'
            Prelude.<$> (x Data..?> "LimitsByRole" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeInstanceTypeLimits where
  hashWithSalt _salt DescribeInstanceTypeLimits' {..} =
    _salt
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` engineVersion

instance Prelude.NFData DescribeInstanceTypeLimits where
  rnf DescribeInstanceTypeLimits' {..} =
    Prelude.rnf domainName `Prelude.seq`
      Prelude.rnf instanceType `Prelude.seq`
        Prelude.rnf engineVersion

instance Data.ToHeaders DescribeInstanceTypeLimits where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeInstanceTypeLimits where
  toPath DescribeInstanceTypeLimits' {..} =
    Prelude.mconcat
      [ "/2021-01-01/opensearch/instanceTypeLimits/",
        Data.toBS engineVersion,
        "/",
        Data.toBS instanceType
      ]

instance Data.ToQuery DescribeInstanceTypeLimits where
  toQuery DescribeInstanceTypeLimits' {..} =
    Prelude.mconcat ["domainName" Data.=: domainName]

-- | Container for the parameters received from the
-- @DescribeInstanceTypeLimits@ operation.
--
-- /See:/ 'newDescribeInstanceTypeLimitsResponse' smart constructor.
data DescribeInstanceTypeLimitsResponse = DescribeInstanceTypeLimitsResponse'
  { -- | Map that contains all applicable instance type limits.@data@ refers to
    -- data nodes.@master@ refers to dedicated master nodes.
    limitsByRole :: Prelude.Maybe (Prelude.HashMap Prelude.Text Limits),
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
-- 'limitsByRole', 'describeInstanceTypeLimitsResponse_limitsByRole' - Map that contains all applicable instance type limits.@data@ refers to
-- data nodes.@master@ refers to dedicated master nodes.
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

-- | Map that contains all applicable instance type limits.@data@ refers to
-- data nodes.@master@ refers to dedicated master nodes.
describeInstanceTypeLimitsResponse_limitsByRole :: Lens.Lens' DescribeInstanceTypeLimitsResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Limits))
describeInstanceTypeLimitsResponse_limitsByRole = Lens.lens (\DescribeInstanceTypeLimitsResponse' {limitsByRole} -> limitsByRole) (\s@DescribeInstanceTypeLimitsResponse' {} a -> s {limitsByRole = a} :: DescribeInstanceTypeLimitsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeInstanceTypeLimitsResponse_httpStatus :: Lens.Lens' DescribeInstanceTypeLimitsResponse Prelude.Int
describeInstanceTypeLimitsResponse_httpStatus = Lens.lens (\DescribeInstanceTypeLimitsResponse' {httpStatus} -> httpStatus) (\s@DescribeInstanceTypeLimitsResponse' {} a -> s {httpStatus = a} :: DescribeInstanceTypeLimitsResponse)

instance
  Prelude.NFData
    DescribeInstanceTypeLimitsResponse
  where
  rnf DescribeInstanceTypeLimitsResponse' {..} =
    Prelude.rnf limitsByRole `Prelude.seq`
      Prelude.rnf httpStatus
