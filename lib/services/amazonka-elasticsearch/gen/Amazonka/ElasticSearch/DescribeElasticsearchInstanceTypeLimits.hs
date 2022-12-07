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
-- Module      : Amazonka.ElasticSearch.DescribeElasticsearchInstanceTypeLimits
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe Elasticsearch Limits for a given InstanceType and
-- ElasticsearchVersion. When modifying existing Domain, specify the
-- @ DomainName @ to know what Limits are supported for modifying.
module Amazonka.ElasticSearch.DescribeElasticsearchInstanceTypeLimits
  ( -- * Creating a Request
    DescribeElasticsearchInstanceTypeLimits (..),
    newDescribeElasticsearchInstanceTypeLimits,

    -- * Request Lenses
    describeElasticsearchInstanceTypeLimits_domainName,
    describeElasticsearchInstanceTypeLimits_instanceType,
    describeElasticsearchInstanceTypeLimits_elasticsearchVersion,

    -- * Destructuring the Response
    DescribeElasticsearchInstanceTypeLimitsResponse (..),
    newDescribeElasticsearchInstanceTypeLimitsResponse,

    -- * Response Lenses
    describeElasticsearchInstanceTypeLimitsResponse_limitsByRole,
    describeElasticsearchInstanceTypeLimitsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to
-- @ DescribeElasticsearchInstanceTypeLimits @ operation.
--
-- /See:/ 'newDescribeElasticsearchInstanceTypeLimits' smart constructor.
data DescribeElasticsearchInstanceTypeLimits = DescribeElasticsearchInstanceTypeLimits'
  { -- | DomainName represents the name of the Domain that we are trying to
    -- modify. This should be present only if we are querying for Elasticsearch
    -- @ Limits @ for existing domain.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The instance type for an Elasticsearch cluster for which Elasticsearch
    -- @ Limits @ are needed.
    instanceType :: ESPartitionInstanceType,
    -- | Version of Elasticsearch for which @ Limits @ are needed.
    elasticsearchVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeElasticsearchInstanceTypeLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'describeElasticsearchInstanceTypeLimits_domainName' - DomainName represents the name of the Domain that we are trying to
-- modify. This should be present only if we are querying for Elasticsearch
-- @ Limits @ for existing domain.
--
-- 'instanceType', 'describeElasticsearchInstanceTypeLimits_instanceType' - The instance type for an Elasticsearch cluster for which Elasticsearch
-- @ Limits @ are needed.
--
-- 'elasticsearchVersion', 'describeElasticsearchInstanceTypeLimits_elasticsearchVersion' - Version of Elasticsearch for which @ Limits @ are needed.
newDescribeElasticsearchInstanceTypeLimits ::
  -- | 'instanceType'
  ESPartitionInstanceType ->
  -- | 'elasticsearchVersion'
  Prelude.Text ->
  DescribeElasticsearchInstanceTypeLimits
newDescribeElasticsearchInstanceTypeLimits
  pInstanceType_
  pElasticsearchVersion_ =
    DescribeElasticsearchInstanceTypeLimits'
      { domainName =
          Prelude.Nothing,
        instanceType = pInstanceType_,
        elasticsearchVersion =
          pElasticsearchVersion_
      }

-- | DomainName represents the name of the Domain that we are trying to
-- modify. This should be present only if we are querying for Elasticsearch
-- @ Limits @ for existing domain.
describeElasticsearchInstanceTypeLimits_domainName :: Lens.Lens' DescribeElasticsearchInstanceTypeLimits (Prelude.Maybe Prelude.Text)
describeElasticsearchInstanceTypeLimits_domainName = Lens.lens (\DescribeElasticsearchInstanceTypeLimits' {domainName} -> domainName) (\s@DescribeElasticsearchInstanceTypeLimits' {} a -> s {domainName = a} :: DescribeElasticsearchInstanceTypeLimits)

-- | The instance type for an Elasticsearch cluster for which Elasticsearch
-- @ Limits @ are needed.
describeElasticsearchInstanceTypeLimits_instanceType :: Lens.Lens' DescribeElasticsearchInstanceTypeLimits ESPartitionInstanceType
describeElasticsearchInstanceTypeLimits_instanceType = Lens.lens (\DescribeElasticsearchInstanceTypeLimits' {instanceType} -> instanceType) (\s@DescribeElasticsearchInstanceTypeLimits' {} a -> s {instanceType = a} :: DescribeElasticsearchInstanceTypeLimits)

-- | Version of Elasticsearch for which @ Limits @ are needed.
describeElasticsearchInstanceTypeLimits_elasticsearchVersion :: Lens.Lens' DescribeElasticsearchInstanceTypeLimits Prelude.Text
describeElasticsearchInstanceTypeLimits_elasticsearchVersion = Lens.lens (\DescribeElasticsearchInstanceTypeLimits' {elasticsearchVersion} -> elasticsearchVersion) (\s@DescribeElasticsearchInstanceTypeLimits' {} a -> s {elasticsearchVersion = a} :: DescribeElasticsearchInstanceTypeLimits)

instance
  Core.AWSRequest
    DescribeElasticsearchInstanceTypeLimits
  where
  type
    AWSResponse
      DescribeElasticsearchInstanceTypeLimits =
      DescribeElasticsearchInstanceTypeLimitsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeElasticsearchInstanceTypeLimitsResponse'
            Prelude.<$> (x Data..?> "LimitsByRole" Core..!@ Prelude.mempty)
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeElasticsearchInstanceTypeLimits
  where
  hashWithSalt
    _salt
    DescribeElasticsearchInstanceTypeLimits' {..} =
      _salt `Prelude.hashWithSalt` domainName
        `Prelude.hashWithSalt` instanceType
        `Prelude.hashWithSalt` elasticsearchVersion

instance
  Prelude.NFData
    DescribeElasticsearchInstanceTypeLimits
  where
  rnf DescribeElasticsearchInstanceTypeLimits' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf elasticsearchVersion

instance
  Data.ToHeaders
    DescribeElasticsearchInstanceTypeLimits
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeElasticsearchInstanceTypeLimits
  where
  toPath DescribeElasticsearchInstanceTypeLimits' {..} =
    Prelude.mconcat
      [ "/2015-01-01/es/instanceTypeLimits/",
        Data.toBS elasticsearchVersion,
        "/",
        Data.toBS instanceType
      ]

instance
  Data.ToQuery
    DescribeElasticsearchInstanceTypeLimits
  where
  toQuery DescribeElasticsearchInstanceTypeLimits' {..} =
    Prelude.mconcat ["domainName" Data.=: domainName]

-- | Container for the parameters received from
-- @ DescribeElasticsearchInstanceTypeLimits @ operation.
--
-- /See:/ 'newDescribeElasticsearchInstanceTypeLimitsResponse' smart constructor.
data DescribeElasticsearchInstanceTypeLimitsResponse = DescribeElasticsearchInstanceTypeLimitsResponse'
  { limitsByRole :: Prelude.Maybe (Prelude.HashMap Prelude.Text Limits),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeElasticsearchInstanceTypeLimitsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limitsByRole', 'describeElasticsearchInstanceTypeLimitsResponse_limitsByRole' - Undocumented member.
--
-- 'httpStatus', 'describeElasticsearchInstanceTypeLimitsResponse_httpStatus' - The response's http status code.
newDescribeElasticsearchInstanceTypeLimitsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeElasticsearchInstanceTypeLimitsResponse
newDescribeElasticsearchInstanceTypeLimitsResponse
  pHttpStatus_ =
    DescribeElasticsearchInstanceTypeLimitsResponse'
      { limitsByRole =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
describeElasticsearchInstanceTypeLimitsResponse_limitsByRole :: Lens.Lens' DescribeElasticsearchInstanceTypeLimitsResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Limits))
describeElasticsearchInstanceTypeLimitsResponse_limitsByRole = Lens.lens (\DescribeElasticsearchInstanceTypeLimitsResponse' {limitsByRole} -> limitsByRole) (\s@DescribeElasticsearchInstanceTypeLimitsResponse' {} a -> s {limitsByRole = a} :: DescribeElasticsearchInstanceTypeLimitsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeElasticsearchInstanceTypeLimitsResponse_httpStatus :: Lens.Lens' DescribeElasticsearchInstanceTypeLimitsResponse Prelude.Int
describeElasticsearchInstanceTypeLimitsResponse_httpStatus = Lens.lens (\DescribeElasticsearchInstanceTypeLimitsResponse' {httpStatus} -> httpStatus) (\s@DescribeElasticsearchInstanceTypeLimitsResponse' {} a -> s {httpStatus = a} :: DescribeElasticsearchInstanceTypeLimitsResponse)

instance
  Prelude.NFData
    DescribeElasticsearchInstanceTypeLimitsResponse
  where
  rnf
    DescribeElasticsearchInstanceTypeLimitsResponse' {..} =
      Prelude.rnf limitsByRole
        `Prelude.seq` Prelude.rnf httpStatus
