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
-- Module      : Network.AWS.ElasticSearch.DescribeElasticsearchInstanceTypeLimits
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe Elasticsearch Limits for a given InstanceType and
-- ElasticsearchVersion. When modifying existing Domain, specify the
-- @ DomainName @ to know what Limits are supported for modifying.
module Network.AWS.ElasticSearch.DescribeElasticsearchInstanceTypeLimits
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

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeElasticsearchInstanceTypeLimitsResponse'
            Prelude.<$> (x Core..?> "LimitsByRole" Core..!@ Prelude.mempty)
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeElasticsearchInstanceTypeLimits

instance
  Prelude.NFData
    DescribeElasticsearchInstanceTypeLimits

instance
  Core.ToHeaders
    DescribeElasticsearchInstanceTypeLimits
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DescribeElasticsearchInstanceTypeLimits
  where
  toPath DescribeElasticsearchInstanceTypeLimits' {..} =
    Prelude.mconcat
      [ "/2015-01-01/es/instanceTypeLimits/",
        Core.toBS elasticsearchVersion,
        "/",
        Core.toBS instanceType
      ]

instance
  Core.ToQuery
    DescribeElasticsearchInstanceTypeLimits
  where
  toQuery DescribeElasticsearchInstanceTypeLimits' {..} =
    Prelude.mconcat ["domainName" Core.=: domainName]

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
describeElasticsearchInstanceTypeLimitsResponse_limitsByRole = Lens.lens (\DescribeElasticsearchInstanceTypeLimitsResponse' {limitsByRole} -> limitsByRole) (\s@DescribeElasticsearchInstanceTypeLimitsResponse' {} a -> s {limitsByRole = a} :: DescribeElasticsearchInstanceTypeLimitsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeElasticsearchInstanceTypeLimitsResponse_httpStatus :: Lens.Lens' DescribeElasticsearchInstanceTypeLimitsResponse Prelude.Int
describeElasticsearchInstanceTypeLimitsResponse_httpStatus = Lens.lens (\DescribeElasticsearchInstanceTypeLimitsResponse' {httpStatus} -> httpStatus) (\s@DescribeElasticsearchInstanceTypeLimitsResponse' {} a -> s {httpStatus = a} :: DescribeElasticsearchInstanceTypeLimitsResponse)

instance
  Prelude.NFData
    DescribeElasticsearchInstanceTypeLimitsResponse
