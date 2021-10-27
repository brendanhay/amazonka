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
-- Module      : Network.AWS.DevOpsGuru.DescribeResourceCollectionHealth
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the number of open proactive insights, open reactive insights,
-- and the Mean Time to Recover (MTTR) for all closed insights in resource
-- collections in your account. You specify the type of AWS resources
-- collection. The one type of AWS resource collection supported is AWS
-- CloudFormation stacks. DevOps Guru can be configured to analyze only the
-- AWS resources that are defined in the stacks. You can specify up to 500
-- AWS CloudFormation stacks.
--
-- This operation returns paginated results.
module Network.AWS.DevOpsGuru.DescribeResourceCollectionHealth
  ( -- * Creating a Request
    DescribeResourceCollectionHealth (..),
    newDescribeResourceCollectionHealth,

    -- * Request Lenses
    describeResourceCollectionHealth_nextToken,
    describeResourceCollectionHealth_resourceCollectionType,

    -- * Destructuring the Response
    DescribeResourceCollectionHealthResponse (..),
    newDescribeResourceCollectionHealthResponse,

    -- * Response Lenses
    describeResourceCollectionHealthResponse_service,
    describeResourceCollectionHealthResponse_nextToken,
    describeResourceCollectionHealthResponse_httpStatus,
    describeResourceCollectionHealthResponse_cloudFormation,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DevOpsGuru.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeResourceCollectionHealth' smart constructor.
data DescribeResourceCollectionHealth = DescribeResourceCollectionHealth'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An AWS resource collection type. This type specifies how analyzed AWS
    -- resources are defined. The one type of AWS resource collection supported
    -- is AWS CloudFormation stacks. DevOps Guru can be configured to analyze
    -- only the AWS resources that are defined in the stacks. You can specify
    -- up to 500 AWS CloudFormation stacks.
    resourceCollectionType :: ResourceCollectionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeResourceCollectionHealth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeResourceCollectionHealth_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
--
-- 'resourceCollectionType', 'describeResourceCollectionHealth_resourceCollectionType' - An AWS resource collection type. This type specifies how analyzed AWS
-- resources are defined. The one type of AWS resource collection supported
-- is AWS CloudFormation stacks. DevOps Guru can be configured to analyze
-- only the AWS resources that are defined in the stacks. You can specify
-- up to 500 AWS CloudFormation stacks.
newDescribeResourceCollectionHealth ::
  -- | 'resourceCollectionType'
  ResourceCollectionType ->
  DescribeResourceCollectionHealth
newDescribeResourceCollectionHealth
  pResourceCollectionType_ =
    DescribeResourceCollectionHealth'
      { nextToken =
          Prelude.Nothing,
        resourceCollectionType =
          pResourceCollectionType_
      }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
describeResourceCollectionHealth_nextToken :: Lens.Lens' DescribeResourceCollectionHealth (Prelude.Maybe Prelude.Text)
describeResourceCollectionHealth_nextToken = Lens.lens (\DescribeResourceCollectionHealth' {nextToken} -> nextToken) (\s@DescribeResourceCollectionHealth' {} a -> s {nextToken = a} :: DescribeResourceCollectionHealth)

-- | An AWS resource collection type. This type specifies how analyzed AWS
-- resources are defined. The one type of AWS resource collection supported
-- is AWS CloudFormation stacks. DevOps Guru can be configured to analyze
-- only the AWS resources that are defined in the stacks. You can specify
-- up to 500 AWS CloudFormation stacks.
describeResourceCollectionHealth_resourceCollectionType :: Lens.Lens' DescribeResourceCollectionHealth ResourceCollectionType
describeResourceCollectionHealth_resourceCollectionType = Lens.lens (\DescribeResourceCollectionHealth' {resourceCollectionType} -> resourceCollectionType) (\s@DescribeResourceCollectionHealth' {} a -> s {resourceCollectionType = a} :: DescribeResourceCollectionHealth)

instance
  Core.AWSPager
    DescribeResourceCollectionHealth
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeResourceCollectionHealthResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. describeResourceCollectionHealthResponse_cloudFormation
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeResourceCollectionHealthResponse_service
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeResourceCollectionHealth_nextToken
          Lens..~ rs
          Lens.^? describeResourceCollectionHealthResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeResourceCollectionHealth
  where
  type
    AWSResponse DescribeResourceCollectionHealth =
      DescribeResourceCollectionHealthResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeResourceCollectionHealthResponse'
            Prelude.<$> (x Core..?> "Service" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "CloudFormation"
                            Core..!@ Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    DescribeResourceCollectionHealth

instance
  Prelude.NFData
    DescribeResourceCollectionHealth

instance
  Core.ToHeaders
    DescribeResourceCollectionHealth
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeResourceCollectionHealth where
  toPath DescribeResourceCollectionHealth' {..} =
    Prelude.mconcat
      [ "/accounts/health/resource-collection/",
        Core.toBS resourceCollectionType
      ]

instance
  Core.ToQuery
    DescribeResourceCollectionHealth
  where
  toQuery DescribeResourceCollectionHealth' {..} =
    Prelude.mconcat ["NextToken" Core.=: nextToken]

-- | /See:/ 'newDescribeResourceCollectionHealthResponse' smart constructor.
data DescribeResourceCollectionHealthResponse = DescribeResourceCollectionHealthResponse'
  { -- | An array of @ServiceHealth@ objects that describes the health of the AWS
    -- services associated with the resources in the collection.
    service :: Prelude.Maybe [ServiceHealth],
    -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The returned @CloudFormationHealthOverview@ object that contains an
    -- @InsightHealthOverview@ object with the requested system health
    -- information.
    cloudFormation :: [CloudFormationHealth]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeResourceCollectionHealthResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'service', 'describeResourceCollectionHealthResponse_service' - An array of @ServiceHealth@ objects that describes the health of the AWS
-- services associated with the resources in the collection.
--
-- 'nextToken', 'describeResourceCollectionHealthResponse_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
--
-- 'httpStatus', 'describeResourceCollectionHealthResponse_httpStatus' - The response's http status code.
--
-- 'cloudFormation', 'describeResourceCollectionHealthResponse_cloudFormation' - The returned @CloudFormationHealthOverview@ object that contains an
-- @InsightHealthOverview@ object with the requested system health
-- information.
newDescribeResourceCollectionHealthResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeResourceCollectionHealthResponse
newDescribeResourceCollectionHealthResponse
  pHttpStatus_ =
    DescribeResourceCollectionHealthResponse'
      { service =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        cloudFormation = Prelude.mempty
      }

-- | An array of @ServiceHealth@ objects that describes the health of the AWS
-- services associated with the resources in the collection.
describeResourceCollectionHealthResponse_service :: Lens.Lens' DescribeResourceCollectionHealthResponse (Prelude.Maybe [ServiceHealth])
describeResourceCollectionHealthResponse_service = Lens.lens (\DescribeResourceCollectionHealthResponse' {service} -> service) (\s@DescribeResourceCollectionHealthResponse' {} a -> s {service = a} :: DescribeResourceCollectionHealthResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
describeResourceCollectionHealthResponse_nextToken :: Lens.Lens' DescribeResourceCollectionHealthResponse (Prelude.Maybe Prelude.Text)
describeResourceCollectionHealthResponse_nextToken = Lens.lens (\DescribeResourceCollectionHealthResponse' {nextToken} -> nextToken) (\s@DescribeResourceCollectionHealthResponse' {} a -> s {nextToken = a} :: DescribeResourceCollectionHealthResponse)

-- | The response's http status code.
describeResourceCollectionHealthResponse_httpStatus :: Lens.Lens' DescribeResourceCollectionHealthResponse Prelude.Int
describeResourceCollectionHealthResponse_httpStatus = Lens.lens (\DescribeResourceCollectionHealthResponse' {httpStatus} -> httpStatus) (\s@DescribeResourceCollectionHealthResponse' {} a -> s {httpStatus = a} :: DescribeResourceCollectionHealthResponse)

-- | The returned @CloudFormationHealthOverview@ object that contains an
-- @InsightHealthOverview@ object with the requested system health
-- information.
describeResourceCollectionHealthResponse_cloudFormation :: Lens.Lens' DescribeResourceCollectionHealthResponse [CloudFormationHealth]
describeResourceCollectionHealthResponse_cloudFormation = Lens.lens (\DescribeResourceCollectionHealthResponse' {cloudFormation} -> cloudFormation) (\s@DescribeResourceCollectionHealthResponse' {} a -> s {cloudFormation = a} :: DescribeResourceCollectionHealthResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    DescribeResourceCollectionHealthResponse
