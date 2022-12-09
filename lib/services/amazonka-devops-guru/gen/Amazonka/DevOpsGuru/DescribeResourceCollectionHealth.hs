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
-- Module      : Amazonka.DevOpsGuru.DescribeResourceCollectionHealth
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the number of open proactive insights, open reactive insights,
-- and the Mean Time to Recover (MTTR) for all closed insights in resource
-- collections in your account. You specify the type of Amazon Web Services
-- resources collection. The two types of Amazon Web Services resource
-- collections supported are Amazon Web Services CloudFormation stacks and
-- Amazon Web Services resources that contain the same Amazon Web Services
-- tag. DevOps Guru can be configured to analyze the Amazon Web Services
-- resources that are defined in the stacks or that are tagged using the
-- same tag /key/. You can specify up to 500 Amazon Web Services
-- CloudFormation stacks.
--
-- This operation returns paginated results.
module Amazonka.DevOpsGuru.DescribeResourceCollectionHealth
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
    describeResourceCollectionHealthResponse_cloudFormation,
    describeResourceCollectionHealthResponse_nextToken,
    describeResourceCollectionHealthResponse_service,
    describeResourceCollectionHealthResponse_tags,
    describeResourceCollectionHealthResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeResourceCollectionHealth' smart constructor.
data DescribeResourceCollectionHealth = DescribeResourceCollectionHealth'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An Amazon Web Services resource collection type. This type specifies how
    -- analyzed Amazon Web Services resources are defined. The two types of
    -- Amazon Web Services resource collections supported are Amazon Web
    -- Services CloudFormation stacks and Amazon Web Services resources that
    -- contain the same Amazon Web Services tag. DevOps Guru can be configured
    -- to analyze the Amazon Web Services resources that are defined in the
    -- stacks or that are tagged using the same tag /key/. You can specify up
    -- to 500 Amazon Web Services CloudFormation stacks.
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
-- 'resourceCollectionType', 'describeResourceCollectionHealth_resourceCollectionType' - An Amazon Web Services resource collection type. This type specifies how
-- analyzed Amazon Web Services resources are defined. The two types of
-- Amazon Web Services resource collections supported are Amazon Web
-- Services CloudFormation stacks and Amazon Web Services resources that
-- contain the same Amazon Web Services tag. DevOps Guru can be configured
-- to analyze the Amazon Web Services resources that are defined in the
-- stacks or that are tagged using the same tag /key/. You can specify up
-- to 500 Amazon Web Services CloudFormation stacks.
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

-- | An Amazon Web Services resource collection type. This type specifies how
-- analyzed Amazon Web Services resources are defined. The two types of
-- Amazon Web Services resource collections supported are Amazon Web
-- Services CloudFormation stacks and Amazon Web Services resources that
-- contain the same Amazon Web Services tag. DevOps Guru can be configured
-- to analyze the Amazon Web Services resources that are defined in the
-- stacks or that are tagged using the same tag /key/. You can specify up
-- to 500 Amazon Web Services CloudFormation stacks.
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
            Lens.^? describeResourceCollectionHealthResponse_cloudFormation
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeResourceCollectionHealthResponse_service
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeResourceCollectionHealthResponse_tags
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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeResourceCollectionHealthResponse'
            Prelude.<$> (x Data..?> "CloudFormation" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Service" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeResourceCollectionHealth
  where
  hashWithSalt
    _salt
    DescribeResourceCollectionHealth' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` resourceCollectionType

instance
  Prelude.NFData
    DescribeResourceCollectionHealth
  where
  rnf DescribeResourceCollectionHealth' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceCollectionType

instance
  Data.ToHeaders
    DescribeResourceCollectionHealth
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeResourceCollectionHealth where
  toPath DescribeResourceCollectionHealth' {..} =
    Prelude.mconcat
      [ "/accounts/health/resource-collection/",
        Data.toBS resourceCollectionType
      ]

instance
  Data.ToQuery
    DescribeResourceCollectionHealth
  where
  toQuery DescribeResourceCollectionHealth' {..} =
    Prelude.mconcat ["NextToken" Data.=: nextToken]

-- | /See:/ 'newDescribeResourceCollectionHealthResponse' smart constructor.
data DescribeResourceCollectionHealthResponse = DescribeResourceCollectionHealthResponse'
  { -- | The returned @CloudFormationHealthOverview@ object that contains an
    -- @InsightHealthOverview@ object with the requested system health
    -- information.
    cloudFormation :: Prelude.Maybe [CloudFormationHealth],
    -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of @ServiceHealth@ objects that describes the health of the
    -- Amazon Web Services services associated with the resources in the
    -- collection.
    service :: Prelude.Maybe [ServiceHealth],
    -- | The Amazon Web Services tags that are used by resources in the resource
    -- collection.
    --
    -- Tags help you identify and organize your Amazon Web Services resources.
    -- Many Amazon Web Services services support tagging, so you can assign the
    -- same tag to resources from different services to indicate that the
    -- resources are related. For example, you can assign the same tag to an
    -- Amazon DynamoDB table resource that you assign to an Lambda function.
    -- For more information about using tags, see the
    -- <https://d1.awsstatic.com/whitepapers/aws-tagging-best-practices.pdf Tagging best practices>
    -- whitepaper.
    --
    -- Each Amazon Web Services tag has two parts.
    --
    -- -   A tag /key/ (for example, @CostCenter@, @Environment@, @Project@, or
    --     @Secret@). Tag /keys/ are case-sensitive.
    --
    -- -   An optional field known as a tag /value/ (for example,
    --     @111122223333@, @Production@, or a team name). Omitting the tag
    --     /value/ is the same as using an empty string. Like tag /keys/, tag
    --     /values/ are case-sensitive.
    --
    -- Together these are known as /key/-/value/ pairs.
    --
    -- The string used for a /key/ in a tag that you use to define your
    -- resource coverage must begin with the prefix @Devops-guru-@. The tag
    -- /key/ might be @DevOps-Guru-deployment-application@ or
    -- @devops-guru-rds-application@. When you create a /key/, the case of
    -- characters in the /key/ can be whatever you choose. After you create a
    -- /key/, it is case-sensitive. For example, DevOps Guru works with a /key/
    -- named @devops-guru-rds@ and a /key/ named @DevOps-Guru-RDS@, and these
    -- act as two different /keys/. Possible /key/\//value/ pairs in your
    -- application might be @Devops-Guru-production-application\/RDS@ or
    -- @Devops-Guru-production-application\/containers@.
    tags :: Prelude.Maybe [TagHealth],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'cloudFormation', 'describeResourceCollectionHealthResponse_cloudFormation' - The returned @CloudFormationHealthOverview@ object that contains an
-- @InsightHealthOverview@ object with the requested system health
-- information.
--
-- 'nextToken', 'describeResourceCollectionHealthResponse_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
--
-- 'service', 'describeResourceCollectionHealthResponse_service' - An array of @ServiceHealth@ objects that describes the health of the
-- Amazon Web Services services associated with the resources in the
-- collection.
--
-- 'tags', 'describeResourceCollectionHealthResponse_tags' - The Amazon Web Services tags that are used by resources in the resource
-- collection.
--
-- Tags help you identify and organize your Amazon Web Services resources.
-- Many Amazon Web Services services support tagging, so you can assign the
-- same tag to resources from different services to indicate that the
-- resources are related. For example, you can assign the same tag to an
-- Amazon DynamoDB table resource that you assign to an Lambda function.
-- For more information about using tags, see the
-- <https://d1.awsstatic.com/whitepapers/aws-tagging-best-practices.pdf Tagging best practices>
-- whitepaper.
--
-- Each Amazon Web Services tag has two parts.
--
-- -   A tag /key/ (for example, @CostCenter@, @Environment@, @Project@, or
--     @Secret@). Tag /keys/ are case-sensitive.
--
-- -   An optional field known as a tag /value/ (for example,
--     @111122223333@, @Production@, or a team name). Omitting the tag
--     /value/ is the same as using an empty string. Like tag /keys/, tag
--     /values/ are case-sensitive.
--
-- Together these are known as /key/-/value/ pairs.
--
-- The string used for a /key/ in a tag that you use to define your
-- resource coverage must begin with the prefix @Devops-guru-@. The tag
-- /key/ might be @DevOps-Guru-deployment-application@ or
-- @devops-guru-rds-application@. When you create a /key/, the case of
-- characters in the /key/ can be whatever you choose. After you create a
-- /key/, it is case-sensitive. For example, DevOps Guru works with a /key/
-- named @devops-guru-rds@ and a /key/ named @DevOps-Guru-RDS@, and these
-- act as two different /keys/. Possible /key/\//value/ pairs in your
-- application might be @Devops-Guru-production-application\/RDS@ or
-- @Devops-Guru-production-application\/containers@.
--
-- 'httpStatus', 'describeResourceCollectionHealthResponse_httpStatus' - The response's http status code.
newDescribeResourceCollectionHealthResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeResourceCollectionHealthResponse
newDescribeResourceCollectionHealthResponse
  pHttpStatus_ =
    DescribeResourceCollectionHealthResponse'
      { cloudFormation =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        service = Prelude.Nothing,
        tags = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The returned @CloudFormationHealthOverview@ object that contains an
-- @InsightHealthOverview@ object with the requested system health
-- information.
describeResourceCollectionHealthResponse_cloudFormation :: Lens.Lens' DescribeResourceCollectionHealthResponse (Prelude.Maybe [CloudFormationHealth])
describeResourceCollectionHealthResponse_cloudFormation = Lens.lens (\DescribeResourceCollectionHealthResponse' {cloudFormation} -> cloudFormation) (\s@DescribeResourceCollectionHealthResponse' {} a -> s {cloudFormation = a} :: DescribeResourceCollectionHealthResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
describeResourceCollectionHealthResponse_nextToken :: Lens.Lens' DescribeResourceCollectionHealthResponse (Prelude.Maybe Prelude.Text)
describeResourceCollectionHealthResponse_nextToken = Lens.lens (\DescribeResourceCollectionHealthResponse' {nextToken} -> nextToken) (\s@DescribeResourceCollectionHealthResponse' {} a -> s {nextToken = a} :: DescribeResourceCollectionHealthResponse)

-- | An array of @ServiceHealth@ objects that describes the health of the
-- Amazon Web Services services associated with the resources in the
-- collection.
describeResourceCollectionHealthResponse_service :: Lens.Lens' DescribeResourceCollectionHealthResponse (Prelude.Maybe [ServiceHealth])
describeResourceCollectionHealthResponse_service = Lens.lens (\DescribeResourceCollectionHealthResponse' {service} -> service) (\s@DescribeResourceCollectionHealthResponse' {} a -> s {service = a} :: DescribeResourceCollectionHealthResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services tags that are used by resources in the resource
-- collection.
--
-- Tags help you identify and organize your Amazon Web Services resources.
-- Many Amazon Web Services services support tagging, so you can assign the
-- same tag to resources from different services to indicate that the
-- resources are related. For example, you can assign the same tag to an
-- Amazon DynamoDB table resource that you assign to an Lambda function.
-- For more information about using tags, see the
-- <https://d1.awsstatic.com/whitepapers/aws-tagging-best-practices.pdf Tagging best practices>
-- whitepaper.
--
-- Each Amazon Web Services tag has two parts.
--
-- -   A tag /key/ (for example, @CostCenter@, @Environment@, @Project@, or
--     @Secret@). Tag /keys/ are case-sensitive.
--
-- -   An optional field known as a tag /value/ (for example,
--     @111122223333@, @Production@, or a team name). Omitting the tag
--     /value/ is the same as using an empty string. Like tag /keys/, tag
--     /values/ are case-sensitive.
--
-- Together these are known as /key/-/value/ pairs.
--
-- The string used for a /key/ in a tag that you use to define your
-- resource coverage must begin with the prefix @Devops-guru-@. The tag
-- /key/ might be @DevOps-Guru-deployment-application@ or
-- @devops-guru-rds-application@. When you create a /key/, the case of
-- characters in the /key/ can be whatever you choose. After you create a
-- /key/, it is case-sensitive. For example, DevOps Guru works with a /key/
-- named @devops-guru-rds@ and a /key/ named @DevOps-Guru-RDS@, and these
-- act as two different /keys/. Possible /key/\//value/ pairs in your
-- application might be @Devops-Guru-production-application\/RDS@ or
-- @Devops-Guru-production-application\/containers@.
describeResourceCollectionHealthResponse_tags :: Lens.Lens' DescribeResourceCollectionHealthResponse (Prelude.Maybe [TagHealth])
describeResourceCollectionHealthResponse_tags = Lens.lens (\DescribeResourceCollectionHealthResponse' {tags} -> tags) (\s@DescribeResourceCollectionHealthResponse' {} a -> s {tags = a} :: DescribeResourceCollectionHealthResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeResourceCollectionHealthResponse_httpStatus :: Lens.Lens' DescribeResourceCollectionHealthResponse Prelude.Int
describeResourceCollectionHealthResponse_httpStatus = Lens.lens (\DescribeResourceCollectionHealthResponse' {httpStatus} -> httpStatus) (\s@DescribeResourceCollectionHealthResponse' {} a -> s {httpStatus = a} :: DescribeResourceCollectionHealthResponse)

instance
  Prelude.NFData
    DescribeResourceCollectionHealthResponse
  where
  rnf DescribeResourceCollectionHealthResponse' {..} =
    Prelude.rnf cloudFormation
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf service
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
