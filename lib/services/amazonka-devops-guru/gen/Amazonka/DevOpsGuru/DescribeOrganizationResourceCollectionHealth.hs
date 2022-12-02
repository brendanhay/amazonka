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
-- Module      : Amazonka.DevOpsGuru.DescribeOrganizationResourceCollectionHealth
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides an overview of your system\'s health. If additional member
-- accounts are part of your organization, you can filter those accounts
-- using the @AccountIds@ field.
--
-- This operation returns paginated results.
module Amazonka.DevOpsGuru.DescribeOrganizationResourceCollectionHealth
  ( -- * Creating a Request
    DescribeOrganizationResourceCollectionHealth (..),
    newDescribeOrganizationResourceCollectionHealth,

    -- * Request Lenses
    describeOrganizationResourceCollectionHealth_accountIds,
    describeOrganizationResourceCollectionHealth_nextToken,
    describeOrganizationResourceCollectionHealth_organizationalUnitIds,
    describeOrganizationResourceCollectionHealth_maxResults,
    describeOrganizationResourceCollectionHealth_organizationResourceCollectionType,

    -- * Destructuring the Response
    DescribeOrganizationResourceCollectionHealthResponse (..),
    newDescribeOrganizationResourceCollectionHealthResponse,

    -- * Response Lenses
    describeOrganizationResourceCollectionHealthResponse_tags,
    describeOrganizationResourceCollectionHealthResponse_nextToken,
    describeOrganizationResourceCollectionHealthResponse_account,
    describeOrganizationResourceCollectionHealthResponse_service,
    describeOrganizationResourceCollectionHealthResponse_cloudFormation,
    describeOrganizationResourceCollectionHealthResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeOrganizationResourceCollectionHealth' smart constructor.
data DescribeOrganizationResourceCollectionHealth = DescribeOrganizationResourceCollectionHealth'
  { -- | The ID of the Amazon Web Services account.
    accountIds :: Prelude.Maybe [Prelude.Text],
    -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the organizational unit.
    organizationalUnitIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | An Amazon Web Services resource collection type. This type specifies how
    -- analyzed Amazon Web Services resources are defined. The two types of
    -- Amazon Web Services resource collections supported are Amazon Web
    -- Services CloudFormation stacks and Amazon Web Services resources that
    -- contain the same Amazon Web Services tag. DevOps Guru can be configured
    -- to analyze the Amazon Web Services resources that are defined in the
    -- stacks or that are tagged using the same tag /key/. You can specify up
    -- to 500 Amazon Web Services CloudFormation stacks.
    organizationResourceCollectionType :: OrganizationResourceCollectionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOrganizationResourceCollectionHealth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIds', 'describeOrganizationResourceCollectionHealth_accountIds' - The ID of the Amazon Web Services account.
--
-- 'nextToken', 'describeOrganizationResourceCollectionHealth_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
--
-- 'organizationalUnitIds', 'describeOrganizationResourceCollectionHealth_organizationalUnitIds' - The ID of the organizational unit.
--
-- 'maxResults', 'describeOrganizationResourceCollectionHealth_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'organizationResourceCollectionType', 'describeOrganizationResourceCollectionHealth_organizationResourceCollectionType' - An Amazon Web Services resource collection type. This type specifies how
-- analyzed Amazon Web Services resources are defined. The two types of
-- Amazon Web Services resource collections supported are Amazon Web
-- Services CloudFormation stacks and Amazon Web Services resources that
-- contain the same Amazon Web Services tag. DevOps Guru can be configured
-- to analyze the Amazon Web Services resources that are defined in the
-- stacks or that are tagged using the same tag /key/. You can specify up
-- to 500 Amazon Web Services CloudFormation stacks.
newDescribeOrganizationResourceCollectionHealth ::
  -- | 'organizationResourceCollectionType'
  OrganizationResourceCollectionType ->
  DescribeOrganizationResourceCollectionHealth
newDescribeOrganizationResourceCollectionHealth
  pOrganizationResourceCollectionType_ =
    DescribeOrganizationResourceCollectionHealth'
      { accountIds =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        organizationalUnitIds =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        organizationResourceCollectionType =
          pOrganizationResourceCollectionType_
      }

-- | The ID of the Amazon Web Services account.
describeOrganizationResourceCollectionHealth_accountIds :: Lens.Lens' DescribeOrganizationResourceCollectionHealth (Prelude.Maybe [Prelude.Text])
describeOrganizationResourceCollectionHealth_accountIds = Lens.lens (\DescribeOrganizationResourceCollectionHealth' {accountIds} -> accountIds) (\s@DescribeOrganizationResourceCollectionHealth' {} a -> s {accountIds = a} :: DescribeOrganizationResourceCollectionHealth) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
describeOrganizationResourceCollectionHealth_nextToken :: Lens.Lens' DescribeOrganizationResourceCollectionHealth (Prelude.Maybe Prelude.Text)
describeOrganizationResourceCollectionHealth_nextToken = Lens.lens (\DescribeOrganizationResourceCollectionHealth' {nextToken} -> nextToken) (\s@DescribeOrganizationResourceCollectionHealth' {} a -> s {nextToken = a} :: DescribeOrganizationResourceCollectionHealth)

-- | The ID of the organizational unit.
describeOrganizationResourceCollectionHealth_organizationalUnitIds :: Lens.Lens' DescribeOrganizationResourceCollectionHealth (Prelude.Maybe [Prelude.Text])
describeOrganizationResourceCollectionHealth_organizationalUnitIds = Lens.lens (\DescribeOrganizationResourceCollectionHealth' {organizationalUnitIds} -> organizationalUnitIds) (\s@DescribeOrganizationResourceCollectionHealth' {} a -> s {organizationalUnitIds = a} :: DescribeOrganizationResourceCollectionHealth) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeOrganizationResourceCollectionHealth_maxResults :: Lens.Lens' DescribeOrganizationResourceCollectionHealth (Prelude.Maybe Prelude.Natural)
describeOrganizationResourceCollectionHealth_maxResults = Lens.lens (\DescribeOrganizationResourceCollectionHealth' {maxResults} -> maxResults) (\s@DescribeOrganizationResourceCollectionHealth' {} a -> s {maxResults = a} :: DescribeOrganizationResourceCollectionHealth)

-- | An Amazon Web Services resource collection type. This type specifies how
-- analyzed Amazon Web Services resources are defined. The two types of
-- Amazon Web Services resource collections supported are Amazon Web
-- Services CloudFormation stacks and Amazon Web Services resources that
-- contain the same Amazon Web Services tag. DevOps Guru can be configured
-- to analyze the Amazon Web Services resources that are defined in the
-- stacks or that are tagged using the same tag /key/. You can specify up
-- to 500 Amazon Web Services CloudFormation stacks.
describeOrganizationResourceCollectionHealth_organizationResourceCollectionType :: Lens.Lens' DescribeOrganizationResourceCollectionHealth OrganizationResourceCollectionType
describeOrganizationResourceCollectionHealth_organizationResourceCollectionType = Lens.lens (\DescribeOrganizationResourceCollectionHealth' {organizationResourceCollectionType} -> organizationResourceCollectionType) (\s@DescribeOrganizationResourceCollectionHealth' {} a -> s {organizationResourceCollectionType = a} :: DescribeOrganizationResourceCollectionHealth)

instance
  Core.AWSPager
    DescribeOrganizationResourceCollectionHealth
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeOrganizationResourceCollectionHealthResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeOrganizationResourceCollectionHealthResponse_cloudFormation
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeOrganizationResourceCollectionHealthResponse_account
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeOrganizationResourceCollectionHealthResponse_service
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeOrganizationResourceCollectionHealthResponse_tags
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeOrganizationResourceCollectionHealth_nextToken
          Lens..~ rs
            Lens.^? describeOrganizationResourceCollectionHealthResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeOrganizationResourceCollectionHealth
  where
  type
    AWSResponse
      DescribeOrganizationResourceCollectionHealth =
      DescribeOrganizationResourceCollectionHealthResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeOrganizationResourceCollectionHealthResponse'
            Prelude.<$> (x Data..?> "Tags" Core..!@ Prelude.mempty)
              Prelude.<*> (x Data..?> "NextToken")
              Prelude.<*> (x Data..?> "Account" Core..!@ Prelude.mempty)
              Prelude.<*> (x Data..?> "Service" Core..!@ Prelude.mempty)
              Prelude.<*> (x Data..?> "CloudFormation" Core..!@ Prelude.mempty)
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeOrganizationResourceCollectionHealth
  where
  hashWithSalt
    _salt
    DescribeOrganizationResourceCollectionHealth' {..} =
      _salt `Prelude.hashWithSalt` accountIds
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` organizationalUnitIds
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` organizationResourceCollectionType

instance
  Prelude.NFData
    DescribeOrganizationResourceCollectionHealth
  where
  rnf DescribeOrganizationResourceCollectionHealth' {..} =
    Prelude.rnf accountIds
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf organizationalUnitIds
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf organizationResourceCollectionType

instance
  Data.ToHeaders
    DescribeOrganizationResourceCollectionHealth
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

instance
  Data.ToJSON
    DescribeOrganizationResourceCollectionHealth
  where
  toJSON
    DescribeOrganizationResourceCollectionHealth' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("AccountIds" Data..=) Prelude.<$> accountIds,
              ("NextToken" Data..=) Prelude.<$> nextToken,
              ("OrganizationalUnitIds" Data..=)
                Prelude.<$> organizationalUnitIds,
              ("MaxResults" Data..=) Prelude.<$> maxResults,
              Prelude.Just
                ( "OrganizationResourceCollectionType"
                    Data..= organizationResourceCollectionType
                )
            ]
        )

instance
  Data.ToPath
    DescribeOrganizationResourceCollectionHealth
  where
  toPath =
    Prelude.const
      "/organization/health/resource-collection"

instance
  Data.ToQuery
    DescribeOrganizationResourceCollectionHealth
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeOrganizationResourceCollectionHealthResponse' smart constructor.
data DescribeOrganizationResourceCollectionHealthResponse = DescribeOrganizationResourceCollectionHealthResponse'
  { -- | Tags help you identify and organize your Amazon Web Services resources.
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
    -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the organization\'s account.
    account :: Prelude.Maybe [AccountHealth],
    -- | An array of @ServiceHealth@ objects that describes the health of the
    -- Amazon Web Services services associated with the resources in the
    -- collection.
    service :: Prelude.Maybe [ServiceHealth],
    -- | The returned @CloudFormationHealthOverview@ object that contains an
    -- @InsightHealthOverview@ object with the requested system health
    -- information.
    cloudFormation :: Prelude.Maybe [CloudFormationHealth],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOrganizationResourceCollectionHealthResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'describeOrganizationResourceCollectionHealthResponse_tags' - Tags help you identify and organize your Amazon Web Services resources.
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
-- 'nextToken', 'describeOrganizationResourceCollectionHealthResponse_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
--
-- 'account', 'describeOrganizationResourceCollectionHealthResponse_account' - The name of the organization\'s account.
--
-- 'service', 'describeOrganizationResourceCollectionHealthResponse_service' - An array of @ServiceHealth@ objects that describes the health of the
-- Amazon Web Services services associated with the resources in the
-- collection.
--
-- 'cloudFormation', 'describeOrganizationResourceCollectionHealthResponse_cloudFormation' - The returned @CloudFormationHealthOverview@ object that contains an
-- @InsightHealthOverview@ object with the requested system health
-- information.
--
-- 'httpStatus', 'describeOrganizationResourceCollectionHealthResponse_httpStatus' - The response's http status code.
newDescribeOrganizationResourceCollectionHealthResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeOrganizationResourceCollectionHealthResponse
newDescribeOrganizationResourceCollectionHealthResponse
  pHttpStatus_ =
    DescribeOrganizationResourceCollectionHealthResponse'
      { tags =
          Prelude.Nothing,
        nextToken =
          Prelude.Nothing,
        account =
          Prelude.Nothing,
        service =
          Prelude.Nothing,
        cloudFormation =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Tags help you identify and organize your Amazon Web Services resources.
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
describeOrganizationResourceCollectionHealthResponse_tags :: Lens.Lens' DescribeOrganizationResourceCollectionHealthResponse (Prelude.Maybe [TagHealth])
describeOrganizationResourceCollectionHealthResponse_tags = Lens.lens (\DescribeOrganizationResourceCollectionHealthResponse' {tags} -> tags) (\s@DescribeOrganizationResourceCollectionHealthResponse' {} a -> s {tags = a} :: DescribeOrganizationResourceCollectionHealthResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
describeOrganizationResourceCollectionHealthResponse_nextToken :: Lens.Lens' DescribeOrganizationResourceCollectionHealthResponse (Prelude.Maybe Prelude.Text)
describeOrganizationResourceCollectionHealthResponse_nextToken = Lens.lens (\DescribeOrganizationResourceCollectionHealthResponse' {nextToken} -> nextToken) (\s@DescribeOrganizationResourceCollectionHealthResponse' {} a -> s {nextToken = a} :: DescribeOrganizationResourceCollectionHealthResponse)

-- | The name of the organization\'s account.
describeOrganizationResourceCollectionHealthResponse_account :: Lens.Lens' DescribeOrganizationResourceCollectionHealthResponse (Prelude.Maybe [AccountHealth])
describeOrganizationResourceCollectionHealthResponse_account = Lens.lens (\DescribeOrganizationResourceCollectionHealthResponse' {account} -> account) (\s@DescribeOrganizationResourceCollectionHealthResponse' {} a -> s {account = a} :: DescribeOrganizationResourceCollectionHealthResponse) Prelude.. Lens.mapping Lens.coerced

-- | An array of @ServiceHealth@ objects that describes the health of the
-- Amazon Web Services services associated with the resources in the
-- collection.
describeOrganizationResourceCollectionHealthResponse_service :: Lens.Lens' DescribeOrganizationResourceCollectionHealthResponse (Prelude.Maybe [ServiceHealth])
describeOrganizationResourceCollectionHealthResponse_service = Lens.lens (\DescribeOrganizationResourceCollectionHealthResponse' {service} -> service) (\s@DescribeOrganizationResourceCollectionHealthResponse' {} a -> s {service = a} :: DescribeOrganizationResourceCollectionHealthResponse) Prelude.. Lens.mapping Lens.coerced

-- | The returned @CloudFormationHealthOverview@ object that contains an
-- @InsightHealthOverview@ object with the requested system health
-- information.
describeOrganizationResourceCollectionHealthResponse_cloudFormation :: Lens.Lens' DescribeOrganizationResourceCollectionHealthResponse (Prelude.Maybe [CloudFormationHealth])
describeOrganizationResourceCollectionHealthResponse_cloudFormation = Lens.lens (\DescribeOrganizationResourceCollectionHealthResponse' {cloudFormation} -> cloudFormation) (\s@DescribeOrganizationResourceCollectionHealthResponse' {} a -> s {cloudFormation = a} :: DescribeOrganizationResourceCollectionHealthResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeOrganizationResourceCollectionHealthResponse_httpStatus :: Lens.Lens' DescribeOrganizationResourceCollectionHealthResponse Prelude.Int
describeOrganizationResourceCollectionHealthResponse_httpStatus = Lens.lens (\DescribeOrganizationResourceCollectionHealthResponse' {httpStatus} -> httpStatus) (\s@DescribeOrganizationResourceCollectionHealthResponse' {} a -> s {httpStatus = a} :: DescribeOrganizationResourceCollectionHealthResponse)

instance
  Prelude.NFData
    DescribeOrganizationResourceCollectionHealthResponse
  where
  rnf
    DescribeOrganizationResourceCollectionHealthResponse' {..} =
      Prelude.rnf tags
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf account
        `Prelude.seq` Prelude.rnf service
        `Prelude.seq` Prelude.rnf cloudFormation
        `Prelude.seq` Prelude.rnf httpStatus
