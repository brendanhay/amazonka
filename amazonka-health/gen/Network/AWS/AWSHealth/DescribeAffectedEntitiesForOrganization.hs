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
-- Module      : Network.AWS.AWSHealth.DescribeAffectedEntitiesForOrganization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of entities that have been affected by one or more events
-- for one or more accounts in your organization in AWS Organizations,
-- based on the filter criteria. Entities can refer to individual customer
-- resources, groups of customer resources, or any other construct,
-- depending on the AWS service.
--
-- At least one event Amazon Resource Name (ARN) and account ID are
-- required. Results are sorted by the @lastUpdatedTime@ of the entity,
-- starting with the most recent.
--
-- Before you can call this operation, you must first enable AWS Health to
-- work with AWS Organizations. To do this, call the
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_EnableHealthServiceAccessForOrganization.html EnableHealthServiceAccessForOrganization>
-- operation from your organization\'s management account.
--
-- -   This API operation uses pagination. Specify the @nextToken@
--     parameter in the next request to return more results.
--
-- -   This operation doesn\'t support resource-level permissions. You
--     can\'t use this operation to allow or deny access to specific AWS
--     Health events. For more information, see
--     <https://docs.aws.amazon.com/health/latest/ug/security_iam_id-based-policy-examples.html#resource-action-based-conditions Resource- and action-based conditions>
--     in the /AWS Health User Guide/.
--
-- This operation returns paginated results.
module Network.AWS.AWSHealth.DescribeAffectedEntitiesForOrganization
  ( -- * Creating a Request
    DescribeAffectedEntitiesForOrganization (..),
    newDescribeAffectedEntitiesForOrganization,

    -- * Request Lenses
    describeAffectedEntitiesForOrganization_nextToken,
    describeAffectedEntitiesForOrganization_maxResults,
    describeAffectedEntitiesForOrganization_locale,
    describeAffectedEntitiesForOrganization_organizationEntityFilters,

    -- * Destructuring the Response
    DescribeAffectedEntitiesForOrganizationResponse (..),
    newDescribeAffectedEntitiesForOrganizationResponse,

    -- * Response Lenses
    describeAffectedEntitiesForOrganizationResponse_nextToken,
    describeAffectedEntitiesForOrganizationResponse_failedSet,
    describeAffectedEntitiesForOrganizationResponse_entities,
    describeAffectedEntitiesForOrganizationResponse_httpStatus,
  )
where

import Network.AWS.AWSHealth.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeAffectedEntitiesForOrganization' smart constructor.
data DescribeAffectedEntitiesForOrganization = DescribeAffectedEntitiesForOrganization'
  { -- | If the results of a search are large, only a portion of the results are
    -- returned, and a @nextToken@ pagination token is returned in the
    -- response. To retrieve the next batch of results, reissue the search
    -- request and include the returned token. When all results have been
    -- returned, the response does not contain a pagination token value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return in one batch, between 10 and 100,
    -- inclusive.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The locale (language) to return information in. English (en) is the
    -- default and the only supported value at this time.
    locale :: Prelude.Maybe Prelude.Text,
    -- | A JSON set of elements including the @awsAccountId@ and the @eventArn@.
    organizationEntityFilters :: Prelude.NonEmpty EventAccountFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAffectedEntitiesForOrganization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAffectedEntitiesForOrganization_nextToken' - If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
--
-- 'maxResults', 'describeAffectedEntitiesForOrganization_maxResults' - The maximum number of items to return in one batch, between 10 and 100,
-- inclusive.
--
-- 'locale', 'describeAffectedEntitiesForOrganization_locale' - The locale (language) to return information in. English (en) is the
-- default and the only supported value at this time.
--
-- 'organizationEntityFilters', 'describeAffectedEntitiesForOrganization_organizationEntityFilters' - A JSON set of elements including the @awsAccountId@ and the @eventArn@.
newDescribeAffectedEntitiesForOrganization ::
  -- | 'organizationEntityFilters'
  Prelude.NonEmpty EventAccountFilter ->
  DescribeAffectedEntitiesForOrganization
newDescribeAffectedEntitiesForOrganization
  pOrganizationEntityFilters_ =
    DescribeAffectedEntitiesForOrganization'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        locale = Prelude.Nothing,
        organizationEntityFilters =
          Lens._Coerce
            Lens.# pOrganizationEntityFilters_
      }

-- | If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
describeAffectedEntitiesForOrganization_nextToken :: Lens.Lens' DescribeAffectedEntitiesForOrganization (Prelude.Maybe Prelude.Text)
describeAffectedEntitiesForOrganization_nextToken = Lens.lens (\DescribeAffectedEntitiesForOrganization' {nextToken} -> nextToken) (\s@DescribeAffectedEntitiesForOrganization' {} a -> s {nextToken = a} :: DescribeAffectedEntitiesForOrganization)

-- | The maximum number of items to return in one batch, between 10 and 100,
-- inclusive.
describeAffectedEntitiesForOrganization_maxResults :: Lens.Lens' DescribeAffectedEntitiesForOrganization (Prelude.Maybe Prelude.Natural)
describeAffectedEntitiesForOrganization_maxResults = Lens.lens (\DescribeAffectedEntitiesForOrganization' {maxResults} -> maxResults) (\s@DescribeAffectedEntitiesForOrganization' {} a -> s {maxResults = a} :: DescribeAffectedEntitiesForOrganization)

-- | The locale (language) to return information in. English (en) is the
-- default and the only supported value at this time.
describeAffectedEntitiesForOrganization_locale :: Lens.Lens' DescribeAffectedEntitiesForOrganization (Prelude.Maybe Prelude.Text)
describeAffectedEntitiesForOrganization_locale = Lens.lens (\DescribeAffectedEntitiesForOrganization' {locale} -> locale) (\s@DescribeAffectedEntitiesForOrganization' {} a -> s {locale = a} :: DescribeAffectedEntitiesForOrganization)

-- | A JSON set of elements including the @awsAccountId@ and the @eventArn@.
describeAffectedEntitiesForOrganization_organizationEntityFilters :: Lens.Lens' DescribeAffectedEntitiesForOrganization (Prelude.NonEmpty EventAccountFilter)
describeAffectedEntitiesForOrganization_organizationEntityFilters = Lens.lens (\DescribeAffectedEntitiesForOrganization' {organizationEntityFilters} -> organizationEntityFilters) (\s@DescribeAffectedEntitiesForOrganization' {} a -> s {organizationEntityFilters = a} :: DescribeAffectedEntitiesForOrganization) Prelude.. Lens._Coerce

instance
  Core.AWSPager
    DescribeAffectedEntitiesForOrganization
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeAffectedEntitiesForOrganizationResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeAffectedEntitiesForOrganizationResponse_entities
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeAffectedEntitiesForOrganization_nextToken
          Lens..~ rs
            Lens.^? describeAffectedEntitiesForOrganizationResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeAffectedEntitiesForOrganization
  where
  type
    AWSResponse
      DescribeAffectedEntitiesForOrganization =
      DescribeAffectedEntitiesForOrganizationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAffectedEntitiesForOrganizationResponse'
            Prelude.<$> (x Core..?> "nextToken")
              Prelude.<*> (x Core..?> "failedSet" Core..!@ Prelude.mempty)
              Prelude.<*> (x Core..?> "entities" Core..!@ Prelude.mempty)
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeAffectedEntitiesForOrganization

instance
  Prelude.NFData
    DescribeAffectedEntitiesForOrganization

instance
  Core.ToHeaders
    DescribeAffectedEntitiesForOrganization
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSHealth_20160804.DescribeAffectedEntitiesForOrganization" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    DescribeAffectedEntitiesForOrganization
  where
  toJSON DescribeAffectedEntitiesForOrganization' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("locale" Core..=) Prelude.<$> locale,
            Prelude.Just
              ( "organizationEntityFilters"
                  Core..= organizationEntityFilters
              )
          ]
      )

instance
  Core.ToPath
    DescribeAffectedEntitiesForOrganization
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeAffectedEntitiesForOrganization
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAffectedEntitiesForOrganizationResponse' smart constructor.
data DescribeAffectedEntitiesForOrganizationResponse = DescribeAffectedEntitiesForOrganizationResponse'
  { -- | If the results of a search are large, only a portion of the results are
    -- returned, and a @nextToken@ pagination token is returned in the
    -- response. To retrieve the next batch of results, reissue the search
    -- request and include the returned token. When all results have been
    -- returned, the response does not contain a pagination token value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A JSON set of elements of the failed response, including the
    -- @awsAccountId@, @errorMessage@, @errorName@, and @eventArn@.
    failedSet :: Prelude.Maybe [OrganizationAffectedEntitiesErrorItem],
    -- | A JSON set of elements including the @awsAccountId@ and its @entityArn@,
    -- @entityValue@ and its @entityArn@, @lastUpdatedTime@, and @statusCode@.
    entities :: Prelude.Maybe [AffectedEntity],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAffectedEntitiesForOrganizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAffectedEntitiesForOrganizationResponse_nextToken' - If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
--
-- 'failedSet', 'describeAffectedEntitiesForOrganizationResponse_failedSet' - A JSON set of elements of the failed response, including the
-- @awsAccountId@, @errorMessage@, @errorName@, and @eventArn@.
--
-- 'entities', 'describeAffectedEntitiesForOrganizationResponse_entities' - A JSON set of elements including the @awsAccountId@ and its @entityArn@,
-- @entityValue@ and its @entityArn@, @lastUpdatedTime@, and @statusCode@.
--
-- 'httpStatus', 'describeAffectedEntitiesForOrganizationResponse_httpStatus' - The response's http status code.
newDescribeAffectedEntitiesForOrganizationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAffectedEntitiesForOrganizationResponse
newDescribeAffectedEntitiesForOrganizationResponse
  pHttpStatus_ =
    DescribeAffectedEntitiesForOrganizationResponse'
      { nextToken =
          Prelude.Nothing,
        failedSet =
          Prelude.Nothing,
        entities = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
describeAffectedEntitiesForOrganizationResponse_nextToken :: Lens.Lens' DescribeAffectedEntitiesForOrganizationResponse (Prelude.Maybe Prelude.Text)
describeAffectedEntitiesForOrganizationResponse_nextToken = Lens.lens (\DescribeAffectedEntitiesForOrganizationResponse' {nextToken} -> nextToken) (\s@DescribeAffectedEntitiesForOrganizationResponse' {} a -> s {nextToken = a} :: DescribeAffectedEntitiesForOrganizationResponse)

-- | A JSON set of elements of the failed response, including the
-- @awsAccountId@, @errorMessage@, @errorName@, and @eventArn@.
describeAffectedEntitiesForOrganizationResponse_failedSet :: Lens.Lens' DescribeAffectedEntitiesForOrganizationResponse (Prelude.Maybe [OrganizationAffectedEntitiesErrorItem])
describeAffectedEntitiesForOrganizationResponse_failedSet = Lens.lens (\DescribeAffectedEntitiesForOrganizationResponse' {failedSet} -> failedSet) (\s@DescribeAffectedEntitiesForOrganizationResponse' {} a -> s {failedSet = a} :: DescribeAffectedEntitiesForOrganizationResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A JSON set of elements including the @awsAccountId@ and its @entityArn@,
-- @entityValue@ and its @entityArn@, @lastUpdatedTime@, and @statusCode@.
describeAffectedEntitiesForOrganizationResponse_entities :: Lens.Lens' DescribeAffectedEntitiesForOrganizationResponse (Prelude.Maybe [AffectedEntity])
describeAffectedEntitiesForOrganizationResponse_entities = Lens.lens (\DescribeAffectedEntitiesForOrganizationResponse' {entities} -> entities) (\s@DescribeAffectedEntitiesForOrganizationResponse' {} a -> s {entities = a} :: DescribeAffectedEntitiesForOrganizationResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeAffectedEntitiesForOrganizationResponse_httpStatus :: Lens.Lens' DescribeAffectedEntitiesForOrganizationResponse Prelude.Int
describeAffectedEntitiesForOrganizationResponse_httpStatus = Lens.lens (\DescribeAffectedEntitiesForOrganizationResponse' {httpStatus} -> httpStatus) (\s@DescribeAffectedEntitiesForOrganizationResponse' {} a -> s {httpStatus = a} :: DescribeAffectedEntitiesForOrganizationResponse)

instance
  Prelude.NFData
    DescribeAffectedEntitiesForOrganizationResponse
