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
-- Module      : Amazonka.AWSHealth.DescribeAffectedEntitiesForOrganization
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of entities that have been affected by one or more events
-- for one or more accounts in your organization in Organizations, based on
-- the filter criteria. Entities can refer to individual customer
-- resources, groups of customer resources, or any other construct,
-- depending on the Amazon Web Services service.
--
-- At least one event Amazon Resource Name (ARN) and account ID are
-- required.
--
-- Before you can call this operation, you must first enable Health to work
-- with Organizations. To do this, call the
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_EnableHealthServiceAccessForOrganization.html EnableHealthServiceAccessForOrganization>
-- operation from your organization\'s management account.
--
-- -   This API operation uses pagination. Specify the @nextToken@
--     parameter in the next request to return more results.
--
-- -   This operation doesn\'t support resource-level permissions. You
--     can\'t use this operation to allow or deny access to specific Health
--     events. For more information, see
--     <https://docs.aws.amazon.com/health/latest/ug/security_iam_id-based-policy-examples.html#resource-action-based-conditions Resource- and action-based conditions>
--     in the /Health User Guide/.
--
-- This operation returns paginated results.
module Amazonka.AWSHealth.DescribeAffectedEntitiesForOrganization
  ( -- * Creating a Request
    DescribeAffectedEntitiesForOrganization (..),
    newDescribeAffectedEntitiesForOrganization,

    -- * Request Lenses
    describeAffectedEntitiesForOrganization_locale,
    describeAffectedEntitiesForOrganization_maxResults,
    describeAffectedEntitiesForOrganization_nextToken,
    describeAffectedEntitiesForOrganization_organizationEntityFilters,

    -- * Destructuring the Response
    DescribeAffectedEntitiesForOrganizationResponse (..),
    newDescribeAffectedEntitiesForOrganizationResponse,

    -- * Response Lenses
    describeAffectedEntitiesForOrganizationResponse_entities,
    describeAffectedEntitiesForOrganizationResponse_failedSet,
    describeAffectedEntitiesForOrganizationResponse_nextToken,
    describeAffectedEntitiesForOrganizationResponse_httpStatus,
  )
where

import Amazonka.AWSHealth.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAffectedEntitiesForOrganization' smart constructor.
data DescribeAffectedEntitiesForOrganization = DescribeAffectedEntitiesForOrganization'
  { -- | The locale (language) to return information in. English (en) is the
    -- default and the only supported value at this time.
    locale :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return in one batch, between 10 and 100,
    -- inclusive.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the results of a search are large, only a portion of the results are
    -- returned, and a @nextToken@ pagination token is returned in the
    -- response. To retrieve the next batch of results, reissue the search
    -- request and include the returned token. When all results have been
    -- returned, the response does not contain a pagination token value.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'locale', 'describeAffectedEntitiesForOrganization_locale' - The locale (language) to return information in. English (en) is the
-- default and the only supported value at this time.
--
-- 'maxResults', 'describeAffectedEntitiesForOrganization_maxResults' - The maximum number of items to return in one batch, between 10 and 100,
-- inclusive.
--
-- 'nextToken', 'describeAffectedEntitiesForOrganization_nextToken' - If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
--
-- 'organizationEntityFilters', 'describeAffectedEntitiesForOrganization_organizationEntityFilters' - A JSON set of elements including the @awsAccountId@ and the @eventArn@.
newDescribeAffectedEntitiesForOrganization ::
  -- | 'organizationEntityFilters'
  Prelude.NonEmpty EventAccountFilter ->
  DescribeAffectedEntitiesForOrganization
newDescribeAffectedEntitiesForOrganization
  pOrganizationEntityFilters_ =
    DescribeAffectedEntitiesForOrganization'
      { locale =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        organizationEntityFilters =
          Lens.coerced
            Lens.# pOrganizationEntityFilters_
      }

-- | The locale (language) to return information in. English (en) is the
-- default and the only supported value at this time.
describeAffectedEntitiesForOrganization_locale :: Lens.Lens' DescribeAffectedEntitiesForOrganization (Prelude.Maybe Prelude.Text)
describeAffectedEntitiesForOrganization_locale = Lens.lens (\DescribeAffectedEntitiesForOrganization' {locale} -> locale) (\s@DescribeAffectedEntitiesForOrganization' {} a -> s {locale = a} :: DescribeAffectedEntitiesForOrganization)

-- | The maximum number of items to return in one batch, between 10 and 100,
-- inclusive.
describeAffectedEntitiesForOrganization_maxResults :: Lens.Lens' DescribeAffectedEntitiesForOrganization (Prelude.Maybe Prelude.Natural)
describeAffectedEntitiesForOrganization_maxResults = Lens.lens (\DescribeAffectedEntitiesForOrganization' {maxResults} -> maxResults) (\s@DescribeAffectedEntitiesForOrganization' {} a -> s {maxResults = a} :: DescribeAffectedEntitiesForOrganization)

-- | If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
describeAffectedEntitiesForOrganization_nextToken :: Lens.Lens' DescribeAffectedEntitiesForOrganization (Prelude.Maybe Prelude.Text)
describeAffectedEntitiesForOrganization_nextToken = Lens.lens (\DescribeAffectedEntitiesForOrganization' {nextToken} -> nextToken) (\s@DescribeAffectedEntitiesForOrganization' {} a -> s {nextToken = a} :: DescribeAffectedEntitiesForOrganization)

-- | A JSON set of elements including the @awsAccountId@ and the @eventArn@.
describeAffectedEntitiesForOrganization_organizationEntityFilters :: Lens.Lens' DescribeAffectedEntitiesForOrganization (Prelude.NonEmpty EventAccountFilter)
describeAffectedEntitiesForOrganization_organizationEntityFilters = Lens.lens (\DescribeAffectedEntitiesForOrganization' {organizationEntityFilters} -> organizationEntityFilters) (\s@DescribeAffectedEntitiesForOrganization' {} a -> s {organizationEntityFilters = a} :: DescribeAffectedEntitiesForOrganization) Prelude.. Lens.coerced

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAffectedEntitiesForOrganizationResponse'
            Prelude.<$> (x Data..?> "entities" Core..!@ Prelude.mempty)
              Prelude.<*> (x Data..?> "failedSet" Core..!@ Prelude.mempty)
              Prelude.<*> (x Data..?> "nextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeAffectedEntitiesForOrganization
  where
  hashWithSalt
    _salt
    DescribeAffectedEntitiesForOrganization' {..} =
      _salt `Prelude.hashWithSalt` locale
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` organizationEntityFilters

instance
  Prelude.NFData
    DescribeAffectedEntitiesForOrganization
  where
  rnf DescribeAffectedEntitiesForOrganization' {..} =
    Prelude.rnf locale
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf organizationEntityFilters

instance
  Data.ToHeaders
    DescribeAffectedEntitiesForOrganization
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHealth_20160804.DescribeAffectedEntitiesForOrganization" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeAffectedEntitiesForOrganization
  where
  toJSON DescribeAffectedEntitiesForOrganization' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("locale" Data..=) Prelude.<$> locale,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ( "organizationEntityFilters"
                  Data..= organizationEntityFilters
              )
          ]
      )

instance
  Data.ToPath
    DescribeAffectedEntitiesForOrganization
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeAffectedEntitiesForOrganization
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAffectedEntitiesForOrganizationResponse' smart constructor.
data DescribeAffectedEntitiesForOrganizationResponse = DescribeAffectedEntitiesForOrganizationResponse'
  { -- | A JSON set of elements including the @awsAccountId@ and its @entityArn@,
    -- @entityValue@ and its @entityArn@, @lastUpdatedTime@, and @statusCode@.
    entities :: Prelude.Maybe [AffectedEntity],
    -- | A JSON set of elements of the failed response, including the
    -- @awsAccountId@, @errorMessage@, @errorName@, and @eventArn@.
    failedSet :: Prelude.Maybe [OrganizationAffectedEntitiesErrorItem],
    -- | If the results of a search are large, only a portion of the results are
    -- returned, and a @nextToken@ pagination token is returned in the
    -- response. To retrieve the next batch of results, reissue the search
    -- request and include the returned token. When all results have been
    -- returned, the response does not contain a pagination token value.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'entities', 'describeAffectedEntitiesForOrganizationResponse_entities' - A JSON set of elements including the @awsAccountId@ and its @entityArn@,
-- @entityValue@ and its @entityArn@, @lastUpdatedTime@, and @statusCode@.
--
-- 'failedSet', 'describeAffectedEntitiesForOrganizationResponse_failedSet' - A JSON set of elements of the failed response, including the
-- @awsAccountId@, @errorMessage@, @errorName@, and @eventArn@.
--
-- 'nextToken', 'describeAffectedEntitiesForOrganizationResponse_nextToken' - If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
--
-- 'httpStatus', 'describeAffectedEntitiesForOrganizationResponse_httpStatus' - The response's http status code.
newDescribeAffectedEntitiesForOrganizationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAffectedEntitiesForOrganizationResponse
newDescribeAffectedEntitiesForOrganizationResponse
  pHttpStatus_ =
    DescribeAffectedEntitiesForOrganizationResponse'
      { entities =
          Prelude.Nothing,
        failedSet =
          Prelude.Nothing,
        nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A JSON set of elements including the @awsAccountId@ and its @entityArn@,
-- @entityValue@ and its @entityArn@, @lastUpdatedTime@, and @statusCode@.
describeAffectedEntitiesForOrganizationResponse_entities :: Lens.Lens' DescribeAffectedEntitiesForOrganizationResponse (Prelude.Maybe [AffectedEntity])
describeAffectedEntitiesForOrganizationResponse_entities = Lens.lens (\DescribeAffectedEntitiesForOrganizationResponse' {entities} -> entities) (\s@DescribeAffectedEntitiesForOrganizationResponse' {} a -> s {entities = a} :: DescribeAffectedEntitiesForOrganizationResponse) Prelude.. Lens.mapping Lens.coerced

-- | A JSON set of elements of the failed response, including the
-- @awsAccountId@, @errorMessage@, @errorName@, and @eventArn@.
describeAffectedEntitiesForOrganizationResponse_failedSet :: Lens.Lens' DescribeAffectedEntitiesForOrganizationResponse (Prelude.Maybe [OrganizationAffectedEntitiesErrorItem])
describeAffectedEntitiesForOrganizationResponse_failedSet = Lens.lens (\DescribeAffectedEntitiesForOrganizationResponse' {failedSet} -> failedSet) (\s@DescribeAffectedEntitiesForOrganizationResponse' {} a -> s {failedSet = a} :: DescribeAffectedEntitiesForOrganizationResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
describeAffectedEntitiesForOrganizationResponse_nextToken :: Lens.Lens' DescribeAffectedEntitiesForOrganizationResponse (Prelude.Maybe Prelude.Text)
describeAffectedEntitiesForOrganizationResponse_nextToken = Lens.lens (\DescribeAffectedEntitiesForOrganizationResponse' {nextToken} -> nextToken) (\s@DescribeAffectedEntitiesForOrganizationResponse' {} a -> s {nextToken = a} :: DescribeAffectedEntitiesForOrganizationResponse)

-- | The response's http status code.
describeAffectedEntitiesForOrganizationResponse_httpStatus :: Lens.Lens' DescribeAffectedEntitiesForOrganizationResponse Prelude.Int
describeAffectedEntitiesForOrganizationResponse_httpStatus = Lens.lens (\DescribeAffectedEntitiesForOrganizationResponse' {httpStatus} -> httpStatus) (\s@DescribeAffectedEntitiesForOrganizationResponse' {} a -> s {httpStatus = a} :: DescribeAffectedEntitiesForOrganizationResponse)

instance
  Prelude.NFData
    DescribeAffectedEntitiesForOrganizationResponse
  where
  rnf
    DescribeAffectedEntitiesForOrganizationResponse' {..} =
      Prelude.rnf entities
        `Prelude.seq` Prelude.rnf failedSet
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf httpStatus
