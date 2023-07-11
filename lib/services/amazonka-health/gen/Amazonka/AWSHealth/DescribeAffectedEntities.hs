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
-- Module      : Amazonka.AWSHealth.DescribeAffectedEntities
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of entities that have been affected by the specified
-- events, based on the specified filter criteria. Entities can refer to
-- individual customer resources, groups of customer resources, or any
-- other construct, depending on the Amazon Web Services service. Events
-- that have impact beyond that of the affected entities, or where the
-- extent of impact is unknown, include at least one entity indicating
-- this.
--
-- At least one event ARN is required.
--
-- -   This API operation uses pagination. Specify the @nextToken@
--     parameter in the next request to return more results.
--
-- -   This operation supports resource-level permissions. You can use this
--     operation to allow or deny access to specific Health events. For
--     more information, see
--     <https://docs.aws.amazon.com/health/latest/ug/security_iam_id-based-policy-examples.html#resource-action-based-conditions Resource- and action-based conditions>
--     in the /Health User Guide/.
--
-- This operation returns paginated results.
module Amazonka.AWSHealth.DescribeAffectedEntities
  ( -- * Creating a Request
    DescribeAffectedEntities (..),
    newDescribeAffectedEntities,

    -- * Request Lenses
    describeAffectedEntities_locale,
    describeAffectedEntities_maxResults,
    describeAffectedEntities_nextToken,
    describeAffectedEntities_filter,

    -- * Destructuring the Response
    DescribeAffectedEntitiesResponse (..),
    newDescribeAffectedEntitiesResponse,

    -- * Response Lenses
    describeAffectedEntitiesResponse_entities,
    describeAffectedEntitiesResponse_nextToken,
    describeAffectedEntitiesResponse_httpStatus,
  )
where

import Amazonka.AWSHealth.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAffectedEntities' smart constructor.
data DescribeAffectedEntities = DescribeAffectedEntities'
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
    -- | Values to narrow the results returned. At least one event ARN is
    -- required.
    filter' :: EntityFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAffectedEntities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locale', 'describeAffectedEntities_locale' - The locale (language) to return information in. English (en) is the
-- default and the only supported value at this time.
--
-- 'maxResults', 'describeAffectedEntities_maxResults' - The maximum number of items to return in one batch, between 10 and 100,
-- inclusive.
--
-- 'nextToken', 'describeAffectedEntities_nextToken' - If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
--
-- 'filter'', 'describeAffectedEntities_filter' - Values to narrow the results returned. At least one event ARN is
-- required.
newDescribeAffectedEntities ::
  -- | 'filter''
  EntityFilter ->
  DescribeAffectedEntities
newDescribeAffectedEntities pFilter_ =
  DescribeAffectedEntities'
    { locale = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      filter' = pFilter_
    }

-- | The locale (language) to return information in. English (en) is the
-- default and the only supported value at this time.
describeAffectedEntities_locale :: Lens.Lens' DescribeAffectedEntities (Prelude.Maybe Prelude.Text)
describeAffectedEntities_locale = Lens.lens (\DescribeAffectedEntities' {locale} -> locale) (\s@DescribeAffectedEntities' {} a -> s {locale = a} :: DescribeAffectedEntities)

-- | The maximum number of items to return in one batch, between 10 and 100,
-- inclusive.
describeAffectedEntities_maxResults :: Lens.Lens' DescribeAffectedEntities (Prelude.Maybe Prelude.Natural)
describeAffectedEntities_maxResults = Lens.lens (\DescribeAffectedEntities' {maxResults} -> maxResults) (\s@DescribeAffectedEntities' {} a -> s {maxResults = a} :: DescribeAffectedEntities)

-- | If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
describeAffectedEntities_nextToken :: Lens.Lens' DescribeAffectedEntities (Prelude.Maybe Prelude.Text)
describeAffectedEntities_nextToken = Lens.lens (\DescribeAffectedEntities' {nextToken} -> nextToken) (\s@DescribeAffectedEntities' {} a -> s {nextToken = a} :: DescribeAffectedEntities)

-- | Values to narrow the results returned. At least one event ARN is
-- required.
describeAffectedEntities_filter :: Lens.Lens' DescribeAffectedEntities EntityFilter
describeAffectedEntities_filter = Lens.lens (\DescribeAffectedEntities' {filter'} -> filter') (\s@DescribeAffectedEntities' {} a -> s {filter' = a} :: DescribeAffectedEntities)

instance Core.AWSPager DescribeAffectedEntities where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeAffectedEntitiesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeAffectedEntitiesResponse_entities
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeAffectedEntities_nextToken
          Lens..~ rs
          Lens.^? describeAffectedEntitiesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeAffectedEntities where
  type
    AWSResponse DescribeAffectedEntities =
      DescribeAffectedEntitiesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAffectedEntitiesResponse'
            Prelude.<$> (x Data..?> "entities" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAffectedEntities where
  hashWithSalt _salt DescribeAffectedEntities' {..} =
    _salt
      `Prelude.hashWithSalt` locale
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filter'

instance Prelude.NFData DescribeAffectedEntities where
  rnf DescribeAffectedEntities' {..} =
    Prelude.rnf locale
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filter'

instance Data.ToHeaders DescribeAffectedEntities where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHealth_20160804.DescribeAffectedEntities" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeAffectedEntities where
  toJSON DescribeAffectedEntities' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("locale" Data..=) Prelude.<$> locale,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("filter" Data..= filter')
          ]
      )

instance Data.ToPath DescribeAffectedEntities where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeAffectedEntities where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAffectedEntitiesResponse' smart constructor.
data DescribeAffectedEntitiesResponse = DescribeAffectedEntitiesResponse'
  { -- | The entities that match the filter criteria.
    entities :: Prelude.Maybe [AffectedEntity],
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
-- Create a value of 'DescribeAffectedEntitiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entities', 'describeAffectedEntitiesResponse_entities' - The entities that match the filter criteria.
--
-- 'nextToken', 'describeAffectedEntitiesResponse_nextToken' - If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
--
-- 'httpStatus', 'describeAffectedEntitiesResponse_httpStatus' - The response's http status code.
newDescribeAffectedEntitiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAffectedEntitiesResponse
newDescribeAffectedEntitiesResponse pHttpStatus_ =
  DescribeAffectedEntitiesResponse'
    { entities =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The entities that match the filter criteria.
describeAffectedEntitiesResponse_entities :: Lens.Lens' DescribeAffectedEntitiesResponse (Prelude.Maybe [AffectedEntity])
describeAffectedEntitiesResponse_entities = Lens.lens (\DescribeAffectedEntitiesResponse' {entities} -> entities) (\s@DescribeAffectedEntitiesResponse' {} a -> s {entities = a} :: DescribeAffectedEntitiesResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
describeAffectedEntitiesResponse_nextToken :: Lens.Lens' DescribeAffectedEntitiesResponse (Prelude.Maybe Prelude.Text)
describeAffectedEntitiesResponse_nextToken = Lens.lens (\DescribeAffectedEntitiesResponse' {nextToken} -> nextToken) (\s@DescribeAffectedEntitiesResponse' {} a -> s {nextToken = a} :: DescribeAffectedEntitiesResponse)

-- | The response's http status code.
describeAffectedEntitiesResponse_httpStatus :: Lens.Lens' DescribeAffectedEntitiesResponse Prelude.Int
describeAffectedEntitiesResponse_httpStatus = Lens.lens (\DescribeAffectedEntitiesResponse' {httpStatus} -> httpStatus) (\s@DescribeAffectedEntitiesResponse' {} a -> s {httpStatus = a} :: DescribeAffectedEntitiesResponse)

instance
  Prelude.NFData
    DescribeAffectedEntitiesResponse
  where
  rnf DescribeAffectedEntitiesResponse' {..} =
    Prelude.rnf entities
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
