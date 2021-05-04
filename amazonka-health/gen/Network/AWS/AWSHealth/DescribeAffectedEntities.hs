{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.AWSHealth.DescribeAffectedEntities
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of entities that have been affected by the specified
-- events, based on the specified filter criteria. Entities can refer to
-- individual customer resources, groups of customer resources, or any
-- other construct, depending on the AWS service. Events that have impact
-- beyond that of the affected entities, or where the extent of impact is
-- unknown, include at least one entity indicating this.
--
-- At least one event ARN is required. Results are sorted by the
-- @lastUpdatedTime@ of the entity, starting with the most recent.
--
-- -   This API operation uses pagination. Specify the @nextToken@
--     parameter in the next request to return more results.
--
-- -   This operation supports resource-level permissions. You can use this
--     operation to allow or deny access to specific AWS Health events. For
--     more information, see
--     <https://docs.aws.amazon.com/health/latest/ug/security_iam_id-based-policy-examples.html#resource-action-based-conditions Resource- and action-based conditions>
--     in the /AWS Health User Guide/.
--
-- This operation returns paginated results.
module Network.AWS.AWSHealth.DescribeAffectedEntities
  ( -- * Creating a Request
    DescribeAffectedEntities (..),
    newDescribeAffectedEntities,

    -- * Request Lenses
    describeAffectedEntities_nextToken,
    describeAffectedEntities_maxResults,
    describeAffectedEntities_locale,
    describeAffectedEntities_filter,

    -- * Destructuring the Response
    DescribeAffectedEntitiesResponse (..),
    newDescribeAffectedEntitiesResponse,

    -- * Response Lenses
    describeAffectedEntitiesResponse_nextToken,
    describeAffectedEntitiesResponse_entities,
    describeAffectedEntitiesResponse_httpStatus,
  )
where

import Network.AWS.AWSHealth.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeAffectedEntities' smart constructor.
data DescribeAffectedEntities = DescribeAffectedEntities'
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
    -- | Values to narrow the results returned. At least one event ARN is
    -- required.
    filter' :: EntityFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeAffectedEntities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAffectedEntities_nextToken' - If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
--
-- 'maxResults', 'describeAffectedEntities_maxResults' - The maximum number of items to return in one batch, between 10 and 100,
-- inclusive.
--
-- 'locale', 'describeAffectedEntities_locale' - The locale (language) to return information in. English (en) is the
-- default and the only supported value at this time.
--
-- 'filter'', 'describeAffectedEntities_filter' - Values to narrow the results returned. At least one event ARN is
-- required.
newDescribeAffectedEntities ::
  -- | 'filter''
  EntityFilter ->
  DescribeAffectedEntities
newDescribeAffectedEntities pFilter_ =
  DescribeAffectedEntities'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      locale = Prelude.Nothing,
      filter' = pFilter_
    }

-- | If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
describeAffectedEntities_nextToken :: Lens.Lens' DescribeAffectedEntities (Prelude.Maybe Prelude.Text)
describeAffectedEntities_nextToken = Lens.lens (\DescribeAffectedEntities' {nextToken} -> nextToken) (\s@DescribeAffectedEntities' {} a -> s {nextToken = a} :: DescribeAffectedEntities)

-- | The maximum number of items to return in one batch, between 10 and 100,
-- inclusive.
describeAffectedEntities_maxResults :: Lens.Lens' DescribeAffectedEntities (Prelude.Maybe Prelude.Natural)
describeAffectedEntities_maxResults = Lens.lens (\DescribeAffectedEntities' {maxResults} -> maxResults) (\s@DescribeAffectedEntities' {} a -> s {maxResults = a} :: DescribeAffectedEntities)

-- | The locale (language) to return information in. English (en) is the
-- default and the only supported value at this time.
describeAffectedEntities_locale :: Lens.Lens' DescribeAffectedEntities (Prelude.Maybe Prelude.Text)
describeAffectedEntities_locale = Lens.lens (\DescribeAffectedEntities' {locale} -> locale) (\s@DescribeAffectedEntities' {} a -> s {locale = a} :: DescribeAffectedEntities)

-- | Values to narrow the results returned. At least one event ARN is
-- required.
describeAffectedEntities_filter :: Lens.Lens' DescribeAffectedEntities EntityFilter
describeAffectedEntities_filter = Lens.lens (\DescribeAffectedEntities' {filter'} -> filter') (\s@DescribeAffectedEntities' {} a -> s {filter' = a} :: DescribeAffectedEntities)

instance Pager.AWSPager DescribeAffectedEntities where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeAffectedEntitiesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeAffectedEntitiesResponse_entities
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeAffectedEntities_nextToken
          Lens..~ rs
          Lens.^? describeAffectedEntitiesResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeAffectedEntities where
  type
    Rs DescribeAffectedEntities =
      DescribeAffectedEntitiesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAffectedEntitiesResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> (x Prelude..?> "entities" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAffectedEntities

instance Prelude.NFData DescribeAffectedEntities

instance Prelude.ToHeaders DescribeAffectedEntities where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSHealth_20160804.DescribeAffectedEntities" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeAffectedEntities where
  toJSON DescribeAffectedEntities' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("nextToken" Prelude..=) Prelude.<$> nextToken,
            ("maxResults" Prelude..=) Prelude.<$> maxResults,
            ("locale" Prelude..=) Prelude.<$> locale,
            Prelude.Just ("filter" Prelude..= filter')
          ]
      )

instance Prelude.ToPath DescribeAffectedEntities where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeAffectedEntities where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAffectedEntitiesResponse' smart constructor.
data DescribeAffectedEntitiesResponse = DescribeAffectedEntitiesResponse'
  { -- | If the results of a search are large, only a portion of the results are
    -- returned, and a @nextToken@ pagination token is returned in the
    -- response. To retrieve the next batch of results, reissue the search
    -- request and include the returned token. When all results have been
    -- returned, the response does not contain a pagination token value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The entities that match the filter criteria.
    entities :: Prelude.Maybe [AffectedEntity],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeAffectedEntitiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAffectedEntitiesResponse_nextToken' - If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
--
-- 'entities', 'describeAffectedEntitiesResponse_entities' - The entities that match the filter criteria.
--
-- 'httpStatus', 'describeAffectedEntitiesResponse_httpStatus' - The response's http status code.
newDescribeAffectedEntitiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAffectedEntitiesResponse
newDescribeAffectedEntitiesResponse pHttpStatus_ =
  DescribeAffectedEntitiesResponse'
    { nextToken =
        Prelude.Nothing,
      entities = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
describeAffectedEntitiesResponse_nextToken :: Lens.Lens' DescribeAffectedEntitiesResponse (Prelude.Maybe Prelude.Text)
describeAffectedEntitiesResponse_nextToken = Lens.lens (\DescribeAffectedEntitiesResponse' {nextToken} -> nextToken) (\s@DescribeAffectedEntitiesResponse' {} a -> s {nextToken = a} :: DescribeAffectedEntitiesResponse)

-- | The entities that match the filter criteria.
describeAffectedEntitiesResponse_entities :: Lens.Lens' DescribeAffectedEntitiesResponse (Prelude.Maybe [AffectedEntity])
describeAffectedEntitiesResponse_entities = Lens.lens (\DescribeAffectedEntitiesResponse' {entities} -> entities) (\s@DescribeAffectedEntitiesResponse' {} a -> s {entities = a} :: DescribeAffectedEntitiesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeAffectedEntitiesResponse_httpStatus :: Lens.Lens' DescribeAffectedEntitiesResponse Prelude.Int
describeAffectedEntitiesResponse_httpStatus = Lens.lens (\DescribeAffectedEntitiesResponse' {httpStatus} -> httpStatus) (\s@DescribeAffectedEntitiesResponse' {} a -> s {httpStatus = a} :: DescribeAffectedEntitiesResponse)

instance
  Prelude.NFData
    DescribeAffectedEntitiesResponse
