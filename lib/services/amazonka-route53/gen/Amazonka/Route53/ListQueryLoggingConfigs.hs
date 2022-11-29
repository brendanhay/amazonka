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
-- Module      : Amazonka.Route53.ListQueryLoggingConfigs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the configurations for DNS query logging that are associated with
-- the current Amazon Web Services account or the configuration that is
-- associated with a specified hosted zone.
--
-- For more information about DNS query logs, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateQueryLoggingConfig.html CreateQueryLoggingConfig>.
-- Additional information, including the format of DNS query logs, appears
-- in
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/query-logs.html Logging DNS Queries>
-- in the /Amazon Route 53 Developer Guide/.
--
-- This operation returns paginated results.
module Amazonka.Route53.ListQueryLoggingConfigs
  ( -- * Creating a Request
    ListQueryLoggingConfigs (..),
    newListQueryLoggingConfigs,

    -- * Request Lenses
    listQueryLoggingConfigs_hostedZoneId,
    listQueryLoggingConfigs_nextToken,
    listQueryLoggingConfigs_maxResults,

    -- * Destructuring the Response
    ListQueryLoggingConfigsResponse (..),
    newListQueryLoggingConfigsResponse,

    -- * Response Lenses
    listQueryLoggingConfigsResponse_nextToken,
    listQueryLoggingConfigsResponse_httpStatus,
    listQueryLoggingConfigsResponse_queryLoggingConfigs,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | /See:/ 'newListQueryLoggingConfigs' smart constructor.
data ListQueryLoggingConfigs = ListQueryLoggingConfigs'
  { -- | (Optional) If you want to list the query logging configuration that is
    -- associated with a hosted zone, specify the ID in @HostedZoneId@.
    --
    -- If you don\'t specify a hosted zone ID, @ListQueryLoggingConfigs@
    -- returns all of the configurations that are associated with the current
    -- Amazon Web Services account.
    hostedZoneId :: Prelude.Maybe ResourceId,
    -- | (Optional) If the current Amazon Web Services account has more than
    -- @MaxResults@ query logging configurations, use @NextToken@ to get the
    -- second and subsequent pages of results.
    --
    -- For the first @ListQueryLoggingConfigs@ request, omit this value.
    --
    -- For the second and subsequent requests, get the value of @NextToken@
    -- from the previous response and specify that value for @NextToken@ in the
    -- request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | (Optional) The maximum number of query logging configurations that you
    -- want Amazon Route 53 to return in response to the current request. If
    -- the current Amazon Web Services account has more than @MaxResults@
    -- configurations, use the value of
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ListQueryLoggingConfigs.html#API_ListQueryLoggingConfigs_RequestSyntax NextToken>
    -- in the response to get the next page of results.
    --
    -- If you don\'t specify a value for @MaxResults@, Route 53 returns up to
    -- 100 configurations.
    maxResults :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListQueryLoggingConfigs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostedZoneId', 'listQueryLoggingConfigs_hostedZoneId' - (Optional) If you want to list the query logging configuration that is
-- associated with a hosted zone, specify the ID in @HostedZoneId@.
--
-- If you don\'t specify a hosted zone ID, @ListQueryLoggingConfigs@
-- returns all of the configurations that are associated with the current
-- Amazon Web Services account.
--
-- 'nextToken', 'listQueryLoggingConfigs_nextToken' - (Optional) If the current Amazon Web Services account has more than
-- @MaxResults@ query logging configurations, use @NextToken@ to get the
-- second and subsequent pages of results.
--
-- For the first @ListQueryLoggingConfigs@ request, omit this value.
--
-- For the second and subsequent requests, get the value of @NextToken@
-- from the previous response and specify that value for @NextToken@ in the
-- request.
--
-- 'maxResults', 'listQueryLoggingConfigs_maxResults' - (Optional) The maximum number of query logging configurations that you
-- want Amazon Route 53 to return in response to the current request. If
-- the current Amazon Web Services account has more than @MaxResults@
-- configurations, use the value of
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ListQueryLoggingConfigs.html#API_ListQueryLoggingConfigs_RequestSyntax NextToken>
-- in the response to get the next page of results.
--
-- If you don\'t specify a value for @MaxResults@, Route 53 returns up to
-- 100 configurations.
newListQueryLoggingConfigs ::
  ListQueryLoggingConfigs
newListQueryLoggingConfigs =
  ListQueryLoggingConfigs'
    { hostedZoneId =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | (Optional) If you want to list the query logging configuration that is
-- associated with a hosted zone, specify the ID in @HostedZoneId@.
--
-- If you don\'t specify a hosted zone ID, @ListQueryLoggingConfigs@
-- returns all of the configurations that are associated with the current
-- Amazon Web Services account.
listQueryLoggingConfigs_hostedZoneId :: Lens.Lens' ListQueryLoggingConfigs (Prelude.Maybe ResourceId)
listQueryLoggingConfigs_hostedZoneId = Lens.lens (\ListQueryLoggingConfigs' {hostedZoneId} -> hostedZoneId) (\s@ListQueryLoggingConfigs' {} a -> s {hostedZoneId = a} :: ListQueryLoggingConfigs)

-- | (Optional) If the current Amazon Web Services account has more than
-- @MaxResults@ query logging configurations, use @NextToken@ to get the
-- second and subsequent pages of results.
--
-- For the first @ListQueryLoggingConfigs@ request, omit this value.
--
-- For the second and subsequent requests, get the value of @NextToken@
-- from the previous response and specify that value for @NextToken@ in the
-- request.
listQueryLoggingConfigs_nextToken :: Lens.Lens' ListQueryLoggingConfigs (Prelude.Maybe Prelude.Text)
listQueryLoggingConfigs_nextToken = Lens.lens (\ListQueryLoggingConfigs' {nextToken} -> nextToken) (\s@ListQueryLoggingConfigs' {} a -> s {nextToken = a} :: ListQueryLoggingConfigs)

-- | (Optional) The maximum number of query logging configurations that you
-- want Amazon Route 53 to return in response to the current request. If
-- the current Amazon Web Services account has more than @MaxResults@
-- configurations, use the value of
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ListQueryLoggingConfigs.html#API_ListQueryLoggingConfigs_RequestSyntax NextToken>
-- in the response to get the next page of results.
--
-- If you don\'t specify a value for @MaxResults@, Route 53 returns up to
-- 100 configurations.
listQueryLoggingConfigs_maxResults :: Lens.Lens' ListQueryLoggingConfigs (Prelude.Maybe Prelude.Text)
listQueryLoggingConfigs_maxResults = Lens.lens (\ListQueryLoggingConfigs' {maxResults} -> maxResults) (\s@ListQueryLoggingConfigs' {} a -> s {maxResults = a} :: ListQueryLoggingConfigs)

instance Core.AWSPager ListQueryLoggingConfigs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listQueryLoggingConfigsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listQueryLoggingConfigsResponse_queryLoggingConfigs
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listQueryLoggingConfigs_nextToken
          Lens..~ rs
          Lens.^? listQueryLoggingConfigsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListQueryLoggingConfigs where
  type
    AWSResponse ListQueryLoggingConfigs =
      ListQueryLoggingConfigsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListQueryLoggingConfigsResponse'
            Prelude.<$> (x Core..@? "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..@? "QueryLoggingConfigs"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.parseXMLList "QueryLoggingConfig"
                        )
      )

instance Prelude.Hashable ListQueryLoggingConfigs where
  hashWithSalt _salt ListQueryLoggingConfigs' {..} =
    _salt `Prelude.hashWithSalt` hostedZoneId
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListQueryLoggingConfigs where
  rnf ListQueryLoggingConfigs' {..} =
    Prelude.rnf hostedZoneId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListQueryLoggingConfigs where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListQueryLoggingConfigs where
  toPath =
    Prelude.const "/2013-04-01/queryloggingconfig"

instance Core.ToQuery ListQueryLoggingConfigs where
  toQuery ListQueryLoggingConfigs' {..} =
    Prelude.mconcat
      [ "hostedzoneid" Core.=: hostedZoneId,
        "nexttoken" Core.=: nextToken,
        "maxresults" Core.=: maxResults
      ]

-- | /See:/ 'newListQueryLoggingConfigsResponse' smart constructor.
data ListQueryLoggingConfigsResponse = ListQueryLoggingConfigsResponse'
  { -- | If a response includes the last of the query logging configurations that
    -- are associated with the current Amazon Web Services account, @NextToken@
    -- doesn\'t appear in the response.
    --
    -- If a response doesn\'t include the last of the configurations, you can
    -- get more configurations by submitting another
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ListQueryLoggingConfigs.html ListQueryLoggingConfigs>
    -- request. Get the value of @NextToken@ that Amazon Route 53 returned in
    -- the previous response and include it in @NextToken@ in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array that contains one
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_QueryLoggingConfig.html QueryLoggingConfig>
    -- element for each configuration for DNS query logging that is associated
    -- with the current Amazon Web Services account.
    queryLoggingConfigs :: [QueryLoggingConfig]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListQueryLoggingConfigsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listQueryLoggingConfigsResponse_nextToken' - If a response includes the last of the query logging configurations that
-- are associated with the current Amazon Web Services account, @NextToken@
-- doesn\'t appear in the response.
--
-- If a response doesn\'t include the last of the configurations, you can
-- get more configurations by submitting another
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ListQueryLoggingConfigs.html ListQueryLoggingConfigs>
-- request. Get the value of @NextToken@ that Amazon Route 53 returned in
-- the previous response and include it in @NextToken@ in the next request.
--
-- 'httpStatus', 'listQueryLoggingConfigsResponse_httpStatus' - The response's http status code.
--
-- 'queryLoggingConfigs', 'listQueryLoggingConfigsResponse_queryLoggingConfigs' - An array that contains one
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_QueryLoggingConfig.html QueryLoggingConfig>
-- element for each configuration for DNS query logging that is associated
-- with the current Amazon Web Services account.
newListQueryLoggingConfigsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListQueryLoggingConfigsResponse
newListQueryLoggingConfigsResponse pHttpStatus_ =
  ListQueryLoggingConfigsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      queryLoggingConfigs = Prelude.mempty
    }

-- | If a response includes the last of the query logging configurations that
-- are associated with the current Amazon Web Services account, @NextToken@
-- doesn\'t appear in the response.
--
-- If a response doesn\'t include the last of the configurations, you can
-- get more configurations by submitting another
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ListQueryLoggingConfigs.html ListQueryLoggingConfigs>
-- request. Get the value of @NextToken@ that Amazon Route 53 returned in
-- the previous response and include it in @NextToken@ in the next request.
listQueryLoggingConfigsResponse_nextToken :: Lens.Lens' ListQueryLoggingConfigsResponse (Prelude.Maybe Prelude.Text)
listQueryLoggingConfigsResponse_nextToken = Lens.lens (\ListQueryLoggingConfigsResponse' {nextToken} -> nextToken) (\s@ListQueryLoggingConfigsResponse' {} a -> s {nextToken = a} :: ListQueryLoggingConfigsResponse)

-- | The response's http status code.
listQueryLoggingConfigsResponse_httpStatus :: Lens.Lens' ListQueryLoggingConfigsResponse Prelude.Int
listQueryLoggingConfigsResponse_httpStatus = Lens.lens (\ListQueryLoggingConfigsResponse' {httpStatus} -> httpStatus) (\s@ListQueryLoggingConfigsResponse' {} a -> s {httpStatus = a} :: ListQueryLoggingConfigsResponse)

-- | An array that contains one
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_QueryLoggingConfig.html QueryLoggingConfig>
-- element for each configuration for DNS query logging that is associated
-- with the current Amazon Web Services account.
listQueryLoggingConfigsResponse_queryLoggingConfigs :: Lens.Lens' ListQueryLoggingConfigsResponse [QueryLoggingConfig]
listQueryLoggingConfigsResponse_queryLoggingConfigs = Lens.lens (\ListQueryLoggingConfigsResponse' {queryLoggingConfigs} -> queryLoggingConfigs) (\s@ListQueryLoggingConfigsResponse' {} a -> s {queryLoggingConfigs = a} :: ListQueryLoggingConfigsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListQueryLoggingConfigsResponse
  where
  rnf ListQueryLoggingConfigsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf queryLoggingConfigs
