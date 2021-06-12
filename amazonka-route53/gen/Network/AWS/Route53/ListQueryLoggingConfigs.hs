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
-- Module      : Network.AWS.Route53.ListQueryLoggingConfigs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the configurations for DNS query logging that are associated with
-- the current AWS account or the configuration that is associated with a
-- specified hosted zone.
--
-- For more information about DNS query logs, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateQueryLoggingConfig.html CreateQueryLoggingConfig>.
-- Additional information, including the format of DNS query logs, appears
-- in
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/query-logs.html Logging DNS Queries>
-- in the /Amazon Route 53 Developer Guide/.
--
-- This operation returns paginated results.
module Network.AWS.Route53.ListQueryLoggingConfigs
  ( -- * Creating a Request
    ListQueryLoggingConfigs (..),
    newListQueryLoggingConfigs,

    -- * Request Lenses
    listQueryLoggingConfigs_nextToken,
    listQueryLoggingConfigs_maxResults,
    listQueryLoggingConfigs_hostedZoneId,

    -- * Destructuring the Response
    ListQueryLoggingConfigsResponse (..),
    newListQueryLoggingConfigsResponse,

    -- * Response Lenses
    listQueryLoggingConfigsResponse_nextToken,
    listQueryLoggingConfigsResponse_httpStatus,
    listQueryLoggingConfigsResponse_queryLoggingConfigs,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53.Types

-- | /See:/ 'newListQueryLoggingConfigs' smart constructor.
data ListQueryLoggingConfigs = ListQueryLoggingConfigs'
  { -- | (Optional) If the current AWS account has more than @MaxResults@ query
    -- logging configurations, use @NextToken@ to get the second and subsequent
    -- pages of results.
    --
    -- For the first @ListQueryLoggingConfigs@ request, omit this value.
    --
    -- For the second and subsequent requests, get the value of @NextToken@
    -- from the previous response and specify that value for @NextToken@ in the
    -- request.
    nextToken :: Core.Maybe Core.Text,
    -- | (Optional) The maximum number of query logging configurations that you
    -- want Amazon Route 53 to return in response to the current request. If
    -- the current AWS account has more than @MaxResults@ configurations, use
    -- the value of
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ListQueryLoggingConfigs.html#API_ListQueryLoggingConfigs_RequestSyntax NextToken>
    -- in the response to get the next page of results.
    --
    -- If you don\'t specify a value for @MaxResults@, Route 53 returns up to
    -- 100 configurations.
    maxResults :: Core.Maybe Core.Text,
    -- | (Optional) If you want to list the query logging configuration that is
    -- associated with a hosted zone, specify the ID in @HostedZoneId@.
    --
    -- If you don\'t specify a hosted zone ID, @ListQueryLoggingConfigs@
    -- returns all of the configurations that are associated with the current
    -- AWS account.
    hostedZoneId :: Core.Maybe ResourceId
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListQueryLoggingConfigs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listQueryLoggingConfigs_nextToken' - (Optional) If the current AWS account has more than @MaxResults@ query
-- logging configurations, use @NextToken@ to get the second and subsequent
-- pages of results.
--
-- For the first @ListQueryLoggingConfigs@ request, omit this value.
--
-- For the second and subsequent requests, get the value of @NextToken@
-- from the previous response and specify that value for @NextToken@ in the
-- request.
--
-- 'maxResults', 'listQueryLoggingConfigs_maxResults' - (Optional) The maximum number of query logging configurations that you
-- want Amazon Route 53 to return in response to the current request. If
-- the current AWS account has more than @MaxResults@ configurations, use
-- the value of
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ListQueryLoggingConfigs.html#API_ListQueryLoggingConfigs_RequestSyntax NextToken>
-- in the response to get the next page of results.
--
-- If you don\'t specify a value for @MaxResults@, Route 53 returns up to
-- 100 configurations.
--
-- 'hostedZoneId', 'listQueryLoggingConfigs_hostedZoneId' - (Optional) If you want to list the query logging configuration that is
-- associated with a hosted zone, specify the ID in @HostedZoneId@.
--
-- If you don\'t specify a hosted zone ID, @ListQueryLoggingConfigs@
-- returns all of the configurations that are associated with the current
-- AWS account.
newListQueryLoggingConfigs ::
  ListQueryLoggingConfigs
newListQueryLoggingConfigs =
  ListQueryLoggingConfigs'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      hostedZoneId = Core.Nothing
    }

-- | (Optional) If the current AWS account has more than @MaxResults@ query
-- logging configurations, use @NextToken@ to get the second and subsequent
-- pages of results.
--
-- For the first @ListQueryLoggingConfigs@ request, omit this value.
--
-- For the second and subsequent requests, get the value of @NextToken@
-- from the previous response and specify that value for @NextToken@ in the
-- request.
listQueryLoggingConfigs_nextToken :: Lens.Lens' ListQueryLoggingConfigs (Core.Maybe Core.Text)
listQueryLoggingConfigs_nextToken = Lens.lens (\ListQueryLoggingConfigs' {nextToken} -> nextToken) (\s@ListQueryLoggingConfigs' {} a -> s {nextToken = a} :: ListQueryLoggingConfigs)

-- | (Optional) The maximum number of query logging configurations that you
-- want Amazon Route 53 to return in response to the current request. If
-- the current AWS account has more than @MaxResults@ configurations, use
-- the value of
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ListQueryLoggingConfigs.html#API_ListQueryLoggingConfigs_RequestSyntax NextToken>
-- in the response to get the next page of results.
--
-- If you don\'t specify a value for @MaxResults@, Route 53 returns up to
-- 100 configurations.
listQueryLoggingConfigs_maxResults :: Lens.Lens' ListQueryLoggingConfigs (Core.Maybe Core.Text)
listQueryLoggingConfigs_maxResults = Lens.lens (\ListQueryLoggingConfigs' {maxResults} -> maxResults) (\s@ListQueryLoggingConfigs' {} a -> s {maxResults = a} :: ListQueryLoggingConfigs)

-- | (Optional) If you want to list the query logging configuration that is
-- associated with a hosted zone, specify the ID in @HostedZoneId@.
--
-- If you don\'t specify a hosted zone ID, @ListQueryLoggingConfigs@
-- returns all of the configurations that are associated with the current
-- AWS account.
listQueryLoggingConfigs_hostedZoneId :: Lens.Lens' ListQueryLoggingConfigs (Core.Maybe ResourceId)
listQueryLoggingConfigs_hostedZoneId = Lens.lens (\ListQueryLoggingConfigs' {hostedZoneId} -> hostedZoneId) (\s@ListQueryLoggingConfigs' {} a -> s {hostedZoneId = a} :: ListQueryLoggingConfigs)

instance Core.AWSPager ListQueryLoggingConfigs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listQueryLoggingConfigsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^. listQueryLoggingConfigsResponse_queryLoggingConfigs
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listQueryLoggingConfigs_nextToken
          Lens..~ rs
          Lens.^? listQueryLoggingConfigsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListQueryLoggingConfigs where
  type
    AWSResponse ListQueryLoggingConfigs =
      ListQueryLoggingConfigsResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ListQueryLoggingConfigsResponse'
            Core.<$> (x Core..@? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..@? "QueryLoggingConfigs"
                         Core..!@ Core.mempty
                         Core.>>= Core.parseXMLList "QueryLoggingConfig"
                     )
      )

instance Core.Hashable ListQueryLoggingConfigs

instance Core.NFData ListQueryLoggingConfigs

instance Core.ToHeaders ListQueryLoggingConfigs where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListQueryLoggingConfigs where
  toPath = Core.const "/2013-04-01/queryloggingconfig"

instance Core.ToQuery ListQueryLoggingConfigs where
  toQuery ListQueryLoggingConfigs' {..} =
    Core.mconcat
      [ "nexttoken" Core.=: nextToken,
        "maxresults" Core.=: maxResults,
        "hostedzoneid" Core.=: hostedZoneId
      ]

-- | /See:/ 'newListQueryLoggingConfigsResponse' smart constructor.
data ListQueryLoggingConfigsResponse = ListQueryLoggingConfigsResponse'
  { -- | If a response includes the last of the query logging configurations that
    -- are associated with the current AWS account, @NextToken@ doesn\'t appear
    -- in the response.
    --
    -- If a response doesn\'t include the last of the configurations, you can
    -- get more configurations by submitting another
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ListQueryLoggingConfigs.html ListQueryLoggingConfigs>
    -- request. Get the value of @NextToken@ that Amazon Route 53 returned in
    -- the previous response and include it in @NextToken@ in the next request.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | An array that contains one
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_QueryLoggingConfig.html QueryLoggingConfig>
    -- element for each configuration for DNS query logging that is associated
    -- with the current AWS account.
    queryLoggingConfigs :: [QueryLoggingConfig]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListQueryLoggingConfigsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listQueryLoggingConfigsResponse_nextToken' - If a response includes the last of the query logging configurations that
-- are associated with the current AWS account, @NextToken@ doesn\'t appear
-- in the response.
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
-- with the current AWS account.
newListQueryLoggingConfigsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListQueryLoggingConfigsResponse
newListQueryLoggingConfigsResponse pHttpStatus_ =
  ListQueryLoggingConfigsResponse'
    { nextToken =
        Core.Nothing,
      httpStatus = pHttpStatus_,
      queryLoggingConfigs = Core.mempty
    }

-- | If a response includes the last of the query logging configurations that
-- are associated with the current AWS account, @NextToken@ doesn\'t appear
-- in the response.
--
-- If a response doesn\'t include the last of the configurations, you can
-- get more configurations by submitting another
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ListQueryLoggingConfigs.html ListQueryLoggingConfigs>
-- request. Get the value of @NextToken@ that Amazon Route 53 returned in
-- the previous response and include it in @NextToken@ in the next request.
listQueryLoggingConfigsResponse_nextToken :: Lens.Lens' ListQueryLoggingConfigsResponse (Core.Maybe Core.Text)
listQueryLoggingConfigsResponse_nextToken = Lens.lens (\ListQueryLoggingConfigsResponse' {nextToken} -> nextToken) (\s@ListQueryLoggingConfigsResponse' {} a -> s {nextToken = a} :: ListQueryLoggingConfigsResponse)

-- | The response's http status code.
listQueryLoggingConfigsResponse_httpStatus :: Lens.Lens' ListQueryLoggingConfigsResponse Core.Int
listQueryLoggingConfigsResponse_httpStatus = Lens.lens (\ListQueryLoggingConfigsResponse' {httpStatus} -> httpStatus) (\s@ListQueryLoggingConfigsResponse' {} a -> s {httpStatus = a} :: ListQueryLoggingConfigsResponse)

-- | An array that contains one
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_QueryLoggingConfig.html QueryLoggingConfig>
-- element for each configuration for DNS query logging that is associated
-- with the current AWS account.
listQueryLoggingConfigsResponse_queryLoggingConfigs :: Lens.Lens' ListQueryLoggingConfigsResponse [QueryLoggingConfig]
listQueryLoggingConfigsResponse_queryLoggingConfigs = Lens.lens (\ListQueryLoggingConfigsResponse' {queryLoggingConfigs} -> queryLoggingConfigs) (\s@ListQueryLoggingConfigsResponse' {} a -> s {queryLoggingConfigs = a} :: ListQueryLoggingConfigsResponse) Core.. Lens._Coerce

instance Core.NFData ListQueryLoggingConfigsResponse
