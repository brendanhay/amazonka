{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ListQueryLoggingConfigs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the configurations for DNS query logging that are associated with the current AWS account or the configuration that is associated with a specified hosted zone.
--
-- For more information about DNS query logs, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateQueryLoggingConfig.html CreateQueryLoggingConfig> . Additional information, including the format of DNS query logs, appears in <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/query-logs.html Logging DNS Queries> in the /Amazon Route 53 Developer Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.Route53.ListQueryLoggingConfigs
  ( -- * Creating a request
    ListQueryLoggingConfigs (..),
    mkListQueryLoggingConfigs,

    -- ** Request lenses
    lqlcHostedZoneId,
    lqlcNextToken,
    lqlcMaxResults,

    -- * Destructuring the response
    ListQueryLoggingConfigsResponse (..),
    mkListQueryLoggingConfigsResponse,

    -- ** Response lenses
    lqlcrsNextToken,
    lqlcrsResponseStatus,
    lqlcrsQueryLoggingConfigs,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | /See:/ 'mkListQueryLoggingConfigs' smart constructor.
data ListQueryLoggingConfigs = ListQueryLoggingConfigs'
  { hostedZoneId ::
      Lude.Maybe ResourceId,
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListQueryLoggingConfigs' with the minimum fields required to make a request.
--
-- * 'hostedZoneId' - (Optional) If you want to list the query logging configuration that is associated with a hosted zone, specify the ID in @HostedZoneId@ .
--
-- If you don't specify a hosted zone ID, @ListQueryLoggingConfigs@ returns all of the configurations that are associated with the current AWS account.
-- * 'maxResults' - (Optional) The maximum number of query logging configurations that you want Amazon Route 53 to return in response to the current request. If the current AWS account has more than @MaxResults@ configurations, use the value of <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ListQueryLoggingConfigs.html#API_ListQueryLoggingConfigs_RequestSyntax NextToken> in the response to get the next page of results.
--
-- If you don't specify a value for @MaxResults@ , Route 53 returns up to 100 configurations.
-- * 'nextToken' - (Optional) If the current AWS account has more than @MaxResults@ query logging configurations, use @NextToken@ to get the second and subsequent pages of results.
--
-- For the first @ListQueryLoggingConfigs@ request, omit this value.
-- For the second and subsequent requests, get the value of @NextToken@ from the previous response and specify that value for @NextToken@ in the request.
mkListQueryLoggingConfigs ::
  ListQueryLoggingConfigs
mkListQueryLoggingConfigs =
  ListQueryLoggingConfigs'
    { hostedZoneId = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | (Optional) If you want to list the query logging configuration that is associated with a hosted zone, specify the ID in @HostedZoneId@ .
--
-- If you don't specify a hosted zone ID, @ListQueryLoggingConfigs@ returns all of the configurations that are associated with the current AWS account.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqlcHostedZoneId :: Lens.Lens' ListQueryLoggingConfigs (Lude.Maybe ResourceId)
lqlcHostedZoneId = Lens.lens (hostedZoneId :: ListQueryLoggingConfigs -> Lude.Maybe ResourceId) (\s a -> s {hostedZoneId = a} :: ListQueryLoggingConfigs)
{-# DEPRECATED lqlcHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | (Optional) If the current AWS account has more than @MaxResults@ query logging configurations, use @NextToken@ to get the second and subsequent pages of results.
--
-- For the first @ListQueryLoggingConfigs@ request, omit this value.
-- For the second and subsequent requests, get the value of @NextToken@ from the previous response and specify that value for @NextToken@ in the request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqlcNextToken :: Lens.Lens' ListQueryLoggingConfigs (Lude.Maybe Lude.Text)
lqlcNextToken = Lens.lens (nextToken :: ListQueryLoggingConfigs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListQueryLoggingConfigs)
{-# DEPRECATED lqlcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | (Optional) The maximum number of query logging configurations that you want Amazon Route 53 to return in response to the current request. If the current AWS account has more than @MaxResults@ configurations, use the value of <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ListQueryLoggingConfigs.html#API_ListQueryLoggingConfigs_RequestSyntax NextToken> in the response to get the next page of results.
--
-- If you don't specify a value for @MaxResults@ , Route 53 returns up to 100 configurations.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqlcMaxResults :: Lens.Lens' ListQueryLoggingConfigs (Lude.Maybe Lude.Text)
lqlcMaxResults = Lens.lens (maxResults :: ListQueryLoggingConfigs -> Lude.Maybe Lude.Text) (\s a -> s {maxResults = a} :: ListQueryLoggingConfigs)
{-# DEPRECATED lqlcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListQueryLoggingConfigs where
  page rq rs
    | Page.stop (rs Lens.^. lqlcrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lqlcrsQueryLoggingConfigs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lqlcNextToken Lens..~ rs Lens.^. lqlcrsNextToken

instance Lude.AWSRequest ListQueryLoggingConfigs where
  type Rs ListQueryLoggingConfigs = ListQueryLoggingConfigsResponse
  request = Req.get route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          ListQueryLoggingConfigsResponse'
            Lude.<$> (x Lude..@? "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> ( x Lude..@? "QueryLoggingConfigs" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "QueryLoggingConfig"
                     )
      )

instance Lude.ToHeaders ListQueryLoggingConfigs where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListQueryLoggingConfigs where
  toPath = Lude.const "/2013-04-01/queryloggingconfig"

instance Lude.ToQuery ListQueryLoggingConfigs where
  toQuery ListQueryLoggingConfigs' {..} =
    Lude.mconcat
      [ "hostedzoneid" Lude.=: hostedZoneId,
        "nexttoken" Lude.=: nextToken,
        "maxresults" Lude.=: maxResults
      ]

-- | /See:/ 'mkListQueryLoggingConfigsResponse' smart constructor.
data ListQueryLoggingConfigsResponse = ListQueryLoggingConfigsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    queryLoggingConfigs ::
      [QueryLoggingConfig]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListQueryLoggingConfigsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If a response includes the last of the query logging configurations that are associated with the current AWS account, @NextToken@ doesn't appear in the response.
--
-- If a response doesn't include the last of the configurations, you can get more configurations by submitting another <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ListQueryLoggingConfigs.html ListQueryLoggingConfigs> request. Get the value of @NextToken@ that Amazon Route 53 returned in the previous response and include it in @NextToken@ in the next request.
-- * 'queryLoggingConfigs' - An array that contains one <https://docs.aws.amazon.com/Route53/latest/APIReference/API_QueryLoggingConfig.html QueryLoggingConfig> element for each configuration for DNS query logging that is associated with the current AWS account.
-- * 'responseStatus' - The response status code.
mkListQueryLoggingConfigsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListQueryLoggingConfigsResponse
mkListQueryLoggingConfigsResponse pResponseStatus_ =
  ListQueryLoggingConfigsResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      queryLoggingConfigs = Lude.mempty
    }

-- | If a response includes the last of the query logging configurations that are associated with the current AWS account, @NextToken@ doesn't appear in the response.
--
-- If a response doesn't include the last of the configurations, you can get more configurations by submitting another <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ListQueryLoggingConfigs.html ListQueryLoggingConfigs> request. Get the value of @NextToken@ that Amazon Route 53 returned in the previous response and include it in @NextToken@ in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqlcrsNextToken :: Lens.Lens' ListQueryLoggingConfigsResponse (Lude.Maybe Lude.Text)
lqlcrsNextToken = Lens.lens (nextToken :: ListQueryLoggingConfigsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListQueryLoggingConfigsResponse)
{-# DEPRECATED lqlcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqlcrsResponseStatus :: Lens.Lens' ListQueryLoggingConfigsResponse Lude.Int
lqlcrsResponseStatus = Lens.lens (responseStatus :: ListQueryLoggingConfigsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListQueryLoggingConfigsResponse)
{-# DEPRECATED lqlcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | An array that contains one <https://docs.aws.amazon.com/Route53/latest/APIReference/API_QueryLoggingConfig.html QueryLoggingConfig> element for each configuration for DNS query logging that is associated with the current AWS account.
--
-- /Note:/ Consider using 'queryLoggingConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqlcrsQueryLoggingConfigs :: Lens.Lens' ListQueryLoggingConfigsResponse [QueryLoggingConfig]
lqlcrsQueryLoggingConfigs = Lens.lens (queryLoggingConfigs :: ListQueryLoggingConfigsResponse -> [QueryLoggingConfig]) (\s a -> s {queryLoggingConfigs = a} :: ListQueryLoggingConfigsResponse)
{-# DEPRECATED lqlcrsQueryLoggingConfigs "Use generic-lens or generic-optics with 'queryLoggingConfigs' instead." #-}
