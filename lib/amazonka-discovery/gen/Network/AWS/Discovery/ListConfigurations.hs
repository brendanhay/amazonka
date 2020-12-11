{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.ListConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of configuration items as specified by the value passed to the required parameter @configurationType@ . Optional filtering may be applied to refine search results.
--
-- This operation returns paginated results.
module Network.AWS.Discovery.ListConfigurations
  ( -- * Creating a request
    ListConfigurations (..),
    mkListConfigurations,

    -- ** Request lenses
    lcOrderBy,
    lcFilters,
    lcNextToken,
    lcMaxResults,
    lcConfigurationType,

    -- * Destructuring the response
    ListConfigurationsResponse (..),
    mkListConfigurationsResponse,

    -- ** Response lenses
    lcrsConfigurations,
    lcrsNextToken,
    lcrsResponseStatus,
  )
where

import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListConfigurations' smart constructor.
data ListConfigurations = ListConfigurations'
  { orderBy ::
      Lude.Maybe [OrderByElement],
    filters :: Lude.Maybe [Filter],
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Int,
    configurationType :: ConfigurationItemType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListConfigurations' with the minimum fields required to make a request.
--
-- * 'configurationType' - A valid configuration identified by Application Discovery Service.
-- * 'filters' - You can filter the request using various logical operators and a /key/ -/value/ format. For example:
--
-- @{"key": "serverType", "value": "webServer"}@
-- For a complete list of filter options and guidance about using them with this action, see <https://docs.aws.amazon.com/application-discovery/latest/userguide/discovery-api-queries.html#ListConfigurations Using the ListConfigurations Action> in the /AWS Application Discovery Service User Guide/ .
-- * 'maxResults' - The total number of items to return. The maximum value is 100.
-- * 'nextToken' - Token to retrieve the next set of results. For example, if a previous call to ListConfigurations returned 100 items, but you set @ListConfigurationsRequest$maxResults@ to 10, you received a set of 10 results along with a token. Use that token in this query to get the next set of 10.
-- * 'orderBy' - Certain filter criteria return output that can be sorted in ascending or descending order. For a list of output characteristics for each filter, see <https://docs.aws.amazon.com/application-discovery/latest/userguide/discovery-api-queries.html#ListConfigurations Using the ListConfigurations Action> in the /AWS Application Discovery Service User Guide/ .
mkListConfigurations ::
  -- | 'configurationType'
  ConfigurationItemType ->
  ListConfigurations
mkListConfigurations pConfigurationType_ =
  ListConfigurations'
    { orderBy = Lude.Nothing,
      filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      configurationType = pConfigurationType_
    }

-- | Certain filter criteria return output that can be sorted in ascending or descending order. For a list of output characteristics for each filter, see <https://docs.aws.amazon.com/application-discovery/latest/userguide/discovery-api-queries.html#ListConfigurations Using the ListConfigurations Action> in the /AWS Application Discovery Service User Guide/ .
--
-- /Note:/ Consider using 'orderBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcOrderBy :: Lens.Lens' ListConfigurations (Lude.Maybe [OrderByElement])
lcOrderBy = Lens.lens (orderBy :: ListConfigurations -> Lude.Maybe [OrderByElement]) (\s a -> s {orderBy = a} :: ListConfigurations)
{-# DEPRECATED lcOrderBy "Use generic-lens or generic-optics with 'orderBy' instead." #-}

-- | You can filter the request using various logical operators and a /key/ -/value/ format. For example:
--
-- @{"key": "serverType", "value": "webServer"}@
-- For a complete list of filter options and guidance about using them with this action, see <https://docs.aws.amazon.com/application-discovery/latest/userguide/discovery-api-queries.html#ListConfigurations Using the ListConfigurations Action> in the /AWS Application Discovery Service User Guide/ .
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcFilters :: Lens.Lens' ListConfigurations (Lude.Maybe [Filter])
lcFilters = Lens.lens (filters :: ListConfigurations -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: ListConfigurations)
{-# DEPRECATED lcFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | Token to retrieve the next set of results. For example, if a previous call to ListConfigurations returned 100 items, but you set @ListConfigurationsRequest$maxResults@ to 10, you received a set of 10 results along with a token. Use that token in this query to get the next set of 10.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcNextToken :: Lens.Lens' ListConfigurations (Lude.Maybe Lude.Text)
lcNextToken = Lens.lens (nextToken :: ListConfigurations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListConfigurations)
{-# DEPRECATED lcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The total number of items to return. The maximum value is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcMaxResults :: Lens.Lens' ListConfigurations (Lude.Maybe Lude.Int)
lcMaxResults = Lens.lens (maxResults :: ListConfigurations -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListConfigurations)
{-# DEPRECATED lcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A valid configuration identified by Application Discovery Service.
--
-- /Note:/ Consider using 'configurationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcConfigurationType :: Lens.Lens' ListConfigurations ConfigurationItemType
lcConfigurationType = Lens.lens (configurationType :: ListConfigurations -> ConfigurationItemType) (\s a -> s {configurationType = a} :: ListConfigurations)
{-# DEPRECATED lcConfigurationType "Use generic-lens or generic-optics with 'configurationType' instead." #-}

instance Page.AWSPager ListConfigurations where
  page rq rs
    | Page.stop (rs Lens.^. lcrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lcrsConfigurations) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lcNextToken Lens..~ rs Lens.^. lcrsNextToken

instance Lude.AWSRequest ListConfigurations where
  type Rs ListConfigurations = ListConfigurationsResponse
  request = Req.postJSON discoveryService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListConfigurationsResponse'
            Lude.<$> (x Lude..?> "configurations" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListConfigurations where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSPoseidonService_V2015_11_01.ListConfigurations" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListConfigurations where
  toJSON ListConfigurations' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("orderBy" Lude..=) Lude.<$> orderBy,
            ("filters" Lude..=) Lude.<$> filters,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("configurationType" Lude..= configurationType)
          ]
      )

instance Lude.ToPath ListConfigurations where
  toPath = Lude.const "/"

instance Lude.ToQuery ListConfigurations where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListConfigurationsResponse' smart constructor.
data ListConfigurationsResponse = ListConfigurationsResponse'
  { configurations ::
      Lude.Maybe
        [ Lude.HashMap
            Lude.Text
            (Lude.Text)
        ],
    nextToken :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListConfigurationsResponse' with the minimum fields required to make a request.
--
-- * 'configurations' - Returns configuration details, including the configuration ID, attribute names, and attribute values.
-- * 'nextToken' - Token to retrieve the next set of results. For example, if your call to ListConfigurations returned 100 items, but you set @ListConfigurationsRequest$maxResults@ to 10, you received a set of 10 results along with this token. Use this token in the next query to retrieve the next set of 10.
-- * 'responseStatus' - The response status code.
mkListConfigurationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListConfigurationsResponse
mkListConfigurationsResponse pResponseStatus_ =
  ListConfigurationsResponse'
    { configurations = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns configuration details, including the configuration ID, attribute names, and attribute values.
--
-- /Note:/ Consider using 'configurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsConfigurations :: Lens.Lens' ListConfigurationsResponse (Lude.Maybe [Lude.HashMap Lude.Text (Lude.Text)])
lcrsConfigurations = Lens.lens (configurations :: ListConfigurationsResponse -> Lude.Maybe [Lude.HashMap Lude.Text (Lude.Text)]) (\s a -> s {configurations = a} :: ListConfigurationsResponse)
{-# DEPRECATED lcrsConfigurations "Use generic-lens or generic-optics with 'configurations' instead." #-}

-- | Token to retrieve the next set of results. For example, if your call to ListConfigurations returned 100 items, but you set @ListConfigurationsRequest$maxResults@ to 10, you received a set of 10 results along with this token. Use this token in the next query to retrieve the next set of 10.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsNextToken :: Lens.Lens' ListConfigurationsResponse (Lude.Maybe Lude.Text)
lcrsNextToken = Lens.lens (nextToken :: ListConfigurationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListConfigurationsResponse)
{-# DEPRECATED lcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsResponseStatus :: Lens.Lens' ListConfigurationsResponse Lude.Int
lcrsResponseStatus = Lens.lens (responseStatus :: ListConfigurationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListConfigurationsResponse)
{-# DEPRECATED lcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
