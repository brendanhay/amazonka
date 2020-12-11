{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.ListAggregateDiscoveredResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a resource type and returns a list of resource identifiers that are aggregated for a specific resource type across accounts and regions. A resource identifier includes the resource type, ID, (if available) the custom resource name, source account, and source region. You can narrow the results to include only resources that have specific resource IDs, or a resource name, or source account ID, or source region.
--
-- For example, if the input consists of accountID 12345678910 and the region is us-east-1 for resource type @AWS::EC2::Instance@ then the API returns all the EC2 instance identifiers of accountID 12345678910 and region us-east-1.
--
-- This operation returns paginated results.
module Network.AWS.Config.ListAggregateDiscoveredResources
  ( -- * Creating a request
    ListAggregateDiscoveredResources (..),
    mkListAggregateDiscoveredResources,

    -- ** Request lenses
    ladrFilters,
    ladrNextToken,
    ladrLimit,
    ladrConfigurationAggregatorName,
    ladrResourceType,

    -- * Destructuring the response
    ListAggregateDiscoveredResourcesResponse (..),
    mkListAggregateDiscoveredResourcesResponse,

    -- ** Response lenses
    ladrrsNextToken,
    ladrrsResourceIdentifiers,
    ladrrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListAggregateDiscoveredResources' smart constructor.
data ListAggregateDiscoveredResources = ListAggregateDiscoveredResources'
  { filters ::
      Lude.Maybe
        ResourceFilters,
    nextToken ::
      Lude.Maybe Lude.Text,
    limit ::
      Lude.Maybe Lude.Natural,
    configurationAggregatorName ::
      Lude.Text,
    resourceType ::
      ResourceType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAggregateDiscoveredResources' with the minimum fields required to make a request.
--
-- * 'configurationAggregatorName' - The name of the configuration aggregator.
-- * 'filters' - Filters the results based on the @ResourceFilters@ object.
-- * 'limit' - The maximum number of resource identifiers returned on each page. The default is 100. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'resourceType' - The type of resources that you want AWS Config to list in the response.
mkListAggregateDiscoveredResources ::
  -- | 'configurationAggregatorName'
  Lude.Text ->
  -- | 'resourceType'
  ResourceType ->
  ListAggregateDiscoveredResources
mkListAggregateDiscoveredResources
  pConfigurationAggregatorName_
  pResourceType_ =
    ListAggregateDiscoveredResources'
      { filters = Lude.Nothing,
        nextToken = Lude.Nothing,
        limit = Lude.Nothing,
        configurationAggregatorName = pConfigurationAggregatorName_,
        resourceType = pResourceType_
      }

-- | Filters the results based on the @ResourceFilters@ object.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ladrFilters :: Lens.Lens' ListAggregateDiscoveredResources (Lude.Maybe ResourceFilters)
ladrFilters = Lens.lens (filters :: ListAggregateDiscoveredResources -> Lude.Maybe ResourceFilters) (\s a -> s {filters = a} :: ListAggregateDiscoveredResources)
{-# DEPRECATED ladrFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ladrNextToken :: Lens.Lens' ListAggregateDiscoveredResources (Lude.Maybe Lude.Text)
ladrNextToken = Lens.lens (nextToken :: ListAggregateDiscoveredResources -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAggregateDiscoveredResources)
{-# DEPRECATED ladrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of resource identifiers returned on each page. The default is 100. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ladrLimit :: Lens.Lens' ListAggregateDiscoveredResources (Lude.Maybe Lude.Natural)
ladrLimit = Lens.lens (limit :: ListAggregateDiscoveredResources -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListAggregateDiscoveredResources)
{-# DEPRECATED ladrLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The name of the configuration aggregator.
--
-- /Note:/ Consider using 'configurationAggregatorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ladrConfigurationAggregatorName :: Lens.Lens' ListAggregateDiscoveredResources Lude.Text
ladrConfigurationAggregatorName = Lens.lens (configurationAggregatorName :: ListAggregateDiscoveredResources -> Lude.Text) (\s a -> s {configurationAggregatorName = a} :: ListAggregateDiscoveredResources)
{-# DEPRECATED ladrConfigurationAggregatorName "Use generic-lens or generic-optics with 'configurationAggregatorName' instead." #-}

-- | The type of resources that you want AWS Config to list in the response.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ladrResourceType :: Lens.Lens' ListAggregateDiscoveredResources ResourceType
ladrResourceType = Lens.lens (resourceType :: ListAggregateDiscoveredResources -> ResourceType) (\s a -> s {resourceType = a} :: ListAggregateDiscoveredResources)
{-# DEPRECATED ladrResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

instance Page.AWSPager ListAggregateDiscoveredResources where
  page rq rs
    | Page.stop (rs Lens.^. ladrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ladrrsResourceIdentifiers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ladrNextToken Lens..~ rs Lens.^. ladrrsNextToken

instance Lude.AWSRequest ListAggregateDiscoveredResources where
  type
    Rs ListAggregateDiscoveredResources =
      ListAggregateDiscoveredResourcesResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAggregateDiscoveredResourcesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "ResourceIdentifiers" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAggregateDiscoveredResources where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.ListAggregateDiscoveredResources" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListAggregateDiscoveredResources where
  toJSON ListAggregateDiscoveredResources' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit,
            Lude.Just
              ( "ConfigurationAggregatorName"
                  Lude..= configurationAggregatorName
              ),
            Lude.Just ("ResourceType" Lude..= resourceType)
          ]
      )

instance Lude.ToPath ListAggregateDiscoveredResources where
  toPath = Lude.const "/"

instance Lude.ToQuery ListAggregateDiscoveredResources where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListAggregateDiscoveredResourcesResponse' smart constructor.
data ListAggregateDiscoveredResourcesResponse = ListAggregateDiscoveredResourcesResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    resourceIdentifiers ::
      Lude.Maybe
        [AggregateResourceIdentifier],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAggregateDiscoveredResourcesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'resourceIdentifiers' - Returns a list of @ResourceIdentifiers@ objects.
-- * 'responseStatus' - The response status code.
mkListAggregateDiscoveredResourcesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAggregateDiscoveredResourcesResponse
mkListAggregateDiscoveredResourcesResponse pResponseStatus_ =
  ListAggregateDiscoveredResourcesResponse'
    { nextToken =
        Lude.Nothing,
      resourceIdentifiers = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ladrrsNextToken :: Lens.Lens' ListAggregateDiscoveredResourcesResponse (Lude.Maybe Lude.Text)
ladrrsNextToken = Lens.lens (nextToken :: ListAggregateDiscoveredResourcesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAggregateDiscoveredResourcesResponse)
{-# DEPRECATED ladrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Returns a list of @ResourceIdentifiers@ objects.
--
-- /Note:/ Consider using 'resourceIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ladrrsResourceIdentifiers :: Lens.Lens' ListAggregateDiscoveredResourcesResponse (Lude.Maybe [AggregateResourceIdentifier])
ladrrsResourceIdentifiers = Lens.lens (resourceIdentifiers :: ListAggregateDiscoveredResourcesResponse -> Lude.Maybe [AggregateResourceIdentifier]) (\s a -> s {resourceIdentifiers = a} :: ListAggregateDiscoveredResourcesResponse)
{-# DEPRECATED ladrrsResourceIdentifiers "Use generic-lens or generic-optics with 'resourceIdentifiers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ladrrsResponseStatus :: Lens.Lens' ListAggregateDiscoveredResourcesResponse Lude.Int
ladrrsResponseStatus = Lens.lens (responseStatus :: ListAggregateDiscoveredResourcesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAggregateDiscoveredResourcesResponse)
{-# DEPRECATED ladrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
