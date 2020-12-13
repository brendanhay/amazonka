{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.ListAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the attributes for Amazon ECS resources within a specified target type and cluster. When you specify a target type and cluster, @ListAttributes@ returns a list of attribute objects, one for each attribute on each resource. You can filter the list of results to a single attribute name to only return results that have that name. You can also filter the results by attribute name and value, for example, to see which container instances in a cluster are running a Linux AMI (@ecs.os-type=linux@ ).
--
-- This operation returns paginated results.
module Network.AWS.ECS.ListAttributes
  ( -- * Creating a request
    ListAttributes (..),
    mkListAttributes,

    -- ** Request lenses
    laAttributeValue,
    laCluster,
    laTargetType,
    laNextToken,
    laAttributeName,
    laMaxResults,

    -- * Destructuring the response
    ListAttributesResponse (..),
    mkListAttributesResponse,

    -- ** Response lenses
    larsNextToken,
    larsAttributes,
    larsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListAttributes' smart constructor.
data ListAttributes = ListAttributes'
  { -- | The value of the attribute with which to filter results. You must also specify an attribute name to use this parameter.
    attributeValue :: Lude.Maybe Lude.Text,
    -- | The short name or full Amazon Resource Name (ARN) of the cluster to list attributes. If you do not specify a cluster, the default cluster is assumed.
    cluster :: Lude.Maybe Lude.Text,
    -- | The type of the target with which to list attributes.
    targetType :: TargetType,
    -- | The @nextToken@ value returned from a @ListAttributes@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
    nextToken :: Lude.Maybe Lude.Text,
    -- | The name of the attribute with which to filter the results.
    attributeName :: Lude.Maybe Lude.Text,
    -- | The maximum number of cluster results returned by @ListAttributes@ in paginated output. When this parameter is used, @ListAttributes@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListAttributes@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListAttributes@ returns up to 100 results and a @nextToken@ value if applicable.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAttributes' with the minimum fields required to make a request.
--
-- * 'attributeValue' - The value of the attribute with which to filter results. You must also specify an attribute name to use this parameter.
-- * 'cluster' - The short name or full Amazon Resource Name (ARN) of the cluster to list attributes. If you do not specify a cluster, the default cluster is assumed.
-- * 'targetType' - The type of the target with which to list attributes.
-- * 'nextToken' - The @nextToken@ value returned from a @ListAttributes@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
-- * 'attributeName' - The name of the attribute with which to filter the results.
-- * 'maxResults' - The maximum number of cluster results returned by @ListAttributes@ in paginated output. When this parameter is used, @ListAttributes@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListAttributes@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListAttributes@ returns up to 100 results and a @nextToken@ value if applicable.
mkListAttributes ::
  -- | 'targetType'
  TargetType ->
  ListAttributes
mkListAttributes pTargetType_ =
  ListAttributes'
    { attributeValue = Lude.Nothing,
      cluster = Lude.Nothing,
      targetType = pTargetType_,
      nextToken = Lude.Nothing,
      attributeName = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The value of the attribute with which to filter results. You must also specify an attribute name to use this parameter.
--
-- /Note:/ Consider using 'attributeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laAttributeValue :: Lens.Lens' ListAttributes (Lude.Maybe Lude.Text)
laAttributeValue = Lens.lens (attributeValue :: ListAttributes -> Lude.Maybe Lude.Text) (\s a -> s {attributeValue = a} :: ListAttributes)
{-# DEPRECATED laAttributeValue "Use generic-lens or generic-optics with 'attributeValue' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of the cluster to list attributes. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laCluster :: Lens.Lens' ListAttributes (Lude.Maybe Lude.Text)
laCluster = Lens.lens (cluster :: ListAttributes -> Lude.Maybe Lude.Text) (\s a -> s {cluster = a} :: ListAttributes)
{-# DEPRECATED laCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The type of the target with which to list attributes.
--
-- /Note:/ Consider using 'targetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laTargetType :: Lens.Lens' ListAttributes TargetType
laTargetType = Lens.lens (targetType :: ListAttributes -> TargetType) (\s a -> s {targetType = a} :: ListAttributes)
{-# DEPRECATED laTargetType "Use generic-lens or generic-optics with 'targetType' instead." #-}

-- | The @nextToken@ value returned from a @ListAttributes@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laNextToken :: Lens.Lens' ListAttributes (Lude.Maybe Lude.Text)
laNextToken = Lens.lens (nextToken :: ListAttributes -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAttributes)
{-# DEPRECATED laNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the attribute with which to filter the results.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laAttributeName :: Lens.Lens' ListAttributes (Lude.Maybe Lude.Text)
laAttributeName = Lens.lens (attributeName :: ListAttributes -> Lude.Maybe Lude.Text) (\s a -> s {attributeName = a} :: ListAttributes)
{-# DEPRECATED laAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

-- | The maximum number of cluster results returned by @ListAttributes@ in paginated output. When this parameter is used, @ListAttributes@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListAttributes@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListAttributes@ returns up to 100 results and a @nextToken@ value if applicable.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laMaxResults :: Lens.Lens' ListAttributes (Lude.Maybe Lude.Int)
laMaxResults = Lens.lens (maxResults :: ListAttributes -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListAttributes)
{-# DEPRECATED laMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListAttributes where
  page rq rs
    | Page.stop (rs Lens.^. larsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. larsAttributes) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& laNextToken Lens..~ rs Lens.^. larsNextToken

instance Lude.AWSRequest ListAttributes where
  type Rs ListAttributes = ListAttributesResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAttributesResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "attributes" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAttributes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.ListAttributes" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListAttributes where
  toJSON ListAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("attributeValue" Lude..=) Lude.<$> attributeValue,
            ("cluster" Lude..=) Lude.<$> cluster,
            Lude.Just ("targetType" Lude..= targetType),
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("attributeName" Lude..=) Lude.<$> attributeName,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery ListAttributes where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListAttributesResponse' smart constructor.
data ListAttributesResponse = ListAttributesResponse'
  { -- | The @nextToken@ value to include in a future @ListAttributes@ request. When the results of a @ListAttributes@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of attribute objects that meet the criteria of the request.
    attributes :: Lude.Maybe [Attribute],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAttributesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The @nextToken@ value to include in a future @ListAttributes@ request. When the results of a @ListAttributes@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'attributes' - A list of attribute objects that meet the criteria of the request.
-- * 'responseStatus' - The response status code.
mkListAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAttributesResponse
mkListAttributesResponse pResponseStatus_ =
  ListAttributesResponse'
    { nextToken = Lude.Nothing,
      attributes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @nextToken@ value to include in a future @ListAttributes@ request. When the results of a @ListAttributes@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsNextToken :: Lens.Lens' ListAttributesResponse (Lude.Maybe Lude.Text)
larsNextToken = Lens.lens (nextToken :: ListAttributesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAttributesResponse)
{-# DEPRECATED larsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of attribute objects that meet the criteria of the request.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsAttributes :: Lens.Lens' ListAttributesResponse (Lude.Maybe [Attribute])
larsAttributes = Lens.lens (attributes :: ListAttributesResponse -> Lude.Maybe [Attribute]) (\s a -> s {attributes = a} :: ListAttributesResponse)
{-# DEPRECATED larsAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsResponseStatus :: Lens.Lens' ListAttributesResponse Lude.Int
larsResponseStatus = Lens.lens (responseStatus :: ListAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAttributesResponse)
{-# DEPRECATED larsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
