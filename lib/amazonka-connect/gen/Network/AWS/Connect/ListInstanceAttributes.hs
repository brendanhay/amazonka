{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListInstanceAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of all attribute types for the given instance.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListInstanceAttributes
  ( -- * Creating a request
    ListInstanceAttributes (..),
    mkListInstanceAttributes,

    -- ** Request lenses
    liaInstanceId,
    liaNextToken,
    liaMaxResults,

    -- * Destructuring the response
    ListInstanceAttributesResponse (..),
    mkListInstanceAttributesResponse,

    -- ** Response lenses
    liarsNextToken,
    liarsAttributes,
    liarsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListInstanceAttributes' smart constructor.
data ListInstanceAttributes = ListInstanceAttributes'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Lude.Text,
    -- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximimum number of results to return per page.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListInstanceAttributes' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'nextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
-- * 'maxResults' - The maximimum number of results to return per page.
mkListInstanceAttributes ::
  -- | 'instanceId'
  Lude.Text ->
  ListInstanceAttributes
mkListInstanceAttributes pInstanceId_ =
  ListInstanceAttributes'
    { instanceId = pInstanceId_,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liaInstanceId :: Lens.Lens' ListInstanceAttributes Lude.Text
liaInstanceId = Lens.lens (instanceId :: ListInstanceAttributes -> Lude.Text) (\s a -> s {instanceId = a} :: ListInstanceAttributes)
{-# DEPRECATED liaInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liaNextToken :: Lens.Lens' ListInstanceAttributes (Lude.Maybe Lude.Text)
liaNextToken = Lens.lens (nextToken :: ListInstanceAttributes -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListInstanceAttributes)
{-# DEPRECATED liaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liaMaxResults :: Lens.Lens' ListInstanceAttributes (Lude.Maybe Lude.Natural)
liaMaxResults = Lens.lens (maxResults :: ListInstanceAttributes -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListInstanceAttributes)
{-# DEPRECATED liaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListInstanceAttributes where
  page rq rs
    | Page.stop (rs Lens.^. liarsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. liarsAttributes) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& liaNextToken Lens..~ rs Lens.^. liarsNextToken

instance Lude.AWSRequest ListInstanceAttributes where
  type Rs ListInstanceAttributes = ListInstanceAttributesResponse
  request = Req.get connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListInstanceAttributesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Attributes" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListInstanceAttributes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListInstanceAttributes where
  toPath ListInstanceAttributes' {..} =
    Lude.mconcat ["/instance/", Lude.toBS instanceId, "/attributes"]

instance Lude.ToQuery ListInstanceAttributes where
  toQuery ListInstanceAttributes' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListInstanceAttributesResponse' smart constructor.
data ListInstanceAttributesResponse = ListInstanceAttributesResponse'
  { -- | If there are additional results, this is the token for the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The attribute types.
    attributes :: Lude.Maybe [Attribute],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListInstanceAttributesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If there are additional results, this is the token for the next set of results.
-- * 'attributes' - The attribute types.
-- * 'responseStatus' - The response status code.
mkListInstanceAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListInstanceAttributesResponse
mkListInstanceAttributesResponse pResponseStatus_ =
  ListInstanceAttributesResponse'
    { nextToken = Lude.Nothing,
      attributes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liarsNextToken :: Lens.Lens' ListInstanceAttributesResponse (Lude.Maybe Lude.Text)
liarsNextToken = Lens.lens (nextToken :: ListInstanceAttributesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListInstanceAttributesResponse)
{-# DEPRECATED liarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The attribute types.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liarsAttributes :: Lens.Lens' ListInstanceAttributesResponse (Lude.Maybe [Attribute])
liarsAttributes = Lens.lens (attributes :: ListInstanceAttributesResponse -> Lude.Maybe [Attribute]) (\s a -> s {attributes = a} :: ListInstanceAttributesResponse)
{-# DEPRECATED liarsAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liarsResponseStatus :: Lens.Lens' ListInstanceAttributesResponse Lude.Int
liarsResponseStatus = Lens.lens (responseStatus :: ListInstanceAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListInstanceAttributesResponse)
{-# DEPRECATED liarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
