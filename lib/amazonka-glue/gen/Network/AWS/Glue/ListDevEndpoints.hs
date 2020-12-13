{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.ListDevEndpoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the names of all @DevEndpoint@ resources in this AWS account, or the resources with the specified tag. This operation allows you to see which resources are available in your account, and their names.
--
-- This operation takes the optional @Tags@ field, which you can use as a filter on the response so that tagged resources can be retrieved as a group. If you choose to use tags filtering, only resources with the tag are retrieved.
module Network.AWS.Glue.ListDevEndpoints
  ( -- * Creating a request
    ListDevEndpoints (..),
    mkListDevEndpoints,

    -- ** Request lenses
    ldeNextToken,
    ldeMaxResults,
    ldeTags,

    -- * Destructuring the response
    ListDevEndpointsResponse (..),
    mkListDevEndpointsResponse,

    -- ** Response lenses
    ldersNextToken,
    ldersDevEndpointNames,
    ldersResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListDevEndpoints' smart constructor.
data ListDevEndpoints = ListDevEndpoints'
  { -- | A continuation token, if this is a continuation request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum size of a list to return.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | Specifies to return only these tagged resources.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDevEndpoints' with the minimum fields required to make a request.
--
-- * 'nextToken' - A continuation token, if this is a continuation request.
-- * 'maxResults' - The maximum size of a list to return.
-- * 'tags' - Specifies to return only these tagged resources.
mkListDevEndpoints ::
  ListDevEndpoints
mkListDevEndpoints =
  ListDevEndpoints'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | A continuation token, if this is a continuation request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldeNextToken :: Lens.Lens' ListDevEndpoints (Lude.Maybe Lude.Text)
ldeNextToken = Lens.lens (nextToken :: ListDevEndpoints -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDevEndpoints)
{-# DEPRECATED ldeNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum size of a list to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldeMaxResults :: Lens.Lens' ListDevEndpoints (Lude.Maybe Lude.Natural)
ldeMaxResults = Lens.lens (maxResults :: ListDevEndpoints -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListDevEndpoints)
{-# DEPRECATED ldeMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Specifies to return only these tagged resources.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldeTags :: Lens.Lens' ListDevEndpoints (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ldeTags = Lens.lens (tags :: ListDevEndpoints -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: ListDevEndpoints)
{-# DEPRECATED ldeTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest ListDevEndpoints where
  type Rs ListDevEndpoints = ListDevEndpointsResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDevEndpointsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "DevEndpointNames" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDevEndpoints where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.ListDevEndpoints" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListDevEndpoints where
  toJSON ListDevEndpoints' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath ListDevEndpoints where
  toPath = Lude.const "/"

instance Lude.ToQuery ListDevEndpoints where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListDevEndpointsResponse' smart constructor.
data ListDevEndpointsResponse = ListDevEndpointsResponse'
  { -- | A continuation token, if the returned list does not contain the last metric available.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The names of all the @DevEndpoint@ s in the account, or the @DevEndpoint@ s with the specified tags.
    devEndpointNames :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDevEndpointsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A continuation token, if the returned list does not contain the last metric available.
-- * 'devEndpointNames' - The names of all the @DevEndpoint@ s in the account, or the @DevEndpoint@ s with the specified tags.
-- * 'responseStatus' - The response status code.
mkListDevEndpointsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDevEndpointsResponse
mkListDevEndpointsResponse pResponseStatus_ =
  ListDevEndpointsResponse'
    { nextToken = Lude.Nothing,
      devEndpointNames = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A continuation token, if the returned list does not contain the last metric available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldersNextToken :: Lens.Lens' ListDevEndpointsResponse (Lude.Maybe Lude.Text)
ldersNextToken = Lens.lens (nextToken :: ListDevEndpointsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDevEndpointsResponse)
{-# DEPRECATED ldersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The names of all the @DevEndpoint@ s in the account, or the @DevEndpoint@ s with the specified tags.
--
-- /Note:/ Consider using 'devEndpointNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldersDevEndpointNames :: Lens.Lens' ListDevEndpointsResponse (Lude.Maybe [Lude.Text])
ldersDevEndpointNames = Lens.lens (devEndpointNames :: ListDevEndpointsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {devEndpointNames = a} :: ListDevEndpointsResponse)
{-# DEPRECATED ldersDevEndpointNames "Use generic-lens or generic-optics with 'devEndpointNames' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldersResponseStatus :: Lens.Lens' ListDevEndpointsResponse Lude.Int
ldersResponseStatus = Lens.lens (responseStatus :: ListDevEndpointsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDevEndpointsResponse)
{-# DEPRECATED ldersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
