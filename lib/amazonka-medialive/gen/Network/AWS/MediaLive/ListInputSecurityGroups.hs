{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.ListInputSecurityGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Produces a list of Input Security Groups for an account
--
-- This operation returns paginated results.
module Network.AWS.MediaLive.ListInputSecurityGroups
  ( -- * Creating a request
    ListInputSecurityGroups (..),
    mkListInputSecurityGroups,

    -- ** Request lenses
    lisgNextToken,
    lisgMaxResults,

    -- * Destructuring the response
    ListInputSecurityGroupsResponse (..),
    mkListInputSecurityGroupsResponse,

    -- ** Response lenses
    lisgrsNextToken,
    lisgrsInputSecurityGroups,
    lisgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Placeholder documentation for ListInputSecurityGroupsRequest
--
-- /See:/ 'mkListInputSecurityGroups' smart constructor.
data ListInputSecurityGroups = ListInputSecurityGroups'
  { nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListInputSecurityGroups' with the minimum fields required to make a request.
--
-- * 'nextToken' -
-- * 'maxResults' -
mkListInputSecurityGroups ::
  ListInputSecurityGroups
mkListInputSecurityGroups =
  ListInputSecurityGroups'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lisgNextToken :: Lens.Lens' ListInputSecurityGroups (Lude.Maybe Lude.Text)
lisgNextToken = Lens.lens (nextToken :: ListInputSecurityGroups -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListInputSecurityGroups)
{-# DEPRECATED lisgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lisgMaxResults :: Lens.Lens' ListInputSecurityGroups (Lude.Maybe Lude.Natural)
lisgMaxResults = Lens.lens (maxResults :: ListInputSecurityGroups -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListInputSecurityGroups)
{-# DEPRECATED lisgMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListInputSecurityGroups where
  page rq rs
    | Page.stop (rs Lens.^. lisgrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lisgrsInputSecurityGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lisgNextToken Lens..~ rs Lens.^. lisgrsNextToken

instance Lude.AWSRequest ListInputSecurityGroups where
  type Rs ListInputSecurityGroups = ListInputSecurityGroupsResponse
  request = Req.get mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListInputSecurityGroupsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "inputSecurityGroups" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListInputSecurityGroups where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListInputSecurityGroups where
  toPath = Lude.const "/prod/inputSecurityGroups"

instance Lude.ToQuery ListInputSecurityGroups where
  toQuery ListInputSecurityGroups' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | Placeholder documentation for ListInputSecurityGroupsResponse
--
-- /See:/ 'mkListInputSecurityGroupsResponse' smart constructor.
data ListInputSecurityGroupsResponse = ListInputSecurityGroupsResponse'
  { nextToken :: Lude.Maybe Lude.Text,
    -- | List of input security groups
    inputSecurityGroups :: Lude.Maybe [InputSecurityGroup],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListInputSecurityGroupsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' -
-- * 'inputSecurityGroups' - List of input security groups
-- * 'responseStatus' - The response status code.
mkListInputSecurityGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListInputSecurityGroupsResponse
mkListInputSecurityGroupsResponse pResponseStatus_ =
  ListInputSecurityGroupsResponse'
    { nextToken = Lude.Nothing,
      inputSecurityGroups = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lisgrsNextToken :: Lens.Lens' ListInputSecurityGroupsResponse (Lude.Maybe Lude.Text)
lisgrsNextToken = Lens.lens (nextToken :: ListInputSecurityGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListInputSecurityGroupsResponse)
{-# DEPRECATED lisgrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | List of input security groups
--
-- /Note:/ Consider using 'inputSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lisgrsInputSecurityGroups :: Lens.Lens' ListInputSecurityGroupsResponse (Lude.Maybe [InputSecurityGroup])
lisgrsInputSecurityGroups = Lens.lens (inputSecurityGroups :: ListInputSecurityGroupsResponse -> Lude.Maybe [InputSecurityGroup]) (\s a -> s {inputSecurityGroups = a} :: ListInputSecurityGroupsResponse)
{-# DEPRECATED lisgrsInputSecurityGroups "Use generic-lens or generic-optics with 'inputSecurityGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lisgrsResponseStatus :: Lens.Lens' ListInputSecurityGroupsResponse Lude.Int
lisgrsResponseStatus = Lens.lens (responseStatus :: ListInputSecurityGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListInputSecurityGroupsResponse)
{-# DEPRECATED lisgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
