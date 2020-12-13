{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.ListCommands
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the commands requested by users of the AWS account.
--
-- This operation returns paginated results.
module Network.AWS.SSM.ListCommands
  ( -- * Creating a request
    ListCommands (..),
    mkListCommands,

    -- ** Request lenses
    lcInstanceId,
    lcFilters,
    lcNextToken,
    lcCommandId,
    lcMaxResults,

    -- * Destructuring the response
    ListCommandsResponse (..),
    mkListCommandsResponse,

    -- ** Response lenses
    lcrsCommands,
    lcrsNextToken,
    lcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkListCommands' smart constructor.
data ListCommands = ListCommands'
  { -- | (Optional) Lists commands issued against this instance ID.
    instanceId :: Lude.Maybe Lude.Text,
    -- | (Optional) One or more filters. Use a filter to return a more specific list of results.
    filters :: Lude.Maybe (Lude.NonEmpty CommandFilter),
    -- | (Optional) The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Lude.Maybe Lude.Text,
    -- | (Optional) If provided, lists only the specified command.
    commandId :: Lude.Maybe Lude.Text,
    -- | (Optional) The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCommands' with the minimum fields required to make a request.
--
-- * 'instanceId' - (Optional) Lists commands issued against this instance ID.
-- * 'filters' - (Optional) One or more filters. Use a filter to return a more specific list of results.
-- * 'nextToken' - (Optional) The token for the next set of items to return. (You received this token from a previous call.)
-- * 'commandId' - (Optional) If provided, lists only the specified command.
-- * 'maxResults' - (Optional) The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
mkListCommands ::
  ListCommands
mkListCommands =
  ListCommands'
    { instanceId = Lude.Nothing,
      filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      commandId = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | (Optional) Lists commands issued against this instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcInstanceId :: Lens.Lens' ListCommands (Lude.Maybe Lude.Text)
lcInstanceId = Lens.lens (instanceId :: ListCommands -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: ListCommands)
{-# DEPRECATED lcInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | (Optional) One or more filters. Use a filter to return a more specific list of results.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcFilters :: Lens.Lens' ListCommands (Lude.Maybe (Lude.NonEmpty CommandFilter))
lcFilters = Lens.lens (filters :: ListCommands -> Lude.Maybe (Lude.NonEmpty CommandFilter)) (\s a -> s {filters = a} :: ListCommands)
{-# DEPRECATED lcFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | (Optional) The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcNextToken :: Lens.Lens' ListCommands (Lude.Maybe Lude.Text)
lcNextToken = Lens.lens (nextToken :: ListCommands -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListCommands)
{-# DEPRECATED lcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | (Optional) If provided, lists only the specified command.
--
-- /Note:/ Consider using 'commandId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcCommandId :: Lens.Lens' ListCommands (Lude.Maybe Lude.Text)
lcCommandId = Lens.lens (commandId :: ListCommands -> Lude.Maybe Lude.Text) (\s a -> s {commandId = a} :: ListCommands)
{-# DEPRECATED lcCommandId "Use generic-lens or generic-optics with 'commandId' instead." #-}

-- | (Optional) The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcMaxResults :: Lens.Lens' ListCommands (Lude.Maybe Lude.Natural)
lcMaxResults = Lens.lens (maxResults :: ListCommands -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListCommands)
{-# DEPRECATED lcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListCommands where
  page rq rs
    | Page.stop (rs Lens.^. lcrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lcrsCommands) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lcNextToken Lens..~ rs Lens.^. lcrsNextToken

instance Lude.AWSRequest ListCommands where
  type Rs ListCommands = ListCommandsResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListCommandsResponse'
            Lude.<$> (x Lude..?> "Commands" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListCommands where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.ListCommands" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListCommands where
  toJSON ListCommands' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InstanceId" Lude..=) Lude.<$> instanceId,
            ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("CommandId" Lude..=) Lude.<$> commandId,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListCommands where
  toPath = Lude.const "/"

instance Lude.ToQuery ListCommands where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListCommandsResponse' smart constructor.
data ListCommandsResponse = ListCommandsResponse'
  { -- | (Optional) The list of commands requested by the user.
    commands :: Lude.Maybe [Command],
    -- | (Optional) The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCommandsResponse' with the minimum fields required to make a request.
--
-- * 'commands' - (Optional) The list of commands requested by the user.
-- * 'nextToken' - (Optional) The token for the next set of items to return. (You received this token from a previous call.)
-- * 'responseStatus' - The response status code.
mkListCommandsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListCommandsResponse
mkListCommandsResponse pResponseStatus_ =
  ListCommandsResponse'
    { commands = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | (Optional) The list of commands requested by the user.
--
-- /Note:/ Consider using 'commands' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsCommands :: Lens.Lens' ListCommandsResponse (Lude.Maybe [Command])
lcrsCommands = Lens.lens (commands :: ListCommandsResponse -> Lude.Maybe [Command]) (\s a -> s {commands = a} :: ListCommandsResponse)
{-# DEPRECATED lcrsCommands "Use generic-lens or generic-optics with 'commands' instead." #-}

-- | (Optional) The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsNextToken :: Lens.Lens' ListCommandsResponse (Lude.Maybe Lude.Text)
lcrsNextToken = Lens.lens (nextToken :: ListCommandsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListCommandsResponse)
{-# DEPRECATED lcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsResponseStatus :: Lens.Lens' ListCommandsResponse Lude.Int
lcrsResponseStatus = Lens.lens (responseStatus :: ListCommandsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListCommandsResponse)
{-# DEPRECATED lcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
