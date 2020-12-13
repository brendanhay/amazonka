{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.ListCommandInvocations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- An invocation is copy of a command sent to a specific instance. A command can apply to one or more instances. A command invocation applies to one instance. For example, if a user runs SendCommand against three instances, then a command invocation is created for each requested instance ID. ListCommandInvocations provide status about command execution.
--
-- This operation returns paginated results.
module Network.AWS.SSM.ListCommandInvocations
  ( -- * Creating a request
    ListCommandInvocations (..),
    mkListCommandInvocations,

    -- ** Request lenses
    lciInstanceId,
    lciFilters,
    lciNextToken,
    lciCommandId,
    lciDetails,
    lciMaxResults,

    -- * Destructuring the response
    ListCommandInvocationsResponse (..),
    mkListCommandInvocationsResponse,

    -- ** Response lenses
    lrsNextToken,
    lrsCommandInvocations,
    lrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkListCommandInvocations' smart constructor.
data ListCommandInvocations = ListCommandInvocations'
  { -- | (Optional) The command execution details for a specific instance ID.
    instanceId :: Lude.Maybe Lude.Text,
    -- | (Optional) One or more filters. Use a filter to return a more specific list of results.
    filters :: Lude.Maybe (Lude.NonEmpty CommandFilter),
    -- | (Optional) The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Lude.Maybe Lude.Text,
    -- | (Optional) The invocations for a specific command ID.
    commandId :: Lude.Maybe Lude.Text,
    -- | (Optional) If set this returns the response of the command executions and any command output. By default this is set to False.
    details :: Lude.Maybe Lude.Bool,
    -- | (Optional) The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCommandInvocations' with the minimum fields required to make a request.
--
-- * 'instanceId' - (Optional) The command execution details for a specific instance ID.
-- * 'filters' - (Optional) One or more filters. Use a filter to return a more specific list of results.
-- * 'nextToken' - (Optional) The token for the next set of items to return. (You received this token from a previous call.)
-- * 'commandId' - (Optional) The invocations for a specific command ID.
-- * 'details' - (Optional) If set this returns the response of the command executions and any command output. By default this is set to False.
-- * 'maxResults' - (Optional) The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
mkListCommandInvocations ::
  ListCommandInvocations
mkListCommandInvocations =
  ListCommandInvocations'
    { instanceId = Lude.Nothing,
      filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      commandId = Lude.Nothing,
      details = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | (Optional) The command execution details for a specific instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciInstanceId :: Lens.Lens' ListCommandInvocations (Lude.Maybe Lude.Text)
lciInstanceId = Lens.lens (instanceId :: ListCommandInvocations -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: ListCommandInvocations)
{-# DEPRECATED lciInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | (Optional) One or more filters. Use a filter to return a more specific list of results.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciFilters :: Lens.Lens' ListCommandInvocations (Lude.Maybe (Lude.NonEmpty CommandFilter))
lciFilters = Lens.lens (filters :: ListCommandInvocations -> Lude.Maybe (Lude.NonEmpty CommandFilter)) (\s a -> s {filters = a} :: ListCommandInvocations)
{-# DEPRECATED lciFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | (Optional) The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciNextToken :: Lens.Lens' ListCommandInvocations (Lude.Maybe Lude.Text)
lciNextToken = Lens.lens (nextToken :: ListCommandInvocations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListCommandInvocations)
{-# DEPRECATED lciNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | (Optional) The invocations for a specific command ID.
--
-- /Note:/ Consider using 'commandId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciCommandId :: Lens.Lens' ListCommandInvocations (Lude.Maybe Lude.Text)
lciCommandId = Lens.lens (commandId :: ListCommandInvocations -> Lude.Maybe Lude.Text) (\s a -> s {commandId = a} :: ListCommandInvocations)
{-# DEPRECATED lciCommandId "Use generic-lens or generic-optics with 'commandId' instead." #-}

-- | (Optional) If set this returns the response of the command executions and any command output. By default this is set to False.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciDetails :: Lens.Lens' ListCommandInvocations (Lude.Maybe Lude.Bool)
lciDetails = Lens.lens (details :: ListCommandInvocations -> Lude.Maybe Lude.Bool) (\s a -> s {details = a} :: ListCommandInvocations)
{-# DEPRECATED lciDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | (Optional) The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciMaxResults :: Lens.Lens' ListCommandInvocations (Lude.Maybe Lude.Natural)
lciMaxResults = Lens.lens (maxResults :: ListCommandInvocations -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListCommandInvocations)
{-# DEPRECATED lciMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListCommandInvocations where
  page rq rs
    | Page.stop (rs Lens.^. lrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrsCommandInvocations) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lciNextToken Lens..~ rs Lens.^. lrsNextToken

instance Lude.AWSRequest ListCommandInvocations where
  type Rs ListCommandInvocations = ListCommandInvocationsResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListCommandInvocationsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "CommandInvocations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListCommandInvocations where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.ListCommandInvocations" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListCommandInvocations where
  toJSON ListCommandInvocations' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InstanceId" Lude..=) Lude.<$> instanceId,
            ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("CommandId" Lude..=) Lude.<$> commandId,
            ("Details" Lude..=) Lude.<$> details,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListCommandInvocations where
  toPath = Lude.const "/"

instance Lude.ToQuery ListCommandInvocations where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListCommandInvocationsResponse' smart constructor.
data ListCommandInvocationsResponse = ListCommandInvocationsResponse'
  { -- | (Optional) The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Lude.Maybe Lude.Text,
    -- | (Optional) A list of all invocations.
    commandInvocations :: Lude.Maybe [CommandInvocation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCommandInvocationsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - (Optional) The token for the next set of items to return. (You received this token from a previous call.)
-- * 'commandInvocations' - (Optional) A list of all invocations.
-- * 'responseStatus' - The response status code.
mkListCommandInvocationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListCommandInvocationsResponse
mkListCommandInvocationsResponse pResponseStatus_ =
  ListCommandInvocationsResponse'
    { nextToken = Lude.Nothing,
      commandInvocations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | (Optional) The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextToken :: Lens.Lens' ListCommandInvocationsResponse (Lude.Maybe Lude.Text)
lrsNextToken = Lens.lens (nextToken :: ListCommandInvocationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListCommandInvocationsResponse)
{-# DEPRECATED lrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | (Optional) A list of all invocations.
--
-- /Note:/ Consider using 'commandInvocations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsCommandInvocations :: Lens.Lens' ListCommandInvocationsResponse (Lude.Maybe [CommandInvocation])
lrsCommandInvocations = Lens.lens (commandInvocations :: ListCommandInvocationsResponse -> Lude.Maybe [CommandInvocation]) (\s a -> s {commandInvocations = a} :: ListCommandInvocationsResponse)
{-# DEPRECATED lrsCommandInvocations "Use generic-lens or generic-optics with 'commandInvocations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListCommandInvocationsResponse Lude.Int
lrsResponseStatus = Lens.lens (responseStatus :: ListCommandInvocationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListCommandInvocationsResponse)
{-# DEPRECATED lrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
