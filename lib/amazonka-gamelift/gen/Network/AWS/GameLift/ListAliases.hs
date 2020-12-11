{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.ListAliases
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all aliases for this AWS account. You can filter the result set by alias name and/or routing strategy type. Use the pagination parameters to retrieve results in sequential pages.
--
--
--     * 'CreateAlias'
--
--
--     * 'ListAliases'
--
--
--     * 'DescribeAlias'
--
--
--     * 'UpdateAlias'
--
--
--     * 'DeleteAlias'
--
--
--     * 'ResolveAlias'
--
--
--
-- This operation returns paginated results.
module Network.AWS.GameLift.ListAliases
  ( -- * Creating a request
    ListAliases (..),
    mkListAliases,

    -- ** Request lenses
    laRoutingStrategyType,
    laNextToken,
    laName,
    laLimit,

    -- * Destructuring the response
    ListAliasesResponse (..),
    mkListAliasesResponse,

    -- ** Response lenses
    larsAliases,
    larsNextToken,
    larsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkListAliases' smart constructor.
data ListAliases = ListAliases'
  { routingStrategyType ::
      Lude.Maybe RoutingStrategyType,
    nextToken :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAliases' with the minimum fields required to make a request.
--
-- * 'limit' - The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
-- * 'name' - A descriptive label that is associated with an alias. Alias names do not need to be unique.
-- * 'nextToken' - A token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
-- * 'routingStrategyType' - The routing type to filter results on. Use this parameter to retrieve only aliases with a certain routing type. To retrieve all aliases, leave this parameter empty.
--
-- Possible routing types include the following:
--
--     * __SIMPLE__ -- The alias resolves to one specific fleet. Use this type when routing to active fleets.
--
--
--     * __TERMINAL__ -- The alias does not resolve to a fleet but instead can be used to display a message to the user. A terminal alias throws a TerminalRoutingStrategyException with the 'RoutingStrategy' message embedded.
mkListAliases ::
  ListAliases
mkListAliases =
  ListAliases'
    { routingStrategyType = Lude.Nothing,
      nextToken = Lude.Nothing,
      name = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | The routing type to filter results on. Use this parameter to retrieve only aliases with a certain routing type. To retrieve all aliases, leave this parameter empty.
--
-- Possible routing types include the following:
--
--     * __SIMPLE__ -- The alias resolves to one specific fleet. Use this type when routing to active fleets.
--
--
--     * __TERMINAL__ -- The alias does not resolve to a fleet but instead can be used to display a message to the user. A terminal alias throws a TerminalRoutingStrategyException with the 'RoutingStrategy' message embedded.
--
--
--
-- /Note:/ Consider using 'routingStrategyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laRoutingStrategyType :: Lens.Lens' ListAliases (Lude.Maybe RoutingStrategyType)
laRoutingStrategyType = Lens.lens (routingStrategyType :: ListAliases -> Lude.Maybe RoutingStrategyType) (\s a -> s {routingStrategyType = a} :: ListAliases)
{-# DEPRECATED laRoutingStrategyType "Use generic-lens or generic-optics with 'routingStrategyType' instead." #-}

-- | A token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laNextToken :: Lens.Lens' ListAliases (Lude.Maybe Lude.Text)
laNextToken = Lens.lens (nextToken :: ListAliases -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAliases)
{-# DEPRECATED laNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A descriptive label that is associated with an alias. Alias names do not need to be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laName :: Lens.Lens' ListAliases (Lude.Maybe Lude.Text)
laName = Lens.lens (name :: ListAliases -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ListAliases)
{-# DEPRECATED laName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laLimit :: Lens.Lens' ListAliases (Lude.Maybe Lude.Natural)
laLimit = Lens.lens (limit :: ListAliases -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListAliases)
{-# DEPRECATED laLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager ListAliases where
  page rq rs
    | Page.stop (rs Lens.^. larsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. larsAliases) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& laNextToken Lens..~ rs Lens.^. larsNextToken

instance Lude.AWSRequest ListAliases where
  type Rs ListAliases = ListAliasesResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAliasesResponse'
            Lude.<$> (x Lude..?> "Aliases" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAliases where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.ListAliases" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListAliases where
  toJSON ListAliases' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RoutingStrategyType" Lude..=) Lude.<$> routingStrategyType,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Name" Lude..=) Lude.<$> name,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListAliases where
  toPath = Lude.const "/"

instance Lude.ToQuery ListAliases where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkListAliasesResponse' smart constructor.
data ListAliasesResponse = ListAliasesResponse'
  { aliases ::
      Lude.Maybe [Alias],
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

-- | Creates a value of 'ListAliasesResponse' with the minimum fields required to make a request.
--
-- * 'aliases' - A collection of alias resources that match the request parameters.
-- * 'nextToken' - A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
-- * 'responseStatus' - The response status code.
mkListAliasesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAliasesResponse
mkListAliasesResponse pResponseStatus_ =
  ListAliasesResponse'
    { aliases = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A collection of alias resources that match the request parameters.
--
-- /Note:/ Consider using 'aliases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsAliases :: Lens.Lens' ListAliasesResponse (Lude.Maybe [Alias])
larsAliases = Lens.lens (aliases :: ListAliasesResponse -> Lude.Maybe [Alias]) (\s a -> s {aliases = a} :: ListAliasesResponse)
{-# DEPRECATED larsAliases "Use generic-lens or generic-optics with 'aliases' instead." #-}

-- | A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsNextToken :: Lens.Lens' ListAliasesResponse (Lude.Maybe Lude.Text)
larsNextToken = Lens.lens (nextToken :: ListAliasesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAliasesResponse)
{-# DEPRECATED larsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsResponseStatus :: Lens.Lens' ListAliasesResponse Lude.Int
larsResponseStatus = Lens.lens (responseStatus :: ListAliasesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAliasesResponse)
{-# DEPRECATED larsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
