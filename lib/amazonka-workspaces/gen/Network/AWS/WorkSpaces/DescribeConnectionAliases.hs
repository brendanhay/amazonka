{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DescribeConnectionAliases
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes the connection aliases used for cross-Region redirection. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces> .
module Network.AWS.WorkSpaces.DescribeConnectionAliases
  ( -- * Creating a request
    DescribeConnectionAliases (..),
    mkDescribeConnectionAliases,

    -- ** Request lenses
    dcaResourceId,
    dcaAliasIds,
    dcaNextToken,
    dcaLimit,

    -- * Destructuring the response
    DescribeConnectionAliasesResponse (..),
    mkDescribeConnectionAliasesResponse,

    -- ** Response lenses
    dcafrsConnectionAliases,
    dcafrsNextToken,
    dcafrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkDescribeConnectionAliases' smart constructor.
data DescribeConnectionAliases = DescribeConnectionAliases'
  { -- | The identifier of the directory associated with the connection alias.
    resourceId :: Lude.Maybe Lude.Text,
    -- | The identifiers of the connection aliases to describe.
    aliasIds :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of connection aliases to return.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConnectionAliases' with the minimum fields required to make a request.
--
-- * 'resourceId' - The identifier of the directory associated with the connection alias.
-- * 'aliasIds' - The identifiers of the connection aliases to describe.
-- * 'nextToken' - If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
-- * 'limit' - The maximum number of connection aliases to return.
mkDescribeConnectionAliases ::
  DescribeConnectionAliases
mkDescribeConnectionAliases =
  DescribeConnectionAliases'
    { resourceId = Lude.Nothing,
      aliasIds = Lude.Nothing,
      nextToken = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | The identifier of the directory associated with the connection alias.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaResourceId :: Lens.Lens' DescribeConnectionAliases (Lude.Maybe Lude.Text)
dcaResourceId = Lens.lens (resourceId :: DescribeConnectionAliases -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: DescribeConnectionAliases)
{-# DEPRECATED dcaResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The identifiers of the connection aliases to describe.
--
-- /Note:/ Consider using 'aliasIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaAliasIds :: Lens.Lens' DescribeConnectionAliases (Lude.Maybe (Lude.NonEmpty Lude.Text))
dcaAliasIds = Lens.lens (aliasIds :: DescribeConnectionAliases -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {aliasIds = a} :: DescribeConnectionAliases)
{-# DEPRECATED dcaAliasIds "Use generic-lens or generic-optics with 'aliasIds' instead." #-}

-- | If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaNextToken :: Lens.Lens' DescribeConnectionAliases (Lude.Maybe Lude.Text)
dcaNextToken = Lens.lens (nextToken :: DescribeConnectionAliases -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeConnectionAliases)
{-# DEPRECATED dcaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of connection aliases to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaLimit :: Lens.Lens' DescribeConnectionAliases (Lude.Maybe Lude.Natural)
dcaLimit = Lens.lens (limit :: DescribeConnectionAliases -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeConnectionAliases)
{-# DEPRECATED dcaLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Lude.AWSRequest DescribeConnectionAliases where
  type
    Rs DescribeConnectionAliases =
      DescribeConnectionAliasesResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeConnectionAliasesResponse'
            Lude.<$> (x Lude..?> "ConnectionAliases")
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeConnectionAliases where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkspacesService.DescribeConnectionAliases" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeConnectionAliases where
  toJSON DescribeConnectionAliases' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ResourceId" Lude..=) Lude.<$> resourceId,
            ("AliasIds" Lude..=) Lude.<$> aliasIds,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath DescribeConnectionAliases where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeConnectionAliases where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeConnectionAliasesResponse' smart constructor.
data DescribeConnectionAliasesResponse = DescribeConnectionAliasesResponse'
  { -- | Information about the specified connection aliases.
    connectionAliases :: Lude.Maybe (Lude.NonEmpty ConnectionAlias),
    -- | The token to use to retrieve the next set of results, or null if no more results are available.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConnectionAliasesResponse' with the minimum fields required to make a request.
--
-- * 'connectionAliases' - Information about the specified connection aliases.
-- * 'nextToken' - The token to use to retrieve the next set of results, or null if no more results are available.
-- * 'responseStatus' - The response status code.
mkDescribeConnectionAliasesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeConnectionAliasesResponse
mkDescribeConnectionAliasesResponse pResponseStatus_ =
  DescribeConnectionAliasesResponse'
    { connectionAliases =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the specified connection aliases.
--
-- /Note:/ Consider using 'connectionAliases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcafrsConnectionAliases :: Lens.Lens' DescribeConnectionAliasesResponse (Lude.Maybe (Lude.NonEmpty ConnectionAlias))
dcafrsConnectionAliases = Lens.lens (connectionAliases :: DescribeConnectionAliasesResponse -> Lude.Maybe (Lude.NonEmpty ConnectionAlias)) (\s a -> s {connectionAliases = a} :: DescribeConnectionAliasesResponse)
{-# DEPRECATED dcafrsConnectionAliases "Use generic-lens or generic-optics with 'connectionAliases' instead." #-}

-- | The token to use to retrieve the next set of results, or null if no more results are available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcafrsNextToken :: Lens.Lens' DescribeConnectionAliasesResponse (Lude.Maybe Lude.Text)
dcafrsNextToken = Lens.lens (nextToken :: DescribeConnectionAliasesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeConnectionAliasesResponse)
{-# DEPRECATED dcafrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcafrsResponseStatus :: Lens.Lens' DescribeConnectionAliasesResponse Lude.Int
dcafrsResponseStatus = Lens.lens (responseStatus :: DescribeConnectionAliasesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeConnectionAliasesResponse)
{-# DEPRECATED dcafrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
