{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DescribeQueryDefinitions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns a paginated list of your saved CloudWatch Logs Insights query definitions.
--
-- You can use the @queryDefinitionNamePrefix@ parameter to limit the results to only the query definitions that have names that start with a certain string.
module Network.AWS.CloudWatchLogs.DescribeQueryDefinitions
  ( -- * Creating a request
    DescribeQueryDefinitions (..),
    mkDescribeQueryDefinitions,

    -- ** Request lenses
    dqdQueryDefinitionNamePrefix,
    dqdNextToken,
    dqdMaxResults,

    -- * Destructuring the response
    DescribeQueryDefinitionsResponse (..),
    mkDescribeQueryDefinitionsResponse,

    -- ** Response lenses
    dqdrsQueryDefinitions,
    dqdrsNextToken,
    dqdrsResponseStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeQueryDefinitions' smart constructor.
data DescribeQueryDefinitions = DescribeQueryDefinitions'
  { queryDefinitionNamePrefix ::
      Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeQueryDefinitions' with the minimum fields required to make a request.
--
-- * 'maxResults' - Limits the number of returned query definitions to the specified number.
-- * 'nextToken' - Undocumented field.
-- * 'queryDefinitionNamePrefix' - Use this parameter to filter your results to only the query definitions that have names that start with the prefix you specify.
mkDescribeQueryDefinitions ::
  DescribeQueryDefinitions
mkDescribeQueryDefinitions =
  DescribeQueryDefinitions'
    { queryDefinitionNamePrefix =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Use this parameter to filter your results to only the query definitions that have names that start with the prefix you specify.
--
-- /Note:/ Consider using 'queryDefinitionNamePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqdQueryDefinitionNamePrefix :: Lens.Lens' DescribeQueryDefinitions (Lude.Maybe Lude.Text)
dqdQueryDefinitionNamePrefix = Lens.lens (queryDefinitionNamePrefix :: DescribeQueryDefinitions -> Lude.Maybe Lude.Text) (\s a -> s {queryDefinitionNamePrefix = a} :: DescribeQueryDefinitions)
{-# DEPRECATED dqdQueryDefinitionNamePrefix "Use generic-lens or generic-optics with 'queryDefinitionNamePrefix' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqdNextToken :: Lens.Lens' DescribeQueryDefinitions (Lude.Maybe Lude.Text)
dqdNextToken = Lens.lens (nextToken :: DescribeQueryDefinitions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeQueryDefinitions)
{-# DEPRECATED dqdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Limits the number of returned query definitions to the specified number.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqdMaxResults :: Lens.Lens' DescribeQueryDefinitions (Lude.Maybe Lude.Natural)
dqdMaxResults = Lens.lens (maxResults :: DescribeQueryDefinitions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeQueryDefinitions)
{-# DEPRECATED dqdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest DescribeQueryDefinitions where
  type Rs DescribeQueryDefinitions = DescribeQueryDefinitionsResponse
  request = Req.postJSON cloudWatchLogsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeQueryDefinitionsResponse'
            Lude.<$> (x Lude..?> "queryDefinitions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeQueryDefinitions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.DescribeQueryDefinitions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeQueryDefinitions where
  toJSON DescribeQueryDefinitions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("queryDefinitionNamePrefix" Lude..=)
              Lude.<$> queryDefinitionNamePrefix,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeQueryDefinitions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeQueryDefinitions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeQueryDefinitionsResponse' smart constructor.
data DescribeQueryDefinitionsResponse = DescribeQueryDefinitionsResponse'
  { queryDefinitions ::
      Lude.Maybe
        [QueryDefinition],
    nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeQueryDefinitionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Undocumented field.
-- * 'queryDefinitions' - The list of query definitions that match your request.
-- * 'responseStatus' - The response status code.
mkDescribeQueryDefinitionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeQueryDefinitionsResponse
mkDescribeQueryDefinitionsResponse pResponseStatus_ =
  DescribeQueryDefinitionsResponse'
    { queryDefinitions =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of query definitions that match your request.
--
-- /Note:/ Consider using 'queryDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqdrsQueryDefinitions :: Lens.Lens' DescribeQueryDefinitionsResponse (Lude.Maybe [QueryDefinition])
dqdrsQueryDefinitions = Lens.lens (queryDefinitions :: DescribeQueryDefinitionsResponse -> Lude.Maybe [QueryDefinition]) (\s a -> s {queryDefinitions = a} :: DescribeQueryDefinitionsResponse)
{-# DEPRECATED dqdrsQueryDefinitions "Use generic-lens or generic-optics with 'queryDefinitions' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqdrsNextToken :: Lens.Lens' DescribeQueryDefinitionsResponse (Lude.Maybe Lude.Text)
dqdrsNextToken = Lens.lens (nextToken :: DescribeQueryDefinitionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeQueryDefinitionsResponse)
{-# DEPRECATED dqdrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqdrsResponseStatus :: Lens.Lens' DescribeQueryDefinitionsResponse Lude.Int
dqdrsResponseStatus = Lens.lens (responseStatus :: DescribeQueryDefinitionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeQueryDefinitionsResponse)
{-# DEPRECATED dqdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
