{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.SelectResourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a structured query language (SQL) @SELECT@ command, performs the corresponding search, and returns resource configurations matching the properties.
--
-- For more information about query components, see the <https://docs.aws.amazon.com/config/latest/developerguide/query-components.html __Query Components__ > section in the AWS Config Developer Guide.
module Network.AWS.Config.SelectResourceConfig
  ( -- * Creating a request
    SelectResourceConfig (..),
    mkSelectResourceConfig,

    -- ** Request lenses
    srcNextToken,
    srcLimit,
    srcExpression,

    -- * Destructuring the response
    SelectResourceConfigResponse (..),
    mkSelectResourceConfigResponse,

    -- ** Response lenses
    srcrsResults,
    srcrsQueryInfo,
    srcrsNextToken,
    srcrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSelectResourceConfig' smart constructor.
data SelectResourceConfig = SelectResourceConfig'
  { nextToken ::
      Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural,
    expression :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SelectResourceConfig' with the minimum fields required to make a request.
--
-- * 'expression' - The SQL query @SELECT@ command.
-- * 'limit' - The maximum number of query results returned on each page.
-- * 'nextToken' - The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
mkSelectResourceConfig ::
  -- | 'expression'
  Lude.Text ->
  SelectResourceConfig
mkSelectResourceConfig pExpression_ =
  SelectResourceConfig'
    { nextToken = Lude.Nothing,
      limit = Lude.Nothing,
      expression = pExpression_
    }

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcNextToken :: Lens.Lens' SelectResourceConfig (Lude.Maybe Lude.Text)
srcNextToken = Lens.lens (nextToken :: SelectResourceConfig -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: SelectResourceConfig)
{-# DEPRECATED srcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of query results returned on each page.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcLimit :: Lens.Lens' SelectResourceConfig (Lude.Maybe Lude.Natural)
srcLimit = Lens.lens (limit :: SelectResourceConfig -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: SelectResourceConfig)
{-# DEPRECATED srcLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The SQL query @SELECT@ command.
--
-- /Note:/ Consider using 'expression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcExpression :: Lens.Lens' SelectResourceConfig Lude.Text
srcExpression = Lens.lens (expression :: SelectResourceConfig -> Lude.Text) (\s a -> s {expression = a} :: SelectResourceConfig)
{-# DEPRECATED srcExpression "Use generic-lens or generic-optics with 'expression' instead." #-}

instance Lude.AWSRequest SelectResourceConfig where
  type Rs SelectResourceConfig = SelectResourceConfigResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          SelectResourceConfigResponse'
            Lude.<$> (x Lude..?> "Results" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "QueryInfo")
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SelectResourceConfig where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StarlingDoveService.SelectResourceConfig" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SelectResourceConfig where
  toJSON SelectResourceConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit,
            Lude.Just ("Expression" Lude..= expression)
          ]
      )

instance Lude.ToPath SelectResourceConfig where
  toPath = Lude.const "/"

instance Lude.ToQuery SelectResourceConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSelectResourceConfigResponse' smart constructor.
data SelectResourceConfigResponse = SelectResourceConfigResponse'
  { results ::
      Lude.Maybe [Lude.Text],
    queryInfo :: Lude.Maybe QueryInfo,
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

-- | Creates a value of 'SelectResourceConfigResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
-- * 'queryInfo' - Returns the @QueryInfo@ object.
-- * 'responseStatus' - The response status code.
-- * 'results' - Returns the results for the SQL query.
mkSelectResourceConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SelectResourceConfigResponse
mkSelectResourceConfigResponse pResponseStatus_ =
  SelectResourceConfigResponse'
    { results = Lude.Nothing,
      queryInfo = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns the results for the SQL query.
--
-- /Note:/ Consider using 'results' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcrsResults :: Lens.Lens' SelectResourceConfigResponse (Lude.Maybe [Lude.Text])
srcrsResults = Lens.lens (results :: SelectResourceConfigResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {results = a} :: SelectResourceConfigResponse)
{-# DEPRECATED srcrsResults "Use generic-lens or generic-optics with 'results' instead." #-}

-- | Returns the @QueryInfo@ object.
--
-- /Note:/ Consider using 'queryInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcrsQueryInfo :: Lens.Lens' SelectResourceConfigResponse (Lude.Maybe QueryInfo)
srcrsQueryInfo = Lens.lens (queryInfo :: SelectResourceConfigResponse -> Lude.Maybe QueryInfo) (\s a -> s {queryInfo = a} :: SelectResourceConfigResponse)
{-# DEPRECATED srcrsQueryInfo "Use generic-lens or generic-optics with 'queryInfo' instead." #-}

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcrsNextToken :: Lens.Lens' SelectResourceConfigResponse (Lude.Maybe Lude.Text)
srcrsNextToken = Lens.lens (nextToken :: SelectResourceConfigResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: SelectResourceConfigResponse)
{-# DEPRECATED srcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcrsResponseStatus :: Lens.Lens' SelectResourceConfigResponse Lude.Int
srcrsResponseStatus = Lens.lens (responseStatus :: SelectResourceConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SelectResourceConfigResponse)
{-# DEPRECATED srcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
