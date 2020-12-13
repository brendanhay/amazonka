{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.SelectAggregateResourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a structured query language (SQL) SELECT command and an aggregator to query configuration state of AWS resources across multiple accounts and regions, performs the corresponding search, and returns resource configurations matching the properties.
--
-- For more information about query components, see the <https://docs.aws.amazon.com/config/latest/developerguide/query-components.html __Query Components__ > section in the AWS Config Developer Guide.
module Network.AWS.Config.SelectAggregateResourceConfig
  ( -- * Creating a request
    SelectAggregateResourceConfig (..),
    mkSelectAggregateResourceConfig,

    -- ** Request lenses
    sarcNextToken,
    sarcExpression,
    sarcLimit,
    sarcConfigurationAggregatorName,
    sarcMaxResults,

    -- * Destructuring the response
    SelectAggregateResourceConfigResponse (..),
    mkSelectAggregateResourceConfigResponse,

    -- ** Response lenses
    sarcrsResults,
    sarcrsQueryInfo,
    sarcrsNextToken,
    sarcrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSelectAggregateResourceConfig' smart constructor.
data SelectAggregateResourceConfig = SelectAggregateResourceConfig'
  { -- | The nextToken string returned in a previous request that you use to request the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The SQL query SELECT command.
    expression :: Lude.Text,
    -- | The maximum number of query results returned on each page.
    limit :: Lude.Maybe Lude.Natural,
    -- | The name of the configuration aggregator.
    configurationAggregatorName :: Lude.Text,
    -- | The maximum number of query results returned on each page. AWS Config also allows the Limit request parameter.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SelectAggregateResourceConfig' with the minimum fields required to make a request.
--
-- * 'nextToken' - The nextToken string returned in a previous request that you use to request the next page of results in a paginated response.
-- * 'expression' - The SQL query SELECT command.
-- * 'limit' - The maximum number of query results returned on each page.
-- * 'configurationAggregatorName' - The name of the configuration aggregator.
-- * 'maxResults' - The maximum number of query results returned on each page. AWS Config also allows the Limit request parameter.
mkSelectAggregateResourceConfig ::
  -- | 'expression'
  Lude.Text ->
  -- | 'configurationAggregatorName'
  Lude.Text ->
  SelectAggregateResourceConfig
mkSelectAggregateResourceConfig
  pExpression_
  pConfigurationAggregatorName_ =
    SelectAggregateResourceConfig'
      { nextToken = Lude.Nothing,
        expression = pExpression_,
        limit = Lude.Nothing,
        configurationAggregatorName = pConfigurationAggregatorName_,
        maxResults = Lude.Nothing
      }

-- | The nextToken string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarcNextToken :: Lens.Lens' SelectAggregateResourceConfig (Lude.Maybe Lude.Text)
sarcNextToken = Lens.lens (nextToken :: SelectAggregateResourceConfig -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: SelectAggregateResourceConfig)
{-# DEPRECATED sarcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The SQL query SELECT command.
--
-- /Note:/ Consider using 'expression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarcExpression :: Lens.Lens' SelectAggregateResourceConfig Lude.Text
sarcExpression = Lens.lens (expression :: SelectAggregateResourceConfig -> Lude.Text) (\s a -> s {expression = a} :: SelectAggregateResourceConfig)
{-# DEPRECATED sarcExpression "Use generic-lens or generic-optics with 'expression' instead." #-}

-- | The maximum number of query results returned on each page.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarcLimit :: Lens.Lens' SelectAggregateResourceConfig (Lude.Maybe Lude.Natural)
sarcLimit = Lens.lens (limit :: SelectAggregateResourceConfig -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: SelectAggregateResourceConfig)
{-# DEPRECATED sarcLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The name of the configuration aggregator.
--
-- /Note:/ Consider using 'configurationAggregatorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarcConfigurationAggregatorName :: Lens.Lens' SelectAggregateResourceConfig Lude.Text
sarcConfigurationAggregatorName = Lens.lens (configurationAggregatorName :: SelectAggregateResourceConfig -> Lude.Text) (\s a -> s {configurationAggregatorName = a} :: SelectAggregateResourceConfig)
{-# DEPRECATED sarcConfigurationAggregatorName "Use generic-lens or generic-optics with 'configurationAggregatorName' instead." #-}

-- | The maximum number of query results returned on each page. AWS Config also allows the Limit request parameter.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarcMaxResults :: Lens.Lens' SelectAggregateResourceConfig (Lude.Maybe Lude.Natural)
sarcMaxResults = Lens.lens (maxResults :: SelectAggregateResourceConfig -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: SelectAggregateResourceConfig)
{-# DEPRECATED sarcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest SelectAggregateResourceConfig where
  type
    Rs SelectAggregateResourceConfig =
      SelectAggregateResourceConfigResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          SelectAggregateResourceConfigResponse'
            Lude.<$> (x Lude..?> "Results" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "QueryInfo")
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SelectAggregateResourceConfig where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.SelectAggregateResourceConfig" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SelectAggregateResourceConfig where
  toJSON SelectAggregateResourceConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("Expression" Lude..= expression),
            ("Limit" Lude..=) Lude.<$> limit,
            Lude.Just
              ( "ConfigurationAggregatorName"
                  Lude..= configurationAggregatorName
              ),
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath SelectAggregateResourceConfig where
  toPath = Lude.const "/"

instance Lude.ToQuery SelectAggregateResourceConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSelectAggregateResourceConfigResponse' smart constructor.
data SelectAggregateResourceConfigResponse = SelectAggregateResourceConfigResponse'
  { -- | Returns the results for the SQL query.
    results :: Lude.Maybe [Lude.Text],
    queryInfo :: Lude.Maybe QueryInfo,
    -- | The nextToken string returned in a previous request that you use to request the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SelectAggregateResourceConfigResponse' with the minimum fields required to make a request.
--
-- * 'results' - Returns the results for the SQL query.
-- * 'queryInfo' -
-- * 'nextToken' - The nextToken string returned in a previous request that you use to request the next page of results in a paginated response.
-- * 'responseStatus' - The response status code.
mkSelectAggregateResourceConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SelectAggregateResourceConfigResponse
mkSelectAggregateResourceConfigResponse pResponseStatus_ =
  SelectAggregateResourceConfigResponse'
    { results = Lude.Nothing,
      queryInfo = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns the results for the SQL query.
--
-- /Note:/ Consider using 'results' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarcrsResults :: Lens.Lens' SelectAggregateResourceConfigResponse (Lude.Maybe [Lude.Text])
sarcrsResults = Lens.lens (results :: SelectAggregateResourceConfigResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {results = a} :: SelectAggregateResourceConfigResponse)
{-# DEPRECATED sarcrsResults "Use generic-lens or generic-optics with 'results' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'queryInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarcrsQueryInfo :: Lens.Lens' SelectAggregateResourceConfigResponse (Lude.Maybe QueryInfo)
sarcrsQueryInfo = Lens.lens (queryInfo :: SelectAggregateResourceConfigResponse -> Lude.Maybe QueryInfo) (\s a -> s {queryInfo = a} :: SelectAggregateResourceConfigResponse)
{-# DEPRECATED sarcrsQueryInfo "Use generic-lens or generic-optics with 'queryInfo' instead." #-}

-- | The nextToken string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarcrsNextToken :: Lens.Lens' SelectAggregateResourceConfigResponse (Lude.Maybe Lude.Text)
sarcrsNextToken = Lens.lens (nextToken :: SelectAggregateResourceConfigResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: SelectAggregateResourceConfigResponse)
{-# DEPRECATED sarcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarcrsResponseStatus :: Lens.Lens' SelectAggregateResourceConfigResponse Lude.Int
sarcrsResponseStatus = Lens.lens (responseStatus :: SelectAggregateResourceConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SelectAggregateResourceConfigResponse)
{-# DEPRECATED sarcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
