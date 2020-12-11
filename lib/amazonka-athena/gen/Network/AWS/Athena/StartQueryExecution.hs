{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.StartQueryExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs the SQL query statements contained in the @Query@ . Requires you to have access to the workgroup in which the query ran. Running queries against an external catalog requires 'GetDataCatalog' permission to the catalog. For code samples using the AWS SDK for Java, see <http://docs.aws.amazon.com/athena/latest/ug/code-samples.html Examples and Code Samples> in the /Amazon Athena User Guide/ .
module Network.AWS.Athena.StartQueryExecution
  ( -- * Creating a request
    StartQueryExecution (..),
    mkStartQueryExecution,

    -- ** Request lenses
    sqeQueryExecutionContext,
    sqeResultConfiguration,
    sqeClientRequestToken,
    sqeWorkGroup,
    sqeQueryString,

    -- * Destructuring the response
    StartQueryExecutionResponse (..),
    mkStartQueryExecutionResponse,

    -- ** Response lenses
    sqersQueryExecutionId,
    sqersResponseStatus,
  )
where

import Network.AWS.Athena.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartQueryExecution' smart constructor.
data StartQueryExecution = StartQueryExecution'
  { queryExecutionContext ::
      Lude.Maybe QueryExecutionContext,
    resultConfiguration ::
      Lude.Maybe ResultConfiguration,
    clientRequestToken :: Lude.Maybe Lude.Text,
    workGroup :: Lude.Maybe Lude.Text,
    queryString :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartQueryExecution' with the minimum fields required to make a request.
--
-- * 'clientRequestToken' - A unique case-sensitive string used to ensure the request to create the query is idempotent (executes only once). If another @StartQueryExecution@ request is received, the same response is returned and another query is not created. If a parameter has changed, for example, the @QueryString@ , an error is returned.
--
-- /Important:/ This token is listed as not required because AWS SDKs (for example the AWS SDK for Java) auto-generate the token for users. If you are not using the AWS SDK or the AWS CLI, you must provide this token or the action will fail.
-- * 'queryExecutionContext' - The database within which the query executes.
-- * 'queryString' - The SQL query statements to be executed.
-- * 'resultConfiguration' - Specifies information about where and how to save the results of the query execution. If the query runs in a workgroup, then workgroup's settings may override query settings. This affects the query results location. The workgroup settings override is specified in EnforceWorkGroupConfiguration (true/false) in the WorkGroupConfiguration. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
-- * 'workGroup' - The name of the workgroup in which the query is being started.
mkStartQueryExecution ::
  -- | 'queryString'
  Lude.Text ->
  StartQueryExecution
mkStartQueryExecution pQueryString_ =
  StartQueryExecution'
    { queryExecutionContext = Lude.Nothing,
      resultConfiguration = Lude.Nothing,
      clientRequestToken = Lude.Nothing,
      workGroup = Lude.Nothing,
      queryString = pQueryString_
    }

-- | The database within which the query executes.
--
-- /Note:/ Consider using 'queryExecutionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqeQueryExecutionContext :: Lens.Lens' StartQueryExecution (Lude.Maybe QueryExecutionContext)
sqeQueryExecutionContext = Lens.lens (queryExecutionContext :: StartQueryExecution -> Lude.Maybe QueryExecutionContext) (\s a -> s {queryExecutionContext = a} :: StartQueryExecution)
{-# DEPRECATED sqeQueryExecutionContext "Use generic-lens or generic-optics with 'queryExecutionContext' instead." #-}

-- | Specifies information about where and how to save the results of the query execution. If the query runs in a workgroup, then workgroup's settings may override query settings. This affects the query results location. The workgroup settings override is specified in EnforceWorkGroupConfiguration (true/false) in the WorkGroupConfiguration. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
--
-- /Note:/ Consider using 'resultConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqeResultConfiguration :: Lens.Lens' StartQueryExecution (Lude.Maybe ResultConfiguration)
sqeResultConfiguration = Lens.lens (resultConfiguration :: StartQueryExecution -> Lude.Maybe ResultConfiguration) (\s a -> s {resultConfiguration = a} :: StartQueryExecution)
{-# DEPRECATED sqeResultConfiguration "Use generic-lens or generic-optics with 'resultConfiguration' instead." #-}

-- | A unique case-sensitive string used to ensure the request to create the query is idempotent (executes only once). If another @StartQueryExecution@ request is received, the same response is returned and another query is not created. If a parameter has changed, for example, the @QueryString@ , an error is returned.
--
-- /Important:/ This token is listed as not required because AWS SDKs (for example the AWS SDK for Java) auto-generate the token for users. If you are not using the AWS SDK or the AWS CLI, you must provide this token or the action will fail.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqeClientRequestToken :: Lens.Lens' StartQueryExecution (Lude.Maybe Lude.Text)
sqeClientRequestToken = Lens.lens (clientRequestToken :: StartQueryExecution -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: StartQueryExecution)
{-# DEPRECATED sqeClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The name of the workgroup in which the query is being started.
--
-- /Note:/ Consider using 'workGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqeWorkGroup :: Lens.Lens' StartQueryExecution (Lude.Maybe Lude.Text)
sqeWorkGroup = Lens.lens (workGroup :: StartQueryExecution -> Lude.Maybe Lude.Text) (\s a -> s {workGroup = a} :: StartQueryExecution)
{-# DEPRECATED sqeWorkGroup "Use generic-lens or generic-optics with 'workGroup' instead." #-}

-- | The SQL query statements to be executed.
--
-- /Note:/ Consider using 'queryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqeQueryString :: Lens.Lens' StartQueryExecution Lude.Text
sqeQueryString = Lens.lens (queryString :: StartQueryExecution -> Lude.Text) (\s a -> s {queryString = a} :: StartQueryExecution)
{-# DEPRECATED sqeQueryString "Use generic-lens or generic-optics with 'queryString' instead." #-}

instance Lude.AWSRequest StartQueryExecution where
  type Rs StartQueryExecution = StartQueryExecutionResponse
  request = Req.postJSON athenaService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartQueryExecutionResponse'
            Lude.<$> (x Lude..?> "QueryExecutionId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartQueryExecution where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonAthena.StartQueryExecution" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartQueryExecution where
  toJSON StartQueryExecution' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("QueryExecutionContext" Lude..=) Lude.<$> queryExecutionContext,
            ("ResultConfiguration" Lude..=) Lude.<$> resultConfiguration,
            ("ClientRequestToken" Lude..=) Lude.<$> clientRequestToken,
            ("WorkGroup" Lude..=) Lude.<$> workGroup,
            Lude.Just ("QueryString" Lude..= queryString)
          ]
      )

instance Lude.ToPath StartQueryExecution where
  toPath = Lude.const "/"

instance Lude.ToQuery StartQueryExecution where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartQueryExecutionResponse' smart constructor.
data StartQueryExecutionResponse = StartQueryExecutionResponse'
  { queryExecutionId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'StartQueryExecutionResponse' with the minimum fields required to make a request.
--
-- * 'queryExecutionId' - The unique ID of the query that ran as a result of this request.
-- * 'responseStatus' - The response status code.
mkStartQueryExecutionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartQueryExecutionResponse
mkStartQueryExecutionResponse pResponseStatus_ =
  StartQueryExecutionResponse'
    { queryExecutionId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique ID of the query that ran as a result of this request.
--
-- /Note:/ Consider using 'queryExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqersQueryExecutionId :: Lens.Lens' StartQueryExecutionResponse (Lude.Maybe Lude.Text)
sqersQueryExecutionId = Lens.lens (queryExecutionId :: StartQueryExecutionResponse -> Lude.Maybe Lude.Text) (\s a -> s {queryExecutionId = a} :: StartQueryExecutionResponse)
{-# DEPRECATED sqersQueryExecutionId "Use generic-lens or generic-optics with 'queryExecutionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqersResponseStatus :: Lens.Lens' StartQueryExecutionResponse Lude.Int
sqersResponseStatus = Lens.lens (responseStatus :: StartQueryExecutionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartQueryExecutionResponse)
{-# DEPRECATED sqersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
