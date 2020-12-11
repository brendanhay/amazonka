{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.PutQueryDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a query definition for CloudWatch Logs Insights. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/AnalyzingLogData.html Analyzing Log Data with CloudWatch Logs Insights> .
--
-- To update a query definition, specify its @queryDefinitionId@ in your request. The values of @name@ , @queryString@ , and @logGroupNames@ are changed to the values that you specify in your update operation. No current values are retained from the current query definition. For example, if you update a current query definition that includes log groups, and you don't specify the @logGroupNames@ parameter in your update operation, the query definition changes to contain no log groups.
-- You must have the @logs:PutQueryDefinition@ permission to be able to perform this operation.
module Network.AWS.CloudWatchLogs.PutQueryDefinition
  ( -- * Creating a request
    PutQueryDefinition (..),
    mkPutQueryDefinition,

    -- ** Request lenses
    pqdLogGroupNames,
    pqdQueryDefinitionId,
    pqdName,
    pqdQueryString,

    -- * Destructuring the response
    PutQueryDefinitionResponse (..),
    mkPutQueryDefinitionResponse,

    -- ** Response lenses
    pqdrsQueryDefinitionId,
    pqdrsResponseStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutQueryDefinition' smart constructor.
data PutQueryDefinition = PutQueryDefinition'
  { logGroupNames ::
      Lude.Maybe [Lude.Text],
    queryDefinitionId :: Lude.Maybe Lude.Text,
    name :: Lude.Text,
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

-- | Creates a value of 'PutQueryDefinition' with the minimum fields required to make a request.
--
-- * 'logGroupNames' - Use this parameter to include specific log groups as part of your query definition.
--
-- If you are updating a query definition and you omit this parameter, then the updated definition will contain no log groups.
-- * 'name' - A name for the query definition. If you are saving a lot of query definitions, we recommend that you name them so that you can easily find the ones you want by using the first part of the name as a filter in the @queryDefinitionNamePrefix@ parameter of <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeQueryDefinitions.html DescribeQueryDefinitions> .
-- * 'queryDefinitionId' - If you are updating a query definition, use this parameter to specify the ID of the query definition that you want to update. You can use <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeQueryDefinitions.html DescribeQueryDefinitions> to retrieve the IDs of your saved query definitions.
--
-- If you are creating a query definition, do not specify this parameter. CloudWatch generates a unique ID for the new query definition and include it in the response to this operation.
-- * 'queryString' - The query string to use for this definition. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/CWL_QuerySyntax.html CloudWatch Logs Insights Query Syntax> .
mkPutQueryDefinition ::
  -- | 'name'
  Lude.Text ->
  -- | 'queryString'
  Lude.Text ->
  PutQueryDefinition
mkPutQueryDefinition pName_ pQueryString_ =
  PutQueryDefinition'
    { logGroupNames = Lude.Nothing,
      queryDefinitionId = Lude.Nothing,
      name = pName_,
      queryString = pQueryString_
    }

-- | Use this parameter to include specific log groups as part of your query definition.
--
-- If you are updating a query definition and you omit this parameter, then the updated definition will contain no log groups.
--
-- /Note:/ Consider using 'logGroupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pqdLogGroupNames :: Lens.Lens' PutQueryDefinition (Lude.Maybe [Lude.Text])
pqdLogGroupNames = Lens.lens (logGroupNames :: PutQueryDefinition -> Lude.Maybe [Lude.Text]) (\s a -> s {logGroupNames = a} :: PutQueryDefinition)
{-# DEPRECATED pqdLogGroupNames "Use generic-lens or generic-optics with 'logGroupNames' instead." #-}

-- | If you are updating a query definition, use this parameter to specify the ID of the query definition that you want to update. You can use <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeQueryDefinitions.html DescribeQueryDefinitions> to retrieve the IDs of your saved query definitions.
--
-- If you are creating a query definition, do not specify this parameter. CloudWatch generates a unique ID for the new query definition and include it in the response to this operation.
--
-- /Note:/ Consider using 'queryDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pqdQueryDefinitionId :: Lens.Lens' PutQueryDefinition (Lude.Maybe Lude.Text)
pqdQueryDefinitionId = Lens.lens (queryDefinitionId :: PutQueryDefinition -> Lude.Maybe Lude.Text) (\s a -> s {queryDefinitionId = a} :: PutQueryDefinition)
{-# DEPRECATED pqdQueryDefinitionId "Use generic-lens or generic-optics with 'queryDefinitionId' instead." #-}

-- | A name for the query definition. If you are saving a lot of query definitions, we recommend that you name them so that you can easily find the ones you want by using the first part of the name as a filter in the @queryDefinitionNamePrefix@ parameter of <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeQueryDefinitions.html DescribeQueryDefinitions> .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pqdName :: Lens.Lens' PutQueryDefinition Lude.Text
pqdName = Lens.lens (name :: PutQueryDefinition -> Lude.Text) (\s a -> s {name = a} :: PutQueryDefinition)
{-# DEPRECATED pqdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The query string to use for this definition. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/CWL_QuerySyntax.html CloudWatch Logs Insights Query Syntax> .
--
-- /Note:/ Consider using 'queryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pqdQueryString :: Lens.Lens' PutQueryDefinition Lude.Text
pqdQueryString = Lens.lens (queryString :: PutQueryDefinition -> Lude.Text) (\s a -> s {queryString = a} :: PutQueryDefinition)
{-# DEPRECATED pqdQueryString "Use generic-lens or generic-optics with 'queryString' instead." #-}

instance Lude.AWSRequest PutQueryDefinition where
  type Rs PutQueryDefinition = PutQueryDefinitionResponse
  request = Req.postJSON cloudWatchLogsService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutQueryDefinitionResponse'
            Lude.<$> (x Lude..?> "queryDefinitionId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutQueryDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.PutQueryDefinition" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutQueryDefinition where
  toJSON PutQueryDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("logGroupNames" Lude..=) Lude.<$> logGroupNames,
            ("queryDefinitionId" Lude..=) Lude.<$> queryDefinitionId,
            Lude.Just ("name" Lude..= name),
            Lude.Just ("queryString" Lude..= queryString)
          ]
      )

instance Lude.ToPath PutQueryDefinition where
  toPath = Lude.const "/"

instance Lude.ToQuery PutQueryDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutQueryDefinitionResponse' smart constructor.
data PutQueryDefinitionResponse = PutQueryDefinitionResponse'
  { queryDefinitionId ::
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

-- | Creates a value of 'PutQueryDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'queryDefinitionId' - The ID of the query definition.
-- * 'responseStatus' - The response status code.
mkPutQueryDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutQueryDefinitionResponse
mkPutQueryDefinitionResponse pResponseStatus_ =
  PutQueryDefinitionResponse'
    { queryDefinitionId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the query definition.
--
-- /Note:/ Consider using 'queryDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pqdrsQueryDefinitionId :: Lens.Lens' PutQueryDefinitionResponse (Lude.Maybe Lude.Text)
pqdrsQueryDefinitionId = Lens.lens (queryDefinitionId :: PutQueryDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {queryDefinitionId = a} :: PutQueryDefinitionResponse)
{-# DEPRECATED pqdrsQueryDefinitionId "Use generic-lens or generic-optics with 'queryDefinitionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pqdrsResponseStatus :: Lens.Lens' PutQueryDefinitionResponse Lude.Int
pqdrsResponseStatus = Lens.lens (responseStatus :: PutQueryDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutQueryDefinitionResponse)
{-# DEPRECATED pqdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
