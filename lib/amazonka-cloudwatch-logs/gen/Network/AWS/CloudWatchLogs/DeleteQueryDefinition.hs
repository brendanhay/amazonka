{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DeleteQueryDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a saved CloudWatch Logs Insights query definition. A query definition contains details about a saved CloudWatch Logs Insights query.
--
-- Each @DeleteQueryDefinition@ operation can delete one query definition.
-- You must have the @logs:DeleteQueryDefinition@ permission to be able to perform this operation.
module Network.AWS.CloudWatchLogs.DeleteQueryDefinition
  ( -- * Creating a request
    DeleteQueryDefinition (..),
    mkDeleteQueryDefinition,

    -- ** Request lenses
    dqdQueryDefinitionId,

    -- * Destructuring the response
    DeleteQueryDefinitionResponse (..),
    mkDeleteQueryDefinitionResponse,

    -- ** Response lenses
    drsSuccess,
    drsResponseStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteQueryDefinition' smart constructor.
newtype DeleteQueryDefinition = DeleteQueryDefinition'
  { -- | The ID of the query definition that you want to delete. You can use <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeQueryDefinitions.html DescribeQueryDefinitions> to retrieve the IDs of your saved query definitions.
    queryDefinitionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteQueryDefinition' with the minimum fields required to make a request.
--
-- * 'queryDefinitionId' - The ID of the query definition that you want to delete. You can use <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeQueryDefinitions.html DescribeQueryDefinitions> to retrieve the IDs of your saved query definitions.
mkDeleteQueryDefinition ::
  -- | 'queryDefinitionId'
  Lude.Text ->
  DeleteQueryDefinition
mkDeleteQueryDefinition pQueryDefinitionId_ =
  DeleteQueryDefinition' {queryDefinitionId = pQueryDefinitionId_}

-- | The ID of the query definition that you want to delete. You can use <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeQueryDefinitions.html DescribeQueryDefinitions> to retrieve the IDs of your saved query definitions.
--
-- /Note:/ Consider using 'queryDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqdQueryDefinitionId :: Lens.Lens' DeleteQueryDefinition Lude.Text
dqdQueryDefinitionId = Lens.lens (queryDefinitionId :: DeleteQueryDefinition -> Lude.Text) (\s a -> s {queryDefinitionId = a} :: DeleteQueryDefinition)
{-# DEPRECATED dqdQueryDefinitionId "Use generic-lens or generic-optics with 'queryDefinitionId' instead." #-}

instance Lude.AWSRequest DeleteQueryDefinition where
  type Rs DeleteQueryDefinition = DeleteQueryDefinitionResponse
  request = Req.postJSON cloudWatchLogsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteQueryDefinitionResponse'
            Lude.<$> (x Lude..?> "success") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteQueryDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.DeleteQueryDefinition" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteQueryDefinition where
  toJSON DeleteQueryDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("queryDefinitionId" Lude..= queryDefinitionId)]
      )

instance Lude.ToPath DeleteQueryDefinition where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteQueryDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteQueryDefinitionResponse' smart constructor.
data DeleteQueryDefinitionResponse = DeleteQueryDefinitionResponse'
  { -- | A value of TRUE indicates that the operation succeeded. FALSE indicates that the operation failed.
    success :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteQueryDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'success' - A value of TRUE indicates that the operation succeeded. FALSE indicates that the operation failed.
-- * 'responseStatus' - The response status code.
mkDeleteQueryDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteQueryDefinitionResponse
mkDeleteQueryDefinitionResponse pResponseStatus_ =
  DeleteQueryDefinitionResponse'
    { success = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A value of TRUE indicates that the operation succeeded. FALSE indicates that the operation failed.
--
-- /Note:/ Consider using 'success' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsSuccess :: Lens.Lens' DeleteQueryDefinitionResponse (Lude.Maybe Lude.Bool)
drsSuccess = Lens.lens (success :: DeleteQueryDefinitionResponse -> Lude.Maybe Lude.Bool) (\s a -> s {success = a} :: DeleteQueryDefinitionResponse)
{-# DEPRECATED drsSuccess "Use generic-lens or generic-optics with 'success' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteQueryDefinitionResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DeleteQueryDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteQueryDefinitionResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
