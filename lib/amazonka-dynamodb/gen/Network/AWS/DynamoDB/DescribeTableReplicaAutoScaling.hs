{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.DescribeTableReplicaAutoScaling
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes auto scaling settings across replicas of the global table at once.
module Network.AWS.DynamoDB.DescribeTableReplicaAutoScaling
  ( -- * Creating a request
    DescribeTableReplicaAutoScaling (..),
    mkDescribeTableReplicaAutoScaling,

    -- ** Request lenses
    dtrasTableName,

    -- * Destructuring the response
    DescribeTableReplicaAutoScalingResponse (..),
    mkDescribeTableReplicaAutoScalingResponse,

    -- ** Response lenses
    dtrasrsTableAutoScalingDescription,
    dtrasrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeTableReplicaAutoScaling' smart constructor.
newtype DescribeTableReplicaAutoScaling = DescribeTableReplicaAutoScaling'
  { tableName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTableReplicaAutoScaling' with the minimum fields required to make a request.
--
-- * 'tableName' - The name of the table.
mkDescribeTableReplicaAutoScaling ::
  -- | 'tableName'
  Lude.Text ->
  DescribeTableReplicaAutoScaling
mkDescribeTableReplicaAutoScaling pTableName_ =
  DescribeTableReplicaAutoScaling' {tableName = pTableName_}

-- | The name of the table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrasTableName :: Lens.Lens' DescribeTableReplicaAutoScaling Lude.Text
dtrasTableName = Lens.lens (tableName :: DescribeTableReplicaAutoScaling -> Lude.Text) (\s a -> s {tableName = a} :: DescribeTableReplicaAutoScaling)
{-# DEPRECATED dtrasTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.AWSRequest DescribeTableReplicaAutoScaling where
  type
    Rs DescribeTableReplicaAutoScaling =
      DescribeTableReplicaAutoScalingResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeTableReplicaAutoScalingResponse'
            Lude.<$> (x Lude..?> "TableAutoScalingDescription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTableReplicaAutoScaling where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DynamoDB_20120810.DescribeTableReplicaAutoScaling" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeTableReplicaAutoScaling where
  toJSON DescribeTableReplicaAutoScaling' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("TableName" Lude..= tableName)])

instance Lude.ToPath DescribeTableReplicaAutoScaling where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTableReplicaAutoScaling where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeTableReplicaAutoScalingResponse' smart constructor.
data DescribeTableReplicaAutoScalingResponse = DescribeTableReplicaAutoScalingResponse'
  { tableAutoScalingDescription ::
      Lude.Maybe
        TableAutoScalingDescription,
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

-- | Creates a value of 'DescribeTableReplicaAutoScalingResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'tableAutoScalingDescription' - Represents the auto scaling properties of the table.
mkDescribeTableReplicaAutoScalingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTableReplicaAutoScalingResponse
mkDescribeTableReplicaAutoScalingResponse pResponseStatus_ =
  DescribeTableReplicaAutoScalingResponse'
    { tableAutoScalingDescription =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Represents the auto scaling properties of the table.
--
-- /Note:/ Consider using 'tableAutoScalingDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrasrsTableAutoScalingDescription :: Lens.Lens' DescribeTableReplicaAutoScalingResponse (Lude.Maybe TableAutoScalingDescription)
dtrasrsTableAutoScalingDescription = Lens.lens (tableAutoScalingDescription :: DescribeTableReplicaAutoScalingResponse -> Lude.Maybe TableAutoScalingDescription) (\s a -> s {tableAutoScalingDescription = a} :: DescribeTableReplicaAutoScalingResponse)
{-# DEPRECATED dtrasrsTableAutoScalingDescription "Use generic-lens or generic-optics with 'tableAutoScalingDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrasrsResponseStatus :: Lens.Lens' DescribeTableReplicaAutoScalingResponse Lude.Int
dtrasrsResponseStatus = Lens.lens (responseStatus :: DescribeTableReplicaAutoScalingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTableReplicaAutoScalingResponse)
{-# DEPRECATED dtrasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
