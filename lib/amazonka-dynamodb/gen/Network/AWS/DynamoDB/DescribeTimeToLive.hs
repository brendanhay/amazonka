{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.DescribeTimeToLive
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gives a description of the Time to Live (TTL) status on the specified table.
module Network.AWS.DynamoDB.DescribeTimeToLive
  ( -- * Creating a request
    DescribeTimeToLive (..),
    mkDescribeTimeToLive,

    -- ** Request lenses
    dttlTableName,

    -- * Destructuring the response
    DescribeTimeToLiveResponse (..),
    mkDescribeTimeToLiveResponse,

    -- ** Response lenses
    dttlrsTimeToLiveDescription,
    dttlrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeTimeToLive' smart constructor.
newtype DescribeTimeToLive = DescribeTimeToLive'
  { -- | The name of the table to be described.
    tableName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTimeToLive' with the minimum fields required to make a request.
--
-- * 'tableName' - The name of the table to be described.
mkDescribeTimeToLive ::
  -- | 'tableName'
  Lude.Text ->
  DescribeTimeToLive
mkDescribeTimeToLive pTableName_ =
  DescribeTimeToLive' {tableName = pTableName_}

-- | The name of the table to be described.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttlTableName :: Lens.Lens' DescribeTimeToLive Lude.Text
dttlTableName = Lens.lens (tableName :: DescribeTimeToLive -> Lude.Text) (\s a -> s {tableName = a} :: DescribeTimeToLive)
{-# DEPRECATED dttlTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.AWSRequest DescribeTimeToLive where
  type Rs DescribeTimeToLive = DescribeTimeToLiveResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeTimeToLiveResponse'
            Lude.<$> (x Lude..?> "TimeToLiveDescription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTimeToLive where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.DescribeTimeToLive" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeTimeToLive where
  toJSON DescribeTimeToLive' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("TableName" Lude..= tableName)])

instance Lude.ToPath DescribeTimeToLive where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTimeToLive where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeTimeToLiveResponse' smart constructor.
data DescribeTimeToLiveResponse = DescribeTimeToLiveResponse'
  { -- |
    timeToLiveDescription :: Lude.Maybe TimeToLiveDescription,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTimeToLiveResponse' with the minimum fields required to make a request.
--
-- * 'timeToLiveDescription' -
-- * 'responseStatus' - The response status code.
mkDescribeTimeToLiveResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTimeToLiveResponse
mkDescribeTimeToLiveResponse pResponseStatus_ =
  DescribeTimeToLiveResponse'
    { timeToLiveDescription = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- |
--
-- /Note:/ Consider using 'timeToLiveDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttlrsTimeToLiveDescription :: Lens.Lens' DescribeTimeToLiveResponse (Lude.Maybe TimeToLiveDescription)
dttlrsTimeToLiveDescription = Lens.lens (timeToLiveDescription :: DescribeTimeToLiveResponse -> Lude.Maybe TimeToLiveDescription) (\s a -> s {timeToLiveDescription = a} :: DescribeTimeToLiveResponse)
{-# DEPRECATED dttlrsTimeToLiveDescription "Use generic-lens or generic-optics with 'timeToLiveDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttlrsResponseStatus :: Lens.Lens' DescribeTimeToLiveResponse Lude.Int
dttlrsResponseStatus = Lens.lens (responseStatus :: DescribeTimeToLiveResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTimeToLiveResponse)
{-# DEPRECATED dttlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
