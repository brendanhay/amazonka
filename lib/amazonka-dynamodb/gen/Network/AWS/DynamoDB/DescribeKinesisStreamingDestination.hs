{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.DescribeKinesisStreamingDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the status of Kinesis streaming.
module Network.AWS.DynamoDB.DescribeKinesisStreamingDestination
  ( -- * Creating a request
    DescribeKinesisStreamingDestination (..),
    mkDescribeKinesisStreamingDestination,

    -- ** Request lenses
    dksdkTableName,

    -- * Destructuring the response
    DescribeKinesisStreamingDestinationResponse (..),
    mkDescribeKinesisStreamingDestinationResponse,

    -- ** Response lenses
    dksdrsKinesisDataStreamDestinations,
    dksdrsTableName,
    dksdrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeKinesisStreamingDestination' smart constructor.
newtype DescribeKinesisStreamingDestination = DescribeKinesisStreamingDestination'
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

-- | Creates a value of 'DescribeKinesisStreamingDestination' with the minimum fields required to make a request.
--
-- * 'tableName' - The name of the table being described.
mkDescribeKinesisStreamingDestination ::
  -- | 'tableName'
  Lude.Text ->
  DescribeKinesisStreamingDestination
mkDescribeKinesisStreamingDestination pTableName_ =
  DescribeKinesisStreamingDestination' {tableName = pTableName_}

-- | The name of the table being described.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dksdkTableName :: Lens.Lens' DescribeKinesisStreamingDestination Lude.Text
dksdkTableName = Lens.lens (tableName :: DescribeKinesisStreamingDestination -> Lude.Text) (\s a -> s {tableName = a} :: DescribeKinesisStreamingDestination)
{-# DEPRECATED dksdkTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.AWSRequest DescribeKinesisStreamingDestination where
  type
    Rs DescribeKinesisStreamingDestination =
      DescribeKinesisStreamingDestinationResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeKinesisStreamingDestinationResponse'
            Lude.<$> (x Lude..?> "KinesisDataStreamDestinations" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "TableName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeKinesisStreamingDestination where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DynamoDB_20120810.DescribeKinesisStreamingDestination" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeKinesisStreamingDestination where
  toJSON DescribeKinesisStreamingDestination' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("TableName" Lude..= tableName)])

instance Lude.ToPath DescribeKinesisStreamingDestination where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeKinesisStreamingDestination where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeKinesisStreamingDestinationResponse' smart constructor.
data DescribeKinesisStreamingDestinationResponse = DescribeKinesisStreamingDestinationResponse'
  { kinesisDataStreamDestinations ::
      Lude.Maybe
        [KinesisDataStreamDestination],
    tableName ::
      Lude.Maybe
        Lude.Text,
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

-- | Creates a value of 'DescribeKinesisStreamingDestinationResponse' with the minimum fields required to make a request.
--
-- * 'kinesisDataStreamDestinations' - The list of replica structures for the table being described.
-- * 'responseStatus' - The response status code.
-- * 'tableName' - The name of the table being described.
mkDescribeKinesisStreamingDestinationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeKinesisStreamingDestinationResponse
mkDescribeKinesisStreamingDestinationResponse pResponseStatus_ =
  DescribeKinesisStreamingDestinationResponse'
    { kinesisDataStreamDestinations =
        Lude.Nothing,
      tableName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of replica structures for the table being described.
--
-- /Note:/ Consider using 'kinesisDataStreamDestinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dksdrsKinesisDataStreamDestinations :: Lens.Lens' DescribeKinesisStreamingDestinationResponse (Lude.Maybe [KinesisDataStreamDestination])
dksdrsKinesisDataStreamDestinations = Lens.lens (kinesisDataStreamDestinations :: DescribeKinesisStreamingDestinationResponse -> Lude.Maybe [KinesisDataStreamDestination]) (\s a -> s {kinesisDataStreamDestinations = a} :: DescribeKinesisStreamingDestinationResponse)
{-# DEPRECATED dksdrsKinesisDataStreamDestinations "Use generic-lens or generic-optics with 'kinesisDataStreamDestinations' instead." #-}

-- | The name of the table being described.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dksdrsTableName :: Lens.Lens' DescribeKinesisStreamingDestinationResponse (Lude.Maybe Lude.Text)
dksdrsTableName = Lens.lens (tableName :: DescribeKinesisStreamingDestinationResponse -> Lude.Maybe Lude.Text) (\s a -> s {tableName = a} :: DescribeKinesisStreamingDestinationResponse)
{-# DEPRECATED dksdrsTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dksdrsResponseStatus :: Lens.Lens' DescribeKinesisStreamingDestinationResponse Lude.Int
dksdrsResponseStatus = Lens.lens (responseStatus :: DescribeKinesisStreamingDestinationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeKinesisStreamingDestinationResponse)
{-# DEPRECATED dksdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
