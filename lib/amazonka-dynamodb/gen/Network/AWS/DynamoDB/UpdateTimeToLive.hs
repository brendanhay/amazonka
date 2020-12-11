{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.UpdateTimeToLive
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @UpdateTimeToLive@ method enables or disables Time to Live (TTL) for the specified table. A successful @UpdateTimeToLive@ call returns the current @TimeToLiveSpecification@ . It can take up to one hour for the change to fully process. Any additional @UpdateTimeToLive@ calls for the same table during this one hour duration result in a @ValidationException@ .
--
-- TTL compares the current time in epoch time format to the time stored in the TTL attribute of an item. If the epoch time value stored in the attribute is less than the current time, the item is marked as expired and subsequently deleted.
-- DynamoDB deletes expired items on a best-effort basis to ensure availability of throughput for other data operations.
-- /Important:/ DynamoDB typically deletes expired items within two days of expiration. The exact duration within which an item gets deleted after expiration is specific to the nature of the workload. Items that have expired and not been deleted will still show up in reads, queries, and scans.
-- As items are deleted, they are removed from any local secondary index and global secondary index immediately in the same eventually consistent way as a standard delete operation.
-- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/TTL.html Time To Live> in the Amazon DynamoDB Developer Guide.
module Network.AWS.DynamoDB.UpdateTimeToLive
  ( -- * Creating a request
    UpdateTimeToLive (..),
    mkUpdateTimeToLive,

    -- ** Request lenses
    uttlTableName,
    uttlTimeToLiveSpecification,

    -- * Destructuring the response
    UpdateTimeToLiveResponse (..),
    mkUpdateTimeToLiveResponse,

    -- ** Response lenses
    uttlrsTimeToLiveSpecification,
    uttlrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of an @UpdateTimeToLive@ operation.
--
-- /See:/ 'mkUpdateTimeToLive' smart constructor.
data UpdateTimeToLive = UpdateTimeToLive'
  { tableName :: Lude.Text,
    timeToLiveSpecification :: TimeToLiveSpecification
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateTimeToLive' with the minimum fields required to make a request.
--
-- * 'tableName' - The name of the table to be configured.
-- * 'timeToLiveSpecification' - Represents the settings used to enable or disable Time to Live for the specified table.
mkUpdateTimeToLive ::
  -- | 'tableName'
  Lude.Text ->
  -- | 'timeToLiveSpecification'
  TimeToLiveSpecification ->
  UpdateTimeToLive
mkUpdateTimeToLive pTableName_ pTimeToLiveSpecification_ =
  UpdateTimeToLive'
    { tableName = pTableName_,
      timeToLiveSpecification = pTimeToLiveSpecification_
    }

-- | The name of the table to be configured.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uttlTableName :: Lens.Lens' UpdateTimeToLive Lude.Text
uttlTableName = Lens.lens (tableName :: UpdateTimeToLive -> Lude.Text) (\s a -> s {tableName = a} :: UpdateTimeToLive)
{-# DEPRECATED uttlTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | Represents the settings used to enable or disable Time to Live for the specified table.
--
-- /Note:/ Consider using 'timeToLiveSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uttlTimeToLiveSpecification :: Lens.Lens' UpdateTimeToLive TimeToLiveSpecification
uttlTimeToLiveSpecification = Lens.lens (timeToLiveSpecification :: UpdateTimeToLive -> TimeToLiveSpecification) (\s a -> s {timeToLiveSpecification = a} :: UpdateTimeToLive)
{-# DEPRECATED uttlTimeToLiveSpecification "Use generic-lens or generic-optics with 'timeToLiveSpecification' instead." #-}

instance Lude.AWSRequest UpdateTimeToLive where
  type Rs UpdateTimeToLive = UpdateTimeToLiveResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateTimeToLiveResponse'
            Lude.<$> (x Lude..?> "TimeToLiveSpecification")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateTimeToLive where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.UpdateTimeToLive" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateTimeToLive where
  toJSON UpdateTimeToLive' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TableName" Lude..= tableName),
            Lude.Just
              ("TimeToLiveSpecification" Lude..= timeToLiveSpecification)
          ]
      )

instance Lude.ToPath UpdateTimeToLive where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateTimeToLive where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateTimeToLiveResponse' smart constructor.
data UpdateTimeToLiveResponse = UpdateTimeToLiveResponse'
  { timeToLiveSpecification ::
      Lude.Maybe TimeToLiveSpecification,
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

-- | Creates a value of 'UpdateTimeToLiveResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'timeToLiveSpecification' - Represents the output of an @UpdateTimeToLive@ operation.
mkUpdateTimeToLiveResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateTimeToLiveResponse
mkUpdateTimeToLiveResponse pResponseStatus_ =
  UpdateTimeToLiveResponse'
    { timeToLiveSpecification = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Represents the output of an @UpdateTimeToLive@ operation.
--
-- /Note:/ Consider using 'timeToLiveSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uttlrsTimeToLiveSpecification :: Lens.Lens' UpdateTimeToLiveResponse (Lude.Maybe TimeToLiveSpecification)
uttlrsTimeToLiveSpecification = Lens.lens (timeToLiveSpecification :: UpdateTimeToLiveResponse -> Lude.Maybe TimeToLiveSpecification) (\s a -> s {timeToLiveSpecification = a} :: UpdateTimeToLiveResponse)
{-# DEPRECATED uttlrsTimeToLiveSpecification "Use generic-lens or generic-optics with 'timeToLiveSpecification' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uttlrsResponseStatus :: Lens.Lens' UpdateTimeToLiveResponse Lude.Int
uttlrsResponseStatus = Lens.lens (responseStatus :: UpdateTimeToLiveResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateTimeToLiveResponse)
{-# DEPRECATED uttlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
