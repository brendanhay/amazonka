{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.IncreaseStreamRetentionPeriod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Increases the Kinesis data stream's retention period, which is the length of time data records are accessible after they are added to the stream. The maximum value of a stream's retention period is 168 hours (7 days).
--
-- If you choose a longer stream retention period, this operation increases the time period during which records that have not yet expired are accessible. However, it does not make previous, expired data (older than the stream's previous retention period) accessible after the operation has been called. For example, if a stream's retention period is set to 24 hours and is increased to 168 hours, any data that is older than 24 hours remains inaccessible to consumer applications.
module Network.AWS.Kinesis.IncreaseStreamRetentionPeriod
  ( -- * Creating a request
    IncreaseStreamRetentionPeriod (..),
    mkIncreaseStreamRetentionPeriod,

    -- ** Request lenses
    isrpStreamName,
    isrpRetentionPeriodHours,

    -- * Destructuring the response
    IncreaseStreamRetentionPeriodResponse (..),
    mkIncreaseStreamRetentionPeriodResponse,
  )
where

import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for 'IncreaseStreamRetentionPeriod' .
--
-- /See:/ 'mkIncreaseStreamRetentionPeriod' smart constructor.
data IncreaseStreamRetentionPeriod = IncreaseStreamRetentionPeriod'
  { streamName ::
      Lude.Text,
    retentionPeriodHours ::
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

-- | Creates a value of 'IncreaseStreamRetentionPeriod' with the minimum fields required to make a request.
--
-- * 'retentionPeriodHours' - The new retention period of the stream, in hours. Must be more than the current retention period.
-- * 'streamName' - The name of the stream to modify.
mkIncreaseStreamRetentionPeriod ::
  -- | 'streamName'
  Lude.Text ->
  -- | 'retentionPeriodHours'
  Lude.Int ->
  IncreaseStreamRetentionPeriod
mkIncreaseStreamRetentionPeriod pStreamName_ pRetentionPeriodHours_ =
  IncreaseStreamRetentionPeriod'
    { streamName = pStreamName_,
      retentionPeriodHours = pRetentionPeriodHours_
    }

-- | The name of the stream to modify.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isrpStreamName :: Lens.Lens' IncreaseStreamRetentionPeriod Lude.Text
isrpStreamName = Lens.lens (streamName :: IncreaseStreamRetentionPeriod -> Lude.Text) (\s a -> s {streamName = a} :: IncreaseStreamRetentionPeriod)
{-# DEPRECATED isrpStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

-- | The new retention period of the stream, in hours. Must be more than the current retention period.
--
-- /Note:/ Consider using 'retentionPeriodHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isrpRetentionPeriodHours :: Lens.Lens' IncreaseStreamRetentionPeriod Lude.Int
isrpRetentionPeriodHours = Lens.lens (retentionPeriodHours :: IncreaseStreamRetentionPeriod -> Lude.Int) (\s a -> s {retentionPeriodHours = a} :: IncreaseStreamRetentionPeriod)
{-# DEPRECATED isrpRetentionPeriodHours "Use generic-lens or generic-optics with 'retentionPeriodHours' instead." #-}

instance Lude.AWSRequest IncreaseStreamRetentionPeriod where
  type
    Rs IncreaseStreamRetentionPeriod =
      IncreaseStreamRetentionPeriodResponse
  request = Req.postJSON kinesisService
  response = Res.receiveNull IncreaseStreamRetentionPeriodResponse'

instance Lude.ToHeaders IncreaseStreamRetentionPeriod where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Kinesis_20131202.IncreaseStreamRetentionPeriod" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON IncreaseStreamRetentionPeriod where
  toJSON IncreaseStreamRetentionPeriod' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("StreamName" Lude..= streamName),
            Lude.Just ("RetentionPeriodHours" Lude..= retentionPeriodHours)
          ]
      )

instance Lude.ToPath IncreaseStreamRetentionPeriod where
  toPath = Lude.const "/"

instance Lude.ToQuery IncreaseStreamRetentionPeriod where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkIncreaseStreamRetentionPeriodResponse' smart constructor.
data IncreaseStreamRetentionPeriodResponse = IncreaseStreamRetentionPeriodResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IncreaseStreamRetentionPeriodResponse' with the minimum fields required to make a request.
mkIncreaseStreamRetentionPeriodResponse ::
  IncreaseStreamRetentionPeriodResponse
mkIncreaseStreamRetentionPeriodResponse =
  IncreaseStreamRetentionPeriodResponse'
