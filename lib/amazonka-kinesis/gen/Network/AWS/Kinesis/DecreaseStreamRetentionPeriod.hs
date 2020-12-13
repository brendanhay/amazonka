{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.DecreaseStreamRetentionPeriod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Decreases the Kinesis data stream's retention period, which is the length of time data records are accessible after they are added to the stream. The minimum value of a stream's retention period is 24 hours.
--
-- This operation may result in lost data. For example, if the stream's retention period is 48 hours and is decreased to 24 hours, any data already in the stream that is older than 24 hours is inaccessible.
module Network.AWS.Kinesis.DecreaseStreamRetentionPeriod
  ( -- * Creating a request
    DecreaseStreamRetentionPeriod (..),
    mkDecreaseStreamRetentionPeriod,

    -- ** Request lenses
    dsrpRetentionPeriodHours,
    dsrpStreamName,

    -- * Destructuring the response
    DecreaseStreamRetentionPeriodResponse (..),
    mkDecreaseStreamRetentionPeriodResponse,
  )
where

import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for 'DecreaseStreamRetentionPeriod' .
--
-- /See:/ 'mkDecreaseStreamRetentionPeriod' smart constructor.
data DecreaseStreamRetentionPeriod = DecreaseStreamRetentionPeriod'
  { -- | The new retention period of the stream, in hours. Must be less than the current retention period.
    retentionPeriodHours :: Lude.Int,
    -- | The name of the stream to modify.
    streamName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DecreaseStreamRetentionPeriod' with the minimum fields required to make a request.
--
-- * 'retentionPeriodHours' - The new retention period of the stream, in hours. Must be less than the current retention period.
-- * 'streamName' - The name of the stream to modify.
mkDecreaseStreamRetentionPeriod ::
  -- | 'retentionPeriodHours'
  Lude.Int ->
  -- | 'streamName'
  Lude.Text ->
  DecreaseStreamRetentionPeriod
mkDecreaseStreamRetentionPeriod pRetentionPeriodHours_ pStreamName_ =
  DecreaseStreamRetentionPeriod'
    { retentionPeriodHours =
        pRetentionPeriodHours_,
      streamName = pStreamName_
    }

-- | The new retention period of the stream, in hours. Must be less than the current retention period.
--
-- /Note:/ Consider using 'retentionPeriodHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrpRetentionPeriodHours :: Lens.Lens' DecreaseStreamRetentionPeriod Lude.Int
dsrpRetentionPeriodHours = Lens.lens (retentionPeriodHours :: DecreaseStreamRetentionPeriod -> Lude.Int) (\s a -> s {retentionPeriodHours = a} :: DecreaseStreamRetentionPeriod)
{-# DEPRECATED dsrpRetentionPeriodHours "Use generic-lens or generic-optics with 'retentionPeriodHours' instead." #-}

-- | The name of the stream to modify.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrpStreamName :: Lens.Lens' DecreaseStreamRetentionPeriod Lude.Text
dsrpStreamName = Lens.lens (streamName :: DecreaseStreamRetentionPeriod -> Lude.Text) (\s a -> s {streamName = a} :: DecreaseStreamRetentionPeriod)
{-# DEPRECATED dsrpStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Lude.AWSRequest DecreaseStreamRetentionPeriod where
  type
    Rs DecreaseStreamRetentionPeriod =
      DecreaseStreamRetentionPeriodResponse
  request = Req.postJSON kinesisService
  response = Res.receiveNull DecreaseStreamRetentionPeriodResponse'

instance Lude.ToHeaders DecreaseStreamRetentionPeriod where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Kinesis_20131202.DecreaseStreamRetentionPeriod" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DecreaseStreamRetentionPeriod where
  toJSON DecreaseStreamRetentionPeriod' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("RetentionPeriodHours" Lude..= retentionPeriodHours),
            Lude.Just ("StreamName" Lude..= streamName)
          ]
      )

instance Lude.ToPath DecreaseStreamRetentionPeriod where
  toPath = Lude.const "/"

instance Lude.ToQuery DecreaseStreamRetentionPeriod where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDecreaseStreamRetentionPeriodResponse' smart constructor.
data DecreaseStreamRetentionPeriodResponse = DecreaseStreamRetentionPeriodResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DecreaseStreamRetentionPeriodResponse' with the minimum fields required to make a request.
mkDecreaseStreamRetentionPeriodResponse ::
  DecreaseStreamRetentionPeriodResponse
mkDecreaseStreamRetentionPeriodResponse =
  DecreaseStreamRetentionPeriodResponse'
