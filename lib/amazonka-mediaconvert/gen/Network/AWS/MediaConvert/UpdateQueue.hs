{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.UpdateQueue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify one of your existing queues.
module Network.AWS.MediaConvert.UpdateQueue
  ( -- * Creating a Request
    updateQueue,
    UpdateQueue,

    -- * Request Lenses
    uqStatus,
    uqDescription,
    uqReservationPlanSettings,
    uqName,

    -- * Destructuring the Response
    updateQueueResponse,
    UpdateQueueResponse,

    -- * Response Lenses
    uqrsQueue,
    uqrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateQueue' smart constructor.
data UpdateQueue = UpdateQueue'
  { _uqStatus :: !(Maybe QueueStatus),
    _uqDescription :: !(Maybe Text),
    _uqReservationPlanSettings :: !(Maybe ReservationPlanSettings),
    _uqName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateQueue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uqStatus' - Pause or activate a queue by changing its status between ACTIVE and PAUSED. If you pause a queue, jobs in that queue won't begin. Jobs that are running when you pause the queue continue to run until they finish or result in an error.
--
-- * 'uqDescription' - The new description for the queue, if you are changing it.
--
-- * 'uqReservationPlanSettings' - The new details of your pricing plan for your reserved queue. When you set up a new pricing plan to replace an expired one, you enter into another 12-month commitment. When you add capacity to your queue by increasing the number of RTS, you extend the term of your commitment to 12 months from when you add capacity. After you make these commitments, you can't cancel them.
--
-- * 'uqName' - The name of the queue that you are modifying.
updateQueue ::
  -- | 'uqName'
  Text ->
  UpdateQueue
updateQueue pName_ =
  UpdateQueue'
    { _uqStatus = Nothing,
      _uqDescription = Nothing,
      _uqReservationPlanSettings = Nothing,
      _uqName = pName_
    }

-- | Pause or activate a queue by changing its status between ACTIVE and PAUSED. If you pause a queue, jobs in that queue won't begin. Jobs that are running when you pause the queue continue to run until they finish or result in an error.
uqStatus :: Lens' UpdateQueue (Maybe QueueStatus)
uqStatus = lens _uqStatus (\s a -> s {_uqStatus = a})

-- | The new description for the queue, if you are changing it.
uqDescription :: Lens' UpdateQueue (Maybe Text)
uqDescription = lens _uqDescription (\s a -> s {_uqDescription = a})

-- | The new details of your pricing plan for your reserved queue. When you set up a new pricing plan to replace an expired one, you enter into another 12-month commitment. When you add capacity to your queue by increasing the number of RTS, you extend the term of your commitment to 12 months from when you add capacity. After you make these commitments, you can't cancel them.
uqReservationPlanSettings :: Lens' UpdateQueue (Maybe ReservationPlanSettings)
uqReservationPlanSettings = lens _uqReservationPlanSettings (\s a -> s {_uqReservationPlanSettings = a})

-- | The name of the queue that you are modifying.
uqName :: Lens' UpdateQueue Text
uqName = lens _uqName (\s a -> s {_uqName = a})

instance AWSRequest UpdateQueue where
  type Rs UpdateQueue = UpdateQueueResponse
  request = putJSON mediaConvert
  response =
    receiveJSON
      ( \s h x ->
          UpdateQueueResponse' <$> (x .?> "queue") <*> (pure (fromEnum s))
      )

instance Hashable UpdateQueue

instance NFData UpdateQueue

instance ToHeaders UpdateQueue where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdateQueue where
  toJSON UpdateQueue' {..} =
    object
      ( catMaybes
          [ ("status" .=) <$> _uqStatus,
            ("description" .=) <$> _uqDescription,
            ("reservationPlanSettings" .=) <$> _uqReservationPlanSettings
          ]
      )

instance ToPath UpdateQueue where
  toPath UpdateQueue' {..} =
    mconcat ["/2017-08-29/queues/", toBS _uqName]

instance ToQuery UpdateQueue where
  toQuery = const mempty

-- | /See:/ 'updateQueueResponse' smart constructor.
data UpdateQueueResponse = UpdateQueueResponse'
  { _uqrsQueue ::
      !(Maybe Queue),
    _uqrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateQueueResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uqrsQueue' - You can use queues to manage the resources that are available to your AWS account for running multiple transcoding jobs at the same time. If you don't specify a queue, the service sends all jobs through the default queue. For more information, see https://docs.aws.amazon.com/mediaconvert/latest/ug/working-with-queues.html.
--
-- * 'uqrsResponseStatus' - -- | The response status code.
updateQueueResponse ::
  -- | 'uqrsResponseStatus'
  Int ->
  UpdateQueueResponse
updateQueueResponse pResponseStatus_ =
  UpdateQueueResponse'
    { _uqrsQueue = Nothing,
      _uqrsResponseStatus = pResponseStatus_
    }

-- | You can use queues to manage the resources that are available to your AWS account for running multiple transcoding jobs at the same time. If you don't specify a queue, the service sends all jobs through the default queue. For more information, see https://docs.aws.amazon.com/mediaconvert/latest/ug/working-with-queues.html.
uqrsQueue :: Lens' UpdateQueueResponse (Maybe Queue)
uqrsQueue = lens _uqrsQueue (\s a -> s {_uqrsQueue = a})

-- | -- | The response status code.
uqrsResponseStatus :: Lens' UpdateQueueResponse Int
uqrsResponseStatus = lens _uqrsResponseStatus (\s a -> s {_uqrsResponseStatus = a})

instance NFData UpdateQueueResponse
