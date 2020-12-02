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
-- Module      : Network.AWS.MediaConvert.CreateQueue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new transcoding queue. For information about queues, see Working With Queues in the User Guide at https://docs.aws.amazon.com/mediaconvert/latest/ug/working-with-queues.html
module Network.AWS.MediaConvert.CreateQueue
  ( -- * Creating a Request
    createQueue,
    CreateQueue,

    -- * Request Lenses
    cqStatus,
    cqPricingPlan,
    cqDescription,
    cqReservationPlanSettings,
    cqTags,
    cqName,

    -- * Destructuring the Response
    createQueueResponse,
    CreateQueueResponse,

    -- * Response Lenses
    cqrsQueue,
    cqrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createQueue' smart constructor.
data CreateQueue = CreateQueue'
  { _cqStatus :: !(Maybe QueueStatus),
    _cqPricingPlan :: !(Maybe PricingPlan),
    _cqDescription :: !(Maybe Text),
    _cqReservationPlanSettings :: !(Maybe ReservationPlanSettings),
    _cqTags :: !(Maybe (Map Text (Text))),
    _cqName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateQueue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cqStatus' - Initial state of the queue. If you create a paused queue, then jobs in that queue won't begin.
--
-- * 'cqPricingPlan' - Specifies whether the pricing plan for the queue is on-demand or reserved. For on-demand, you pay per minute, billed in increments of .01 minute. For reserved, you pay for the transcoding capacity of the entire queue, regardless of how much or how little you use it. Reserved pricing requires a 12-month commitment. When you use the API to create a queue, the default is on-demand.
--
-- * 'cqDescription' - Optional. A description of the queue that you are creating.
--
-- * 'cqReservationPlanSettings' - Details about the pricing plan for your reserved queue. Required for reserved queues and not applicable to on-demand queues.
--
-- * 'cqTags' - The tags that you want to add to the resource. You can tag resources with a key-value pair or with only a key.
--
-- * 'cqName' - The name of the queue that you are creating.
createQueue ::
  -- | 'cqName'
  Text ->
  CreateQueue
createQueue pName_ =
  CreateQueue'
    { _cqStatus = Nothing,
      _cqPricingPlan = Nothing,
      _cqDescription = Nothing,
      _cqReservationPlanSettings = Nothing,
      _cqTags = Nothing,
      _cqName = pName_
    }

-- | Initial state of the queue. If you create a paused queue, then jobs in that queue won't begin.
cqStatus :: Lens' CreateQueue (Maybe QueueStatus)
cqStatus = lens _cqStatus (\s a -> s {_cqStatus = a})

-- | Specifies whether the pricing plan for the queue is on-demand or reserved. For on-demand, you pay per minute, billed in increments of .01 minute. For reserved, you pay for the transcoding capacity of the entire queue, regardless of how much or how little you use it. Reserved pricing requires a 12-month commitment. When you use the API to create a queue, the default is on-demand.
cqPricingPlan :: Lens' CreateQueue (Maybe PricingPlan)
cqPricingPlan = lens _cqPricingPlan (\s a -> s {_cqPricingPlan = a})

-- | Optional. A description of the queue that you are creating.
cqDescription :: Lens' CreateQueue (Maybe Text)
cqDescription = lens _cqDescription (\s a -> s {_cqDescription = a})

-- | Details about the pricing plan for your reserved queue. Required for reserved queues and not applicable to on-demand queues.
cqReservationPlanSettings :: Lens' CreateQueue (Maybe ReservationPlanSettings)
cqReservationPlanSettings = lens _cqReservationPlanSettings (\s a -> s {_cqReservationPlanSettings = a})

-- | The tags that you want to add to the resource. You can tag resources with a key-value pair or with only a key.
cqTags :: Lens' CreateQueue (HashMap Text (Text))
cqTags = lens _cqTags (\s a -> s {_cqTags = a}) . _Default . _Map

-- | The name of the queue that you are creating.
cqName :: Lens' CreateQueue Text
cqName = lens _cqName (\s a -> s {_cqName = a})

instance AWSRequest CreateQueue where
  type Rs CreateQueue = CreateQueueResponse
  request = postJSON mediaConvert
  response =
    receiveJSON
      ( \s h x ->
          CreateQueueResponse' <$> (x .?> "queue") <*> (pure (fromEnum s))
      )

instance Hashable CreateQueue

instance NFData CreateQueue

instance ToHeaders CreateQueue where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON CreateQueue where
  toJSON CreateQueue' {..} =
    object
      ( catMaybes
          [ ("status" .=) <$> _cqStatus,
            ("pricingPlan" .=) <$> _cqPricingPlan,
            ("description" .=) <$> _cqDescription,
            ("reservationPlanSettings" .=) <$> _cqReservationPlanSettings,
            ("tags" .=) <$> _cqTags,
            Just ("name" .= _cqName)
          ]
      )

instance ToPath CreateQueue where
  toPath = const "/2017-08-29/queues"

instance ToQuery CreateQueue where
  toQuery = const mempty

-- | /See:/ 'createQueueResponse' smart constructor.
data CreateQueueResponse = CreateQueueResponse'
  { _cqrsQueue ::
      !(Maybe Queue),
    _cqrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateQueueResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cqrsQueue' - You can use queues to manage the resources that are available to your AWS account for running multiple transcoding jobs at the same time. If you don't specify a queue, the service sends all jobs through the default queue. For more information, see https://docs.aws.amazon.com/mediaconvert/latest/ug/working-with-queues.html.
--
-- * 'cqrsResponseStatus' - -- | The response status code.
createQueueResponse ::
  -- | 'cqrsResponseStatus'
  Int ->
  CreateQueueResponse
createQueueResponse pResponseStatus_ =
  CreateQueueResponse'
    { _cqrsQueue = Nothing,
      _cqrsResponseStatus = pResponseStatus_
    }

-- | You can use queues to manage the resources that are available to your AWS account for running multiple transcoding jobs at the same time. If you don't specify a queue, the service sends all jobs through the default queue. For more information, see https://docs.aws.amazon.com/mediaconvert/latest/ug/working-with-queues.html.
cqrsQueue :: Lens' CreateQueueResponse (Maybe Queue)
cqrsQueue = lens _cqrsQueue (\s a -> s {_cqrsQueue = a})

-- | -- | The response status code.
cqrsResponseStatus :: Lens' CreateQueueResponse Int
cqrsResponseStatus = lens _cqrsResponseStatus (\s a -> s {_cqrsResponseStatus = a})

instance NFData CreateQueueResponse
