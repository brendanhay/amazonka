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
-- Module      : Network.AWS.MediaLive.BatchUpdateSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a channel schedule
module Network.AWS.MediaLive.BatchUpdateSchedule
  ( -- * Creating a Request
    batchUpdateSchedule,
    BatchUpdateSchedule,

    -- * Request Lenses
    busCreates,
    busDeletes,
    busChannelId,

    -- * Destructuring the Response
    batchUpdateScheduleResponse,
    BatchUpdateScheduleResponse,

    -- * Response Lenses
    busrsCreates,
    busrsDeletes,
    busrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | List of actions to create and list of actions to delete.
--
-- /See:/ 'batchUpdateSchedule' smart constructor.
data BatchUpdateSchedule = BatchUpdateSchedule'
  { _busCreates ::
      !(Maybe BatchScheduleActionCreateRequest),
    _busDeletes ::
      !(Maybe BatchScheduleActionDeleteRequest),
    _busChannelId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchUpdateSchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'busCreates' - Schedule actions to create in the schedule.
--
-- * 'busDeletes' - Schedule actions to delete from the schedule.
--
-- * 'busChannelId' - Id of the channel whose schedule is being updated.
batchUpdateSchedule ::
  -- | 'busChannelId'
  Text ->
  BatchUpdateSchedule
batchUpdateSchedule pChannelId_ =
  BatchUpdateSchedule'
    { _busCreates = Nothing,
      _busDeletes = Nothing,
      _busChannelId = pChannelId_
    }

-- | Schedule actions to create in the schedule.
busCreates :: Lens' BatchUpdateSchedule (Maybe BatchScheduleActionCreateRequest)
busCreates = lens _busCreates (\s a -> s {_busCreates = a})

-- | Schedule actions to delete from the schedule.
busDeletes :: Lens' BatchUpdateSchedule (Maybe BatchScheduleActionDeleteRequest)
busDeletes = lens _busDeletes (\s a -> s {_busDeletes = a})

-- | Id of the channel whose schedule is being updated.
busChannelId :: Lens' BatchUpdateSchedule Text
busChannelId = lens _busChannelId (\s a -> s {_busChannelId = a})

instance AWSRequest BatchUpdateSchedule where
  type Rs BatchUpdateSchedule = BatchUpdateScheduleResponse
  request = putJSON mediaLive
  response =
    receiveJSON
      ( \s h x ->
          BatchUpdateScheduleResponse'
            <$> (x .?> "creates") <*> (x .?> "deletes") <*> (pure (fromEnum s))
      )

instance Hashable BatchUpdateSchedule

instance NFData BatchUpdateSchedule

instance ToHeaders BatchUpdateSchedule where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON BatchUpdateSchedule where
  toJSON BatchUpdateSchedule' {..} =
    object
      ( catMaybes
          [("creates" .=) <$> _busCreates, ("deletes" .=) <$> _busDeletes]
      )

instance ToPath BatchUpdateSchedule where
  toPath BatchUpdateSchedule' {..} =
    mconcat ["/prod/channels/", toBS _busChannelId, "/schedule"]

instance ToQuery BatchUpdateSchedule where
  toQuery = const mempty

-- | Placeholder documentation for BatchUpdateScheduleResponse
--
-- /See:/ 'batchUpdateScheduleResponse' smart constructor.
data BatchUpdateScheduleResponse = BatchUpdateScheduleResponse'
  { _busrsCreates ::
      !( Maybe
           BatchScheduleActionCreateResult
       ),
    _busrsDeletes ::
      !( Maybe
           BatchScheduleActionDeleteResult
       ),
    _busrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchUpdateScheduleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'busrsCreates' - Schedule actions created in the schedule.
--
-- * 'busrsDeletes' - Schedule actions deleted from the schedule.
--
-- * 'busrsResponseStatus' - -- | The response status code.
batchUpdateScheduleResponse ::
  -- | 'busrsResponseStatus'
  Int ->
  BatchUpdateScheduleResponse
batchUpdateScheduleResponse pResponseStatus_ =
  BatchUpdateScheduleResponse'
    { _busrsCreates = Nothing,
      _busrsDeletes = Nothing,
      _busrsResponseStatus = pResponseStatus_
    }

-- | Schedule actions created in the schedule.
busrsCreates :: Lens' BatchUpdateScheduleResponse (Maybe BatchScheduleActionCreateResult)
busrsCreates = lens _busrsCreates (\s a -> s {_busrsCreates = a})

-- | Schedule actions deleted from the schedule.
busrsDeletes :: Lens' BatchUpdateScheduleResponse (Maybe BatchScheduleActionDeleteResult)
busrsDeletes = lens _busrsDeletes (\s a -> s {_busrsDeletes = a})

-- | -- | The response status code.
busrsResponseStatus :: Lens' BatchUpdateScheduleResponse Int
busrsResponseStatus = lens _busrsResponseStatus (\s a -> s {_busrsResponseStatus = a})

instance NFData BatchUpdateScheduleResponse
