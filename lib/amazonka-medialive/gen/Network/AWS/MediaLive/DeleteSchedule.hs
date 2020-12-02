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
-- Module      : Network.AWS.MediaLive.DeleteSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete all schedule actions on a channel.
module Network.AWS.MediaLive.DeleteSchedule
  ( -- * Creating a Request
    deleteSchedule,
    DeleteSchedule,

    -- * Request Lenses
    dsChannelId,

    -- * Destructuring the Response
    deleteScheduleResponse,
    DeleteScheduleResponse,

    -- * Response Lenses
    dsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for DeleteScheduleRequest
--
-- /See:/ 'deleteSchedule' smart constructor.
newtype DeleteSchedule = DeleteSchedule' {_dsChannelId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteSchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsChannelId' - Id of the channel whose schedule is being deleted.
deleteSchedule ::
  -- | 'dsChannelId'
  Text ->
  DeleteSchedule
deleteSchedule pChannelId_ =
  DeleteSchedule' {_dsChannelId = pChannelId_}

-- | Id of the channel whose schedule is being deleted.
dsChannelId :: Lens' DeleteSchedule Text
dsChannelId = lens _dsChannelId (\s a -> s {_dsChannelId = a})

instance AWSRequest DeleteSchedule where
  type Rs DeleteSchedule = DeleteScheduleResponse
  request = delete mediaLive
  response =
    receiveEmpty
      (\s h x -> DeleteScheduleResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteSchedule

instance NFData DeleteSchedule

instance ToHeaders DeleteSchedule where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DeleteSchedule where
  toPath DeleteSchedule' {..} =
    mconcat ["/prod/channels/", toBS _dsChannelId, "/schedule"]

instance ToQuery DeleteSchedule where
  toQuery = const mempty

-- | Placeholder documentation for DeleteScheduleResponse
--
-- /See:/ 'deleteScheduleResponse' smart constructor.
newtype DeleteScheduleResponse = DeleteScheduleResponse'
  { _dsrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteScheduleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrsResponseStatus' - -- | The response status code.
deleteScheduleResponse ::
  -- | 'dsrsResponseStatus'
  Int ->
  DeleteScheduleResponse
deleteScheduleResponse pResponseStatus_ =
  DeleteScheduleResponse' {_dsrsResponseStatus = pResponseStatus_}

-- | -- | The response status code.
dsrsResponseStatus :: Lens' DeleteScheduleResponse Int
dsrsResponseStatus = lens _dsrsResponseStatus (\s a -> s {_dsrsResponseStatus = a})

instance NFData DeleteScheduleResponse
