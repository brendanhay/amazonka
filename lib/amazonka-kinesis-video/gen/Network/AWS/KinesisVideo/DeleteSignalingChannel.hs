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
-- Module      : Network.AWS.KinesisVideo.DeleteSignalingChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified signaling channel. @DeleteSignalingChannel@ is an asynchronous operation. If you don't specify the channel's current version, the most recent version is deleted.
module Network.AWS.KinesisVideo.DeleteSignalingChannel
  ( -- * Creating a Request
    deleteSignalingChannel,
    DeleteSignalingChannel,

    -- * Request Lenses
    dscCurrentVersion,
    dscChannelARN,

    -- * Destructuring the Response
    deleteSignalingChannelResponse,
    DeleteSignalingChannelResponse,

    -- * Response Lenses
    dscrsResponseStatus,
  )
where

import Network.AWS.KinesisVideo.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteSignalingChannel' smart constructor.
data DeleteSignalingChannel = DeleteSignalingChannel'
  { _dscCurrentVersion ::
      !(Maybe Text),
    _dscChannelARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteSignalingChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscCurrentVersion' - The current version of the signaling channel that you want to delete. You can obtain the current version by invoking the @DescribeSignalingChannel@ or @ListSignalingChannels@ API operations.
--
-- * 'dscChannelARN' - The Amazon Resource Name (ARN) of the signaling channel that you want to delete.
deleteSignalingChannel ::
  -- | 'dscChannelARN'
  Text ->
  DeleteSignalingChannel
deleteSignalingChannel pChannelARN_ =
  DeleteSignalingChannel'
    { _dscCurrentVersion = Nothing,
      _dscChannelARN = pChannelARN_
    }

-- | The current version of the signaling channel that you want to delete. You can obtain the current version by invoking the @DescribeSignalingChannel@ or @ListSignalingChannels@ API operations.
dscCurrentVersion :: Lens' DeleteSignalingChannel (Maybe Text)
dscCurrentVersion = lens _dscCurrentVersion (\s a -> s {_dscCurrentVersion = a})

-- | The Amazon Resource Name (ARN) of the signaling channel that you want to delete.
dscChannelARN :: Lens' DeleteSignalingChannel Text
dscChannelARN = lens _dscChannelARN (\s a -> s {_dscChannelARN = a})

instance AWSRequest DeleteSignalingChannel where
  type Rs DeleteSignalingChannel = DeleteSignalingChannelResponse
  request = postJSON kinesisVideo
  response =
    receiveEmpty
      ( \s h x ->
          DeleteSignalingChannelResponse' <$> (pure (fromEnum s))
      )

instance Hashable DeleteSignalingChannel

instance NFData DeleteSignalingChannel

instance ToHeaders DeleteSignalingChannel where
  toHeaders = const mempty

instance ToJSON DeleteSignalingChannel where
  toJSON DeleteSignalingChannel' {..} =
    object
      ( catMaybes
          [ ("CurrentVersion" .=) <$> _dscCurrentVersion,
            Just ("ChannelARN" .= _dscChannelARN)
          ]
      )

instance ToPath DeleteSignalingChannel where
  toPath = const "/deleteSignalingChannel"

instance ToQuery DeleteSignalingChannel where
  toQuery = const mempty

-- | /See:/ 'deleteSignalingChannelResponse' smart constructor.
newtype DeleteSignalingChannelResponse = DeleteSignalingChannelResponse'
  { _dscrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteSignalingChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscrsResponseStatus' - -- | The response status code.
deleteSignalingChannelResponse ::
  -- | 'dscrsResponseStatus'
  Int ->
  DeleteSignalingChannelResponse
deleteSignalingChannelResponse pResponseStatus_ =
  DeleteSignalingChannelResponse'
    { _dscrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
dscrsResponseStatus :: Lens' DeleteSignalingChannelResponse Int
dscrsResponseStatus = lens _dscrsResponseStatus (\s a -> s {_dscrsResponseStatus = a})

instance NFData DeleteSignalingChannelResponse
