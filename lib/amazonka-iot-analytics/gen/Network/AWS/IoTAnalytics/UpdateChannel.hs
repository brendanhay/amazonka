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
-- Module      : Network.AWS.IoTAnalytics.UpdateChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the settings of a channel.
module Network.AWS.IoTAnalytics.UpdateChannel
  ( -- * Creating a Request
    updateChannel,
    UpdateChannel,

    -- * Request Lenses
    ucRetentionPeriod,
    ucChannelStorage,
    ucChannelName,

    -- * Destructuring the Response
    updateChannelResponse,
    UpdateChannelResponse,
  )
where

import Network.AWS.IoTAnalytics.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateChannel' smart constructor.
data UpdateChannel = UpdateChannel'
  { _ucRetentionPeriod ::
      !(Maybe RetentionPeriod),
    _ucChannelStorage :: !(Maybe ChannelStorage),
    _ucChannelName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucRetentionPeriod' - How long, in days, message data is kept for the channel. The retention period cannot be updated if the channel's S3 storage is customer-managed.
--
-- * 'ucChannelStorage' - Where channel data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the channel is created.
--
-- * 'ucChannelName' - The name of the channel to be updated.
updateChannel ::
  -- | 'ucChannelName'
  Text ->
  UpdateChannel
updateChannel pChannelName_ =
  UpdateChannel'
    { _ucRetentionPeriod = Nothing,
      _ucChannelStorage = Nothing,
      _ucChannelName = pChannelName_
    }

-- | How long, in days, message data is kept for the channel. The retention period cannot be updated if the channel's S3 storage is customer-managed.
ucRetentionPeriod :: Lens' UpdateChannel (Maybe RetentionPeriod)
ucRetentionPeriod = lens _ucRetentionPeriod (\s a -> s {_ucRetentionPeriod = a})

-- | Where channel data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the channel is created.
ucChannelStorage :: Lens' UpdateChannel (Maybe ChannelStorage)
ucChannelStorage = lens _ucChannelStorage (\s a -> s {_ucChannelStorage = a})

-- | The name of the channel to be updated.
ucChannelName :: Lens' UpdateChannel Text
ucChannelName = lens _ucChannelName (\s a -> s {_ucChannelName = a})

instance AWSRequest UpdateChannel where
  type Rs UpdateChannel = UpdateChannelResponse
  request = putJSON ioTAnalytics
  response = receiveNull UpdateChannelResponse'

instance Hashable UpdateChannel

instance NFData UpdateChannel

instance ToHeaders UpdateChannel where
  toHeaders = const mempty

instance ToJSON UpdateChannel where
  toJSON UpdateChannel' {..} =
    object
      ( catMaybes
          [ ("retentionPeriod" .=) <$> _ucRetentionPeriod,
            ("channelStorage" .=) <$> _ucChannelStorage
          ]
      )

instance ToPath UpdateChannel where
  toPath UpdateChannel' {..} =
    mconcat ["/channels/", toBS _ucChannelName]

instance ToQuery UpdateChannel where
  toQuery = const mempty

-- | /See:/ 'updateChannelResponse' smart constructor.
data UpdateChannelResponse = UpdateChannelResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateChannelResponse' with the minimum fields required to make a request.
updateChannelResponse ::
  UpdateChannelResponse
updateChannelResponse = UpdateChannelResponse'

instance NFData UpdateChannelResponse
