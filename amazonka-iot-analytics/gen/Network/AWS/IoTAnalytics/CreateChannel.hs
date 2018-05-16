{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.CreateChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a channel. A channel collects data from an MQTT topic and archives the raw, unprocessed messages before publishing the data to a pipeline.
--
--
module Network.AWS.IoTAnalytics.CreateChannel
    (
    -- * Creating a Request
      createChannel
    , CreateChannel
    -- * Request Lenses
    , ccRetentionPeriod
    , ccChannelName

    -- * Destructuring the Response
    , createChannelResponse
    , CreateChannelResponse
    -- * Response Lenses
    , ccrsChannelARN
    , ccrsRetentionPeriod
    , ccrsChannelName
    , ccrsResponseStatus
    ) where

import Network.AWS.IoTAnalytics.Types
import Network.AWS.IoTAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createChannel' smart constructor.
data CreateChannel = CreateChannel'
  { _ccRetentionPeriod :: !(Maybe RetentionPeriod)
  , _ccChannelName     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccRetentionPeriod' - How long, in days, message data is kept for the channel.
--
-- * 'ccChannelName' - The name of the channel.
createChannel
    :: Text -- ^ 'ccChannelName'
    -> CreateChannel
createChannel pChannelName_ =
  CreateChannel' {_ccRetentionPeriod = Nothing, _ccChannelName = pChannelName_}


-- | How long, in days, message data is kept for the channel.
ccRetentionPeriod :: Lens' CreateChannel (Maybe RetentionPeriod)
ccRetentionPeriod = lens _ccRetentionPeriod (\ s a -> s{_ccRetentionPeriod = a})

-- | The name of the channel.
ccChannelName :: Lens' CreateChannel Text
ccChannelName = lens _ccChannelName (\ s a -> s{_ccChannelName = a})

instance AWSRequest CreateChannel where
        type Rs CreateChannel = CreateChannelResponse
        request = postJSON ioTAnalytics
        response
          = receiveJSON
              (\ s h x ->
                 CreateChannelResponse' <$>
                   (x .?> "channelArn") <*> (x .?> "retentionPeriod")
                     <*> (x .?> "channelName")
                     <*> (pure (fromEnum s)))

instance Hashable CreateChannel where

instance NFData CreateChannel where

instance ToHeaders CreateChannel where
        toHeaders = const mempty

instance ToJSON CreateChannel where
        toJSON CreateChannel'{..}
          = object
              (catMaybes
                 [("retentionPeriod" .=) <$> _ccRetentionPeriod,
                  Just ("channelName" .= _ccChannelName)])

instance ToPath CreateChannel where
        toPath = const "/channels"

instance ToQuery CreateChannel where
        toQuery = const mempty

-- | /See:/ 'createChannelResponse' smart constructor.
data CreateChannelResponse = CreateChannelResponse'
  { _ccrsChannelARN      :: !(Maybe Text)
  , _ccrsRetentionPeriod :: !(Maybe RetentionPeriod)
  , _ccrsChannelName     :: !(Maybe Text)
  , _ccrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccrsChannelARN' - The ARN of the channel.
--
-- * 'ccrsRetentionPeriod' - How long, in days, message data is kept for the channel.
--
-- * 'ccrsChannelName' - The name of the channel.
--
-- * 'ccrsResponseStatus' - -- | The response status code.
createChannelResponse
    :: Int -- ^ 'ccrsResponseStatus'
    -> CreateChannelResponse
createChannelResponse pResponseStatus_ =
  CreateChannelResponse'
    { _ccrsChannelARN = Nothing
    , _ccrsRetentionPeriod = Nothing
    , _ccrsChannelName = Nothing
    , _ccrsResponseStatus = pResponseStatus_
    }


-- | The ARN of the channel.
ccrsChannelARN :: Lens' CreateChannelResponse (Maybe Text)
ccrsChannelARN = lens _ccrsChannelARN (\ s a -> s{_ccrsChannelARN = a})

-- | How long, in days, message data is kept for the channel.
ccrsRetentionPeriod :: Lens' CreateChannelResponse (Maybe RetentionPeriod)
ccrsRetentionPeriod = lens _ccrsRetentionPeriod (\ s a -> s{_ccrsRetentionPeriod = a})

-- | The name of the channel.
ccrsChannelName :: Lens' CreateChannelResponse (Maybe Text)
ccrsChannelName = lens _ccrsChannelName (\ s a -> s{_ccrsChannelName = a})

-- | -- | The response status code.
ccrsResponseStatus :: Lens' CreateChannelResponse Int
ccrsResponseStatus = lens _ccrsResponseStatus (\ s a -> s{_ccrsResponseStatus = a})

instance NFData CreateChannelResponse where
