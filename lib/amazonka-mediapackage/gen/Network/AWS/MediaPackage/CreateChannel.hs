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
-- Module      : Network.AWS.MediaPackage.CreateChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Channel.
module Network.AWS.MediaPackage.CreateChannel
    (
    -- * Creating a Request
      createChannel
    , CreateChannel
    -- * Request Lenses
    , ccDescription
    , ccId

    -- * Destructuring the Response
    , createChannelResponse
    , CreateChannelResponse
    -- * Response Lenses
    , ccrsHlsIngest
    , ccrsARN
    , ccrsId
    , ccrsDescription
    , ccrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaPackage.Types
import Network.AWS.MediaPackage.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A new Channel configuration.
--
-- /See:/ 'createChannel' smart constructor.
data CreateChannel = CreateChannel'
  { _ccDescription :: !(Maybe Text)
  , _ccId          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccDescription' - A short text description of the Channel.
--
-- * 'ccId' - The ID of the Channel. The ID must be unique within the region and it cannot be changed after a Channel is created.
createChannel
    :: Text -- ^ 'ccId'
    -> CreateChannel
createChannel pId_ = CreateChannel' {_ccDescription = Nothing, _ccId = pId_}


-- | A short text description of the Channel.
ccDescription :: Lens' CreateChannel (Maybe Text)
ccDescription = lens _ccDescription (\ s a -> s{_ccDescription = a})

-- | The ID of the Channel. The ID must be unique within the region and it cannot be changed after a Channel is created.
ccId :: Lens' CreateChannel Text
ccId = lens _ccId (\ s a -> s{_ccId = a})

instance AWSRequest CreateChannel where
        type Rs CreateChannel = CreateChannelResponse
        request = postJSON mediaPackage
        response
          = receiveJSON
              (\ s h x ->
                 CreateChannelResponse' <$>
                   (x .?> "hlsIngest") <*> (x .?> "arn") <*>
                     (x .?> "id")
                     <*> (x .?> "description")
                     <*> (pure (fromEnum s)))

instance Hashable CreateChannel where

instance NFData CreateChannel where

instance ToHeaders CreateChannel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateChannel where
        toJSON CreateChannel'{..}
          = object
              (catMaybes
                 [("description" .=) <$> _ccDescription,
                  Just ("id" .= _ccId)])

instance ToPath CreateChannel where
        toPath = const "/channels"

instance ToQuery CreateChannel where
        toQuery = const mempty

-- | /See:/ 'createChannelResponse' smart constructor.
data CreateChannelResponse = CreateChannelResponse'
  { _ccrsHlsIngest      :: !(Maybe HlsIngest)
  , _ccrsARN            :: !(Maybe Text)
  , _ccrsId             :: !(Maybe Text)
  , _ccrsDescription    :: !(Maybe Text)
  , _ccrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccrsHlsIngest' - Undocumented member.
--
-- * 'ccrsARN' - The Amazon Resource Name (ARN) assigned to the Channel.
--
-- * 'ccrsId' - The ID of the Channel.
--
-- * 'ccrsDescription' - A short text description of the Channel.
--
-- * 'ccrsResponseStatus' - -- | The response status code.
createChannelResponse
    :: Int -- ^ 'ccrsResponseStatus'
    -> CreateChannelResponse
createChannelResponse pResponseStatus_ =
  CreateChannelResponse'
    { _ccrsHlsIngest = Nothing
    , _ccrsARN = Nothing
    , _ccrsId = Nothing
    , _ccrsDescription = Nothing
    , _ccrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
ccrsHlsIngest :: Lens' CreateChannelResponse (Maybe HlsIngest)
ccrsHlsIngest = lens _ccrsHlsIngest (\ s a -> s{_ccrsHlsIngest = a})

-- | The Amazon Resource Name (ARN) assigned to the Channel.
ccrsARN :: Lens' CreateChannelResponse (Maybe Text)
ccrsARN = lens _ccrsARN (\ s a -> s{_ccrsARN = a})

-- | The ID of the Channel.
ccrsId :: Lens' CreateChannelResponse (Maybe Text)
ccrsId = lens _ccrsId (\ s a -> s{_ccrsId = a})

-- | A short text description of the Channel.
ccrsDescription :: Lens' CreateChannelResponse (Maybe Text)
ccrsDescription = lens _ccrsDescription (\ s a -> s{_ccrsDescription = a})

-- | -- | The response status code.
ccrsResponseStatus :: Lens' CreateChannelResponse Int
ccrsResponseStatus = lens _ccrsResponseStatus (\ s a -> s{_ccrsResponseStatus = a})

instance NFData CreateChannelResponse where
