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
-- Module      : Network.AWS.MediaLive.UpdateChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a channel.
module Network.AWS.MediaLive.UpdateChannel
    (
    -- * Creating a Request
      updateChannel
    , UpdateChannel
    -- * Request Lenses
    , ucInputSpecification
    , ucInputAttachments
    , ucDestinations
    , ucName
    , ucEncoderSettings
    , ucRoleARN
    , ucChannelId

    -- * Destructuring the Response
    , updateChannelResponse
    , UpdateChannelResponse
    -- * Response Lenses
    , ucrsChannel
    , ucrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.MediaLive.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A request to update a channel.
--
-- /See:/ 'updateChannel' smart constructor.
data UpdateChannel = UpdateChannel'
  { _ucInputSpecification :: !(Maybe InputSpecification)
  , _ucInputAttachments   :: !(Maybe [InputAttachment])
  , _ucDestinations       :: !(Maybe [OutputDestination])
  , _ucName               :: !(Maybe Text)
  , _ucEncoderSettings    :: !(Maybe EncoderSettings)
  , _ucRoleARN            :: !(Maybe Text)
  , _ucChannelId          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucInputSpecification' - Specification of input for this channel (max. bitrate, resolution, codec, etc.)
--
-- * 'ucInputAttachments' - Undocumented member.
--
-- * 'ucDestinations' - A list of output destinations for this channel.
--
-- * 'ucName' - The name of the channel.
--
-- * 'ucEncoderSettings' - The encoder settings for this channel.
--
-- * 'ucRoleARN' - An optional Amazon Resource Name (ARN) of the role to assume when running the Channel. If you do not specify this on an update call but the role was previously set that role will be removed.
--
-- * 'ucChannelId' - channel ID
updateChannel
    :: Text -- ^ 'ucChannelId'
    -> UpdateChannel
updateChannel pChannelId_ =
  UpdateChannel'
    { _ucInputSpecification = Nothing
    , _ucInputAttachments = Nothing
    , _ucDestinations = Nothing
    , _ucName = Nothing
    , _ucEncoderSettings = Nothing
    , _ucRoleARN = Nothing
    , _ucChannelId = pChannelId_
    }


-- | Specification of input for this channel (max. bitrate, resolution, codec, etc.)
ucInputSpecification :: Lens' UpdateChannel (Maybe InputSpecification)
ucInputSpecification = lens _ucInputSpecification (\ s a -> s{_ucInputSpecification = a})

-- | Undocumented member.
ucInputAttachments :: Lens' UpdateChannel [InputAttachment]
ucInputAttachments = lens _ucInputAttachments (\ s a -> s{_ucInputAttachments = a}) . _Default . _Coerce

-- | A list of output destinations for this channel.
ucDestinations :: Lens' UpdateChannel [OutputDestination]
ucDestinations = lens _ucDestinations (\ s a -> s{_ucDestinations = a}) . _Default . _Coerce

-- | The name of the channel.
ucName :: Lens' UpdateChannel (Maybe Text)
ucName = lens _ucName (\ s a -> s{_ucName = a})

-- | The encoder settings for this channel.
ucEncoderSettings :: Lens' UpdateChannel (Maybe EncoderSettings)
ucEncoderSettings = lens _ucEncoderSettings (\ s a -> s{_ucEncoderSettings = a})

-- | An optional Amazon Resource Name (ARN) of the role to assume when running the Channel. If you do not specify this on an update call but the role was previously set that role will be removed.
ucRoleARN :: Lens' UpdateChannel (Maybe Text)
ucRoleARN = lens _ucRoleARN (\ s a -> s{_ucRoleARN = a})

-- | channel ID
ucChannelId :: Lens' UpdateChannel Text
ucChannelId = lens _ucChannelId (\ s a -> s{_ucChannelId = a})

instance AWSRequest UpdateChannel where
        type Rs UpdateChannel = UpdateChannelResponse
        request = putJSON mediaLive
        response
          = receiveJSON
              (\ s h x ->
                 UpdateChannelResponse' <$>
                   (x .?> "channel") <*> (pure (fromEnum s)))

instance Hashable UpdateChannel where

instance NFData UpdateChannel where

instance ToHeaders UpdateChannel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateChannel where
        toJSON UpdateChannel'{..}
          = object
              (catMaybes
                 [("inputSpecification" .=) <$> _ucInputSpecification,
                  ("inputAttachments" .=) <$> _ucInputAttachments,
                  ("destinations" .=) <$> _ucDestinations,
                  ("name" .=) <$> _ucName,
                  ("encoderSettings" .=) <$> _ucEncoderSettings,
                  ("roleArn" .=) <$> _ucRoleARN])

instance ToPath UpdateChannel where
        toPath UpdateChannel'{..}
          = mconcat ["/prod/channels/", toBS _ucChannelId]

instance ToQuery UpdateChannel where
        toQuery = const mempty

-- | Placeholder documentation for UpdateChannelResponse
--
-- /See:/ 'updateChannelResponse' smart constructor.
data UpdateChannelResponse = UpdateChannelResponse'
  { _ucrsChannel        :: !(Maybe Channel)
  , _ucrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucrsChannel' - Undocumented member.
--
-- * 'ucrsResponseStatus' - -- | The response status code.
updateChannelResponse
    :: Int -- ^ 'ucrsResponseStatus'
    -> UpdateChannelResponse
updateChannelResponse pResponseStatus_ =
  UpdateChannelResponse'
    {_ucrsChannel = Nothing, _ucrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
ucrsChannel :: Lens' UpdateChannelResponse (Maybe Channel)
ucrsChannel = lens _ucrsChannel (\ s a -> s{_ucrsChannel = a})

-- | -- | The response status code.
ucrsResponseStatus :: Lens' UpdateChannelResponse Int
ucrsResponseStatus = lens _ucrsResponseStatus (\ s a -> s{_ucrsResponseStatus = a})

instance NFData UpdateChannelResponse where
