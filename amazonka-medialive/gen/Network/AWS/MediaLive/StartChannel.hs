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
-- Module      : Network.AWS.MediaLive.StartChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an existing channel
module Network.AWS.MediaLive.StartChannel
    (
    -- * Creating a Request
      startChannel
    , StartChannel
    -- * Request Lenses
    , scChannelId

    -- * Destructuring the Response
    , startChannelResponse
    , StartChannelResponse
    -- * Response Lenses
    , scrsState
    , scrsARN
    , scrsPipelinesRunningCount
    , scrsInputSpecification
    , scrsInputAttachments
    , scrsDestinations
    , scrsName
    , scrsId
    , scrsEgressEndpoints
    , scrsEncoderSettings
    , scrsRoleARN
    , scrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.MediaLive.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for StartChannelRequest
--
-- /See:/ 'startChannel' smart constructor.
newtype StartChannel = StartChannel'
  { _scChannelId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scChannelId' - A request to start a channel
startChannel
    :: Text -- ^ 'scChannelId'
    -> StartChannel
startChannel pChannelId_ = StartChannel' {_scChannelId = pChannelId_}


-- | A request to start a channel
scChannelId :: Lens' StartChannel Text
scChannelId = lens _scChannelId (\ s a -> s{_scChannelId = a})

instance AWSRequest StartChannel where
        type Rs StartChannel = StartChannelResponse
        request = postJSON mediaLive
        response
          = receiveJSON
              (\ s h x ->
                 StartChannelResponse' <$>
                   (x .?> "state") <*> (x .?> "arn") <*>
                     (x .?> "pipelinesRunningCount")
                     <*> (x .?> "inputSpecification")
                     <*> (x .?> "inputAttachments" .!@ mempty)
                     <*> (x .?> "destinations" .!@ mempty)
                     <*> (x .?> "name")
                     <*> (x .?> "id")
                     <*> (x .?> "egressEndpoints" .!@ mempty)
                     <*> (x .?> "encoderSettings")
                     <*> (x .?> "roleArn")
                     <*> (pure (fromEnum s)))

instance Hashable StartChannel where

instance NFData StartChannel where

instance ToHeaders StartChannel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartChannel where
        toJSON = const (Object mempty)

instance ToPath StartChannel where
        toPath StartChannel'{..}
          = mconcat
              ["/prod/channels/", toBS _scChannelId, "/start"]

instance ToQuery StartChannel where
        toQuery = const mempty

-- | Placeholder documentation for StartChannelResponse
--
-- /See:/ 'startChannelResponse' smart constructor.
data StartChannelResponse = StartChannelResponse'
  { _scrsState                 :: !(Maybe ChannelState)
  , _scrsARN                   :: !(Maybe Text)
  , _scrsPipelinesRunningCount :: !(Maybe Int)
  , _scrsInputSpecification    :: !(Maybe InputSpecification)
  , _scrsInputAttachments      :: !(Maybe [InputAttachment])
  , _scrsDestinations          :: !(Maybe [OutputDestination])
  , _scrsName                  :: !(Maybe Text)
  , _scrsId                    :: !(Maybe Text)
  , _scrsEgressEndpoints       :: !(Maybe [ChannelEgressEndpoint])
  , _scrsEncoderSettings       :: !(Maybe EncoderSettings)
  , _scrsRoleARN               :: !(Maybe Text)
  , _scrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scrsState' - Undocumented member.
--
-- * 'scrsARN' - The unique arn of the channel.
--
-- * 'scrsPipelinesRunningCount' - The number of currently healthy pipelines.
--
-- * 'scrsInputSpecification' - Undocumented member.
--
-- * 'scrsInputAttachments' - List of input attachments for channel.
--
-- * 'scrsDestinations' - A list of destinations of the channel. For UDP outputs, there is one destination per output. For other types (HLS, for example), there is one destination per packager.
--
-- * 'scrsName' - The name of the channel. (user-mutable)
--
-- * 'scrsId' - The unique id of the channel.
--
-- * 'scrsEgressEndpoints' - The endpoints where outgoing connections initiate from
--
-- * 'scrsEncoderSettings' - Undocumented member.
--
-- * 'scrsRoleARN' - The Amazon Resource Name (ARN) of the role assumed when running the Channel.
--
-- * 'scrsResponseStatus' - -- | The response status code.
startChannelResponse
    :: Int -- ^ 'scrsResponseStatus'
    -> StartChannelResponse
startChannelResponse pResponseStatus_ =
  StartChannelResponse'
    { _scrsState = Nothing
    , _scrsARN = Nothing
    , _scrsPipelinesRunningCount = Nothing
    , _scrsInputSpecification = Nothing
    , _scrsInputAttachments = Nothing
    , _scrsDestinations = Nothing
    , _scrsName = Nothing
    , _scrsId = Nothing
    , _scrsEgressEndpoints = Nothing
    , _scrsEncoderSettings = Nothing
    , _scrsRoleARN = Nothing
    , _scrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
scrsState :: Lens' StartChannelResponse (Maybe ChannelState)
scrsState = lens _scrsState (\ s a -> s{_scrsState = a})

-- | The unique arn of the channel.
scrsARN :: Lens' StartChannelResponse (Maybe Text)
scrsARN = lens _scrsARN (\ s a -> s{_scrsARN = a})

-- | The number of currently healthy pipelines.
scrsPipelinesRunningCount :: Lens' StartChannelResponse (Maybe Int)
scrsPipelinesRunningCount = lens _scrsPipelinesRunningCount (\ s a -> s{_scrsPipelinesRunningCount = a})

-- | Undocumented member.
scrsInputSpecification :: Lens' StartChannelResponse (Maybe InputSpecification)
scrsInputSpecification = lens _scrsInputSpecification (\ s a -> s{_scrsInputSpecification = a})

-- | List of input attachments for channel.
scrsInputAttachments :: Lens' StartChannelResponse [InputAttachment]
scrsInputAttachments = lens _scrsInputAttachments (\ s a -> s{_scrsInputAttachments = a}) . _Default . _Coerce

-- | A list of destinations of the channel. For UDP outputs, there is one destination per output. For other types (HLS, for example), there is one destination per packager.
scrsDestinations :: Lens' StartChannelResponse [OutputDestination]
scrsDestinations = lens _scrsDestinations (\ s a -> s{_scrsDestinations = a}) . _Default . _Coerce

-- | The name of the channel. (user-mutable)
scrsName :: Lens' StartChannelResponse (Maybe Text)
scrsName = lens _scrsName (\ s a -> s{_scrsName = a})

-- | The unique id of the channel.
scrsId :: Lens' StartChannelResponse (Maybe Text)
scrsId = lens _scrsId (\ s a -> s{_scrsId = a})

-- | The endpoints where outgoing connections initiate from
scrsEgressEndpoints :: Lens' StartChannelResponse [ChannelEgressEndpoint]
scrsEgressEndpoints = lens _scrsEgressEndpoints (\ s a -> s{_scrsEgressEndpoints = a}) . _Default . _Coerce

-- | Undocumented member.
scrsEncoderSettings :: Lens' StartChannelResponse (Maybe EncoderSettings)
scrsEncoderSettings = lens _scrsEncoderSettings (\ s a -> s{_scrsEncoderSettings = a})

-- | The Amazon Resource Name (ARN) of the role assumed when running the Channel.
scrsRoleARN :: Lens' StartChannelResponse (Maybe Text)
scrsRoleARN = lens _scrsRoleARN (\ s a -> s{_scrsRoleARN = a})

-- | -- | The response status code.
scrsResponseStatus :: Lens' StartChannelResponse Int
scrsResponseStatus = lens _scrsResponseStatus (\ s a -> s{_scrsResponseStatus = a})

instance NFData StartChannelResponse where
