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
-- Module      : Network.AWS.MediaLive.StopChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running channel
module Network.AWS.MediaLive.StopChannel
    (
    -- * Creating a Request
      stopChannel
    , StopChannel
    -- * Request Lenses
    , sChannelId

    -- * Destructuring the Response
    , stopChannelResponse
    , StopChannelResponse
    -- * Response Lenses
    , srsState
    , srsARN
    , srsPipelinesRunningCount
    , srsInputSpecification
    , srsInputAttachments
    , srsDestinations
    , srsName
    , srsId
    , srsEgressEndpoints
    , srsEncoderSettings
    , srsRoleARN
    , srsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.MediaLive.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for StopChannelRequest
--
-- /See:/ 'stopChannel' smart constructor.
newtype StopChannel = StopChannel'
  { _sChannelId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sChannelId' - A request to stop a running channel
stopChannel
    :: Text -- ^ 'sChannelId'
    -> StopChannel
stopChannel pChannelId_ = StopChannel' {_sChannelId = pChannelId_}


-- | A request to stop a running channel
sChannelId :: Lens' StopChannel Text
sChannelId = lens _sChannelId (\ s a -> s{_sChannelId = a})

instance AWSRequest StopChannel where
        type Rs StopChannel = StopChannelResponse
        request = postJSON mediaLive
        response
          = receiveJSON
              (\ s h x ->
                 StopChannelResponse' <$>
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

instance Hashable StopChannel where

instance NFData StopChannel where

instance ToHeaders StopChannel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopChannel where
        toJSON = const (Object mempty)

instance ToPath StopChannel where
        toPath StopChannel'{..}
          = mconcat
              ["/prod/channels/", toBS _sChannelId, "/stop"]

instance ToQuery StopChannel where
        toQuery = const mempty

-- | Placeholder documentation for StopChannelResponse
--
-- /See:/ 'stopChannelResponse' smart constructor.
data StopChannelResponse = StopChannelResponse'
  { _srsState                 :: !(Maybe ChannelState)
  , _srsARN                   :: !(Maybe Text)
  , _srsPipelinesRunningCount :: !(Maybe Int)
  , _srsInputSpecification    :: !(Maybe InputSpecification)
  , _srsInputAttachments      :: !(Maybe [InputAttachment])
  , _srsDestinations          :: !(Maybe [OutputDestination])
  , _srsName                  :: !(Maybe Text)
  , _srsId                    :: !(Maybe Text)
  , _srsEgressEndpoints       :: !(Maybe [ChannelEgressEndpoint])
  , _srsEncoderSettings       :: !(Maybe EncoderSettings)
  , _srsRoleARN               :: !(Maybe Text)
  , _srsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsState' - Undocumented member.
--
-- * 'srsARN' - The unique arn of the channel.
--
-- * 'srsPipelinesRunningCount' - The number of currently healthy pipelines.
--
-- * 'srsInputSpecification' - Undocumented member.
--
-- * 'srsInputAttachments' - List of input attachments for channel.
--
-- * 'srsDestinations' - A list of destinations of the channel. For UDP outputs, there is one destination per output. For other types (HLS, for example), there is one destination per packager.
--
-- * 'srsName' - The name of the channel. (user-mutable)
--
-- * 'srsId' - The unique id of the channel.
--
-- * 'srsEgressEndpoints' - The endpoints where outgoing connections initiate from
--
-- * 'srsEncoderSettings' - Undocumented member.
--
-- * 'srsRoleARN' - The Amazon Resource Name (ARN) of the role assumed when running the Channel.
--
-- * 'srsResponseStatus' - -- | The response status code.
stopChannelResponse
    :: Int -- ^ 'srsResponseStatus'
    -> StopChannelResponse
stopChannelResponse pResponseStatus_ =
  StopChannelResponse'
    { _srsState = Nothing
    , _srsARN = Nothing
    , _srsPipelinesRunningCount = Nothing
    , _srsInputSpecification = Nothing
    , _srsInputAttachments = Nothing
    , _srsDestinations = Nothing
    , _srsName = Nothing
    , _srsId = Nothing
    , _srsEgressEndpoints = Nothing
    , _srsEncoderSettings = Nothing
    , _srsRoleARN = Nothing
    , _srsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
srsState :: Lens' StopChannelResponse (Maybe ChannelState)
srsState = lens _srsState (\ s a -> s{_srsState = a})

-- | The unique arn of the channel.
srsARN :: Lens' StopChannelResponse (Maybe Text)
srsARN = lens _srsARN (\ s a -> s{_srsARN = a})

-- | The number of currently healthy pipelines.
srsPipelinesRunningCount :: Lens' StopChannelResponse (Maybe Int)
srsPipelinesRunningCount = lens _srsPipelinesRunningCount (\ s a -> s{_srsPipelinesRunningCount = a})

-- | Undocumented member.
srsInputSpecification :: Lens' StopChannelResponse (Maybe InputSpecification)
srsInputSpecification = lens _srsInputSpecification (\ s a -> s{_srsInputSpecification = a})

-- | List of input attachments for channel.
srsInputAttachments :: Lens' StopChannelResponse [InputAttachment]
srsInputAttachments = lens _srsInputAttachments (\ s a -> s{_srsInputAttachments = a}) . _Default . _Coerce

-- | A list of destinations of the channel. For UDP outputs, there is one destination per output. For other types (HLS, for example), there is one destination per packager.
srsDestinations :: Lens' StopChannelResponse [OutputDestination]
srsDestinations = lens _srsDestinations (\ s a -> s{_srsDestinations = a}) . _Default . _Coerce

-- | The name of the channel. (user-mutable)
srsName :: Lens' StopChannelResponse (Maybe Text)
srsName = lens _srsName (\ s a -> s{_srsName = a})

-- | The unique id of the channel.
srsId :: Lens' StopChannelResponse (Maybe Text)
srsId = lens _srsId (\ s a -> s{_srsId = a})

-- | The endpoints where outgoing connections initiate from
srsEgressEndpoints :: Lens' StopChannelResponse [ChannelEgressEndpoint]
srsEgressEndpoints = lens _srsEgressEndpoints (\ s a -> s{_srsEgressEndpoints = a}) . _Default . _Coerce

-- | Undocumented member.
srsEncoderSettings :: Lens' StopChannelResponse (Maybe EncoderSettings)
srsEncoderSettings = lens _srsEncoderSettings (\ s a -> s{_srsEncoderSettings = a})

-- | The Amazon Resource Name (ARN) of the role assumed when running the Channel.
srsRoleARN :: Lens' StopChannelResponse (Maybe Text)
srsRoleARN = lens _srsRoleARN (\ s a -> s{_srsRoleARN = a})

-- | -- | The response status code.
srsResponseStatus :: Lens' StopChannelResponse Int
srsResponseStatus = lens _srsResponseStatus (\ s a -> s{_srsResponseStatus = a})

instance NFData StopChannelResponse where
