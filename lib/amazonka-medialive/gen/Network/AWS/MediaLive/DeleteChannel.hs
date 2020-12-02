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
-- Module      : Network.AWS.MediaLive.DeleteChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts deletion of channel. The associated outputs are also deleted.
module Network.AWS.MediaLive.DeleteChannel
    (
    -- * Creating a Request
      deleteChannel
    , DeleteChannel
    -- * Request Lenses
    , dcChannelId

    -- * Destructuring the Response
    , deleteChannelResponse
    , DeleteChannelResponse
    -- * Response Lenses
    , drsState
    , drsARN
    , drsPipelinesRunningCount
    , drsInputSpecification
    , drsInputAttachments
    , drsDestinations
    , drsName
    , drsId
    , drsEgressEndpoints
    , drsEncoderSettings
    , drsRoleARN
    , drsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.MediaLive.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for DeleteChannelRequest
--
-- /See:/ 'deleteChannel' smart constructor.
newtype DeleteChannel = DeleteChannel'
  { _dcChannelId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcChannelId' - Unique ID of the channel.
deleteChannel
    :: Text -- ^ 'dcChannelId'
    -> DeleteChannel
deleteChannel pChannelId_ = DeleteChannel' {_dcChannelId = pChannelId_}


-- | Unique ID of the channel.
dcChannelId :: Lens' DeleteChannel Text
dcChannelId = lens _dcChannelId (\ s a -> s{_dcChannelId = a})

instance AWSRequest DeleteChannel where
        type Rs DeleteChannel = DeleteChannelResponse
        request = delete mediaLive
        response
          = receiveJSON
              (\ s h x ->
                 DeleteChannelResponse' <$>
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

instance Hashable DeleteChannel where

instance NFData DeleteChannel where

instance ToHeaders DeleteChannel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteChannel where
        toPath DeleteChannel'{..}
          = mconcat ["/prod/channels/", toBS _dcChannelId]

instance ToQuery DeleteChannel where
        toQuery = const mempty

-- | Placeholder documentation for DeleteChannelResponse
--
-- /See:/ 'deleteChannelResponse' smart constructor.
data DeleteChannelResponse = DeleteChannelResponse'
  { _drsState                 :: !(Maybe ChannelState)
  , _drsARN                   :: !(Maybe Text)
  , _drsPipelinesRunningCount :: !(Maybe Int)
  , _drsInputSpecification    :: !(Maybe InputSpecification)
  , _drsInputAttachments      :: !(Maybe [InputAttachment])
  , _drsDestinations          :: !(Maybe [OutputDestination])
  , _drsName                  :: !(Maybe Text)
  , _drsId                    :: !(Maybe Text)
  , _drsEgressEndpoints       :: !(Maybe [ChannelEgressEndpoint])
  , _drsEncoderSettings       :: !(Maybe EncoderSettings)
  , _drsRoleARN               :: !(Maybe Text)
  , _drsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsState' - Undocumented member.
--
-- * 'drsARN' - The unique arn of the channel.
--
-- * 'drsPipelinesRunningCount' - The number of currently healthy pipelines.
--
-- * 'drsInputSpecification' - Undocumented member.
--
-- * 'drsInputAttachments' - List of input attachments for channel.
--
-- * 'drsDestinations' - A list of destinations of the channel. For UDP outputs, there is one destination per output. For other types (HLS, for example), there is one destination per packager.
--
-- * 'drsName' - The name of the channel. (user-mutable)
--
-- * 'drsId' - The unique id of the channel.
--
-- * 'drsEgressEndpoints' - The endpoints where outgoing connections initiate from
--
-- * 'drsEncoderSettings' - Undocumented member.
--
-- * 'drsRoleARN' - The Amazon Resource Name (ARN) of the role assumed when running the Channel.
--
-- * 'drsResponseStatus' - -- | The response status code.
deleteChannelResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DeleteChannelResponse
deleteChannelResponse pResponseStatus_ =
  DeleteChannelResponse'
    { _drsState = Nothing
    , _drsARN = Nothing
    , _drsPipelinesRunningCount = Nothing
    , _drsInputSpecification = Nothing
    , _drsInputAttachments = Nothing
    , _drsDestinations = Nothing
    , _drsName = Nothing
    , _drsId = Nothing
    , _drsEgressEndpoints = Nothing
    , _drsEncoderSettings = Nothing
    , _drsRoleARN = Nothing
    , _drsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
drsState :: Lens' DeleteChannelResponse (Maybe ChannelState)
drsState = lens _drsState (\ s a -> s{_drsState = a})

-- | The unique arn of the channel.
drsARN :: Lens' DeleteChannelResponse (Maybe Text)
drsARN = lens _drsARN (\ s a -> s{_drsARN = a})

-- | The number of currently healthy pipelines.
drsPipelinesRunningCount :: Lens' DeleteChannelResponse (Maybe Int)
drsPipelinesRunningCount = lens _drsPipelinesRunningCount (\ s a -> s{_drsPipelinesRunningCount = a})

-- | Undocumented member.
drsInputSpecification :: Lens' DeleteChannelResponse (Maybe InputSpecification)
drsInputSpecification = lens _drsInputSpecification (\ s a -> s{_drsInputSpecification = a})

-- | List of input attachments for channel.
drsInputAttachments :: Lens' DeleteChannelResponse [InputAttachment]
drsInputAttachments = lens _drsInputAttachments (\ s a -> s{_drsInputAttachments = a}) . _Default . _Coerce

-- | A list of destinations of the channel. For UDP outputs, there is one destination per output. For other types (HLS, for example), there is one destination per packager.
drsDestinations :: Lens' DeleteChannelResponse [OutputDestination]
drsDestinations = lens _drsDestinations (\ s a -> s{_drsDestinations = a}) . _Default . _Coerce

-- | The name of the channel. (user-mutable)
drsName :: Lens' DeleteChannelResponse (Maybe Text)
drsName = lens _drsName (\ s a -> s{_drsName = a})

-- | The unique id of the channel.
drsId :: Lens' DeleteChannelResponse (Maybe Text)
drsId = lens _drsId (\ s a -> s{_drsId = a})

-- | The endpoints where outgoing connections initiate from
drsEgressEndpoints :: Lens' DeleteChannelResponse [ChannelEgressEndpoint]
drsEgressEndpoints = lens _drsEgressEndpoints (\ s a -> s{_drsEgressEndpoints = a}) . _Default . _Coerce

-- | Undocumented member.
drsEncoderSettings :: Lens' DeleteChannelResponse (Maybe EncoderSettings)
drsEncoderSettings = lens _drsEncoderSettings (\ s a -> s{_drsEncoderSettings = a})

-- | The Amazon Resource Name (ARN) of the role assumed when running the Channel.
drsRoleARN :: Lens' DeleteChannelResponse (Maybe Text)
drsRoleARN = lens _drsRoleARN (\ s a -> s{_drsRoleARN = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DeleteChannelResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DeleteChannelResponse where
