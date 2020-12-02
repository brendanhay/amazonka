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
-- Module      : Network.AWS.MediaLive.DescribeChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a channel
module Network.AWS.MediaLive.DescribeChannel
    (
    -- * Creating a Request
      describeChannel
    , DescribeChannel
    -- * Request Lenses
    , dChannelId

    -- * Destructuring the Response
    , describeChannelResponse
    , DescribeChannelResponse
    -- * Response Lenses
    , dcrsState
    , dcrsARN
    , dcrsPipelinesRunningCount
    , dcrsInputSpecification
    , dcrsInputAttachments
    , dcrsDestinations
    , dcrsName
    , dcrsId
    , dcrsEgressEndpoints
    , dcrsEncoderSettings
    , dcrsRoleARN
    , dcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.MediaLive.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for DescribeChannelRequest
--
-- /See:/ 'describeChannel' smart constructor.
newtype DescribeChannel = DescribeChannel'
  { _dChannelId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dChannelId' - channel ID
describeChannel
    :: Text -- ^ 'dChannelId'
    -> DescribeChannel
describeChannel pChannelId_ = DescribeChannel' {_dChannelId = pChannelId_}


-- | channel ID
dChannelId :: Lens' DescribeChannel Text
dChannelId = lens _dChannelId (\ s a -> s{_dChannelId = a})

instance AWSRequest DescribeChannel where
        type Rs DescribeChannel = DescribeChannelResponse
        request = get mediaLive
        response
          = receiveJSON
              (\ s h x ->
                 DescribeChannelResponse' <$>
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

instance Hashable DescribeChannel where

instance NFData DescribeChannel where

instance ToHeaders DescribeChannel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DescribeChannel where
        toPath DescribeChannel'{..}
          = mconcat ["/prod/channels/", toBS _dChannelId]

instance ToQuery DescribeChannel where
        toQuery = const mempty

-- | Placeholder documentation for DescribeChannelResponse
--
-- /See:/ 'describeChannelResponse' smart constructor.
data DescribeChannelResponse = DescribeChannelResponse'
  { _dcrsState                 :: !(Maybe ChannelState)
  , _dcrsARN                   :: !(Maybe Text)
  , _dcrsPipelinesRunningCount :: !(Maybe Int)
  , _dcrsInputSpecification    :: !(Maybe InputSpecification)
  , _dcrsInputAttachments      :: !(Maybe [InputAttachment])
  , _dcrsDestinations          :: !(Maybe [OutputDestination])
  , _dcrsName                  :: !(Maybe Text)
  , _dcrsId                    :: !(Maybe Text)
  , _dcrsEgressEndpoints       :: !(Maybe [ChannelEgressEndpoint])
  , _dcrsEncoderSettings       :: !(Maybe EncoderSettings)
  , _dcrsRoleARN               :: !(Maybe Text)
  , _dcrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrsState' - Undocumented member.
--
-- * 'dcrsARN' - The unique arn of the channel.
--
-- * 'dcrsPipelinesRunningCount' - The number of currently healthy pipelines.
--
-- * 'dcrsInputSpecification' - Undocumented member.
--
-- * 'dcrsInputAttachments' - List of input attachments for channel.
--
-- * 'dcrsDestinations' - A list of destinations of the channel. For UDP outputs, there is one destination per output. For other types (HLS, for example), there is one destination per packager.
--
-- * 'dcrsName' - The name of the channel. (user-mutable)
--
-- * 'dcrsId' - The unique id of the channel.
--
-- * 'dcrsEgressEndpoints' - The endpoints where outgoing connections initiate from
--
-- * 'dcrsEncoderSettings' - Undocumented member.
--
-- * 'dcrsRoleARN' - The Amazon Resource Name (ARN) of the role assumed when running the Channel.
--
-- * 'dcrsResponseStatus' - -- | The response status code.
describeChannelResponse
    :: Int -- ^ 'dcrsResponseStatus'
    -> DescribeChannelResponse
describeChannelResponse pResponseStatus_ =
  DescribeChannelResponse'
    { _dcrsState = Nothing
    , _dcrsARN = Nothing
    , _dcrsPipelinesRunningCount = Nothing
    , _dcrsInputSpecification = Nothing
    , _dcrsInputAttachments = Nothing
    , _dcrsDestinations = Nothing
    , _dcrsName = Nothing
    , _dcrsId = Nothing
    , _dcrsEgressEndpoints = Nothing
    , _dcrsEncoderSettings = Nothing
    , _dcrsRoleARN = Nothing
    , _dcrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
dcrsState :: Lens' DescribeChannelResponse (Maybe ChannelState)
dcrsState = lens _dcrsState (\ s a -> s{_dcrsState = a})

-- | The unique arn of the channel.
dcrsARN :: Lens' DescribeChannelResponse (Maybe Text)
dcrsARN = lens _dcrsARN (\ s a -> s{_dcrsARN = a})

-- | The number of currently healthy pipelines.
dcrsPipelinesRunningCount :: Lens' DescribeChannelResponse (Maybe Int)
dcrsPipelinesRunningCount = lens _dcrsPipelinesRunningCount (\ s a -> s{_dcrsPipelinesRunningCount = a})

-- | Undocumented member.
dcrsInputSpecification :: Lens' DescribeChannelResponse (Maybe InputSpecification)
dcrsInputSpecification = lens _dcrsInputSpecification (\ s a -> s{_dcrsInputSpecification = a})

-- | List of input attachments for channel.
dcrsInputAttachments :: Lens' DescribeChannelResponse [InputAttachment]
dcrsInputAttachments = lens _dcrsInputAttachments (\ s a -> s{_dcrsInputAttachments = a}) . _Default . _Coerce

-- | A list of destinations of the channel. For UDP outputs, there is one destination per output. For other types (HLS, for example), there is one destination per packager.
dcrsDestinations :: Lens' DescribeChannelResponse [OutputDestination]
dcrsDestinations = lens _dcrsDestinations (\ s a -> s{_dcrsDestinations = a}) . _Default . _Coerce

-- | The name of the channel. (user-mutable)
dcrsName :: Lens' DescribeChannelResponse (Maybe Text)
dcrsName = lens _dcrsName (\ s a -> s{_dcrsName = a})

-- | The unique id of the channel.
dcrsId :: Lens' DescribeChannelResponse (Maybe Text)
dcrsId = lens _dcrsId (\ s a -> s{_dcrsId = a})

-- | The endpoints where outgoing connections initiate from
dcrsEgressEndpoints :: Lens' DescribeChannelResponse [ChannelEgressEndpoint]
dcrsEgressEndpoints = lens _dcrsEgressEndpoints (\ s a -> s{_dcrsEgressEndpoints = a}) . _Default . _Coerce

-- | Undocumented member.
dcrsEncoderSettings :: Lens' DescribeChannelResponse (Maybe EncoderSettings)
dcrsEncoderSettings = lens _dcrsEncoderSettings (\ s a -> s{_dcrsEncoderSettings = a})

-- | The Amazon Resource Name (ARN) of the role assumed when running the Channel.
dcrsRoleARN :: Lens' DescribeChannelResponse (Maybe Text)
dcrsRoleARN = lens _dcrsRoleARN (\ s a -> s{_dcrsRoleARN = a})

-- | -- | The response status code.
dcrsResponseStatus :: Lens' DescribeChannelResponse Int
dcrsResponseStatus = lens _dcrsResponseStatus (\ s a -> s{_dcrsResponseStatus = a})

instance NFData DescribeChannelResponse where
