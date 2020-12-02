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
-- Module      : Network.AWS.MediaPackage.DescribeChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a Channel.
module Network.AWS.MediaPackage.DescribeChannel
  ( -- * Creating a Request
    describeChannel,
    DescribeChannel,

    -- * Request Lenses
    dId,

    -- * Destructuring the Response
    describeChannelResponse,
    DescribeChannelResponse,

    -- * Response Lenses
    dcrsIngressAccessLogs,
    dcrsHlsIngest,
    dcrsARN,
    dcrsId,
    dcrsDescription,
    dcrsEgressAccessLogs,
    dcrsTags,
    dcrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaPackage.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeChannel' smart constructor.
newtype DescribeChannel = DescribeChannel' {_dId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dId' - The ID of a Channel.
describeChannel ::
  -- | 'dId'
  Text ->
  DescribeChannel
describeChannel pId_ = DescribeChannel' {_dId = pId_}

-- | The ID of a Channel.
dId :: Lens' DescribeChannel Text
dId = lens _dId (\s a -> s {_dId = a})

instance AWSRequest DescribeChannel where
  type Rs DescribeChannel = DescribeChannelResponse
  request = get mediaPackage
  response =
    receiveJSON
      ( \s h x ->
          DescribeChannelResponse'
            <$> (x .?> "ingressAccessLogs")
            <*> (x .?> "hlsIngest")
            <*> (x .?> "arn")
            <*> (x .?> "id")
            <*> (x .?> "description")
            <*> (x .?> "egressAccessLogs")
            <*> (x .?> "tags" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeChannel

instance NFData DescribeChannel

instance ToHeaders DescribeChannel where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DescribeChannel where
  toPath DescribeChannel' {..} = mconcat ["/channels/", toBS _dId]

instance ToQuery DescribeChannel where
  toQuery = const mempty

-- | /See:/ 'describeChannelResponse' smart constructor.
data DescribeChannelResponse = DescribeChannelResponse'
  { _dcrsIngressAccessLogs ::
      !(Maybe IngressAccessLogs),
    _dcrsHlsIngest :: !(Maybe HlsIngest),
    _dcrsARN :: !(Maybe Text),
    _dcrsId :: !(Maybe Text),
    _dcrsDescription :: !(Maybe Text),
    _dcrsEgressAccessLogs ::
      !(Maybe EgressAccessLogs),
    _dcrsTags :: !(Maybe (Map Text (Text))),
    _dcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrsIngressAccessLogs' - Undocumented member.
--
-- * 'dcrsHlsIngest' - Undocumented member.
--
-- * 'dcrsARN' - The Amazon Resource Name (ARN) assigned to the Channel.
--
-- * 'dcrsId' - The ID of the Channel.
--
-- * 'dcrsDescription' - A short text description of the Channel.
--
-- * 'dcrsEgressAccessLogs' - Undocumented member.
--
-- * 'dcrsTags' - Undocumented member.
--
-- * 'dcrsResponseStatus' - -- | The response status code.
describeChannelResponse ::
  -- | 'dcrsResponseStatus'
  Int ->
  DescribeChannelResponse
describeChannelResponse pResponseStatus_ =
  DescribeChannelResponse'
    { _dcrsIngressAccessLogs = Nothing,
      _dcrsHlsIngest = Nothing,
      _dcrsARN = Nothing,
      _dcrsId = Nothing,
      _dcrsDescription = Nothing,
      _dcrsEgressAccessLogs = Nothing,
      _dcrsTags = Nothing,
      _dcrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
dcrsIngressAccessLogs :: Lens' DescribeChannelResponse (Maybe IngressAccessLogs)
dcrsIngressAccessLogs = lens _dcrsIngressAccessLogs (\s a -> s {_dcrsIngressAccessLogs = a})

-- | Undocumented member.
dcrsHlsIngest :: Lens' DescribeChannelResponse (Maybe HlsIngest)
dcrsHlsIngest = lens _dcrsHlsIngest (\s a -> s {_dcrsHlsIngest = a})

-- | The Amazon Resource Name (ARN) assigned to the Channel.
dcrsARN :: Lens' DescribeChannelResponse (Maybe Text)
dcrsARN = lens _dcrsARN (\s a -> s {_dcrsARN = a})

-- | The ID of the Channel.
dcrsId :: Lens' DescribeChannelResponse (Maybe Text)
dcrsId = lens _dcrsId (\s a -> s {_dcrsId = a})

-- | A short text description of the Channel.
dcrsDescription :: Lens' DescribeChannelResponse (Maybe Text)
dcrsDescription = lens _dcrsDescription (\s a -> s {_dcrsDescription = a})

-- | Undocumented member.
dcrsEgressAccessLogs :: Lens' DescribeChannelResponse (Maybe EgressAccessLogs)
dcrsEgressAccessLogs = lens _dcrsEgressAccessLogs (\s a -> s {_dcrsEgressAccessLogs = a})

-- | Undocumented member.
dcrsTags :: Lens' DescribeChannelResponse (HashMap Text (Text))
dcrsTags = lens _dcrsTags (\s a -> s {_dcrsTags = a}) . _Default . _Map

-- | -- | The response status code.
dcrsResponseStatus :: Lens' DescribeChannelResponse Int
dcrsResponseStatus = lens _dcrsResponseStatus (\s a -> s {_dcrsResponseStatus = a})

instance NFData DescribeChannelResponse
