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
-- Module      : Network.AWS.MediaPackage.DescribeOriginEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about an existing OriginEndpoint.
module Network.AWS.MediaPackage.DescribeOriginEndpoint
  ( -- * Creating a Request
    describeOriginEndpoint,
    DescribeOriginEndpoint,

    -- * Request Lenses
    doeId,

    -- * Destructuring the Response
    describeOriginEndpointResponse,
    DescribeOriginEndpointResponse,

    -- * Response Lenses
    desrsWhitelist,
    desrsHlsPackage,
    desrsARN,
    desrsManifestName,
    desrsURL,
    desrsAuthorization,
    desrsChannelId,
    desrsStartoverWindowSeconds,
    desrsDashPackage,
    desrsMssPackage,
    desrsId,
    desrsTimeDelaySeconds,
    desrsCmafPackage,
    desrsDescription,
    desrsTags,
    desrsOrigination,
    desrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaPackage.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeOriginEndpoint' smart constructor.
newtype DescribeOriginEndpoint = DescribeOriginEndpoint'
  { _doeId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeOriginEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'doeId' - The ID of the OriginEndpoint.
describeOriginEndpoint ::
  -- | 'doeId'
  Text ->
  DescribeOriginEndpoint
describeOriginEndpoint pId_ =
  DescribeOriginEndpoint' {_doeId = pId_}

-- | The ID of the OriginEndpoint.
doeId :: Lens' DescribeOriginEndpoint Text
doeId = lens _doeId (\s a -> s {_doeId = a})

instance AWSRequest DescribeOriginEndpoint where
  type Rs DescribeOriginEndpoint = DescribeOriginEndpointResponse
  request = get mediaPackage
  response =
    receiveJSON
      ( \s h x ->
          DescribeOriginEndpointResponse'
            <$> (x .?> "whitelist" .!@ mempty)
            <*> (x .?> "hlsPackage")
            <*> (x .?> "arn")
            <*> (x .?> "manifestName")
            <*> (x .?> "url")
            <*> (x .?> "authorization")
            <*> (x .?> "channelId")
            <*> (x .?> "startoverWindowSeconds")
            <*> (x .?> "dashPackage")
            <*> (x .?> "mssPackage")
            <*> (x .?> "id")
            <*> (x .?> "timeDelaySeconds")
            <*> (x .?> "cmafPackage")
            <*> (x .?> "description")
            <*> (x .?> "tags" .!@ mempty)
            <*> (x .?> "origination")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeOriginEndpoint

instance NFData DescribeOriginEndpoint

instance ToHeaders DescribeOriginEndpoint where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DescribeOriginEndpoint where
  toPath DescribeOriginEndpoint' {..} =
    mconcat ["/origin_endpoints/", toBS _doeId]

instance ToQuery DescribeOriginEndpoint where
  toQuery = const mempty

-- | /See:/ 'describeOriginEndpointResponse' smart constructor.
data DescribeOriginEndpointResponse = DescribeOriginEndpointResponse'
  { _desrsWhitelist ::
      !(Maybe [Text]),
    _desrsHlsPackage ::
      !(Maybe HlsPackage),
    _desrsARN :: !(Maybe Text),
    _desrsManifestName ::
      !(Maybe Text),
    _desrsURL :: !(Maybe Text),
    _desrsAuthorization ::
      !(Maybe Authorization),
    _desrsChannelId ::
      !(Maybe Text),
    _desrsStartoverWindowSeconds ::
      !(Maybe Int),
    _desrsDashPackage ::
      !(Maybe DashPackage),
    _desrsMssPackage ::
      !(Maybe MssPackage),
    _desrsId :: !(Maybe Text),
    _desrsTimeDelaySeconds ::
      !(Maybe Int),
    _desrsCmafPackage ::
      !(Maybe CmafPackage),
    _desrsDescription ::
      !(Maybe Text),
    _desrsTags ::
      !(Maybe (Map Text (Text))),
    _desrsOrigination ::
      !(Maybe Origination),
    _desrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeOriginEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desrsWhitelist' - A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
--
-- * 'desrsHlsPackage' - Undocumented member.
--
-- * 'desrsARN' - The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
--
-- * 'desrsManifestName' - A short string appended to the end of the OriginEndpoint URL.
--
-- * 'desrsURL' - The URL of the packaged OriginEndpoint for consumption.
--
-- * 'desrsAuthorization' - Undocumented member.
--
-- * 'desrsChannelId' - The ID of the Channel the OriginEndpoint is associated with.
--
-- * 'desrsStartoverWindowSeconds' - Maximum duration (seconds) of content to retain for startover playback. If not specified, startover playback will be disabled for the OriginEndpoint.
--
-- * 'desrsDashPackage' - Undocumented member.
--
-- * 'desrsMssPackage' - Undocumented member.
--
-- * 'desrsId' - The ID of the OriginEndpoint.
--
-- * 'desrsTimeDelaySeconds' - Amount of delay (seconds) to enforce on the playback of live content. If not specified, there will be no time delay in effect for the OriginEndpoint.
--
-- * 'desrsCmafPackage' - Undocumented member.
--
-- * 'desrsDescription' - A short text description of the OriginEndpoint.
--
-- * 'desrsTags' - Undocumented member.
--
-- * 'desrsOrigination' - Control whether origination of video is allowed for this OriginEndpoint. If set to ALLOW, the OriginEndpoint may by requested, pursuant to any other form of access control. If set to DENY, the OriginEndpoint may not be requested. This can be helpful for Live to VOD harvesting, or for temporarily disabling origination
--
-- * 'desrsResponseStatus' - -- | The response status code.
describeOriginEndpointResponse ::
  -- | 'desrsResponseStatus'
  Int ->
  DescribeOriginEndpointResponse
describeOriginEndpointResponse pResponseStatus_ =
  DescribeOriginEndpointResponse'
    { _desrsWhitelist = Nothing,
      _desrsHlsPackage = Nothing,
      _desrsARN = Nothing,
      _desrsManifestName = Nothing,
      _desrsURL = Nothing,
      _desrsAuthorization = Nothing,
      _desrsChannelId = Nothing,
      _desrsStartoverWindowSeconds = Nothing,
      _desrsDashPackage = Nothing,
      _desrsMssPackage = Nothing,
      _desrsId = Nothing,
      _desrsTimeDelaySeconds = Nothing,
      _desrsCmafPackage = Nothing,
      _desrsDescription = Nothing,
      _desrsTags = Nothing,
      _desrsOrigination = Nothing,
      _desrsResponseStatus = pResponseStatus_
    }

-- | A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
desrsWhitelist :: Lens' DescribeOriginEndpointResponse [Text]
desrsWhitelist = lens _desrsWhitelist (\s a -> s {_desrsWhitelist = a}) . _Default . _Coerce

-- | Undocumented member.
desrsHlsPackage :: Lens' DescribeOriginEndpointResponse (Maybe HlsPackage)
desrsHlsPackage = lens _desrsHlsPackage (\s a -> s {_desrsHlsPackage = a})

-- | The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
desrsARN :: Lens' DescribeOriginEndpointResponse (Maybe Text)
desrsARN = lens _desrsARN (\s a -> s {_desrsARN = a})

-- | A short string appended to the end of the OriginEndpoint URL.
desrsManifestName :: Lens' DescribeOriginEndpointResponse (Maybe Text)
desrsManifestName = lens _desrsManifestName (\s a -> s {_desrsManifestName = a})

-- | The URL of the packaged OriginEndpoint for consumption.
desrsURL :: Lens' DescribeOriginEndpointResponse (Maybe Text)
desrsURL = lens _desrsURL (\s a -> s {_desrsURL = a})

-- | Undocumented member.
desrsAuthorization :: Lens' DescribeOriginEndpointResponse (Maybe Authorization)
desrsAuthorization = lens _desrsAuthorization (\s a -> s {_desrsAuthorization = a})

-- | The ID of the Channel the OriginEndpoint is associated with.
desrsChannelId :: Lens' DescribeOriginEndpointResponse (Maybe Text)
desrsChannelId = lens _desrsChannelId (\s a -> s {_desrsChannelId = a})

-- | Maximum duration (seconds) of content to retain for startover playback. If not specified, startover playback will be disabled for the OriginEndpoint.
desrsStartoverWindowSeconds :: Lens' DescribeOriginEndpointResponse (Maybe Int)
desrsStartoverWindowSeconds = lens _desrsStartoverWindowSeconds (\s a -> s {_desrsStartoverWindowSeconds = a})

-- | Undocumented member.
desrsDashPackage :: Lens' DescribeOriginEndpointResponse (Maybe DashPackage)
desrsDashPackage = lens _desrsDashPackage (\s a -> s {_desrsDashPackage = a})

-- | Undocumented member.
desrsMssPackage :: Lens' DescribeOriginEndpointResponse (Maybe MssPackage)
desrsMssPackage = lens _desrsMssPackage (\s a -> s {_desrsMssPackage = a})

-- | The ID of the OriginEndpoint.
desrsId :: Lens' DescribeOriginEndpointResponse (Maybe Text)
desrsId = lens _desrsId (\s a -> s {_desrsId = a})

-- | Amount of delay (seconds) to enforce on the playback of live content. If not specified, there will be no time delay in effect for the OriginEndpoint.
desrsTimeDelaySeconds :: Lens' DescribeOriginEndpointResponse (Maybe Int)
desrsTimeDelaySeconds = lens _desrsTimeDelaySeconds (\s a -> s {_desrsTimeDelaySeconds = a})

-- | Undocumented member.
desrsCmafPackage :: Lens' DescribeOriginEndpointResponse (Maybe CmafPackage)
desrsCmafPackage = lens _desrsCmafPackage (\s a -> s {_desrsCmafPackage = a})

-- | A short text description of the OriginEndpoint.
desrsDescription :: Lens' DescribeOriginEndpointResponse (Maybe Text)
desrsDescription = lens _desrsDescription (\s a -> s {_desrsDescription = a})

-- | Undocumented member.
desrsTags :: Lens' DescribeOriginEndpointResponse (HashMap Text (Text))
desrsTags = lens _desrsTags (\s a -> s {_desrsTags = a}) . _Default . _Map

-- | Control whether origination of video is allowed for this OriginEndpoint. If set to ALLOW, the OriginEndpoint may by requested, pursuant to any other form of access control. If set to DENY, the OriginEndpoint may not be requested. This can be helpful for Live to VOD harvesting, or for temporarily disabling origination
desrsOrigination :: Lens' DescribeOriginEndpointResponse (Maybe Origination)
desrsOrigination = lens _desrsOrigination (\s a -> s {_desrsOrigination = a})

-- | -- | The response status code.
desrsResponseStatus :: Lens' DescribeOriginEndpointResponse Int
desrsResponseStatus = lens _desrsResponseStatus (\s a -> s {_desrsResponseStatus = a})

instance NFData DescribeOriginEndpointResponse
