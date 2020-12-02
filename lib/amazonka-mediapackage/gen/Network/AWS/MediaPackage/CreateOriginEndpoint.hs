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
-- Module      : Network.AWS.MediaPackage.CreateOriginEndpoint
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new OriginEndpoint record.
module Network.AWS.MediaPackage.CreateOriginEndpoint
    (
    -- * Creating a Request
      createOriginEndpoint
    , CreateOriginEndpoint
    -- * Request Lenses
    , coeWhitelist
    , coeHlsPackage
    , coeManifestName
    , coeStartoverWindowSeconds
    , coeDashPackage
    , coeMssPackage
    , coeTimeDelaySeconds
    , coeCmafPackage
    , coeDescription
    , coeChannelId
    , coeId

    -- * Destructuring the Response
    , createOriginEndpointResponse
    , CreateOriginEndpointResponse
    -- * Response Lenses
    , coersWhitelist
    , coersHlsPackage
    , coersARN
    , coersManifestName
    , coersURL
    , coersChannelId
    , coersStartoverWindowSeconds
    , coersDashPackage
    , coersMssPackage
    , coersId
    , coersTimeDelaySeconds
    , coersCmafPackage
    , coersDescription
    , coersResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaPackage.Types
import Network.AWS.MediaPackage.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Configuration parameters used to create a new OriginEndpoint.
--
-- /See:/ 'createOriginEndpoint' smart constructor.
data CreateOriginEndpoint = CreateOriginEndpoint'
  { _coeWhitelist              :: !(Maybe [Text])
  , _coeHlsPackage             :: !(Maybe HlsPackage)
  , _coeManifestName           :: !(Maybe Text)
  , _coeStartoverWindowSeconds :: !(Maybe Int)
  , _coeDashPackage            :: !(Maybe DashPackage)
  , _coeMssPackage             :: !(Maybe MssPackage)
  , _coeTimeDelaySeconds       :: !(Maybe Int)
  , _coeCmafPackage            :: !(Maybe CmafPackageCreateOrUpdateParameters)
  , _coeDescription            :: !(Maybe Text)
  , _coeChannelId              :: !Text
  , _coeId                     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateOriginEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coeWhitelist' - A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
--
-- * 'coeHlsPackage' - Undocumented member.
--
-- * 'coeManifestName' - A short string that will be used as the filename of the OriginEndpoint URL (defaults to "index").
--
-- * 'coeStartoverWindowSeconds' - Maximum duration (seconds) of content to retain for startover playback. If not specified, startover playback will be disabled for the OriginEndpoint.
--
-- * 'coeDashPackage' - Undocumented member.
--
-- * 'coeMssPackage' - Undocumented member.
--
-- * 'coeTimeDelaySeconds' - Amount of delay (seconds) to enforce on the playback of live content. If not specified, there will be no time delay in effect for the OriginEndpoint.
--
-- * 'coeCmafPackage' - Undocumented member.
--
-- * 'coeDescription' - A short text description of the OriginEndpoint.
--
-- * 'coeChannelId' - The ID of the Channel that the OriginEndpoint will be associated with. This cannot be changed after the OriginEndpoint is created.
--
-- * 'coeId' - The ID of the OriginEndpoint.  The ID must be unique within the region and it cannot be changed after the OriginEndpoint is created.
createOriginEndpoint
    :: Text -- ^ 'coeChannelId'
    -> Text -- ^ 'coeId'
    -> CreateOriginEndpoint
createOriginEndpoint pChannelId_ pId_ =
  CreateOriginEndpoint'
    { _coeWhitelist = Nothing
    , _coeHlsPackage = Nothing
    , _coeManifestName = Nothing
    , _coeStartoverWindowSeconds = Nothing
    , _coeDashPackage = Nothing
    , _coeMssPackage = Nothing
    , _coeTimeDelaySeconds = Nothing
    , _coeCmafPackage = Nothing
    , _coeDescription = Nothing
    , _coeChannelId = pChannelId_
    , _coeId = pId_
    }


-- | A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
coeWhitelist :: Lens' CreateOriginEndpoint [Text]
coeWhitelist = lens _coeWhitelist (\ s a -> s{_coeWhitelist = a}) . _Default . _Coerce

-- | Undocumented member.
coeHlsPackage :: Lens' CreateOriginEndpoint (Maybe HlsPackage)
coeHlsPackage = lens _coeHlsPackage (\ s a -> s{_coeHlsPackage = a})

-- | A short string that will be used as the filename of the OriginEndpoint URL (defaults to "index").
coeManifestName :: Lens' CreateOriginEndpoint (Maybe Text)
coeManifestName = lens _coeManifestName (\ s a -> s{_coeManifestName = a})

-- | Maximum duration (seconds) of content to retain for startover playback. If not specified, startover playback will be disabled for the OriginEndpoint.
coeStartoverWindowSeconds :: Lens' CreateOriginEndpoint (Maybe Int)
coeStartoverWindowSeconds = lens _coeStartoverWindowSeconds (\ s a -> s{_coeStartoverWindowSeconds = a})

-- | Undocumented member.
coeDashPackage :: Lens' CreateOriginEndpoint (Maybe DashPackage)
coeDashPackage = lens _coeDashPackage (\ s a -> s{_coeDashPackage = a})

-- | Undocumented member.
coeMssPackage :: Lens' CreateOriginEndpoint (Maybe MssPackage)
coeMssPackage = lens _coeMssPackage (\ s a -> s{_coeMssPackage = a})

-- | Amount of delay (seconds) to enforce on the playback of live content. If not specified, there will be no time delay in effect for the OriginEndpoint.
coeTimeDelaySeconds :: Lens' CreateOriginEndpoint (Maybe Int)
coeTimeDelaySeconds = lens _coeTimeDelaySeconds (\ s a -> s{_coeTimeDelaySeconds = a})

-- | Undocumented member.
coeCmafPackage :: Lens' CreateOriginEndpoint (Maybe CmafPackageCreateOrUpdateParameters)
coeCmafPackage = lens _coeCmafPackage (\ s a -> s{_coeCmafPackage = a})

-- | A short text description of the OriginEndpoint.
coeDescription :: Lens' CreateOriginEndpoint (Maybe Text)
coeDescription = lens _coeDescription (\ s a -> s{_coeDescription = a})

-- | The ID of the Channel that the OriginEndpoint will be associated with. This cannot be changed after the OriginEndpoint is created.
coeChannelId :: Lens' CreateOriginEndpoint Text
coeChannelId = lens _coeChannelId (\ s a -> s{_coeChannelId = a})

-- | The ID of the OriginEndpoint.  The ID must be unique within the region and it cannot be changed after the OriginEndpoint is created.
coeId :: Lens' CreateOriginEndpoint Text
coeId = lens _coeId (\ s a -> s{_coeId = a})

instance AWSRequest CreateOriginEndpoint where
        type Rs CreateOriginEndpoint =
             CreateOriginEndpointResponse
        request = postJSON mediaPackage
        response
          = receiveJSON
              (\ s h x ->
                 CreateOriginEndpointResponse' <$>
                   (x .?> "whitelist" .!@ mempty) <*>
                     (x .?> "hlsPackage")
                     <*> (x .?> "arn")
                     <*> (x .?> "manifestName")
                     <*> (x .?> "url")
                     <*> (x .?> "channelId")
                     <*> (x .?> "startoverWindowSeconds")
                     <*> (x .?> "dashPackage")
                     <*> (x .?> "mssPackage")
                     <*> (x .?> "id")
                     <*> (x .?> "timeDelaySeconds")
                     <*> (x .?> "cmafPackage")
                     <*> (x .?> "description")
                     <*> (pure (fromEnum s)))

instance Hashable CreateOriginEndpoint where

instance NFData CreateOriginEndpoint where

instance ToHeaders CreateOriginEndpoint where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateOriginEndpoint where
        toJSON CreateOriginEndpoint'{..}
          = object
              (catMaybes
                 [("whitelist" .=) <$> _coeWhitelist,
                  ("hlsPackage" .=) <$> _coeHlsPackage,
                  ("manifestName" .=) <$> _coeManifestName,
                  ("startoverWindowSeconds" .=) <$>
                    _coeStartoverWindowSeconds,
                  ("dashPackage" .=) <$> _coeDashPackage,
                  ("mssPackage" .=) <$> _coeMssPackage,
                  ("timeDelaySeconds" .=) <$> _coeTimeDelaySeconds,
                  ("cmafPackage" .=) <$> _coeCmafPackage,
                  ("description" .=) <$> _coeDescription,
                  Just ("channelId" .= _coeChannelId),
                  Just ("id" .= _coeId)])

instance ToPath CreateOriginEndpoint where
        toPath = const "/origin_endpoints"

instance ToQuery CreateOriginEndpoint where
        toQuery = const mempty

-- | /See:/ 'createOriginEndpointResponse' smart constructor.
data CreateOriginEndpointResponse = CreateOriginEndpointResponse'
  { _coersWhitelist              :: !(Maybe [Text])
  , _coersHlsPackage             :: !(Maybe HlsPackage)
  , _coersARN                    :: !(Maybe Text)
  , _coersManifestName           :: !(Maybe Text)
  , _coersURL                    :: !(Maybe Text)
  , _coersChannelId              :: !(Maybe Text)
  , _coersStartoverWindowSeconds :: !(Maybe Int)
  , _coersDashPackage            :: !(Maybe DashPackage)
  , _coersMssPackage             :: !(Maybe MssPackage)
  , _coersId                     :: !(Maybe Text)
  , _coersTimeDelaySeconds       :: !(Maybe Int)
  , _coersCmafPackage            :: !(Maybe CmafPackage)
  , _coersDescription            :: !(Maybe Text)
  , _coersResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateOriginEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coersWhitelist' - A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
--
-- * 'coersHlsPackage' - Undocumented member.
--
-- * 'coersARN' - The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
--
-- * 'coersManifestName' - A short string appended to the end of the OriginEndpoint URL.
--
-- * 'coersURL' - The URL of the packaged OriginEndpoint for consumption.
--
-- * 'coersChannelId' - The ID of the Channel the OriginEndpoint is associated with.
--
-- * 'coersStartoverWindowSeconds' - Maximum duration (seconds) of content to retain for startover playback. If not specified, startover playback will be disabled for the OriginEndpoint.
--
-- * 'coersDashPackage' - Undocumented member.
--
-- * 'coersMssPackage' - Undocumented member.
--
-- * 'coersId' - The ID of the OriginEndpoint.
--
-- * 'coersTimeDelaySeconds' - Amount of delay (seconds) to enforce on the playback of live content. If not specified, there will be no time delay in effect for the OriginEndpoint.
--
-- * 'coersCmafPackage' - Undocumented member.
--
-- * 'coersDescription' - A short text description of the OriginEndpoint.
--
-- * 'coersResponseStatus' - -- | The response status code.
createOriginEndpointResponse
    :: Int -- ^ 'coersResponseStatus'
    -> CreateOriginEndpointResponse
createOriginEndpointResponse pResponseStatus_ =
  CreateOriginEndpointResponse'
    { _coersWhitelist = Nothing
    , _coersHlsPackage = Nothing
    , _coersARN = Nothing
    , _coersManifestName = Nothing
    , _coersURL = Nothing
    , _coersChannelId = Nothing
    , _coersStartoverWindowSeconds = Nothing
    , _coersDashPackage = Nothing
    , _coersMssPackage = Nothing
    , _coersId = Nothing
    , _coersTimeDelaySeconds = Nothing
    , _coersCmafPackage = Nothing
    , _coersDescription = Nothing
    , _coersResponseStatus = pResponseStatus_
    }


-- | A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
coersWhitelist :: Lens' CreateOriginEndpointResponse [Text]
coersWhitelist = lens _coersWhitelist (\ s a -> s{_coersWhitelist = a}) . _Default . _Coerce

-- | Undocumented member.
coersHlsPackage :: Lens' CreateOriginEndpointResponse (Maybe HlsPackage)
coersHlsPackage = lens _coersHlsPackage (\ s a -> s{_coersHlsPackage = a})

-- | The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
coersARN :: Lens' CreateOriginEndpointResponse (Maybe Text)
coersARN = lens _coersARN (\ s a -> s{_coersARN = a})

-- | A short string appended to the end of the OriginEndpoint URL.
coersManifestName :: Lens' CreateOriginEndpointResponse (Maybe Text)
coersManifestName = lens _coersManifestName (\ s a -> s{_coersManifestName = a})

-- | The URL of the packaged OriginEndpoint for consumption.
coersURL :: Lens' CreateOriginEndpointResponse (Maybe Text)
coersURL = lens _coersURL (\ s a -> s{_coersURL = a})

-- | The ID of the Channel the OriginEndpoint is associated with.
coersChannelId :: Lens' CreateOriginEndpointResponse (Maybe Text)
coersChannelId = lens _coersChannelId (\ s a -> s{_coersChannelId = a})

-- | Maximum duration (seconds) of content to retain for startover playback. If not specified, startover playback will be disabled for the OriginEndpoint.
coersStartoverWindowSeconds :: Lens' CreateOriginEndpointResponse (Maybe Int)
coersStartoverWindowSeconds = lens _coersStartoverWindowSeconds (\ s a -> s{_coersStartoverWindowSeconds = a})

-- | Undocumented member.
coersDashPackage :: Lens' CreateOriginEndpointResponse (Maybe DashPackage)
coersDashPackage = lens _coersDashPackage (\ s a -> s{_coersDashPackage = a})

-- | Undocumented member.
coersMssPackage :: Lens' CreateOriginEndpointResponse (Maybe MssPackage)
coersMssPackage = lens _coersMssPackage (\ s a -> s{_coersMssPackage = a})

-- | The ID of the OriginEndpoint.
coersId :: Lens' CreateOriginEndpointResponse (Maybe Text)
coersId = lens _coersId (\ s a -> s{_coersId = a})

-- | Amount of delay (seconds) to enforce on the playback of live content. If not specified, there will be no time delay in effect for the OriginEndpoint.
coersTimeDelaySeconds :: Lens' CreateOriginEndpointResponse (Maybe Int)
coersTimeDelaySeconds = lens _coersTimeDelaySeconds (\ s a -> s{_coersTimeDelaySeconds = a})

-- | Undocumented member.
coersCmafPackage :: Lens' CreateOriginEndpointResponse (Maybe CmafPackage)
coersCmafPackage = lens _coersCmafPackage (\ s a -> s{_coersCmafPackage = a})

-- | A short text description of the OriginEndpoint.
coersDescription :: Lens' CreateOriginEndpointResponse (Maybe Text)
coersDescription = lens _coersDescription (\ s a -> s{_coersDescription = a})

-- | -- | The response status code.
coersResponseStatus :: Lens' CreateOriginEndpointResponse Int
coersResponseStatus = lens _coersResponseStatus (\ s a -> s{_coersResponseStatus = a})

instance NFData CreateOriginEndpointResponse where
