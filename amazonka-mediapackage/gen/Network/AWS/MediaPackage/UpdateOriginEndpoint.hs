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
-- Module      : Network.AWS.MediaPackage.UpdateOriginEndpoint
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing OriginEndpoint.
module Network.AWS.MediaPackage.UpdateOriginEndpoint
    (
    -- * Creating a Request
      updateOriginEndpoint
    , UpdateOriginEndpoint
    -- * Request Lenses
    , uoeWhitelist
    , uoeHlsPackage
    , uoeManifestName
    , uoeStartoverWindowSeconds
    , uoeDashPackage
    , uoeMssPackage
    , uoeTimeDelaySeconds
    , uoeCmafPackage
    , uoeDescription
    , uoeId

    -- * Destructuring the Response
    , updateOriginEndpointResponse
    , UpdateOriginEndpointResponse
    -- * Response Lenses
    , uoersWhitelist
    , uoersHlsPackage
    , uoersARN
    , uoersManifestName
    , uoersURL
    , uoersChannelId
    , uoersStartoverWindowSeconds
    , uoersDashPackage
    , uoersMssPackage
    , uoersId
    , uoersTimeDelaySeconds
    , uoersCmafPackage
    , uoersDescription
    , uoersResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaPackage.Types
import Network.AWS.MediaPackage.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Configuration parameters used to update an existing OriginEndpoint.
--
-- /See:/ 'updateOriginEndpoint' smart constructor.
data UpdateOriginEndpoint = UpdateOriginEndpoint'
  { _uoeWhitelist              :: !(Maybe [Text])
  , _uoeHlsPackage             :: !(Maybe HlsPackage)
  , _uoeManifestName           :: !(Maybe Text)
  , _uoeStartoverWindowSeconds :: !(Maybe Int)
  , _uoeDashPackage            :: !(Maybe DashPackage)
  , _uoeMssPackage             :: !(Maybe MssPackage)
  , _uoeTimeDelaySeconds       :: !(Maybe Int)
  , _uoeCmafPackage            :: !(Maybe CmafPackageCreateOrUpdateParameters)
  , _uoeDescription            :: !(Maybe Text)
  , _uoeId                     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateOriginEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uoeWhitelist' - A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
--
-- * 'uoeHlsPackage' - Undocumented member.
--
-- * 'uoeManifestName' - A short string that will be appended to the end of the Endpoint URL.
--
-- * 'uoeStartoverWindowSeconds' - Maximum duration (in seconds) of content to retain for startover playback. If not specified, startover playback will be disabled for the OriginEndpoint.
--
-- * 'uoeDashPackage' - Undocumented member.
--
-- * 'uoeMssPackage' - Undocumented member.
--
-- * 'uoeTimeDelaySeconds' - Amount of delay (in seconds) to enforce on the playback of live content. If not specified, there will be no time delay in effect for the OriginEndpoint.
--
-- * 'uoeCmafPackage' - Undocumented member.
--
-- * 'uoeDescription' - A short text description of the OriginEndpoint.
--
-- * 'uoeId' - The ID of the OriginEndpoint to update.
updateOriginEndpoint
    :: Text -- ^ 'uoeId'
    -> UpdateOriginEndpoint
updateOriginEndpoint pId_ =
  UpdateOriginEndpoint'
    { _uoeWhitelist = Nothing
    , _uoeHlsPackage = Nothing
    , _uoeManifestName = Nothing
    , _uoeStartoverWindowSeconds = Nothing
    , _uoeDashPackage = Nothing
    , _uoeMssPackage = Nothing
    , _uoeTimeDelaySeconds = Nothing
    , _uoeCmafPackage = Nothing
    , _uoeDescription = Nothing
    , _uoeId = pId_
    }


-- | A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
uoeWhitelist :: Lens' UpdateOriginEndpoint [Text]
uoeWhitelist = lens _uoeWhitelist (\ s a -> s{_uoeWhitelist = a}) . _Default . _Coerce

-- | Undocumented member.
uoeHlsPackage :: Lens' UpdateOriginEndpoint (Maybe HlsPackage)
uoeHlsPackage = lens _uoeHlsPackage (\ s a -> s{_uoeHlsPackage = a})

-- | A short string that will be appended to the end of the Endpoint URL.
uoeManifestName :: Lens' UpdateOriginEndpoint (Maybe Text)
uoeManifestName = lens _uoeManifestName (\ s a -> s{_uoeManifestName = a})

-- | Maximum duration (in seconds) of content to retain for startover playback. If not specified, startover playback will be disabled for the OriginEndpoint.
uoeStartoverWindowSeconds :: Lens' UpdateOriginEndpoint (Maybe Int)
uoeStartoverWindowSeconds = lens _uoeStartoverWindowSeconds (\ s a -> s{_uoeStartoverWindowSeconds = a})

-- | Undocumented member.
uoeDashPackage :: Lens' UpdateOriginEndpoint (Maybe DashPackage)
uoeDashPackage = lens _uoeDashPackage (\ s a -> s{_uoeDashPackage = a})

-- | Undocumented member.
uoeMssPackage :: Lens' UpdateOriginEndpoint (Maybe MssPackage)
uoeMssPackage = lens _uoeMssPackage (\ s a -> s{_uoeMssPackage = a})

-- | Amount of delay (in seconds) to enforce on the playback of live content. If not specified, there will be no time delay in effect for the OriginEndpoint.
uoeTimeDelaySeconds :: Lens' UpdateOriginEndpoint (Maybe Int)
uoeTimeDelaySeconds = lens _uoeTimeDelaySeconds (\ s a -> s{_uoeTimeDelaySeconds = a})

-- | Undocumented member.
uoeCmafPackage :: Lens' UpdateOriginEndpoint (Maybe CmafPackageCreateOrUpdateParameters)
uoeCmafPackage = lens _uoeCmafPackage (\ s a -> s{_uoeCmafPackage = a})

-- | A short text description of the OriginEndpoint.
uoeDescription :: Lens' UpdateOriginEndpoint (Maybe Text)
uoeDescription = lens _uoeDescription (\ s a -> s{_uoeDescription = a})

-- | The ID of the OriginEndpoint to update.
uoeId :: Lens' UpdateOriginEndpoint Text
uoeId = lens _uoeId (\ s a -> s{_uoeId = a})

instance AWSRequest UpdateOriginEndpoint where
        type Rs UpdateOriginEndpoint =
             UpdateOriginEndpointResponse
        request = putJSON mediaPackage
        response
          = receiveJSON
              (\ s h x ->
                 UpdateOriginEndpointResponse' <$>
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

instance Hashable UpdateOriginEndpoint where

instance NFData UpdateOriginEndpoint where

instance ToHeaders UpdateOriginEndpoint where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateOriginEndpoint where
        toJSON UpdateOriginEndpoint'{..}
          = object
              (catMaybes
                 [("whitelist" .=) <$> _uoeWhitelist,
                  ("hlsPackage" .=) <$> _uoeHlsPackage,
                  ("manifestName" .=) <$> _uoeManifestName,
                  ("startoverWindowSeconds" .=) <$>
                    _uoeStartoverWindowSeconds,
                  ("dashPackage" .=) <$> _uoeDashPackage,
                  ("mssPackage" .=) <$> _uoeMssPackage,
                  ("timeDelaySeconds" .=) <$> _uoeTimeDelaySeconds,
                  ("cmafPackage" .=) <$> _uoeCmafPackage,
                  ("description" .=) <$> _uoeDescription])

instance ToPath UpdateOriginEndpoint where
        toPath UpdateOriginEndpoint'{..}
          = mconcat ["/origin_endpoints/", toBS _uoeId]

instance ToQuery UpdateOriginEndpoint where
        toQuery = const mempty

-- | /See:/ 'updateOriginEndpointResponse' smart constructor.
data UpdateOriginEndpointResponse = UpdateOriginEndpointResponse'
  { _uoersWhitelist              :: !(Maybe [Text])
  , _uoersHlsPackage             :: !(Maybe HlsPackage)
  , _uoersARN                    :: !(Maybe Text)
  , _uoersManifestName           :: !(Maybe Text)
  , _uoersURL                    :: !(Maybe Text)
  , _uoersChannelId              :: !(Maybe Text)
  , _uoersStartoverWindowSeconds :: !(Maybe Int)
  , _uoersDashPackage            :: !(Maybe DashPackage)
  , _uoersMssPackage             :: !(Maybe MssPackage)
  , _uoersId                     :: !(Maybe Text)
  , _uoersTimeDelaySeconds       :: !(Maybe Int)
  , _uoersCmafPackage            :: !(Maybe CmafPackage)
  , _uoersDescription            :: !(Maybe Text)
  , _uoersResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateOriginEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uoersWhitelist' - A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
--
-- * 'uoersHlsPackage' - Undocumented member.
--
-- * 'uoersARN' - The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
--
-- * 'uoersManifestName' - A short string appended to the end of the OriginEndpoint URL.
--
-- * 'uoersURL' - The URL of the packaged OriginEndpoint for consumption.
--
-- * 'uoersChannelId' - The ID of the Channel the OriginEndpoint is associated with.
--
-- * 'uoersStartoverWindowSeconds' - Maximum duration (seconds) of content to retain for startover playback. If not specified, startover playback will be disabled for the OriginEndpoint.
--
-- * 'uoersDashPackage' - Undocumented member.
--
-- * 'uoersMssPackage' - Undocumented member.
--
-- * 'uoersId' - The ID of the OriginEndpoint.
--
-- * 'uoersTimeDelaySeconds' - Amount of delay (seconds) to enforce on the playback of live content. If not specified, there will be no time delay in effect for the OriginEndpoint.
--
-- * 'uoersCmafPackage' - Undocumented member.
--
-- * 'uoersDescription' - A short text description of the OriginEndpoint.
--
-- * 'uoersResponseStatus' - -- | The response status code.
updateOriginEndpointResponse
    :: Int -- ^ 'uoersResponseStatus'
    -> UpdateOriginEndpointResponse
updateOriginEndpointResponse pResponseStatus_ =
  UpdateOriginEndpointResponse'
    { _uoersWhitelist = Nothing
    , _uoersHlsPackage = Nothing
    , _uoersARN = Nothing
    , _uoersManifestName = Nothing
    , _uoersURL = Nothing
    , _uoersChannelId = Nothing
    , _uoersStartoverWindowSeconds = Nothing
    , _uoersDashPackage = Nothing
    , _uoersMssPackage = Nothing
    , _uoersId = Nothing
    , _uoersTimeDelaySeconds = Nothing
    , _uoersCmafPackage = Nothing
    , _uoersDescription = Nothing
    , _uoersResponseStatus = pResponseStatus_
    }


-- | A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
uoersWhitelist :: Lens' UpdateOriginEndpointResponse [Text]
uoersWhitelist = lens _uoersWhitelist (\ s a -> s{_uoersWhitelist = a}) . _Default . _Coerce

-- | Undocumented member.
uoersHlsPackage :: Lens' UpdateOriginEndpointResponse (Maybe HlsPackage)
uoersHlsPackage = lens _uoersHlsPackage (\ s a -> s{_uoersHlsPackage = a})

-- | The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
uoersARN :: Lens' UpdateOriginEndpointResponse (Maybe Text)
uoersARN = lens _uoersARN (\ s a -> s{_uoersARN = a})

-- | A short string appended to the end of the OriginEndpoint URL.
uoersManifestName :: Lens' UpdateOriginEndpointResponse (Maybe Text)
uoersManifestName = lens _uoersManifestName (\ s a -> s{_uoersManifestName = a})

-- | The URL of the packaged OriginEndpoint for consumption.
uoersURL :: Lens' UpdateOriginEndpointResponse (Maybe Text)
uoersURL = lens _uoersURL (\ s a -> s{_uoersURL = a})

-- | The ID of the Channel the OriginEndpoint is associated with.
uoersChannelId :: Lens' UpdateOriginEndpointResponse (Maybe Text)
uoersChannelId = lens _uoersChannelId (\ s a -> s{_uoersChannelId = a})

-- | Maximum duration (seconds) of content to retain for startover playback. If not specified, startover playback will be disabled for the OriginEndpoint.
uoersStartoverWindowSeconds :: Lens' UpdateOriginEndpointResponse (Maybe Int)
uoersStartoverWindowSeconds = lens _uoersStartoverWindowSeconds (\ s a -> s{_uoersStartoverWindowSeconds = a})

-- | Undocumented member.
uoersDashPackage :: Lens' UpdateOriginEndpointResponse (Maybe DashPackage)
uoersDashPackage = lens _uoersDashPackage (\ s a -> s{_uoersDashPackage = a})

-- | Undocumented member.
uoersMssPackage :: Lens' UpdateOriginEndpointResponse (Maybe MssPackage)
uoersMssPackage = lens _uoersMssPackage (\ s a -> s{_uoersMssPackage = a})

-- | The ID of the OriginEndpoint.
uoersId :: Lens' UpdateOriginEndpointResponse (Maybe Text)
uoersId = lens _uoersId (\ s a -> s{_uoersId = a})

-- | Amount of delay (seconds) to enforce on the playback of live content. If not specified, there will be no time delay in effect for the OriginEndpoint.
uoersTimeDelaySeconds :: Lens' UpdateOriginEndpointResponse (Maybe Int)
uoersTimeDelaySeconds = lens _uoersTimeDelaySeconds (\ s a -> s{_uoersTimeDelaySeconds = a})

-- | Undocumented member.
uoersCmafPackage :: Lens' UpdateOriginEndpointResponse (Maybe CmafPackage)
uoersCmafPackage = lens _uoersCmafPackage (\ s a -> s{_uoersCmafPackage = a})

-- | A short text description of the OriginEndpoint.
uoersDescription :: Lens' UpdateOriginEndpointResponse (Maybe Text)
uoersDescription = lens _uoersDescription (\ s a -> s{_uoersDescription = a})

-- | -- | The response status code.
uoersResponseStatus :: Lens' UpdateOriginEndpointResponse Int
uoersResponseStatus = lens _uoersResponseStatus (\ s a -> s{_uoersResponseStatus = a})

instance NFData UpdateOriginEndpointResponse where
