{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.OriginEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.OriginEndpoint where

import Network.AWS.Lens
import Network.AWS.MediaPackage.Types.Authorization
import Network.AWS.MediaPackage.Types.CmafPackage
import Network.AWS.MediaPackage.Types.DashPackage
import Network.AWS.MediaPackage.Types.HlsPackage
import Network.AWS.MediaPackage.Types.MssPackage
import Network.AWS.MediaPackage.Types.Origination
import Network.AWS.Prelude

-- | An OriginEndpoint resource configuration.
--
-- /See:/ 'originEndpoint' smart constructor.
data OriginEndpoint = OriginEndpoint'
  { _oeWhitelist ::
      !(Maybe [Text]),
    _oeHlsPackage :: !(Maybe HlsPackage),
    _oeARN :: !(Maybe Text),
    _oeManifestName :: !(Maybe Text),
    _oeURL :: !(Maybe Text),
    _oeAuthorization :: !(Maybe Authorization),
    _oeChannelId :: !(Maybe Text),
    _oeStartoverWindowSeconds :: !(Maybe Int),
    _oeDashPackage :: !(Maybe DashPackage),
    _oeMssPackage :: !(Maybe MssPackage),
    _oeId :: !(Maybe Text),
    _oeTimeDelaySeconds :: !(Maybe Int),
    _oeCmafPackage :: !(Maybe CmafPackage),
    _oeDescription :: !(Maybe Text),
    _oeTags :: !(Maybe (Map Text (Text))),
    _oeOrigination :: !(Maybe Origination)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OriginEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oeWhitelist' - A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
--
-- * 'oeHlsPackage' - Undocumented member.
--
-- * 'oeARN' - The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
--
-- * 'oeManifestName' - A short string appended to the end of the OriginEndpoint URL.
--
-- * 'oeURL' - The URL of the packaged OriginEndpoint for consumption.
--
-- * 'oeAuthorization' - Undocumented member.
--
-- * 'oeChannelId' - The ID of the Channel the OriginEndpoint is associated with.
--
-- * 'oeStartoverWindowSeconds' - Maximum duration (seconds) of content to retain for startover playback. If not specified, startover playback will be disabled for the OriginEndpoint.
--
-- * 'oeDashPackage' - Undocumented member.
--
-- * 'oeMssPackage' - Undocumented member.
--
-- * 'oeId' - The ID of the OriginEndpoint.
--
-- * 'oeTimeDelaySeconds' - Amount of delay (seconds) to enforce on the playback of live content. If not specified, there will be no time delay in effect for the OriginEndpoint.
--
-- * 'oeCmafPackage' - Undocumented member.
--
-- * 'oeDescription' - A short text description of the OriginEndpoint.
--
-- * 'oeTags' - Undocumented member.
--
-- * 'oeOrigination' - Control whether origination of video is allowed for this OriginEndpoint. If set to ALLOW, the OriginEndpoint may by requested, pursuant to any other form of access control. If set to DENY, the OriginEndpoint may not be requested. This can be helpful for Live to VOD harvesting, or for temporarily disabling origination
originEndpoint ::
  OriginEndpoint
originEndpoint =
  OriginEndpoint'
    { _oeWhitelist = Nothing,
      _oeHlsPackage = Nothing,
      _oeARN = Nothing,
      _oeManifestName = Nothing,
      _oeURL = Nothing,
      _oeAuthorization = Nothing,
      _oeChannelId = Nothing,
      _oeStartoverWindowSeconds = Nothing,
      _oeDashPackage = Nothing,
      _oeMssPackage = Nothing,
      _oeId = Nothing,
      _oeTimeDelaySeconds = Nothing,
      _oeCmafPackage = Nothing,
      _oeDescription = Nothing,
      _oeTags = Nothing,
      _oeOrigination = Nothing
    }

-- | A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
oeWhitelist :: Lens' OriginEndpoint [Text]
oeWhitelist = lens _oeWhitelist (\s a -> s {_oeWhitelist = a}) . _Default . _Coerce

-- | Undocumented member.
oeHlsPackage :: Lens' OriginEndpoint (Maybe HlsPackage)
oeHlsPackage = lens _oeHlsPackage (\s a -> s {_oeHlsPackage = a})

-- | The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
oeARN :: Lens' OriginEndpoint (Maybe Text)
oeARN = lens _oeARN (\s a -> s {_oeARN = a})

-- | A short string appended to the end of the OriginEndpoint URL.
oeManifestName :: Lens' OriginEndpoint (Maybe Text)
oeManifestName = lens _oeManifestName (\s a -> s {_oeManifestName = a})

-- | The URL of the packaged OriginEndpoint for consumption.
oeURL :: Lens' OriginEndpoint (Maybe Text)
oeURL = lens _oeURL (\s a -> s {_oeURL = a})

-- | Undocumented member.
oeAuthorization :: Lens' OriginEndpoint (Maybe Authorization)
oeAuthorization = lens _oeAuthorization (\s a -> s {_oeAuthorization = a})

-- | The ID of the Channel the OriginEndpoint is associated with.
oeChannelId :: Lens' OriginEndpoint (Maybe Text)
oeChannelId = lens _oeChannelId (\s a -> s {_oeChannelId = a})

-- | Maximum duration (seconds) of content to retain for startover playback. If not specified, startover playback will be disabled for the OriginEndpoint.
oeStartoverWindowSeconds :: Lens' OriginEndpoint (Maybe Int)
oeStartoverWindowSeconds = lens _oeStartoverWindowSeconds (\s a -> s {_oeStartoverWindowSeconds = a})

-- | Undocumented member.
oeDashPackage :: Lens' OriginEndpoint (Maybe DashPackage)
oeDashPackage = lens _oeDashPackage (\s a -> s {_oeDashPackage = a})

-- | Undocumented member.
oeMssPackage :: Lens' OriginEndpoint (Maybe MssPackage)
oeMssPackage = lens _oeMssPackage (\s a -> s {_oeMssPackage = a})

-- | The ID of the OriginEndpoint.
oeId :: Lens' OriginEndpoint (Maybe Text)
oeId = lens _oeId (\s a -> s {_oeId = a})

-- | Amount of delay (seconds) to enforce on the playback of live content. If not specified, there will be no time delay in effect for the OriginEndpoint.
oeTimeDelaySeconds :: Lens' OriginEndpoint (Maybe Int)
oeTimeDelaySeconds = lens _oeTimeDelaySeconds (\s a -> s {_oeTimeDelaySeconds = a})

-- | Undocumented member.
oeCmafPackage :: Lens' OriginEndpoint (Maybe CmafPackage)
oeCmafPackage = lens _oeCmafPackage (\s a -> s {_oeCmafPackage = a})

-- | A short text description of the OriginEndpoint.
oeDescription :: Lens' OriginEndpoint (Maybe Text)
oeDescription = lens _oeDescription (\s a -> s {_oeDescription = a})

-- | Undocumented member.
oeTags :: Lens' OriginEndpoint (HashMap Text (Text))
oeTags = lens _oeTags (\s a -> s {_oeTags = a}) . _Default . _Map

-- | Control whether origination of video is allowed for this OriginEndpoint. If set to ALLOW, the OriginEndpoint may by requested, pursuant to any other form of access control. If set to DENY, the OriginEndpoint may not be requested. This can be helpful for Live to VOD harvesting, or for temporarily disabling origination
oeOrigination :: Lens' OriginEndpoint (Maybe Origination)
oeOrigination = lens _oeOrigination (\s a -> s {_oeOrigination = a})

instance FromJSON OriginEndpoint where
  parseJSON =
    withObject
      "OriginEndpoint"
      ( \x ->
          OriginEndpoint'
            <$> (x .:? "whitelist" .!= mempty)
            <*> (x .:? "hlsPackage")
            <*> (x .:? "arn")
            <*> (x .:? "manifestName")
            <*> (x .:? "url")
            <*> (x .:? "authorization")
            <*> (x .:? "channelId")
            <*> (x .:? "startoverWindowSeconds")
            <*> (x .:? "dashPackage")
            <*> (x .:? "mssPackage")
            <*> (x .:? "id")
            <*> (x .:? "timeDelaySeconds")
            <*> (x .:? "cmafPackage")
            <*> (x .:? "description")
            <*> (x .:? "tags" .!= mempty)
            <*> (x .:? "origination")
      )

instance Hashable OriginEndpoint

instance NFData OriginEndpoint
