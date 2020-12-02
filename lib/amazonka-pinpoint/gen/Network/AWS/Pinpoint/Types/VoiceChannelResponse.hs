{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.VoiceChannelResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.VoiceChannelResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about the status and settings of the voice channel for an application.
--
--
--
-- /See:/ 'voiceChannelResponse' smart constructor.
data VoiceChannelResponse = VoiceChannelResponse'
  { _vcLastModifiedDate ::
      !(Maybe Text),
    _vcEnabled :: !(Maybe Bool),
    _vcIsArchived :: !(Maybe Bool),
    _vcApplicationId :: !(Maybe Text),
    _vcVersion :: !(Maybe Int),
    _vcId :: !(Maybe Text),
    _vcCreationDate :: !(Maybe Text),
    _vcLastModifiedBy :: !(Maybe Text),
    _vcHasCredential :: !(Maybe Bool),
    _vcPlatform :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VoiceChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcLastModifiedDate' - The date and time, in ISO 8601 format, when the voice channel was last modified.
--
-- * 'vcEnabled' - Specifies whether the voice channel is enabled for the application.
--
-- * 'vcIsArchived' - Specifies whether the voice channel is archived.
--
-- * 'vcApplicationId' - The unique identifier for the application that the voice channel applies to.
--
-- * 'vcVersion' - The current version of the voice channel.
--
-- * 'vcId' - (Deprecated) An identifier for the voice channel. This property is retained only for backward compatibility.
--
-- * 'vcCreationDate' - The date and time, in ISO 8601 format, when the voice channel was enabled.
--
-- * 'vcLastModifiedBy' - The user who last modified the voice channel.
--
-- * 'vcHasCredential' - (Not used) This property is retained only for backward compatibility.
--
-- * 'vcPlatform' - The type of messaging or notification platform for the channel. For the voice channel, this value is VOICE.
voiceChannelResponse ::
  -- | 'vcPlatform'
  Text ->
  VoiceChannelResponse
voiceChannelResponse pPlatform_ =
  VoiceChannelResponse'
    { _vcLastModifiedDate = Nothing,
      _vcEnabled = Nothing,
      _vcIsArchived = Nothing,
      _vcApplicationId = Nothing,
      _vcVersion = Nothing,
      _vcId = Nothing,
      _vcCreationDate = Nothing,
      _vcLastModifiedBy = Nothing,
      _vcHasCredential = Nothing,
      _vcPlatform = pPlatform_
    }

-- | The date and time, in ISO 8601 format, when the voice channel was last modified.
vcLastModifiedDate :: Lens' VoiceChannelResponse (Maybe Text)
vcLastModifiedDate = lens _vcLastModifiedDate (\s a -> s {_vcLastModifiedDate = a})

-- | Specifies whether the voice channel is enabled for the application.
vcEnabled :: Lens' VoiceChannelResponse (Maybe Bool)
vcEnabled = lens _vcEnabled (\s a -> s {_vcEnabled = a})

-- | Specifies whether the voice channel is archived.
vcIsArchived :: Lens' VoiceChannelResponse (Maybe Bool)
vcIsArchived = lens _vcIsArchived (\s a -> s {_vcIsArchived = a})

-- | The unique identifier for the application that the voice channel applies to.
vcApplicationId :: Lens' VoiceChannelResponse (Maybe Text)
vcApplicationId = lens _vcApplicationId (\s a -> s {_vcApplicationId = a})

-- | The current version of the voice channel.
vcVersion :: Lens' VoiceChannelResponse (Maybe Int)
vcVersion = lens _vcVersion (\s a -> s {_vcVersion = a})

-- | (Deprecated) An identifier for the voice channel. This property is retained only for backward compatibility.
vcId :: Lens' VoiceChannelResponse (Maybe Text)
vcId = lens _vcId (\s a -> s {_vcId = a})

-- | The date and time, in ISO 8601 format, when the voice channel was enabled.
vcCreationDate :: Lens' VoiceChannelResponse (Maybe Text)
vcCreationDate = lens _vcCreationDate (\s a -> s {_vcCreationDate = a})

-- | The user who last modified the voice channel.
vcLastModifiedBy :: Lens' VoiceChannelResponse (Maybe Text)
vcLastModifiedBy = lens _vcLastModifiedBy (\s a -> s {_vcLastModifiedBy = a})

-- | (Not used) This property is retained only for backward compatibility.
vcHasCredential :: Lens' VoiceChannelResponse (Maybe Bool)
vcHasCredential = lens _vcHasCredential (\s a -> s {_vcHasCredential = a})

-- | The type of messaging or notification platform for the channel. For the voice channel, this value is VOICE.
vcPlatform :: Lens' VoiceChannelResponse Text
vcPlatform = lens _vcPlatform (\s a -> s {_vcPlatform = a})

instance FromJSON VoiceChannelResponse where
  parseJSON =
    withObject
      "VoiceChannelResponse"
      ( \x ->
          VoiceChannelResponse'
            <$> (x .:? "LastModifiedDate")
            <*> (x .:? "Enabled")
            <*> (x .:? "IsArchived")
            <*> (x .:? "ApplicationId")
            <*> (x .:? "Version")
            <*> (x .:? "Id")
            <*> (x .:? "CreationDate")
            <*> (x .:? "LastModifiedBy")
            <*> (x .:? "HasCredential")
            <*> (x .: "Platform")
      )

instance Hashable VoiceChannelResponse

instance NFData VoiceChannelResponse
