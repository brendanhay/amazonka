{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ChannelResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ChannelResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about the general settings and status of a channel for an application.
--
--
--
-- /See:/ 'channelResponse' smart constructor.
data ChannelResponse = ChannelResponse'
  { _chaLastModifiedDate ::
      !(Maybe Text),
    _chaEnabled :: !(Maybe Bool),
    _chaIsArchived :: !(Maybe Bool),
    _chaApplicationId :: !(Maybe Text),
    _chaVersion :: !(Maybe Int),
    _chaId :: !(Maybe Text),
    _chaCreationDate :: !(Maybe Text),
    _chaLastModifiedBy :: !(Maybe Text),
    _chaHasCredential :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chaLastModifiedDate' - The date and time, in ISO 8601 format, when the channel was last modified.
--
-- * 'chaEnabled' - Specifies whether the channel is enabled for the application.
--
-- * 'chaIsArchived' - Specifies whether the channel is archived.
--
-- * 'chaApplicationId' - The unique identifier for the application.
--
-- * 'chaVersion' - The current version of the channel.
--
-- * 'chaId' - (Deprecated) An identifier for the channel. This property is retained only for backward compatibility.
--
-- * 'chaCreationDate' - The date and time, in ISO 8601 format, when the channel was enabled.
--
-- * 'chaLastModifiedBy' - The user who last modified the channel.
--
-- * 'chaHasCredential' - (Not used) This property is retained only for backward compatibility.
channelResponse ::
  ChannelResponse
channelResponse =
  ChannelResponse'
    { _chaLastModifiedDate = Nothing,
      _chaEnabled = Nothing,
      _chaIsArchived = Nothing,
      _chaApplicationId = Nothing,
      _chaVersion = Nothing,
      _chaId = Nothing,
      _chaCreationDate = Nothing,
      _chaLastModifiedBy = Nothing,
      _chaHasCredential = Nothing
    }

-- | The date and time, in ISO 8601 format, when the channel was last modified.
chaLastModifiedDate :: Lens' ChannelResponse (Maybe Text)
chaLastModifiedDate = lens _chaLastModifiedDate (\s a -> s {_chaLastModifiedDate = a})

-- | Specifies whether the channel is enabled for the application.
chaEnabled :: Lens' ChannelResponse (Maybe Bool)
chaEnabled = lens _chaEnabled (\s a -> s {_chaEnabled = a})

-- | Specifies whether the channel is archived.
chaIsArchived :: Lens' ChannelResponse (Maybe Bool)
chaIsArchived = lens _chaIsArchived (\s a -> s {_chaIsArchived = a})

-- | The unique identifier for the application.
chaApplicationId :: Lens' ChannelResponse (Maybe Text)
chaApplicationId = lens _chaApplicationId (\s a -> s {_chaApplicationId = a})

-- | The current version of the channel.
chaVersion :: Lens' ChannelResponse (Maybe Int)
chaVersion = lens _chaVersion (\s a -> s {_chaVersion = a})

-- | (Deprecated) An identifier for the channel. This property is retained only for backward compatibility.
chaId :: Lens' ChannelResponse (Maybe Text)
chaId = lens _chaId (\s a -> s {_chaId = a})

-- | The date and time, in ISO 8601 format, when the channel was enabled.
chaCreationDate :: Lens' ChannelResponse (Maybe Text)
chaCreationDate = lens _chaCreationDate (\s a -> s {_chaCreationDate = a})

-- | The user who last modified the channel.
chaLastModifiedBy :: Lens' ChannelResponse (Maybe Text)
chaLastModifiedBy = lens _chaLastModifiedBy (\s a -> s {_chaLastModifiedBy = a})

-- | (Not used) This property is retained only for backward compatibility.
chaHasCredential :: Lens' ChannelResponse (Maybe Bool)
chaHasCredential = lens _chaHasCredential (\s a -> s {_chaHasCredential = a})

instance FromJSON ChannelResponse where
  parseJSON =
    withObject
      "ChannelResponse"
      ( \x ->
          ChannelResponse'
            <$> (x .:? "LastModifiedDate")
            <*> (x .:? "Enabled")
            <*> (x .:? "IsArchived")
            <*> (x .:? "ApplicationId")
            <*> (x .:? "Version")
            <*> (x .:? "Id")
            <*> (x .:? "CreationDate")
            <*> (x .:? "LastModifiedBy")
            <*> (x .:? "HasCredential")
      )

instance Hashable ChannelResponse

instance NFData ChannelResponse
