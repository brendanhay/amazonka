{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ADMChannelResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ADMChannelResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about the status and settings of the ADM (Amazon Device Messaging) channel for an application.
--
--
--
-- /See:/ 'aDMChannelResponse' smart constructor.
data ADMChannelResponse = ADMChannelResponse'
  { _admcLastModifiedDate ::
      !(Maybe Text),
    _admcEnabled :: !(Maybe Bool),
    _admcIsArchived :: !(Maybe Bool),
    _admcApplicationId :: !(Maybe Text),
    _admcVersion :: !(Maybe Int),
    _admcId :: !(Maybe Text),
    _admcCreationDate :: !(Maybe Text),
    _admcLastModifiedBy :: !(Maybe Text),
    _admcHasCredential :: !(Maybe Bool),
    _admcPlatform :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ADMChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'admcLastModifiedDate' - The date and time when the ADM channel was last modified.
--
-- * 'admcEnabled' - Specifies whether the ADM channel is enabled for the application.
--
-- * 'admcIsArchived' - Specifies whether the ADM channel is archived.
--
-- * 'admcApplicationId' - The unique identifier for the application that the ADM channel applies to.
--
-- * 'admcVersion' - The current version of the ADM channel.
--
-- * 'admcId' - (Deprecated) An identifier for the ADM channel. This property is retained only for backward compatibility.
--
-- * 'admcCreationDate' - The date and time when the ADM channel was enabled.
--
-- * 'admcLastModifiedBy' - The user who last modified the ADM channel.
--
-- * 'admcHasCredential' - (Not used) This property is retained only for backward compatibility.
--
-- * 'admcPlatform' - The type of messaging or notification platform for the channel. For the ADM channel, this value is ADM.
aDMChannelResponse ::
  -- | 'admcPlatform'
  Text ->
  ADMChannelResponse
aDMChannelResponse pPlatform_ =
  ADMChannelResponse'
    { _admcLastModifiedDate = Nothing,
      _admcEnabled = Nothing,
      _admcIsArchived = Nothing,
      _admcApplicationId = Nothing,
      _admcVersion = Nothing,
      _admcId = Nothing,
      _admcCreationDate = Nothing,
      _admcLastModifiedBy = Nothing,
      _admcHasCredential = Nothing,
      _admcPlatform = pPlatform_
    }

-- | The date and time when the ADM channel was last modified.
admcLastModifiedDate :: Lens' ADMChannelResponse (Maybe Text)
admcLastModifiedDate = lens _admcLastModifiedDate (\s a -> s {_admcLastModifiedDate = a})

-- | Specifies whether the ADM channel is enabled for the application.
admcEnabled :: Lens' ADMChannelResponse (Maybe Bool)
admcEnabled = lens _admcEnabled (\s a -> s {_admcEnabled = a})

-- | Specifies whether the ADM channel is archived.
admcIsArchived :: Lens' ADMChannelResponse (Maybe Bool)
admcIsArchived = lens _admcIsArchived (\s a -> s {_admcIsArchived = a})

-- | The unique identifier for the application that the ADM channel applies to.
admcApplicationId :: Lens' ADMChannelResponse (Maybe Text)
admcApplicationId = lens _admcApplicationId (\s a -> s {_admcApplicationId = a})

-- | The current version of the ADM channel.
admcVersion :: Lens' ADMChannelResponse (Maybe Int)
admcVersion = lens _admcVersion (\s a -> s {_admcVersion = a})

-- | (Deprecated) An identifier for the ADM channel. This property is retained only for backward compatibility.
admcId :: Lens' ADMChannelResponse (Maybe Text)
admcId = lens _admcId (\s a -> s {_admcId = a})

-- | The date and time when the ADM channel was enabled.
admcCreationDate :: Lens' ADMChannelResponse (Maybe Text)
admcCreationDate = lens _admcCreationDate (\s a -> s {_admcCreationDate = a})

-- | The user who last modified the ADM channel.
admcLastModifiedBy :: Lens' ADMChannelResponse (Maybe Text)
admcLastModifiedBy = lens _admcLastModifiedBy (\s a -> s {_admcLastModifiedBy = a})

-- | (Not used) This property is retained only for backward compatibility.
admcHasCredential :: Lens' ADMChannelResponse (Maybe Bool)
admcHasCredential = lens _admcHasCredential (\s a -> s {_admcHasCredential = a})

-- | The type of messaging or notification platform for the channel. For the ADM channel, this value is ADM.
admcPlatform :: Lens' ADMChannelResponse Text
admcPlatform = lens _admcPlatform (\s a -> s {_admcPlatform = a})

instance FromJSON ADMChannelResponse where
  parseJSON =
    withObject
      "ADMChannelResponse"
      ( \x ->
          ADMChannelResponse'
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

instance Hashable ADMChannelResponse

instance NFData ADMChannelResponse
