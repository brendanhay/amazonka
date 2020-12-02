{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SMSChannelResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SMSChannelResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about the status and settings of the SMS channel for an application.
--
--
--
-- /See:/ 'sMSChannelResponse' smart constructor.
data SMSChannelResponse = SMSChannelResponse'
  { _smscShortCode ::
      !(Maybe Text),
    _smscLastModifiedDate :: !(Maybe Text),
    _smscEnabled :: !(Maybe Bool),
    _smscSenderId :: !(Maybe Text),
    _smscTransactionalMessagesPerSecond :: !(Maybe Int),
    _smscPromotionalMessagesPerSecond :: !(Maybe Int),
    _smscIsArchived :: !(Maybe Bool),
    _smscApplicationId :: !(Maybe Text),
    _smscVersion :: !(Maybe Int),
    _smscId :: !(Maybe Text),
    _smscCreationDate :: !(Maybe Text),
    _smscLastModifiedBy :: !(Maybe Text),
    _smscHasCredential :: !(Maybe Bool),
    _smscPlatform :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SMSChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smscShortCode' - The registered short code to use when you send messages through the SMS channel.
--
-- * 'smscLastModifiedDate' - The date and time, in ISO 8601 format, when the SMS channel was last modified.
--
-- * 'smscEnabled' - Specifies whether the SMS channel is enabled for the application.
--
-- * 'smscSenderId' - The identity that displays on recipients' devices when they receive messages from the SMS channel.
--
-- * 'smscTransactionalMessagesPerSecond' - The maximum number of transactional messages that you can send through the SMS channel each second.
--
-- * 'smscPromotionalMessagesPerSecond' - The maximum number of promotional messages that you can send through the SMS channel each second.
--
-- * 'smscIsArchived' - Specifies whether the SMS channel is archived.
--
-- * 'smscApplicationId' - The unique identifier for the application that the SMS channel applies to.
--
-- * 'smscVersion' - The current version of the SMS channel.
--
-- * 'smscId' - (Deprecated) An identifier for the SMS channel. This property is retained only for backward compatibility.
--
-- * 'smscCreationDate' - The date and time, in ISO 8601 format, when the SMS channel was enabled.
--
-- * 'smscLastModifiedBy' - The user who last modified the SMS channel.
--
-- * 'smscHasCredential' - (Not used) This property is retained only for backward compatibility.
--
-- * 'smscPlatform' - The type of messaging or notification platform for the channel. For the SMS channel, this value is SMS.
sMSChannelResponse ::
  -- | 'smscPlatform'
  Text ->
  SMSChannelResponse
sMSChannelResponse pPlatform_ =
  SMSChannelResponse'
    { _smscShortCode = Nothing,
      _smscLastModifiedDate = Nothing,
      _smscEnabled = Nothing,
      _smscSenderId = Nothing,
      _smscTransactionalMessagesPerSecond = Nothing,
      _smscPromotionalMessagesPerSecond = Nothing,
      _smscIsArchived = Nothing,
      _smscApplicationId = Nothing,
      _smscVersion = Nothing,
      _smscId = Nothing,
      _smscCreationDate = Nothing,
      _smscLastModifiedBy = Nothing,
      _smscHasCredential = Nothing,
      _smscPlatform = pPlatform_
    }

-- | The registered short code to use when you send messages through the SMS channel.
smscShortCode :: Lens' SMSChannelResponse (Maybe Text)
smscShortCode = lens _smscShortCode (\s a -> s {_smscShortCode = a})

-- | The date and time, in ISO 8601 format, when the SMS channel was last modified.
smscLastModifiedDate :: Lens' SMSChannelResponse (Maybe Text)
smscLastModifiedDate = lens _smscLastModifiedDate (\s a -> s {_smscLastModifiedDate = a})

-- | Specifies whether the SMS channel is enabled for the application.
smscEnabled :: Lens' SMSChannelResponse (Maybe Bool)
smscEnabled = lens _smscEnabled (\s a -> s {_smscEnabled = a})

-- | The identity that displays on recipients' devices when they receive messages from the SMS channel.
smscSenderId :: Lens' SMSChannelResponse (Maybe Text)
smscSenderId = lens _smscSenderId (\s a -> s {_smscSenderId = a})

-- | The maximum number of transactional messages that you can send through the SMS channel each second.
smscTransactionalMessagesPerSecond :: Lens' SMSChannelResponse (Maybe Int)
smscTransactionalMessagesPerSecond = lens _smscTransactionalMessagesPerSecond (\s a -> s {_smscTransactionalMessagesPerSecond = a})

-- | The maximum number of promotional messages that you can send through the SMS channel each second.
smscPromotionalMessagesPerSecond :: Lens' SMSChannelResponse (Maybe Int)
smscPromotionalMessagesPerSecond = lens _smscPromotionalMessagesPerSecond (\s a -> s {_smscPromotionalMessagesPerSecond = a})

-- | Specifies whether the SMS channel is archived.
smscIsArchived :: Lens' SMSChannelResponse (Maybe Bool)
smscIsArchived = lens _smscIsArchived (\s a -> s {_smscIsArchived = a})

-- | The unique identifier for the application that the SMS channel applies to.
smscApplicationId :: Lens' SMSChannelResponse (Maybe Text)
smscApplicationId = lens _smscApplicationId (\s a -> s {_smscApplicationId = a})

-- | The current version of the SMS channel.
smscVersion :: Lens' SMSChannelResponse (Maybe Int)
smscVersion = lens _smscVersion (\s a -> s {_smscVersion = a})

-- | (Deprecated) An identifier for the SMS channel. This property is retained only for backward compatibility.
smscId :: Lens' SMSChannelResponse (Maybe Text)
smscId = lens _smscId (\s a -> s {_smscId = a})

-- | The date and time, in ISO 8601 format, when the SMS channel was enabled.
smscCreationDate :: Lens' SMSChannelResponse (Maybe Text)
smscCreationDate = lens _smscCreationDate (\s a -> s {_smscCreationDate = a})

-- | The user who last modified the SMS channel.
smscLastModifiedBy :: Lens' SMSChannelResponse (Maybe Text)
smscLastModifiedBy = lens _smscLastModifiedBy (\s a -> s {_smscLastModifiedBy = a})

-- | (Not used) This property is retained only for backward compatibility.
smscHasCredential :: Lens' SMSChannelResponse (Maybe Bool)
smscHasCredential = lens _smscHasCredential (\s a -> s {_smscHasCredential = a})

-- | The type of messaging or notification platform for the channel. For the SMS channel, this value is SMS.
smscPlatform :: Lens' SMSChannelResponse Text
smscPlatform = lens _smscPlatform (\s a -> s {_smscPlatform = a})

instance FromJSON SMSChannelResponse where
  parseJSON =
    withObject
      "SMSChannelResponse"
      ( \x ->
          SMSChannelResponse'
            <$> (x .:? "ShortCode")
            <*> (x .:? "LastModifiedDate")
            <*> (x .:? "Enabled")
            <*> (x .:? "SenderId")
            <*> (x .:? "TransactionalMessagesPerSecond")
            <*> (x .:? "PromotionalMessagesPerSecond")
            <*> (x .:? "IsArchived")
            <*> (x .:? "ApplicationId")
            <*> (x .:? "Version")
            <*> (x .:? "Id")
            <*> (x .:? "CreationDate")
            <*> (x .:? "LastModifiedBy")
            <*> (x .:? "HasCredential")
            <*> (x .: "Platform")
      )

instance Hashable SMSChannelResponse

instance NFData SMSChannelResponse
