{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.TemplateConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.TemplateConfiguration where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.Template
import Network.AWS.Prelude

-- | Specifies the message template to use for the message, for each type of channel.
--
--
--
-- /See:/ 'templateConfiguration' smart constructor.
data TemplateConfiguration = TemplateConfiguration'
  { _tcSMSTemplate ::
      !(Maybe Template),
    _tcVoiceTemplate :: !(Maybe Template),
    _tcPushTemplate :: !(Maybe Template),
    _tcEmailTemplate :: !(Maybe Template)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TemplateConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcSMSTemplate' - The SMS template to use for the message.
--
-- * 'tcVoiceTemplate' - The voice template to use for the message. This object isn't supported for campaigns.
--
-- * 'tcPushTemplate' - The push notification template to use for the message.
--
-- * 'tcEmailTemplate' - The email template to use for the message.
templateConfiguration ::
  TemplateConfiguration
templateConfiguration =
  TemplateConfiguration'
    { _tcSMSTemplate = Nothing,
      _tcVoiceTemplate = Nothing,
      _tcPushTemplate = Nothing,
      _tcEmailTemplate = Nothing
    }

-- | The SMS template to use for the message.
tcSMSTemplate :: Lens' TemplateConfiguration (Maybe Template)
tcSMSTemplate = lens _tcSMSTemplate (\s a -> s {_tcSMSTemplate = a})

-- | The voice template to use for the message. This object isn't supported for campaigns.
tcVoiceTemplate :: Lens' TemplateConfiguration (Maybe Template)
tcVoiceTemplate = lens _tcVoiceTemplate (\s a -> s {_tcVoiceTemplate = a})

-- | The push notification template to use for the message.
tcPushTemplate :: Lens' TemplateConfiguration (Maybe Template)
tcPushTemplate = lens _tcPushTemplate (\s a -> s {_tcPushTemplate = a})

-- | The email template to use for the message.
tcEmailTemplate :: Lens' TemplateConfiguration (Maybe Template)
tcEmailTemplate = lens _tcEmailTemplate (\s a -> s {_tcEmailTemplate = a})

instance FromJSON TemplateConfiguration where
  parseJSON =
    withObject
      "TemplateConfiguration"
      ( \x ->
          TemplateConfiguration'
            <$> (x .:? "SMSTemplate")
            <*> (x .:? "VoiceTemplate")
            <*> (x .:? "PushTemplate")
            <*> (x .:? "EmailTemplate")
      )

instance Hashable TemplateConfiguration

instance NFData TemplateConfiguration

instance ToJSON TemplateConfiguration where
  toJSON TemplateConfiguration' {..} =
    object
      ( catMaybes
          [ ("SMSTemplate" .=) <$> _tcSMSTemplate,
            ("VoiceTemplate" .=) <$> _tcVoiceTemplate,
            ("PushTemplate" .=) <$> _tcPushTemplate,
            ("EmailTemplate" .=) <$> _tcEmailTemplate
          ]
      )
