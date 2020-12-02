{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SMSMessageActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SMSMessageActivity where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.JourneySMSMessage
import Network.AWS.Prelude

-- | Specifies the settings for an SMS activity in a journey. This type of activity sends a text message to participants.
--
--
--
-- /See:/ 'sMSMessageActivity' smart constructor.
data SMSMessageActivity = SMSMessageActivity'
  { _smsmaTemplateName ::
      !(Maybe Text),
    _smsmaTemplateVersion :: !(Maybe Text),
    _smsmaNextActivity :: !(Maybe Text),
    _smsmaMessageConfig :: !(Maybe JourneySMSMessage)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SMSMessageActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smsmaTemplateName' - The name of the SMS message template to use for the message. If specified, this value must match the name of an existing message template.
--
-- * 'smsmaTemplateVersion' - The unique identifier for the version of the SMS template to use for the message. If specified, this value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource. If you don't specify a value for this property, Amazon Pinpoint uses the /active version/ of the template. The /active version/ is typically the version of a template that's been most recently reviewed and approved for use, depending on your workflow. It isn't necessarily the latest version of a template.
--
-- * 'smsmaNextActivity' - The unique identifier for the next activity to perform, after the message is sent.
--
-- * 'smsmaMessageConfig' - Specifies the sender ID and message type for an SMS message that's sent to participants in a journey.
sMSMessageActivity ::
  SMSMessageActivity
sMSMessageActivity =
  SMSMessageActivity'
    { _smsmaTemplateName = Nothing,
      _smsmaTemplateVersion = Nothing,
      _smsmaNextActivity = Nothing,
      _smsmaMessageConfig = Nothing
    }

-- | The name of the SMS message template to use for the message. If specified, this value must match the name of an existing message template.
smsmaTemplateName :: Lens' SMSMessageActivity (Maybe Text)
smsmaTemplateName = lens _smsmaTemplateName (\s a -> s {_smsmaTemplateName = a})

-- | The unique identifier for the version of the SMS template to use for the message. If specified, this value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource. If you don't specify a value for this property, Amazon Pinpoint uses the /active version/ of the template. The /active version/ is typically the version of a template that's been most recently reviewed and approved for use, depending on your workflow. It isn't necessarily the latest version of a template.
smsmaTemplateVersion :: Lens' SMSMessageActivity (Maybe Text)
smsmaTemplateVersion = lens _smsmaTemplateVersion (\s a -> s {_smsmaTemplateVersion = a})

-- | The unique identifier for the next activity to perform, after the message is sent.
smsmaNextActivity :: Lens' SMSMessageActivity (Maybe Text)
smsmaNextActivity = lens _smsmaNextActivity (\s a -> s {_smsmaNextActivity = a})

-- | Specifies the sender ID and message type for an SMS message that's sent to participants in a journey.
smsmaMessageConfig :: Lens' SMSMessageActivity (Maybe JourneySMSMessage)
smsmaMessageConfig = lens _smsmaMessageConfig (\s a -> s {_smsmaMessageConfig = a})

instance FromJSON SMSMessageActivity where
  parseJSON =
    withObject
      "SMSMessageActivity"
      ( \x ->
          SMSMessageActivity'
            <$> (x .:? "TemplateName")
            <*> (x .:? "TemplateVersion")
            <*> (x .:? "NextActivity")
            <*> (x .:? "MessageConfig")
      )

instance Hashable SMSMessageActivity

instance NFData SMSMessageActivity

instance ToJSON SMSMessageActivity where
  toJSON SMSMessageActivity' {..} =
    object
      ( catMaybes
          [ ("TemplateName" .=) <$> _smsmaTemplateName,
            ("TemplateVersion" .=) <$> _smsmaTemplateVersion,
            ("NextActivity" .=) <$> _smsmaNextActivity,
            ("MessageConfig" .=) <$> _smsmaMessageConfig
          ]
      )
