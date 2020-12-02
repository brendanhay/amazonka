{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EmailMessageActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EmailMessageActivity where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.JourneyEmailMessage
import Network.AWS.Prelude

-- | Specifies the settings for an email activity in a journey. This type of activity sends an email message to participants.
--
--
--
-- /See:/ 'emailMessageActivity' smart constructor.
data EmailMessageActivity = EmailMessageActivity'
  { _emaTemplateName ::
      !(Maybe Text),
    _emaTemplateVersion :: !(Maybe Text),
    _emaNextActivity :: !(Maybe Text),
    _emaMessageConfig :: !(Maybe JourneyEmailMessage)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EmailMessageActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'emaTemplateName' - The name of the email message template to use for the message. If specified, this value must match the name of an existing message template.
--
-- * 'emaTemplateVersion' - The unique identifier for the version of the email template to use for the message. If specified, this value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource. If you don't specify a value for this property, Amazon Pinpoint uses the /active version/ of the template. The /active version/ is typically the version of a template that's been most recently reviewed and approved for use, depending on your workflow. It isn't necessarily the latest version of a template.
--
-- * 'emaNextActivity' - The unique identifier for the next activity to perform, after the message is sent.
--
-- * 'emaMessageConfig' - Specifies the sender address for an email message that's sent to participants in the journey.
emailMessageActivity ::
  EmailMessageActivity
emailMessageActivity =
  EmailMessageActivity'
    { _emaTemplateName = Nothing,
      _emaTemplateVersion = Nothing,
      _emaNextActivity = Nothing,
      _emaMessageConfig = Nothing
    }

-- | The name of the email message template to use for the message. If specified, this value must match the name of an existing message template.
emaTemplateName :: Lens' EmailMessageActivity (Maybe Text)
emaTemplateName = lens _emaTemplateName (\s a -> s {_emaTemplateName = a})

-- | The unique identifier for the version of the email template to use for the message. If specified, this value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource. If you don't specify a value for this property, Amazon Pinpoint uses the /active version/ of the template. The /active version/ is typically the version of a template that's been most recently reviewed and approved for use, depending on your workflow. It isn't necessarily the latest version of a template.
emaTemplateVersion :: Lens' EmailMessageActivity (Maybe Text)
emaTemplateVersion = lens _emaTemplateVersion (\s a -> s {_emaTemplateVersion = a})

-- | The unique identifier for the next activity to perform, after the message is sent.
emaNextActivity :: Lens' EmailMessageActivity (Maybe Text)
emaNextActivity = lens _emaNextActivity (\s a -> s {_emaNextActivity = a})

-- | Specifies the sender address for an email message that's sent to participants in the journey.
emaMessageConfig :: Lens' EmailMessageActivity (Maybe JourneyEmailMessage)
emaMessageConfig = lens _emaMessageConfig (\s a -> s {_emaMessageConfig = a})

instance FromJSON EmailMessageActivity where
  parseJSON =
    withObject
      "EmailMessageActivity"
      ( \x ->
          EmailMessageActivity'
            <$> (x .:? "TemplateName")
            <*> (x .:? "TemplateVersion")
            <*> (x .:? "NextActivity")
            <*> (x .:? "MessageConfig")
      )

instance Hashable EmailMessageActivity

instance NFData EmailMessageActivity

instance ToJSON EmailMessageActivity where
  toJSON EmailMessageActivity' {..} =
    object
      ( catMaybes
          [ ("TemplateName" .=) <$> _emaTemplateName,
            ("TemplateVersion" .=) <$> _emaTemplateVersion,
            ("NextActivity" .=) <$> _emaNextActivity,
            ("MessageConfig" .=) <$> _emaMessageConfig
          ]
      )
