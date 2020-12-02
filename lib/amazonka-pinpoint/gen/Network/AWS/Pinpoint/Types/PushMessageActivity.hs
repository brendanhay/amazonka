{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.PushMessageActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.PushMessageActivity where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.JourneyPushMessage
import Network.AWS.Prelude

-- | Specifies the settings for a push notification activity in a journey. This type of activity sends a push notification to participants.
--
--
--
-- /See:/ 'pushMessageActivity' smart constructor.
data PushMessageActivity = PushMessageActivity'
  { _pmaTemplateName ::
      !(Maybe Text),
    _pmaTemplateVersion :: !(Maybe Text),
    _pmaNextActivity :: !(Maybe Text),
    _pmaMessageConfig :: !(Maybe JourneyPushMessage)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PushMessageActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmaTemplateName' - The name of the push notification template to use for the message. If specified, this value must match the name of an existing message template.
--
-- * 'pmaTemplateVersion' - The unique identifier for the version of the push notification template to use for the message. If specified, this value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource. If you don't specify a value for this property, Amazon Pinpoint uses the /active version/ of the template. The /active version/ is typically the version of a template that's been most recently reviewed and approved for use, depending on your workflow. It isn't necessarily the latest version of a template.
--
-- * 'pmaNextActivity' - The unique identifier for the next activity to perform, after the message is sent.
--
-- * 'pmaMessageConfig' - Specifies the time to live (TTL) value for push notifications that are sent to participants in a journey.
pushMessageActivity ::
  PushMessageActivity
pushMessageActivity =
  PushMessageActivity'
    { _pmaTemplateName = Nothing,
      _pmaTemplateVersion = Nothing,
      _pmaNextActivity = Nothing,
      _pmaMessageConfig = Nothing
    }

-- | The name of the push notification template to use for the message. If specified, this value must match the name of an existing message template.
pmaTemplateName :: Lens' PushMessageActivity (Maybe Text)
pmaTemplateName = lens _pmaTemplateName (\s a -> s {_pmaTemplateName = a})

-- | The unique identifier for the version of the push notification template to use for the message. If specified, this value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource. If you don't specify a value for this property, Amazon Pinpoint uses the /active version/ of the template. The /active version/ is typically the version of a template that's been most recently reviewed and approved for use, depending on your workflow. It isn't necessarily the latest version of a template.
pmaTemplateVersion :: Lens' PushMessageActivity (Maybe Text)
pmaTemplateVersion = lens _pmaTemplateVersion (\s a -> s {_pmaTemplateVersion = a})

-- | The unique identifier for the next activity to perform, after the message is sent.
pmaNextActivity :: Lens' PushMessageActivity (Maybe Text)
pmaNextActivity = lens _pmaNextActivity (\s a -> s {_pmaNextActivity = a})

-- | Specifies the time to live (TTL) value for push notifications that are sent to participants in a journey.
pmaMessageConfig :: Lens' PushMessageActivity (Maybe JourneyPushMessage)
pmaMessageConfig = lens _pmaMessageConfig (\s a -> s {_pmaMessageConfig = a})

instance FromJSON PushMessageActivity where
  parseJSON =
    withObject
      "PushMessageActivity"
      ( \x ->
          PushMessageActivity'
            <$> (x .:? "TemplateName")
            <*> (x .:? "TemplateVersion")
            <*> (x .:? "NextActivity")
            <*> (x .:? "MessageConfig")
      )

instance Hashable PushMessageActivity

instance NFData PushMessageActivity

instance ToJSON PushMessageActivity where
  toJSON PushMessageActivity' {..} =
    object
      ( catMaybes
          [ ("TemplateName" .=) <$> _pmaTemplateName,
            ("TemplateVersion" .=) <$> _pmaTemplateVersion,
            ("NextActivity" .=) <$> _pmaNextActivity,
            ("MessageConfig" .=) <$> _pmaMessageConfig
          ]
      )
