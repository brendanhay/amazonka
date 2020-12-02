{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.VoiceTemplateRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.VoiceTemplateRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the content and settings for a message template that can be used in messages that are sent through the voice channel.
--
--
--
-- /See:/ 'voiceTemplateRequest' smart constructor.
data VoiceTemplateRequest = VoiceTemplateRequest'
  { _vtrLanguageCode ::
      !(Maybe Text),
    _vtrBody :: !(Maybe Text),
    _vtrTemplateDescription :: !(Maybe Text),
    _vtrDefaultSubstitutions :: !(Maybe Text),
    _vtrVoiceId :: !(Maybe Text),
    _vtrTags :: !(Maybe (Map Text (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VoiceTemplateRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vtrLanguageCode' - The code for the language to use when synthesizing the text of the script in messages that are based on the message template. For a list of supported languages and the code for each one, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
--
-- * 'vtrBody' - The text of the script to use in messages that are based on the message template, in plain text format.
--
-- * 'vtrTemplateDescription' - A custom description of the message template.
--
-- * 'vtrDefaultSubstitutions' - A JSON object that specifies the default values to use for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable. When you create a message that's based on the template, you can override these defaults with message-specific and address-specific variables and values.
--
-- * 'vtrVoiceId' - The name of the voice to use when delivering messages that are based on the message template. For a list of supported voices, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
--
-- * 'vtrTags' - A string-to-string map of key-value pairs that defines the tags to associate with the message template. Each tag consists of a required tag key and an associated tag value.
voiceTemplateRequest ::
  VoiceTemplateRequest
voiceTemplateRequest =
  VoiceTemplateRequest'
    { _vtrLanguageCode = Nothing,
      _vtrBody = Nothing,
      _vtrTemplateDescription = Nothing,
      _vtrDefaultSubstitutions = Nothing,
      _vtrVoiceId = Nothing,
      _vtrTags = Nothing
    }

-- | The code for the language to use when synthesizing the text of the script in messages that are based on the message template. For a list of supported languages and the code for each one, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
vtrLanguageCode :: Lens' VoiceTemplateRequest (Maybe Text)
vtrLanguageCode = lens _vtrLanguageCode (\s a -> s {_vtrLanguageCode = a})

-- | The text of the script to use in messages that are based on the message template, in plain text format.
vtrBody :: Lens' VoiceTemplateRequest (Maybe Text)
vtrBody = lens _vtrBody (\s a -> s {_vtrBody = a})

-- | A custom description of the message template.
vtrTemplateDescription :: Lens' VoiceTemplateRequest (Maybe Text)
vtrTemplateDescription = lens _vtrTemplateDescription (\s a -> s {_vtrTemplateDescription = a})

-- | A JSON object that specifies the default values to use for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable. When you create a message that's based on the template, you can override these defaults with message-specific and address-specific variables and values.
vtrDefaultSubstitutions :: Lens' VoiceTemplateRequest (Maybe Text)
vtrDefaultSubstitutions = lens _vtrDefaultSubstitutions (\s a -> s {_vtrDefaultSubstitutions = a})

-- | The name of the voice to use when delivering messages that are based on the message template. For a list of supported voices, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
vtrVoiceId :: Lens' VoiceTemplateRequest (Maybe Text)
vtrVoiceId = lens _vtrVoiceId (\s a -> s {_vtrVoiceId = a})

-- | A string-to-string map of key-value pairs that defines the tags to associate with the message template. Each tag consists of a required tag key and an associated tag value.
vtrTags :: Lens' VoiceTemplateRequest (HashMap Text (Text))
vtrTags = lens _vtrTags (\s a -> s {_vtrTags = a}) . _Default . _Map

instance Hashable VoiceTemplateRequest

instance NFData VoiceTemplateRequest

instance ToJSON VoiceTemplateRequest where
  toJSON VoiceTemplateRequest' {..} =
    object
      ( catMaybes
          [ ("LanguageCode" .=) <$> _vtrLanguageCode,
            ("Body" .=) <$> _vtrBody,
            ("TemplateDescription" .=) <$> _vtrTemplateDescription,
            ("DefaultSubstitutions" .=) <$> _vtrDefaultSubstitutions,
            ("VoiceId" .=) <$> _vtrVoiceId,
            ("tags" .=) <$> _vtrTags
          ]
      )
