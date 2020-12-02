{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.VoiceTemplateResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.VoiceTemplateResponse where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.TemplateType
import Network.AWS.Prelude

-- | Provides information about the content and settings for a message template that can be used in messages that are sent through the voice channel.
--
--
--
-- /See:/ 'voiceTemplateResponse' smart constructor.
data VoiceTemplateResponse = VoiceTemplateResponse'
  { _vtLanguageCode ::
      !(Maybe Text),
    _vtARN :: !(Maybe Text),
    _vtBody :: !(Maybe Text),
    _vtTemplateDescription :: !(Maybe Text),
    _vtDefaultSubstitutions :: !(Maybe Text),
    _vtVersion :: !(Maybe Text),
    _vtVoiceId :: !(Maybe Text),
    _vtTags :: !(Maybe (Map Text (Text))),
    _vtLastModifiedDate :: !Text,
    _vtCreationDate :: !Text,
    _vtTemplateName :: !Text,
    _vtTemplateType :: !TemplateType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VoiceTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vtLanguageCode' - The code for the language that's used when synthesizing the text of the script in messages that are based on the message template. For a list of supported languages and the code for each one, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
--
-- * 'vtARN' - The Amazon Resource Name (ARN) of the message template.
--
-- * 'vtBody' - The text of the script that's used in messages that are based on the message template, in plain text format.
--
-- * 'vtTemplateDescription' - The custom description of the message template.
--
-- * 'vtDefaultSubstitutions' - The JSON object that specifies the default values that are used for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable.
--
-- * 'vtVersion' - The unique identifier, as an integer, for the active version of the message template, or the version of the template that you specified by using the version parameter in your request.
--
-- * 'vtVoiceId' - The name of the voice that's used when delivering messages that are based on the message template. For a list of supported voices, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
--
-- * 'vtTags' - A string-to-string map of key-value pairs that identifies the tags that are associated with the message template. Each tag consists of a required tag key and an associated tag value.
--
-- * 'vtLastModifiedDate' - The date, in ISO 8601 format, when the message template was last modified.
--
-- * 'vtCreationDate' - The date, in ISO 8601 format, when the message template was created.
--
-- * 'vtTemplateName' - The name of the message template.
--
-- * 'vtTemplateType' - The type of channel that the message template is designed for. For a voice template, this value is VOICE.
voiceTemplateResponse ::
  -- | 'vtLastModifiedDate'
  Text ->
  -- | 'vtCreationDate'
  Text ->
  -- | 'vtTemplateName'
  Text ->
  -- | 'vtTemplateType'
  TemplateType ->
  VoiceTemplateResponse
voiceTemplateResponse
  pLastModifiedDate_
  pCreationDate_
  pTemplateName_
  pTemplateType_ =
    VoiceTemplateResponse'
      { _vtLanguageCode = Nothing,
        _vtARN = Nothing,
        _vtBody = Nothing,
        _vtTemplateDescription = Nothing,
        _vtDefaultSubstitutions = Nothing,
        _vtVersion = Nothing,
        _vtVoiceId = Nothing,
        _vtTags = Nothing,
        _vtLastModifiedDate = pLastModifiedDate_,
        _vtCreationDate = pCreationDate_,
        _vtTemplateName = pTemplateName_,
        _vtTemplateType = pTemplateType_
      }

-- | The code for the language that's used when synthesizing the text of the script in messages that are based on the message template. For a list of supported languages and the code for each one, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
vtLanguageCode :: Lens' VoiceTemplateResponse (Maybe Text)
vtLanguageCode = lens _vtLanguageCode (\s a -> s {_vtLanguageCode = a})

-- | The Amazon Resource Name (ARN) of the message template.
vtARN :: Lens' VoiceTemplateResponse (Maybe Text)
vtARN = lens _vtARN (\s a -> s {_vtARN = a})

-- | The text of the script that's used in messages that are based on the message template, in plain text format.
vtBody :: Lens' VoiceTemplateResponse (Maybe Text)
vtBody = lens _vtBody (\s a -> s {_vtBody = a})

-- | The custom description of the message template.
vtTemplateDescription :: Lens' VoiceTemplateResponse (Maybe Text)
vtTemplateDescription = lens _vtTemplateDescription (\s a -> s {_vtTemplateDescription = a})

-- | The JSON object that specifies the default values that are used for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable.
vtDefaultSubstitutions :: Lens' VoiceTemplateResponse (Maybe Text)
vtDefaultSubstitutions = lens _vtDefaultSubstitutions (\s a -> s {_vtDefaultSubstitutions = a})

-- | The unique identifier, as an integer, for the active version of the message template, or the version of the template that you specified by using the version parameter in your request.
vtVersion :: Lens' VoiceTemplateResponse (Maybe Text)
vtVersion = lens _vtVersion (\s a -> s {_vtVersion = a})

-- | The name of the voice that's used when delivering messages that are based on the message template. For a list of supported voices, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
vtVoiceId :: Lens' VoiceTemplateResponse (Maybe Text)
vtVoiceId = lens _vtVoiceId (\s a -> s {_vtVoiceId = a})

-- | A string-to-string map of key-value pairs that identifies the tags that are associated with the message template. Each tag consists of a required tag key and an associated tag value.
vtTags :: Lens' VoiceTemplateResponse (HashMap Text (Text))
vtTags = lens _vtTags (\s a -> s {_vtTags = a}) . _Default . _Map

-- | The date, in ISO 8601 format, when the message template was last modified.
vtLastModifiedDate :: Lens' VoiceTemplateResponse Text
vtLastModifiedDate = lens _vtLastModifiedDate (\s a -> s {_vtLastModifiedDate = a})

-- | The date, in ISO 8601 format, when the message template was created.
vtCreationDate :: Lens' VoiceTemplateResponse Text
vtCreationDate = lens _vtCreationDate (\s a -> s {_vtCreationDate = a})

-- | The name of the message template.
vtTemplateName :: Lens' VoiceTemplateResponse Text
vtTemplateName = lens _vtTemplateName (\s a -> s {_vtTemplateName = a})

-- | The type of channel that the message template is designed for. For a voice template, this value is VOICE.
vtTemplateType :: Lens' VoiceTemplateResponse TemplateType
vtTemplateType = lens _vtTemplateType (\s a -> s {_vtTemplateType = a})

instance FromJSON VoiceTemplateResponse where
  parseJSON =
    withObject
      "VoiceTemplateResponse"
      ( \x ->
          VoiceTemplateResponse'
            <$> (x .:? "LanguageCode")
            <*> (x .:? "Arn")
            <*> (x .:? "Body")
            <*> (x .:? "TemplateDescription")
            <*> (x .:? "DefaultSubstitutions")
            <*> (x .:? "Version")
            <*> (x .:? "VoiceId")
            <*> (x .:? "tags" .!= mempty)
            <*> (x .: "LastModifiedDate")
            <*> (x .: "CreationDate")
            <*> (x .: "TemplateName")
            <*> (x .: "TemplateType")
      )

instance Hashable VoiceTemplateResponse

instance NFData VoiceTemplateResponse
