{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EmailTemplateResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EmailTemplateResponse where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.TemplateType
import Network.AWS.Prelude

-- | Provides information about the content and settings for a message template that can be used in messages that are sent through the email channel.
--
--
--
-- /See:/ 'emailTemplateResponse' smart constructor.
data EmailTemplateResponse = EmailTemplateResponse'
  { _etSubject ::
      !(Maybe Text),
    _etTextPart :: !(Maybe Text),
    _etARN :: !(Maybe Text),
    _etTemplateDescription :: !(Maybe Text),
    _etDefaultSubstitutions :: !(Maybe Text),
    _etVersion :: !(Maybe Text),
    _etHTMLPart :: !(Maybe Text),
    _etRecommenderId :: !(Maybe Text),
    _etTags :: !(Maybe (Map Text (Text))),
    _etLastModifiedDate :: !Text,
    _etCreationDate :: !Text,
    _etTemplateName :: !Text,
    _etTemplateType :: !TemplateType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EmailTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etSubject' - The subject line, or title, that's used in email messages that are based on the message template.
--
-- * 'etTextPart' - The message body, in plain text format, that's used in email messages that are based on the message template.
--
-- * 'etARN' - The Amazon Resource Name (ARN) of the message template.
--
-- * 'etTemplateDescription' - The custom description of the message template.
--
-- * 'etDefaultSubstitutions' - The JSON object that specifies the default values that are used for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable.
--
-- * 'etVersion' - The unique identifier, as an integer, for the active version of the message template, or the version of the template that you specified by using the version parameter in your request.
--
-- * 'etHTMLPart' - The message body, in HTML format, that's used in email messages that are based on the message template.
--
-- * 'etRecommenderId' - The unique identifier for the recommender model that's used by the message template.
--
-- * 'etTags' - A string-to-string map of key-value pairs that identifies the tags that are associated with the message template. Each tag consists of a required tag key and an associated tag value.
--
-- * 'etLastModifiedDate' - The date, in ISO 8601 format, when the message template was last modified.
--
-- * 'etCreationDate' - The date, in ISO 8601 format, when the message template was created.
--
-- * 'etTemplateName' - The name of the message template.
--
-- * 'etTemplateType' - The type of channel that the message template is designed for. For an email template, this value is EMAIL.
emailTemplateResponse ::
  -- | 'etLastModifiedDate'
  Text ->
  -- | 'etCreationDate'
  Text ->
  -- | 'etTemplateName'
  Text ->
  -- | 'etTemplateType'
  TemplateType ->
  EmailTemplateResponse
emailTemplateResponse
  pLastModifiedDate_
  pCreationDate_
  pTemplateName_
  pTemplateType_ =
    EmailTemplateResponse'
      { _etSubject = Nothing,
        _etTextPart = Nothing,
        _etARN = Nothing,
        _etTemplateDescription = Nothing,
        _etDefaultSubstitutions = Nothing,
        _etVersion = Nothing,
        _etHTMLPart = Nothing,
        _etRecommenderId = Nothing,
        _etTags = Nothing,
        _etLastModifiedDate = pLastModifiedDate_,
        _etCreationDate = pCreationDate_,
        _etTemplateName = pTemplateName_,
        _etTemplateType = pTemplateType_
      }

-- | The subject line, or title, that's used in email messages that are based on the message template.
etSubject :: Lens' EmailTemplateResponse (Maybe Text)
etSubject = lens _etSubject (\s a -> s {_etSubject = a})

-- | The message body, in plain text format, that's used in email messages that are based on the message template.
etTextPart :: Lens' EmailTemplateResponse (Maybe Text)
etTextPart = lens _etTextPart (\s a -> s {_etTextPart = a})

-- | The Amazon Resource Name (ARN) of the message template.
etARN :: Lens' EmailTemplateResponse (Maybe Text)
etARN = lens _etARN (\s a -> s {_etARN = a})

-- | The custom description of the message template.
etTemplateDescription :: Lens' EmailTemplateResponse (Maybe Text)
etTemplateDescription = lens _etTemplateDescription (\s a -> s {_etTemplateDescription = a})

-- | The JSON object that specifies the default values that are used for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable.
etDefaultSubstitutions :: Lens' EmailTemplateResponse (Maybe Text)
etDefaultSubstitutions = lens _etDefaultSubstitutions (\s a -> s {_etDefaultSubstitutions = a})

-- | The unique identifier, as an integer, for the active version of the message template, or the version of the template that you specified by using the version parameter in your request.
etVersion :: Lens' EmailTemplateResponse (Maybe Text)
etVersion = lens _etVersion (\s a -> s {_etVersion = a})

-- | The message body, in HTML format, that's used in email messages that are based on the message template.
etHTMLPart :: Lens' EmailTemplateResponse (Maybe Text)
etHTMLPart = lens _etHTMLPart (\s a -> s {_etHTMLPart = a})

-- | The unique identifier for the recommender model that's used by the message template.
etRecommenderId :: Lens' EmailTemplateResponse (Maybe Text)
etRecommenderId = lens _etRecommenderId (\s a -> s {_etRecommenderId = a})

-- | A string-to-string map of key-value pairs that identifies the tags that are associated with the message template. Each tag consists of a required tag key and an associated tag value.
etTags :: Lens' EmailTemplateResponse (HashMap Text (Text))
etTags = lens _etTags (\s a -> s {_etTags = a}) . _Default . _Map

-- | The date, in ISO 8601 format, when the message template was last modified.
etLastModifiedDate :: Lens' EmailTemplateResponse Text
etLastModifiedDate = lens _etLastModifiedDate (\s a -> s {_etLastModifiedDate = a})

-- | The date, in ISO 8601 format, when the message template was created.
etCreationDate :: Lens' EmailTemplateResponse Text
etCreationDate = lens _etCreationDate (\s a -> s {_etCreationDate = a})

-- | The name of the message template.
etTemplateName :: Lens' EmailTemplateResponse Text
etTemplateName = lens _etTemplateName (\s a -> s {_etTemplateName = a})

-- | The type of channel that the message template is designed for. For an email template, this value is EMAIL.
etTemplateType :: Lens' EmailTemplateResponse TemplateType
etTemplateType = lens _etTemplateType (\s a -> s {_etTemplateType = a})

instance FromJSON EmailTemplateResponse where
  parseJSON =
    withObject
      "EmailTemplateResponse"
      ( \x ->
          EmailTemplateResponse'
            <$> (x .:? "Subject")
            <*> (x .:? "TextPart")
            <*> (x .:? "Arn")
            <*> (x .:? "TemplateDescription")
            <*> (x .:? "DefaultSubstitutions")
            <*> (x .:? "Version")
            <*> (x .:? "HtmlPart")
            <*> (x .:? "RecommenderId")
            <*> (x .:? "tags" .!= mempty)
            <*> (x .: "LastModifiedDate")
            <*> (x .: "CreationDate")
            <*> (x .: "TemplateName")
            <*> (x .: "TemplateType")
      )

instance Hashable EmailTemplateResponse

instance NFData EmailTemplateResponse
