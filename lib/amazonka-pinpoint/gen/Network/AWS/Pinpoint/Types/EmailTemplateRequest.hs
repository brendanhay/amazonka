{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EmailTemplateRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EmailTemplateRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the content and settings for a message template that can be used in messages that are sent through the email channel.
--
--
--
-- /See:/ 'emailTemplateRequest' smart constructor.
data EmailTemplateRequest = EmailTemplateRequest'
  { _etrSubject ::
      !(Maybe Text),
    _etrTextPart :: !(Maybe Text),
    _etrTemplateDescription :: !(Maybe Text),
    _etrDefaultSubstitutions :: !(Maybe Text),
    _etrHTMLPart :: !(Maybe Text),
    _etrRecommenderId :: !(Maybe Text),
    _etrTags :: !(Maybe (Map Text (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EmailTemplateRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etrSubject' - The subject line, or title, to use in email messages that are based on the message template.
--
-- * 'etrTextPart' - The message body, in plain text format, to use in email messages that are based on the message template. We recommend using plain text format for email clients that don't render HTML content and clients that are connected to high-latency networks, such as mobile devices.
--
-- * 'etrTemplateDescription' - A custom description of the message template.
--
-- * 'etrDefaultSubstitutions' - A JSON object that specifies the default values to use for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable. When you create a message that's based on the template, you can override these defaults with message-specific and address-specific variables and values.
--
-- * 'etrHTMLPart' - The message body, in HTML format, to use in email messages that are based on the message template. We recommend using HTML format for email clients that render HTML content. You can include links, formatted text, and more in an HTML message.
--
-- * 'etrRecommenderId' - The unique identifier for the recommender model to use for the message template. Amazon Pinpoint uses this value to determine how to retrieve and process data from a recommender model when it sends messages that use the template, if the template contains message variables for recommendation data.
--
-- * 'etrTags' - A string-to-string map of key-value pairs that defines the tags to associate with the message template. Each tag consists of a required tag key and an associated tag value.
emailTemplateRequest ::
  EmailTemplateRequest
emailTemplateRequest =
  EmailTemplateRequest'
    { _etrSubject = Nothing,
      _etrTextPart = Nothing,
      _etrTemplateDescription = Nothing,
      _etrDefaultSubstitutions = Nothing,
      _etrHTMLPart = Nothing,
      _etrRecommenderId = Nothing,
      _etrTags = Nothing
    }

-- | The subject line, or title, to use in email messages that are based on the message template.
etrSubject :: Lens' EmailTemplateRequest (Maybe Text)
etrSubject = lens _etrSubject (\s a -> s {_etrSubject = a})

-- | The message body, in plain text format, to use in email messages that are based on the message template. We recommend using plain text format for email clients that don't render HTML content and clients that are connected to high-latency networks, such as mobile devices.
etrTextPart :: Lens' EmailTemplateRequest (Maybe Text)
etrTextPart = lens _etrTextPart (\s a -> s {_etrTextPart = a})

-- | A custom description of the message template.
etrTemplateDescription :: Lens' EmailTemplateRequest (Maybe Text)
etrTemplateDescription = lens _etrTemplateDescription (\s a -> s {_etrTemplateDescription = a})

-- | A JSON object that specifies the default values to use for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable. When you create a message that's based on the template, you can override these defaults with message-specific and address-specific variables and values.
etrDefaultSubstitutions :: Lens' EmailTemplateRequest (Maybe Text)
etrDefaultSubstitutions = lens _etrDefaultSubstitutions (\s a -> s {_etrDefaultSubstitutions = a})

-- | The message body, in HTML format, to use in email messages that are based on the message template. We recommend using HTML format for email clients that render HTML content. You can include links, formatted text, and more in an HTML message.
etrHTMLPart :: Lens' EmailTemplateRequest (Maybe Text)
etrHTMLPart = lens _etrHTMLPart (\s a -> s {_etrHTMLPart = a})

-- | The unique identifier for the recommender model to use for the message template. Amazon Pinpoint uses this value to determine how to retrieve and process data from a recommender model when it sends messages that use the template, if the template contains message variables for recommendation data.
etrRecommenderId :: Lens' EmailTemplateRequest (Maybe Text)
etrRecommenderId = lens _etrRecommenderId (\s a -> s {_etrRecommenderId = a})

-- | A string-to-string map of key-value pairs that defines the tags to associate with the message template. Each tag consists of a required tag key and an associated tag value.
etrTags :: Lens' EmailTemplateRequest (HashMap Text (Text))
etrTags = lens _etrTags (\s a -> s {_etrTags = a}) . _Default . _Map

instance Hashable EmailTemplateRequest

instance NFData EmailTemplateRequest

instance ToJSON EmailTemplateRequest where
  toJSON EmailTemplateRequest' {..} =
    object
      ( catMaybes
          [ ("Subject" .=) <$> _etrSubject,
            ("TextPart" .=) <$> _etrTextPart,
            ("TemplateDescription" .=) <$> _etrTemplateDescription,
            ("DefaultSubstitutions" .=) <$> _etrDefaultSubstitutions,
            ("HtmlPart" .=) <$> _etrHTMLPart,
            ("RecommenderId" .=) <$> _etrRecommenderId,
            ("tags" .=) <$> _etrTags
          ]
      )
