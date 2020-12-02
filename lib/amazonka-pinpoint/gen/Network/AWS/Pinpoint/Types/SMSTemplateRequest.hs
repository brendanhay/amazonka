{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SMSTemplateRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SMSTemplateRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the content and settings for a message template that can be used in text messages that are sent through the SMS channel.
--
--
--
-- /See:/ 'sMSTemplateRequest' smart constructor.
data SMSTemplateRequest = SMSTemplateRequest'
  { _smstrBody ::
      !(Maybe Text),
    _smstrTemplateDescription :: !(Maybe Text),
    _smstrDefaultSubstitutions :: !(Maybe Text),
    _smstrRecommenderId :: !(Maybe Text),
    _smstrTags :: !(Maybe (Map Text (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SMSTemplateRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smstrBody' - The message body to use in text messages that are based on the message template.
--
-- * 'smstrTemplateDescription' - A custom description of the message template.
--
-- * 'smstrDefaultSubstitutions' - A JSON object that specifies the default values to use for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable. When you create a message that's based on the template, you can override these defaults with message-specific and address-specific variables and values.
--
-- * 'smstrRecommenderId' - The unique identifier for the recommender model to use for the message template. Amazon Pinpoint uses this value to determine how to retrieve and process data from a recommender model when it sends messages that use the template, if the template contains message variables for recommendation data.
--
-- * 'smstrTags' - A string-to-string map of key-value pairs that defines the tags to associate with the message template. Each tag consists of a required tag key and an associated tag value.
sMSTemplateRequest ::
  SMSTemplateRequest
sMSTemplateRequest =
  SMSTemplateRequest'
    { _smstrBody = Nothing,
      _smstrTemplateDescription = Nothing,
      _smstrDefaultSubstitutions = Nothing,
      _smstrRecommenderId = Nothing,
      _smstrTags = Nothing
    }

-- | The message body to use in text messages that are based on the message template.
smstrBody :: Lens' SMSTemplateRequest (Maybe Text)
smstrBody = lens _smstrBody (\s a -> s {_smstrBody = a})

-- | A custom description of the message template.
smstrTemplateDescription :: Lens' SMSTemplateRequest (Maybe Text)
smstrTemplateDescription = lens _smstrTemplateDescription (\s a -> s {_smstrTemplateDescription = a})

-- | A JSON object that specifies the default values to use for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable. When you create a message that's based on the template, you can override these defaults with message-specific and address-specific variables and values.
smstrDefaultSubstitutions :: Lens' SMSTemplateRequest (Maybe Text)
smstrDefaultSubstitutions = lens _smstrDefaultSubstitutions (\s a -> s {_smstrDefaultSubstitutions = a})

-- | The unique identifier for the recommender model to use for the message template. Amazon Pinpoint uses this value to determine how to retrieve and process data from a recommender model when it sends messages that use the template, if the template contains message variables for recommendation data.
smstrRecommenderId :: Lens' SMSTemplateRequest (Maybe Text)
smstrRecommenderId = lens _smstrRecommenderId (\s a -> s {_smstrRecommenderId = a})

-- | A string-to-string map of key-value pairs that defines the tags to associate with the message template. Each tag consists of a required tag key and an associated tag value.
smstrTags :: Lens' SMSTemplateRequest (HashMap Text (Text))
smstrTags = lens _smstrTags (\s a -> s {_smstrTags = a}) . _Default . _Map

instance Hashable SMSTemplateRequest

instance NFData SMSTemplateRequest

instance ToJSON SMSTemplateRequest where
  toJSON SMSTemplateRequest' {..} =
    object
      ( catMaybes
          [ ("Body" .=) <$> _smstrBody,
            ("TemplateDescription" .=) <$> _smstrTemplateDescription,
            ("DefaultSubstitutions" .=) <$> _smstrDefaultSubstitutions,
            ("RecommenderId" .=) <$> _smstrRecommenderId,
            ("tags" .=) <$> _smstrTags
          ]
      )
