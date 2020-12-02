{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SMSTemplateResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SMSTemplateResponse where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.TemplateType
import Network.AWS.Prelude

-- | Provides information about the content and settings for a message template that can be used in text messages that are sent through the SMS channel.
--
--
--
-- /See:/ 'sMSTemplateResponse' smart constructor.
data SMSTemplateResponse = SMSTemplateResponse'
  { _smstARN ::
      !(Maybe Text),
    _smstBody :: !(Maybe Text),
    _smstTemplateDescription :: !(Maybe Text),
    _smstDefaultSubstitutions :: !(Maybe Text),
    _smstVersion :: !(Maybe Text),
    _smstRecommenderId :: !(Maybe Text),
    _smstTags :: !(Maybe (Map Text (Text))),
    _smstLastModifiedDate :: !Text,
    _smstCreationDate :: !Text,
    _smstTemplateName :: !Text,
    _smstTemplateType :: !TemplateType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SMSTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smstARN' - The Amazon Resource Name (ARN) of the message template.
--
-- * 'smstBody' - The message body that's used in text messages that are based on the message template.
--
-- * 'smstTemplateDescription' - The custom description of the message template.
--
-- * 'smstDefaultSubstitutions' - The JSON object that specifies the default values that are used for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable.
--
-- * 'smstVersion' - The unique identifier, as an integer, for the active version of the message template, or the version of the template that you specified by using the version parameter in your request.
--
-- * 'smstRecommenderId' - The unique identifier for the recommender model that's used by the message template.
--
-- * 'smstTags' - A string-to-string map of key-value pairs that identifies the tags that are associated with the message template. Each tag consists of a required tag key and an associated tag value.
--
-- * 'smstLastModifiedDate' - The date, in ISO 8601 format, when the message template was last modified.
--
-- * 'smstCreationDate' - The date, in ISO 8601 format, when the message template was created.
--
-- * 'smstTemplateName' - The name of the message template.
--
-- * 'smstTemplateType' - The type of channel that the message template is designed for. For an SMS template, this value is SMS.
sMSTemplateResponse ::
  -- | 'smstLastModifiedDate'
  Text ->
  -- | 'smstCreationDate'
  Text ->
  -- | 'smstTemplateName'
  Text ->
  -- | 'smstTemplateType'
  TemplateType ->
  SMSTemplateResponse
sMSTemplateResponse
  pLastModifiedDate_
  pCreationDate_
  pTemplateName_
  pTemplateType_ =
    SMSTemplateResponse'
      { _smstARN = Nothing,
        _smstBody = Nothing,
        _smstTemplateDescription = Nothing,
        _smstDefaultSubstitutions = Nothing,
        _smstVersion = Nothing,
        _smstRecommenderId = Nothing,
        _smstTags = Nothing,
        _smstLastModifiedDate = pLastModifiedDate_,
        _smstCreationDate = pCreationDate_,
        _smstTemplateName = pTemplateName_,
        _smstTemplateType = pTemplateType_
      }

-- | The Amazon Resource Name (ARN) of the message template.
smstARN :: Lens' SMSTemplateResponse (Maybe Text)
smstARN = lens _smstARN (\s a -> s {_smstARN = a})

-- | The message body that's used in text messages that are based on the message template.
smstBody :: Lens' SMSTemplateResponse (Maybe Text)
smstBody = lens _smstBody (\s a -> s {_smstBody = a})

-- | The custom description of the message template.
smstTemplateDescription :: Lens' SMSTemplateResponse (Maybe Text)
smstTemplateDescription = lens _smstTemplateDescription (\s a -> s {_smstTemplateDescription = a})

-- | The JSON object that specifies the default values that are used for message variables in the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable.
smstDefaultSubstitutions :: Lens' SMSTemplateResponse (Maybe Text)
smstDefaultSubstitutions = lens _smstDefaultSubstitutions (\s a -> s {_smstDefaultSubstitutions = a})

-- | The unique identifier, as an integer, for the active version of the message template, or the version of the template that you specified by using the version parameter in your request.
smstVersion :: Lens' SMSTemplateResponse (Maybe Text)
smstVersion = lens _smstVersion (\s a -> s {_smstVersion = a})

-- | The unique identifier for the recommender model that's used by the message template.
smstRecommenderId :: Lens' SMSTemplateResponse (Maybe Text)
smstRecommenderId = lens _smstRecommenderId (\s a -> s {_smstRecommenderId = a})

-- | A string-to-string map of key-value pairs that identifies the tags that are associated with the message template. Each tag consists of a required tag key and an associated tag value.
smstTags :: Lens' SMSTemplateResponse (HashMap Text (Text))
smstTags = lens _smstTags (\s a -> s {_smstTags = a}) . _Default . _Map

-- | The date, in ISO 8601 format, when the message template was last modified.
smstLastModifiedDate :: Lens' SMSTemplateResponse Text
smstLastModifiedDate = lens _smstLastModifiedDate (\s a -> s {_smstLastModifiedDate = a})

-- | The date, in ISO 8601 format, when the message template was created.
smstCreationDate :: Lens' SMSTemplateResponse Text
smstCreationDate = lens _smstCreationDate (\s a -> s {_smstCreationDate = a})

-- | The name of the message template.
smstTemplateName :: Lens' SMSTemplateResponse Text
smstTemplateName = lens _smstTemplateName (\s a -> s {_smstTemplateName = a})

-- | The type of channel that the message template is designed for. For an SMS template, this value is SMS.
smstTemplateType :: Lens' SMSTemplateResponse TemplateType
smstTemplateType = lens _smstTemplateType (\s a -> s {_smstTemplateType = a})

instance FromJSON SMSTemplateResponse where
  parseJSON =
    withObject
      "SMSTemplateResponse"
      ( \x ->
          SMSTemplateResponse'
            <$> (x .:? "Arn")
            <*> (x .:? "Body")
            <*> (x .:? "TemplateDescription")
            <*> (x .:? "DefaultSubstitutions")
            <*> (x .:? "Version")
            <*> (x .:? "RecommenderId")
            <*> (x .:? "tags" .!= mempty)
            <*> (x .: "LastModifiedDate")
            <*> (x .: "CreationDate")
            <*> (x .: "TemplateName")
            <*> (x .: "TemplateType")
      )

instance Hashable SMSTemplateResponse

instance NFData SMSTemplateResponse
