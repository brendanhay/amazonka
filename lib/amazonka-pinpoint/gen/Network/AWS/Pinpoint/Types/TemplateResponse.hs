{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.TemplateResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.TemplateResponse where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.TemplateType
import Network.AWS.Prelude

-- | Provides information about a message template that's associated with your Amazon Pinpoint account.
--
--
--
-- /See:/ 'templateResponse' smart constructor.
data TemplateResponse = TemplateResponse'
  { _temARN :: !(Maybe Text),
    _temTemplateDescription :: !(Maybe Text),
    _temDefaultSubstitutions :: !(Maybe Text),
    _temVersion :: !(Maybe Text),
    _temTags :: !(Maybe (Map Text (Text))),
    _temLastModifiedDate :: !Text,
    _temCreationDate :: !Text,
    _temTemplateName :: !Text,
    _temTemplateType :: !TemplateType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'temARN' - The Amazon Resource Name (ARN) of the message template. This value isn't included in a TemplateResponse object. To retrieve the ARN of a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the ARN for.
--
-- * 'temTemplateDescription' - The custom description of the message template. This value isn't included in a TemplateResponse object. To retrieve the description of a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the description for.
--
-- * 'temDefaultSubstitutions' - The JSON object that specifies the default values that are used for message variables in the message template. This object isn't included in a TemplateResponse object. To retrieve this object for a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the object for.
--
-- * 'temVersion' - The unique identifier, as an integer, for the active version of the message template.
--
-- * 'temTags' - A map of key-value pairs that identifies the tags that are associated with the message template. This object isn't included in a TemplateResponse object. To retrieve this object for a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the object for.
--
-- * 'temLastModifiedDate' - The date, in ISO 8601 format, when the message template was last modified.
--
-- * 'temCreationDate' - The date, in ISO 8601 format, when the message template was created.
--
-- * 'temTemplateName' - The name of the message template.
--
-- * 'temTemplateType' - The type of channel that the message template is designed for. Possible values are: EMAIL, PUSH, SMS, and VOICE.
templateResponse ::
  -- | 'temLastModifiedDate'
  Text ->
  -- | 'temCreationDate'
  Text ->
  -- | 'temTemplateName'
  Text ->
  -- | 'temTemplateType'
  TemplateType ->
  TemplateResponse
templateResponse
  pLastModifiedDate_
  pCreationDate_
  pTemplateName_
  pTemplateType_ =
    TemplateResponse'
      { _temARN = Nothing,
        _temTemplateDescription = Nothing,
        _temDefaultSubstitutions = Nothing,
        _temVersion = Nothing,
        _temTags = Nothing,
        _temLastModifiedDate = pLastModifiedDate_,
        _temCreationDate = pCreationDate_,
        _temTemplateName = pTemplateName_,
        _temTemplateType = pTemplateType_
      }

-- | The Amazon Resource Name (ARN) of the message template. This value isn't included in a TemplateResponse object. To retrieve the ARN of a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the ARN for.
temARN :: Lens' TemplateResponse (Maybe Text)
temARN = lens _temARN (\s a -> s {_temARN = a})

-- | The custom description of the message template. This value isn't included in a TemplateResponse object. To retrieve the description of a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the description for.
temTemplateDescription :: Lens' TemplateResponse (Maybe Text)
temTemplateDescription = lens _temTemplateDescription (\s a -> s {_temTemplateDescription = a})

-- | The JSON object that specifies the default values that are used for message variables in the message template. This object isn't included in a TemplateResponse object. To retrieve this object for a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the object for.
temDefaultSubstitutions :: Lens' TemplateResponse (Maybe Text)
temDefaultSubstitutions = lens _temDefaultSubstitutions (\s a -> s {_temDefaultSubstitutions = a})

-- | The unique identifier, as an integer, for the active version of the message template.
temVersion :: Lens' TemplateResponse (Maybe Text)
temVersion = lens _temVersion (\s a -> s {_temVersion = a})

-- | A map of key-value pairs that identifies the tags that are associated with the message template. This object isn't included in a TemplateResponse object. To retrieve this object for a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the object for.
temTags :: Lens' TemplateResponse (HashMap Text (Text))
temTags = lens _temTags (\s a -> s {_temTags = a}) . _Default . _Map

-- | The date, in ISO 8601 format, when the message template was last modified.
temLastModifiedDate :: Lens' TemplateResponse Text
temLastModifiedDate = lens _temLastModifiedDate (\s a -> s {_temLastModifiedDate = a})

-- | The date, in ISO 8601 format, when the message template was created.
temCreationDate :: Lens' TemplateResponse Text
temCreationDate = lens _temCreationDate (\s a -> s {_temCreationDate = a})

-- | The name of the message template.
temTemplateName :: Lens' TemplateResponse Text
temTemplateName = lens _temTemplateName (\s a -> s {_temTemplateName = a})

-- | The type of channel that the message template is designed for. Possible values are: EMAIL, PUSH, SMS, and VOICE.
temTemplateType :: Lens' TemplateResponse TemplateType
temTemplateType = lens _temTemplateType (\s a -> s {_temTemplateType = a})

instance FromJSON TemplateResponse where
  parseJSON =
    withObject
      "TemplateResponse"
      ( \x ->
          TemplateResponse'
            <$> (x .:? "Arn")
            <*> (x .:? "TemplateDescription")
            <*> (x .:? "DefaultSubstitutions")
            <*> (x .:? "Version")
            <*> (x .:? "tags" .!= mempty)
            <*> (x .: "LastModifiedDate")
            <*> (x .: "CreationDate")
            <*> (x .: "TemplateName")
            <*> (x .: "TemplateType")
      )

instance Hashable TemplateResponse

instance NFData TemplateResponse
