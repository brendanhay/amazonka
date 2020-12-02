{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.TemplateVersionResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.TemplateVersionResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about a specific version of a message template.
--
--
--
-- /See:/ 'templateVersionResponse' smart constructor.
data TemplateVersionResponse = TemplateVersionResponse'
  { _tvTemplateDescription ::
      !(Maybe Text),
    _tvDefaultSubstitutions :: !(Maybe Text),
    _tvVersion :: !(Maybe Text),
    _tvLastModifiedDate :: !Text,
    _tvCreationDate :: !Text,
    _tvTemplateName :: !Text,
    _tvTemplateType :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TemplateVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tvTemplateDescription' - The custom description of the version of the message template.
--
-- * 'tvDefaultSubstitutions' - A JSON object that specifies the default values that are used for message variables in the version of the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable.
--
-- * 'tvVersion' - The unique identifier for the version of the message template. This value is an integer that Amazon Pinpoint automatically increments and assigns to each new version of a template.
--
-- * 'tvLastModifiedDate' - The date, in ISO 8601 format, when the version of the message template was last modified.
--
-- * 'tvCreationDate' - The date, in ISO 8601 format, when the version of the message template was created.
--
-- * 'tvTemplateName' - The name of the message template.
--
-- * 'tvTemplateType' - The type of channel that the message template is designed for. Possible values are: EMAIL, PUSH, SMS, and VOICE.
templateVersionResponse ::
  -- | 'tvLastModifiedDate'
  Text ->
  -- | 'tvCreationDate'
  Text ->
  -- | 'tvTemplateName'
  Text ->
  -- | 'tvTemplateType'
  Text ->
  TemplateVersionResponse
templateVersionResponse
  pLastModifiedDate_
  pCreationDate_
  pTemplateName_
  pTemplateType_ =
    TemplateVersionResponse'
      { _tvTemplateDescription = Nothing,
        _tvDefaultSubstitutions = Nothing,
        _tvVersion = Nothing,
        _tvLastModifiedDate = pLastModifiedDate_,
        _tvCreationDate = pCreationDate_,
        _tvTemplateName = pTemplateName_,
        _tvTemplateType = pTemplateType_
      }

-- | The custom description of the version of the message template.
tvTemplateDescription :: Lens' TemplateVersionResponse (Maybe Text)
tvTemplateDescription = lens _tvTemplateDescription (\s a -> s {_tvTemplateDescription = a})

-- | A JSON object that specifies the default values that are used for message variables in the version of the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable.
tvDefaultSubstitutions :: Lens' TemplateVersionResponse (Maybe Text)
tvDefaultSubstitutions = lens _tvDefaultSubstitutions (\s a -> s {_tvDefaultSubstitutions = a})

-- | The unique identifier for the version of the message template. This value is an integer that Amazon Pinpoint automatically increments and assigns to each new version of a template.
tvVersion :: Lens' TemplateVersionResponse (Maybe Text)
tvVersion = lens _tvVersion (\s a -> s {_tvVersion = a})

-- | The date, in ISO 8601 format, when the version of the message template was last modified.
tvLastModifiedDate :: Lens' TemplateVersionResponse Text
tvLastModifiedDate = lens _tvLastModifiedDate (\s a -> s {_tvLastModifiedDate = a})

-- | The date, in ISO 8601 format, when the version of the message template was created.
tvCreationDate :: Lens' TemplateVersionResponse Text
tvCreationDate = lens _tvCreationDate (\s a -> s {_tvCreationDate = a})

-- | The name of the message template.
tvTemplateName :: Lens' TemplateVersionResponse Text
tvTemplateName = lens _tvTemplateName (\s a -> s {_tvTemplateName = a})

-- | The type of channel that the message template is designed for. Possible values are: EMAIL, PUSH, SMS, and VOICE.
tvTemplateType :: Lens' TemplateVersionResponse Text
tvTemplateType = lens _tvTemplateType (\s a -> s {_tvTemplateType = a})

instance FromJSON TemplateVersionResponse where
  parseJSON =
    withObject
      "TemplateVersionResponse"
      ( \x ->
          TemplateVersionResponse'
            <$> (x .:? "TemplateDescription")
            <*> (x .:? "DefaultSubstitutions")
            <*> (x .:? "Version")
            <*> (x .: "LastModifiedDate")
            <*> (x .: "CreationDate")
            <*> (x .: "TemplateName")
            <*> (x .: "TemplateType")
      )

instance Hashable TemplateVersionResponse

instance NFData TemplateVersionResponse
