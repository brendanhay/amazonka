{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.RedactionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.RedactionConfig where

import Network.AWS.Comprehend.Types.PiiEntitiesDetectionMaskMode
import Network.AWS.Comprehend.Types.PiiEntityType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides configuration parameters for PII entity redaction.
--
--
--
-- /See:/ 'redactionConfig' smart constructor.
data RedactionConfig = RedactionConfig'
  { _rcMaskCharacter ::
      !(Maybe Text),
    _rcMaskMode :: !(Maybe PiiEntitiesDetectionMaskMode),
    _rcPiiEntityTypes :: !(Maybe [PiiEntityType])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RedactionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcMaskCharacter' - A character that replaces each character in the redacted PII entity.
--
-- * 'rcMaskMode' - Specifies whether the PII entity is redacted with the mask character or the entity type.
--
-- * 'rcPiiEntityTypes' - An array of the types of PII entities that Amazon Comprehend detects in the input text for your request.
redactionConfig ::
  RedactionConfig
redactionConfig =
  RedactionConfig'
    { _rcMaskCharacter = Nothing,
      _rcMaskMode = Nothing,
      _rcPiiEntityTypes = Nothing
    }

-- | A character that replaces each character in the redacted PII entity.
rcMaskCharacter :: Lens' RedactionConfig (Maybe Text)
rcMaskCharacter = lens _rcMaskCharacter (\s a -> s {_rcMaskCharacter = a})

-- | Specifies whether the PII entity is redacted with the mask character or the entity type.
rcMaskMode :: Lens' RedactionConfig (Maybe PiiEntitiesDetectionMaskMode)
rcMaskMode = lens _rcMaskMode (\s a -> s {_rcMaskMode = a})

-- | An array of the types of PII entities that Amazon Comprehend detects in the input text for your request.
rcPiiEntityTypes :: Lens' RedactionConfig [PiiEntityType]
rcPiiEntityTypes = lens _rcPiiEntityTypes (\s a -> s {_rcPiiEntityTypes = a}) . _Default . _Coerce

instance FromJSON RedactionConfig where
  parseJSON =
    withObject
      "RedactionConfig"
      ( \x ->
          RedactionConfig'
            <$> (x .:? "MaskCharacter")
            <*> (x .:? "MaskMode")
            <*> (x .:? "PiiEntityTypes" .!= mempty)
      )

instance Hashable RedactionConfig

instance NFData RedactionConfig

instance ToJSON RedactionConfig where
  toJSON RedactionConfig' {..} =
    object
      ( catMaybes
          [ ("MaskCharacter" .=) <$> _rcMaskCharacter,
            ("MaskMode" .=) <$> _rcMaskMode,
            ("PiiEntityTypes" .=) <$> _rcPiiEntityTypes
          ]
      )
