{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.PiiEntity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.PiiEntity where

import Network.AWS.Comprehend.Types.PiiEntityType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about a PII entity.
--
--
--
-- /See:/ 'piiEntity' smart constructor.
data PiiEntity = PiiEntity'
  { _peBeginOffset :: !(Maybe Int),
    _peScore :: !(Maybe Double),
    _peEndOffset :: !(Maybe Int),
    _peType :: !(Maybe PiiEntityType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PiiEntity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'peBeginOffset' - A character offset in the input text that shows where the PII entity begins (the first character is at position 0). The offset returns the position of each UTF-8 code point in the string. A /code point/ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
--
-- * 'peScore' - The level of confidence that Amazon Comprehend has in the accuracy of the detection.
--
-- * 'peEndOffset' - A character offset in the input text that shows where the PII entity ends. The offset returns the position of each UTF-8 code point in the string. A /code point/ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
--
-- * 'peType' - The entity's type.
piiEntity ::
  PiiEntity
piiEntity =
  PiiEntity'
    { _peBeginOffset = Nothing,
      _peScore = Nothing,
      _peEndOffset = Nothing,
      _peType = Nothing
    }

-- | A character offset in the input text that shows where the PII entity begins (the first character is at position 0). The offset returns the position of each UTF-8 code point in the string. A /code point/ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
peBeginOffset :: Lens' PiiEntity (Maybe Int)
peBeginOffset = lens _peBeginOffset (\s a -> s {_peBeginOffset = a})

-- | The level of confidence that Amazon Comprehend has in the accuracy of the detection.
peScore :: Lens' PiiEntity (Maybe Double)
peScore = lens _peScore (\s a -> s {_peScore = a})

-- | A character offset in the input text that shows where the PII entity ends. The offset returns the position of each UTF-8 code point in the string. A /code point/ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
peEndOffset :: Lens' PiiEntity (Maybe Int)
peEndOffset = lens _peEndOffset (\s a -> s {_peEndOffset = a})

-- | The entity's type.
peType :: Lens' PiiEntity (Maybe PiiEntityType)
peType = lens _peType (\s a -> s {_peType = a})

instance FromJSON PiiEntity where
  parseJSON =
    withObject
      "PiiEntity"
      ( \x ->
          PiiEntity'
            <$> (x .:? "BeginOffset")
            <*> (x .:? "Score")
            <*> (x .:? "EndOffset")
            <*> (x .:? "Type")
      )

instance Hashable PiiEntity

instance NFData PiiEntity
