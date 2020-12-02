{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.KeyPhrase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.KeyPhrase where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a key noun phrase.
--
--
--
-- /See:/ 'keyPhrase' smart constructor.
data KeyPhrase = KeyPhrase'
  { _kpBeginOffset :: !(Maybe Int),
    _kpText :: !(Maybe Text),
    _kpScore :: !(Maybe Double),
    _kpEndOffset :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KeyPhrase' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kpBeginOffset' - A character offset in the input text that shows where the key phrase begins (the first character is at position 0). The offset returns the position of each UTF-8 code point in the string. A /code point/ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
--
-- * 'kpText' - The text of a key noun phrase.
--
-- * 'kpScore' - The level of confidence that Amazon Comprehend has in the accuracy of the detection.
--
-- * 'kpEndOffset' - A character offset in the input text where the key phrase ends. The offset returns the position of each UTF-8 code point in the string. A @code point@ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
keyPhrase ::
  KeyPhrase
keyPhrase =
  KeyPhrase'
    { _kpBeginOffset = Nothing,
      _kpText = Nothing,
      _kpScore = Nothing,
      _kpEndOffset = Nothing
    }

-- | A character offset in the input text that shows where the key phrase begins (the first character is at position 0). The offset returns the position of each UTF-8 code point in the string. A /code point/ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
kpBeginOffset :: Lens' KeyPhrase (Maybe Int)
kpBeginOffset = lens _kpBeginOffset (\s a -> s {_kpBeginOffset = a})

-- | The text of a key noun phrase.
kpText :: Lens' KeyPhrase (Maybe Text)
kpText = lens _kpText (\s a -> s {_kpText = a})

-- | The level of confidence that Amazon Comprehend has in the accuracy of the detection.
kpScore :: Lens' KeyPhrase (Maybe Double)
kpScore = lens _kpScore (\s a -> s {_kpScore = a})

-- | A character offset in the input text where the key phrase ends. The offset returns the position of each UTF-8 code point in the string. A @code point@ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
kpEndOffset :: Lens' KeyPhrase (Maybe Int)
kpEndOffset = lens _kpEndOffset (\s a -> s {_kpEndOffset = a})

instance FromJSON KeyPhrase where
  parseJSON =
    withObject
      "KeyPhrase"
      ( \x ->
          KeyPhrase'
            <$> (x .:? "BeginOffset")
            <*> (x .:? "Text")
            <*> (x .:? "Score")
            <*> (x .:? "EndOffset")
      )

instance Hashable KeyPhrase

instance NFData KeyPhrase
