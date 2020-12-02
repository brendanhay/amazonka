{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.SlotTypeRegexConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.SlotTypeRegexConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides a regular expression used to validate the value of a slot.
--
--
--
-- /See:/ 'slotTypeRegexConfiguration' smart constructor.
newtype SlotTypeRegexConfiguration = SlotTypeRegexConfiguration'
  { _strcPattern ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SlotTypeRegexConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'strcPattern' - A regular expression used to validate the value of a slot.  Use a standard regular expression. Amazon Lex supports the following characters in the regular expression:     * A-Z, a-z     * 0-9     * Unicode characters ("\ u<Unicode>") Represent Unicode characters with four digits, for example "\u0041" or "\u005A". The following regular expression operators are not supported:     * Infinite repeaters: *, +, or {x,} with no upper bound.     * Wild card (.)
slotTypeRegexConfiguration ::
  -- | 'strcPattern'
  Text ->
  SlotTypeRegexConfiguration
slotTypeRegexConfiguration pPattern_ =
  SlotTypeRegexConfiguration' {_strcPattern = pPattern_}

-- | A regular expression used to validate the value of a slot.  Use a standard regular expression. Amazon Lex supports the following characters in the regular expression:     * A-Z, a-z     * 0-9     * Unicode characters ("\ u<Unicode>") Represent Unicode characters with four digits, for example "\u0041" or "\u005A". The following regular expression operators are not supported:     * Infinite repeaters: *, +, or {x,} with no upper bound.     * Wild card (.)
strcPattern :: Lens' SlotTypeRegexConfiguration Text
strcPattern = lens _strcPattern (\s a -> s {_strcPattern = a})

instance FromJSON SlotTypeRegexConfiguration where
  parseJSON =
    withObject
      "SlotTypeRegexConfiguration"
      (\x -> SlotTypeRegexConfiguration' <$> (x .: "pattern"))

instance Hashable SlotTypeRegexConfiguration

instance NFData SlotTypeRegexConfiguration

instance ToJSON SlotTypeRegexConfiguration where
  toJSON SlotTypeRegexConfiguration' {..} =
    object (catMaybes [Just ("pattern" .= _strcPattern)])
