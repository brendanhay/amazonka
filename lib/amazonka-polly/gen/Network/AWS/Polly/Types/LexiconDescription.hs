{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types.LexiconDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Polly.Types.LexiconDescription where

import Network.AWS.Lens
import Network.AWS.Polly.Types.LexiconAttributes
import Network.AWS.Prelude

-- | Describes the content of the lexicon.
--
--
--
-- /See:/ 'lexiconDescription' smart constructor.
data LexiconDescription = LexiconDescription'
  { _ldAttributes ::
      !(Maybe LexiconAttributes),
    _ldName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LexiconDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldAttributes' - Provides lexicon metadata.
--
-- * 'ldName' - Name of the lexicon.
lexiconDescription ::
  LexiconDescription
lexiconDescription =
  LexiconDescription' {_ldAttributes = Nothing, _ldName = Nothing}

-- | Provides lexicon metadata.
ldAttributes :: Lens' LexiconDescription (Maybe LexiconAttributes)
ldAttributes = lens _ldAttributes (\s a -> s {_ldAttributes = a})

-- | Name of the lexicon.
ldName :: Lens' LexiconDescription (Maybe Text)
ldName = lens _ldName (\s a -> s {_ldName = a})

instance FromJSON LexiconDescription where
  parseJSON =
    withObject
      "LexiconDescription"
      ( \x ->
          LexiconDescription' <$> (x .:? "Attributes") <*> (x .:? "Name")
      )

instance Hashable LexiconDescription

instance NFData LexiconDescription
