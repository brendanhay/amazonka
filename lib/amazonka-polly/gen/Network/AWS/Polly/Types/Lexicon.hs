{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types.Lexicon
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Polly.Types.Lexicon where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides lexicon name and lexicon content in string format. For more information, see <https://www.w3.org/TR/pronunciation-lexicon/ Pronunciation Lexicon Specification (PLS) Version 1.0> .
--
--
--
-- /See:/ 'lexicon' smart constructor.
data Lexicon = Lexicon'
  { _lContent :: !(Maybe (Sensitive Text)),
    _lName :: !(Maybe Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'Lexicon' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lContent' - Lexicon content in string format. The content of a lexicon must be in PLS format.
--
-- * 'lName' - Name of the lexicon.
lexicon ::
  Lexicon
lexicon = Lexicon' {_lContent = Nothing, _lName = Nothing}

-- | Lexicon content in string format. The content of a lexicon must be in PLS format.
lContent :: Lens' Lexicon (Maybe Text)
lContent = lens _lContent (\s a -> s {_lContent = a}) . mapping _Sensitive

-- | Name of the lexicon.
lName :: Lens' Lexicon (Maybe Text)
lName = lens _lName (\s a -> s {_lName = a})

instance FromJSON Lexicon where
  parseJSON =
    withObject
      "Lexicon"
      (\x -> Lexicon' <$> (x .:? "Content") <*> (x .:? "Name"))

instance Hashable Lexicon

instance NFData Lexicon
