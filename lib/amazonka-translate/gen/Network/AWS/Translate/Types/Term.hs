{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.Term
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.Term where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The term being translated by the custom terminology.
--
--
--
-- /See:/ 'term' smart constructor.
data Term = Term'
  { _tTargetText :: !(Maybe Text),
    _tSourceText :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Term' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tTargetText' - The target text of the term being translated by the custom terminology.
--
-- * 'tSourceText' - The source text of the term being translated by the custom terminology.
term ::
  Term
term = Term' {_tTargetText = Nothing, _tSourceText = Nothing}

-- | The target text of the term being translated by the custom terminology.
tTargetText :: Lens' Term (Maybe Text)
tTargetText = lens _tTargetText (\s a -> s {_tTargetText = a})

-- | The source text of the term being translated by the custom terminology.
tSourceText :: Lens' Term (Maybe Text)
tSourceText = lens _tSourceText (\s a -> s {_tSourceText = a})

instance FromJSON Term where
  parseJSON =
    withObject
      "Term"
      (\x -> Term' <$> (x .:? "TargetText") <*> (x .:? "SourceText"))

instance Hashable Term

instance NFData Term
