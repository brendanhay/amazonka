{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.SuggestionMatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.SuggestionMatch where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An autocomplete suggestion that matches the query string specified in a @SuggestRequest@ .
--
--
--
-- /See:/ 'suggestionMatch' smart constructor.
data SuggestionMatch = SuggestionMatch'
  { _smSuggestion ::
      !(Maybe Text),
    _smScore :: !(Maybe Integer),
    _smId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SuggestionMatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smSuggestion' - The string that matches the query string specified in the @SuggestRequest@ .
--
-- * 'smScore' - The relevance score of a suggested match.
--
-- * 'smId' - The document ID of the suggested document.
suggestionMatch ::
  SuggestionMatch
suggestionMatch =
  SuggestionMatch'
    { _smSuggestion = Nothing,
      _smScore = Nothing,
      _smId = Nothing
    }

-- | The string that matches the query string specified in the @SuggestRequest@ .
smSuggestion :: Lens' SuggestionMatch (Maybe Text)
smSuggestion = lens _smSuggestion (\s a -> s {_smSuggestion = a})

-- | The relevance score of a suggested match.
smScore :: Lens' SuggestionMatch (Maybe Integer)
smScore = lens _smScore (\s a -> s {_smScore = a})

-- | The document ID of the suggested document.
smId :: Lens' SuggestionMatch (Maybe Text)
smId = lens _smId (\s a -> s {_smId = a})

instance FromJSON SuggestionMatch where
  parseJSON =
    withObject
      "SuggestionMatch"
      ( \x ->
          SuggestionMatch'
            <$> (x .:? "suggestion") <*> (x .:? "score") <*> (x .:? "id")
      )

instance Hashable SuggestionMatch

instance NFData SuggestionMatch
