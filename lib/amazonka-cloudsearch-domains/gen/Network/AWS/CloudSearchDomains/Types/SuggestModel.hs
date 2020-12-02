{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.SuggestModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.SuggestModel where

import Network.AWS.CloudSearchDomains.Types.SuggestionMatch
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Container for the suggestion information returned in a @SuggestResponse@ .
--
--
--
-- /See:/ 'suggestModel' smart constructor.
data SuggestModel = SuggestModel'
  { _smFound :: !(Maybe Integer),
    _smSuggestions :: !(Maybe [SuggestionMatch]),
    _smQuery :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SuggestModel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smFound' - The number of documents that were found to match the query string.
--
-- * 'smSuggestions' - The documents that match the query string.
--
-- * 'smQuery' - The query string specified in the suggest request.
suggestModel ::
  SuggestModel
suggestModel =
  SuggestModel'
    { _smFound = Nothing,
      _smSuggestions = Nothing,
      _smQuery = Nothing
    }

-- | The number of documents that were found to match the query string.
smFound :: Lens' SuggestModel (Maybe Integer)
smFound = lens _smFound (\s a -> s {_smFound = a})

-- | The documents that match the query string.
smSuggestions :: Lens' SuggestModel [SuggestionMatch]
smSuggestions = lens _smSuggestions (\s a -> s {_smSuggestions = a}) . _Default . _Coerce

-- | The query string specified in the suggest request.
smQuery :: Lens' SuggestModel (Maybe Text)
smQuery = lens _smQuery (\s a -> s {_smQuery = a})

instance FromJSON SuggestModel where
  parseJSON =
    withObject
      "SuggestModel"
      ( \x ->
          SuggestModel'
            <$> (x .:? "found")
            <*> (x .:? "suggestions" .!= mempty)
            <*> (x .:? "query")
      )

instance Hashable SuggestModel

instance NFData SuggestModel
