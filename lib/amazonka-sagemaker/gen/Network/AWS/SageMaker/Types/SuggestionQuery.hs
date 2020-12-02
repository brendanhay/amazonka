{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SuggestionQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SuggestionQuery where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.PropertyNameQuery

-- | Specified in the 'GetSearchSuggestions' request. Limits the property names that are included in the response.
--
--
--
-- /See:/ 'suggestionQuery' smart constructor.
newtype SuggestionQuery = SuggestionQuery'
  { _sqPropertyNameQuery ::
      Maybe PropertyNameQuery
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SuggestionQuery' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sqPropertyNameQuery' - Defines a property name hint. Only property names that begin with the specified hint are included in the response.
suggestionQuery ::
  SuggestionQuery
suggestionQuery = SuggestionQuery' {_sqPropertyNameQuery = Nothing}

-- | Defines a property name hint. Only property names that begin with the specified hint are included in the response.
sqPropertyNameQuery :: Lens' SuggestionQuery (Maybe PropertyNameQuery)
sqPropertyNameQuery = lens _sqPropertyNameQuery (\s a -> s {_sqPropertyNameQuery = a})

instance Hashable SuggestionQuery

instance NFData SuggestionQuery

instance ToJSON SuggestionQuery where
  toJSON SuggestionQuery' {..} =
    object
      (catMaybes [("PropertyNameQuery" .=) <$> _sqPropertyNameQuery])
