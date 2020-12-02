{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.Hit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.Hit where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a document that matches the search request.
--
--
--
-- /See:/ 'hit' smart constructor.
data Hit = Hit'
  { _hitExprs :: !(Maybe (Map Text (Text))),
    _hitId :: !(Maybe Text),
    _hitHighlights :: !(Maybe (Map Text (Text))),
    _hitFields :: !(Maybe (Map Text ([Text])))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Hit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hitExprs' - The expressions returned from a document that matches the search request.
--
-- * 'hitId' - The document ID of a document that matches the search request.
--
-- * 'hitHighlights' - The highlights returned from a document that matches the search request.
--
-- * 'hitFields' - The fields returned from a document that matches the search request.
hit ::
  Hit
hit =
  Hit'
    { _hitExprs = Nothing,
      _hitId = Nothing,
      _hitHighlights = Nothing,
      _hitFields = Nothing
    }

-- | The expressions returned from a document that matches the search request.
hitExprs :: Lens' Hit (HashMap Text (Text))
hitExprs = lens _hitExprs (\s a -> s {_hitExprs = a}) . _Default . _Map

-- | The document ID of a document that matches the search request.
hitId :: Lens' Hit (Maybe Text)
hitId = lens _hitId (\s a -> s {_hitId = a})

-- | The highlights returned from a document that matches the search request.
hitHighlights :: Lens' Hit (HashMap Text (Text))
hitHighlights = lens _hitHighlights (\s a -> s {_hitHighlights = a}) . _Default . _Map

-- | The fields returned from a document that matches the search request.
hitFields :: Lens' Hit (HashMap Text ([Text]))
hitFields = lens _hitFields (\s a -> s {_hitFields = a}) . _Default . _Map

instance FromJSON Hit where
  parseJSON =
    withObject
      "Hit"
      ( \x ->
          Hit'
            <$> (x .:? "exprs" .!= mempty)
            <*> (x .:? "id")
            <*> (x .:? "highlights" .!= mempty)
            <*> (x .:? "fields" .!= mempty)
      )

instance Hashable Hit

instance NFData Hit
