{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.Hits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.Hits where

import Network.AWS.CloudSearchDomains.Types.Hit
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The collection of documents that match the search request.
--
--
--
-- /See:/ 'hits' smart constructor.
data Hits = Hits'
  { _hCursor :: !(Maybe Text),
    _hHit :: !(Maybe [Hit]),
    _hStart :: !(Maybe Integer),
    _hFound :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Hits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hCursor' - A cursor that can be used to retrieve the next set of matching documents when you want to page through a large result set.
--
-- * 'hHit' - A document that matches the search request.
--
-- * 'hStart' - The index of the first matching document.
--
-- * 'hFound' - The total number of documents that match the search request.
hits ::
  Hits
hits =
  Hits'
    { _hCursor = Nothing,
      _hHit = Nothing,
      _hStart = Nothing,
      _hFound = Nothing
    }

-- | A cursor that can be used to retrieve the next set of matching documents when you want to page through a large result set.
hCursor :: Lens' Hits (Maybe Text)
hCursor = lens _hCursor (\s a -> s {_hCursor = a})

-- | A document that matches the search request.
hHit :: Lens' Hits [Hit]
hHit = lens _hHit (\s a -> s {_hHit = a}) . _Default . _Coerce

-- | The index of the first matching document.
hStart :: Lens' Hits (Maybe Integer)
hStart = lens _hStart (\s a -> s {_hStart = a})

-- | The total number of documents that match the search request.
hFound :: Lens' Hits (Maybe Integer)
hFound = lens _hFound (\s a -> s {_hFound = a})

instance FromJSON Hits where
  parseJSON =
    withObject
      "Hits"
      ( \x ->
          Hits'
            <$> (x .:? "cursor")
            <*> (x .:? "hit" .!= mempty)
            <*> (x .:? "start")
            <*> (x .:? "found")
      )

instance Hashable Hits

instance NFData Hits
