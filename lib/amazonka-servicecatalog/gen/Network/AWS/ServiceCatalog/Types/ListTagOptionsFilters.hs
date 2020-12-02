{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ListTagOptionsFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ListTagOptionsFilters where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Filters to use when listing TagOptions.
--
--
--
-- /See:/ 'listTagOptionsFilters' smart constructor.
data ListTagOptionsFilters = ListTagOptionsFilters'
  { _ltofValue ::
      !(Maybe Text),
    _ltofActive :: !(Maybe Bool),
    _ltofKey :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTagOptionsFilters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltofValue' - The TagOption value.
--
-- * 'ltofActive' - The active state.
--
-- * 'ltofKey' - The TagOption key.
listTagOptionsFilters ::
  ListTagOptionsFilters
listTagOptionsFilters =
  ListTagOptionsFilters'
    { _ltofValue = Nothing,
      _ltofActive = Nothing,
      _ltofKey = Nothing
    }

-- | The TagOption value.
ltofValue :: Lens' ListTagOptionsFilters (Maybe Text)
ltofValue = lens _ltofValue (\s a -> s {_ltofValue = a})

-- | The active state.
ltofActive :: Lens' ListTagOptionsFilters (Maybe Bool)
ltofActive = lens _ltofActive (\s a -> s {_ltofActive = a})

-- | The TagOption key.
ltofKey :: Lens' ListTagOptionsFilters (Maybe Text)
ltofKey = lens _ltofKey (\s a -> s {_ltofKey = a})

instance Hashable ListTagOptionsFilters

instance NFData ListTagOptionsFilters

instance ToJSON ListTagOptionsFilters where
  toJSON ListTagOptionsFilters' {..} =
    object
      ( catMaybes
          [ ("Value" .=) <$> _ltofValue,
            ("Active" .=) <$> _ltofActive,
            ("Key" .=) <$> _ltofKey
          ]
      )
