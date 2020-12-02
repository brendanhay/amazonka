{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.Types.Filter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecretsManager.Types.Filter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SecretsManager.Types.FilterNameStringType

-- | Allows you to filter your list of secrets.
--
--
--
-- /See:/ 'filter'' smart constructor.
data Filter = Filter'
  { _fValues :: !(Maybe (List1 Text)),
    _fKey :: !(Maybe FilterNameStringType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Filter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fValues' - Filters your list of secrets by a specific value.
--
-- * 'fKey' - Filters your list of secrets by a specific key.
filter' ::
  Filter
filter' = Filter' {_fValues = Nothing, _fKey = Nothing}

-- | Filters your list of secrets by a specific value.
fValues :: Lens' Filter (Maybe (NonEmpty Text))
fValues = lens _fValues (\s a -> s {_fValues = a}) . mapping _List1

-- | Filters your list of secrets by a specific key.
fKey :: Lens' Filter (Maybe FilterNameStringType)
fKey = lens _fKey (\s a -> s {_fKey = a})

instance Hashable Filter

instance NFData Filter

instance ToJSON Filter where
  toJSON Filter' {..} =
    object
      (catMaybes [("Values" .=) <$> _fValues, ("Key" .=) <$> _fKey])
