{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Filter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Filter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A filter name and value pair that is used to return a more specific list of results. Filters can be used to match a set of resources by various criteria.
--
--
--
-- /See:/ 'filter'' smart constructor.
data Filter = Filter' {_fKey :: !Text, _fValues :: ![Text]}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Filter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fKey' - The key of a filter.
--
-- * 'fValues' - The values of a filter.
filter' ::
  -- | 'fKey'
  Text ->
  Filter
filter' pKey_ = Filter' {_fKey = pKey_, _fValues = mempty}

-- | The key of a filter.
fKey :: Lens' Filter Text
fKey = lens _fKey (\s a -> s {_fKey = a})

-- | The values of a filter.
fValues :: Lens' Filter [Text]
fValues = lens _fValues (\s a -> s {_fValues = a}) . _Coerce

instance Hashable Filter

instance NFData Filter

instance ToJSON Filter where
  toJSON Filter' {..} =
    object
      (catMaybes [Just ("Key" .= _fKey), Just ("Values" .= _fValues)])
