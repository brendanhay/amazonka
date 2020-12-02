{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.Filter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.Filter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Used to streamline results of a search based on the property being filtered.
--
--
--
-- /See:/ 'filter'' smart constructor.
data Filter = Filter' {_fName :: !Text, _fValues :: !(List1 Text)}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Filter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fName' - The property being filtered. For example, UserId.
--
-- * 'fValues' - The property values to filter on. For example, "user-123".
filter' ::
  -- | 'fName'
  Text ->
  -- | 'fValues'
  NonEmpty Text ->
  Filter
filter' pName_ pValues_ =
  Filter' {_fName = pName_, _fValues = _List1 # pValues_}

-- | The property being filtered. For example, UserId.
fName :: Lens' Filter Text
fName = lens _fName (\s a -> s {_fName = a})

-- | The property values to filter on. For example, "user-123".
fValues :: Lens' Filter (NonEmpty Text)
fValues = lens _fValues (\s a -> s {_fValues = a}) . _List1

instance Hashable Filter

instance NFData Filter

instance ToQuery Filter where
  toQuery Filter' {..} =
    mconcat
      ["Name" =: _fName, "Values" =: toQueryList "member" _fValues]
