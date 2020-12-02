{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.GroupFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.GroupFilter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ResourceGroups.Types.GroupFilterName

-- | A filter collection that you can use to restrict the results from a @List@ operation to only those you want to include.
--
--
--
-- /See:/ 'groupFilter' smart constructor.
data GroupFilter = GroupFilter'
  { _gfName :: !GroupFilterName,
    _gfValues :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GroupFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfName' - The name of the filter. Filter names are case-sensitive.
--
-- * 'gfValues' - One or more filter values. Allowed filter values vary by group filter name, and are case-sensitive.
groupFilter ::
  -- | 'gfName'
  GroupFilterName ->
  -- | 'gfValues'
  NonEmpty Text ->
  GroupFilter
groupFilter pName_ pValues_ =
  GroupFilter' {_gfName = pName_, _gfValues = _List1 # pValues_}

-- | The name of the filter. Filter names are case-sensitive.
gfName :: Lens' GroupFilter GroupFilterName
gfName = lens _gfName (\s a -> s {_gfName = a})

-- | One or more filter values. Allowed filter values vary by group filter name, and are case-sensitive.
gfValues :: Lens' GroupFilter (NonEmpty Text)
gfValues = lens _gfValues (\s a -> s {_gfValues = a}) . _List1

instance Hashable GroupFilter

instance NFData GroupFilter

instance ToJSON GroupFilter where
  toJSON GroupFilter' {..} =
    object
      ( catMaybes
          [Just ("Name" .= _gfName), Just ("Values" .= _gfValues)]
      )
