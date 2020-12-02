{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.ResourceFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.ResourceFilter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ResourceGroups.Types.ResourceFilterName

-- | A filter name and value pair that is used to obtain more specific results from a list of resources.
--
--
--
-- /See:/ 'resourceFilter' smart constructor.
data ResourceFilter = ResourceFilter'
  { _rfName ::
      !ResourceFilterName,
    _rfValues :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rfName' - The name of the filter. Filter names are case-sensitive.
--
-- * 'rfValues' - One or more filter values. Allowed filter values vary by resource filter name, and are case-sensitive.
resourceFilter ::
  -- | 'rfName'
  ResourceFilterName ->
  -- | 'rfValues'
  NonEmpty Text ->
  ResourceFilter
resourceFilter pName_ pValues_ =
  ResourceFilter' {_rfName = pName_, _rfValues = _List1 # pValues_}

-- | The name of the filter. Filter names are case-sensitive.
rfName :: Lens' ResourceFilter ResourceFilterName
rfName = lens _rfName (\s a -> s {_rfName = a})

-- | One or more filter values. Allowed filter values vary by resource filter name, and are case-sensitive.
rfValues :: Lens' ResourceFilter (NonEmpty Text)
rfValues = lens _rfValues (\s a -> s {_rfValues = a}) . _List1

instance Hashable ResourceFilter

instance NFData ResourceFilter

instance ToJSON ResourceFilter where
  toJSON ResourceFilter' {..} =
    object
      ( catMaybes
          [Just ("Name" .= _rfName), Just ("Values" .= _rfValues)]
      )
