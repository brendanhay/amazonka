{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.Filter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.Filter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Identifies the name and value of a filter object. This filter is used to limit the number and type of AWS DMS objects that are returned for a particular @Describe*@ call or similar operation. Filters are used as an optional parameter to the following APIs.
--
--
--
-- /See:/ 'filter'' smart constructor.
data Filter = Filter' {_fName :: !Text, _fValues :: ![Text]}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Filter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fName' - The name of the filter as specified for a @Describe*@ or similar operation.
--
-- * 'fValues' - The filter value, which can specify one or more values used to narrow the returned results.
filter' ::
  -- | 'fName'
  Text ->
  Filter
filter' pName_ = Filter' {_fName = pName_, _fValues = mempty}

-- | The name of the filter as specified for a @Describe*@ or similar operation.
fName :: Lens' Filter Text
fName = lens _fName (\s a -> s {_fName = a})

-- | The filter value, which can specify one or more values used to narrow the returned results.
fValues :: Lens' Filter [Text]
fValues = lens _fValues (\s a -> s {_fValues = a}) . _Coerce

instance Hashable Filter

instance NFData Filter

instance ToJSON Filter where
  toJSON Filter' {..} =
    object
      (catMaybes [Just ("Name" .= _fName), Just ("Values" .= _fValues)])
