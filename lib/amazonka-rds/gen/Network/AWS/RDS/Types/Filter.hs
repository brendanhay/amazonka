{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.Filter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.Filter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A filter name and value pair that is used to return a more specific list of results from a describe operation. Filters can be used to match a set of resources by specific criteria, such as IDs. The filters supported by a describe operation are documented with the describe operation.
--
--
-- The following actions can be filtered:
--
--     * @DescribeDBClusterBacktracks@
--
--     * @DescribeDBClusterEndpoints@
--
--     * @DescribeDBClusters@
--
--     * @DescribeDBInstances@
--
--     * @DescribePendingMaintenanceActions@
--
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
-- * 'fName' - The name of the filter. Filter names are case-sensitive.
--
-- * 'fValues' - One or more filter values. Filter values are case-sensitive.
filter' ::
  -- | 'fName'
  Text ->
  Filter
filter' pName_ = Filter' {_fName = pName_, _fValues = mempty}

-- | The name of the filter. Filter names are case-sensitive.
fName :: Lens' Filter Text
fName = lens _fName (\s a -> s {_fName = a})

-- | One or more filter values. Filter values are case-sensitive.
fValues :: Lens' Filter [Text]
fValues = lens _fValues (\s a -> s {_fValues = a}) . _Coerce

instance Hashable Filter

instance NFData Filter

instance ToQuery Filter where
  toQuery Filter' {..} =
    mconcat
      ["Name" =: _fName, "Values" =: toQueryList "Value" _fValues]
