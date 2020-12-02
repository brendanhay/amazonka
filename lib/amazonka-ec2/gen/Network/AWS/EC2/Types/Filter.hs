{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Filter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Filter where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A filter name and value pair that is used to return a more specific list of results from a describe operation. Filters can be used to match a set of resources by specific criteria, such as tags, attributes, or IDs. The filters supported by a describe operation are documented with the describe operation. For example:
--
--
--     * 'DescribeAvailabilityZones'
--
--     * 'DescribeImages'
--
--     * 'DescribeInstances'
--
--     * 'DescribeKeyPairs'
--
--     * 'DescribeSecurityGroups'
--
--     * 'DescribeSnapshots'
--
--     * 'DescribeSubnets'
--
--     * 'DescribeTags'
--
--     * 'DescribeVolumes'
--
--     * 'DescribeVpcs'
--
--
--
--
-- /See:/ 'filter'' smart constructor.
data Filter = Filter' {_fValues :: !(Maybe [Text]), _fName :: !Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Filter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fValues' - The filter values. Filter values are case-sensitive.
--
-- * 'fName' - The name of the filter. Filter names are case-sensitive.
filter' ::
  -- | 'fName'
  Text ->
  Filter
filter' pName_ = Filter' {_fValues = Nothing, _fName = pName_}

-- | The filter values. Filter values are case-sensitive.
fValues :: Lens' Filter [Text]
fValues = lens _fValues (\s a -> s {_fValues = a}) . _Default . _Coerce

-- | The name of the filter. Filter names are case-sensitive.
fName :: Lens' Filter Text
fName = lens _fName (\s a -> s {_fName = a})

instance Hashable Filter

instance NFData Filter

instance ToQuery Filter where
  toQuery Filter' {..} =
    mconcat
      [toQuery (toQueryList "Value" <$> _fValues), "Name" =: _fName]
