{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.GroupedResourceCount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.GroupedResourceCount where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The count of resources that are grouped by the group name.
--
--
--
-- /See:/ 'groupedResourceCount' smart constructor.
data GroupedResourceCount = GroupedResourceCount'
  { _grcGroupName ::
      !Text,
    _grcResourceCount :: !Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GroupedResourceCount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grcGroupName' - The name of the group that can be region, account ID, or resource type. For example, region1, region2 if the region was chosen as @GroupByKey@ .
--
-- * 'grcResourceCount' - The number of resources in the group.
groupedResourceCount ::
  -- | 'grcGroupName'
  Text ->
  -- | 'grcResourceCount'
  Integer ->
  GroupedResourceCount
groupedResourceCount pGroupName_ pResourceCount_ =
  GroupedResourceCount'
    { _grcGroupName = pGroupName_,
      _grcResourceCount = pResourceCount_
    }

-- | The name of the group that can be region, account ID, or resource type. For example, region1, region2 if the region was chosen as @GroupByKey@ .
grcGroupName :: Lens' GroupedResourceCount Text
grcGroupName = lens _grcGroupName (\s a -> s {_grcGroupName = a})

-- | The number of resources in the group.
grcResourceCount :: Lens' GroupedResourceCount Integer
grcResourceCount = lens _grcResourceCount (\s a -> s {_grcResourceCount = a})

instance FromJSON GroupedResourceCount where
  parseJSON =
    withObject
      "GroupedResourceCount"
      ( \x ->
          GroupedResourceCount'
            <$> (x .: "GroupName") <*> (x .: "ResourceCount")
      )

instance Hashable GroupedResourceCount

instance NFData GroupedResourceCount
