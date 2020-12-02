{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.GroupQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.GroupQuery where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ResourceGroups.Types.ResourceQuery

-- | A mapping of a query attached to a resource group that determines the AWS resources that are members of the group.
--
--
--
-- /See:/ 'groupQuery' smart constructor.
data GroupQuery = GroupQuery'
  { _gqGroupName :: !Text,
    _gqResourceQuery :: !ResourceQuery
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GroupQuery' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gqGroupName' - The name of the resource group that is associated with the specified resource query.
--
-- * 'gqResourceQuery' - The resource query that determines which AWS resources are members of the associated resource group.
groupQuery ::
  -- | 'gqGroupName'
  Text ->
  -- | 'gqResourceQuery'
  ResourceQuery ->
  GroupQuery
groupQuery pGroupName_ pResourceQuery_ =
  GroupQuery'
    { _gqGroupName = pGroupName_,
      _gqResourceQuery = pResourceQuery_
    }

-- | The name of the resource group that is associated with the specified resource query.
gqGroupName :: Lens' GroupQuery Text
gqGroupName = lens _gqGroupName (\s a -> s {_gqGroupName = a})

-- | The resource query that determines which AWS resources are members of the associated resource group.
gqResourceQuery :: Lens' GroupQuery ResourceQuery
gqResourceQuery = lens _gqResourceQuery (\s a -> s {_gqResourceQuery = a})

instance FromJSON GroupQuery where
  parseJSON =
    withObject
      "GroupQuery"
      ( \x ->
          GroupQuery' <$> (x .: "GroupName") <*> (x .: "ResourceQuery")
      )

instance Hashable GroupQuery

instance NFData GroupQuery
