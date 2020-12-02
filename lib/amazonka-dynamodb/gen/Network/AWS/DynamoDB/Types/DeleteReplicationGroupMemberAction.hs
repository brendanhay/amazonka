{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.DeleteReplicationGroupMemberAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.DeleteReplicationGroupMemberAction where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a replica to be deleted.
--
--
--
-- /See:/ 'deleteReplicationGroupMemberAction' smart constructor.
newtype DeleteReplicationGroupMemberAction = DeleteReplicationGroupMemberAction'
  { _drgmaRegionName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteReplicationGroupMemberAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drgmaRegionName' - The Region where the replica exists.
deleteReplicationGroupMemberAction ::
  -- | 'drgmaRegionName'
  Text ->
  DeleteReplicationGroupMemberAction
deleteReplicationGroupMemberAction pRegionName_ =
  DeleteReplicationGroupMemberAction'
    { _drgmaRegionName =
        pRegionName_
    }

-- | The Region where the replica exists.
drgmaRegionName :: Lens' DeleteReplicationGroupMemberAction Text
drgmaRegionName = lens _drgmaRegionName (\s a -> s {_drgmaRegionName = a})

instance Hashable DeleteReplicationGroupMemberAction

instance NFData DeleteReplicationGroupMemberAction

instance ToJSON DeleteReplicationGroupMemberAction where
  toJSON DeleteReplicationGroupMemberAction' {..} =
    object (catMaybes [Just ("RegionName" .= _drgmaRegionName)])
