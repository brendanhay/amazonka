{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.DeleteReplicaAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.DeleteReplicaAction where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a replica to be removed.
--
--
--
-- /See:/ 'deleteReplicaAction' smart constructor.
newtype DeleteReplicaAction = DeleteReplicaAction'
  { _draRegionName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteReplicaAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'draRegionName' - The Region of the replica to be removed.
deleteReplicaAction ::
  -- | 'draRegionName'
  Text ->
  DeleteReplicaAction
deleteReplicaAction pRegionName_ =
  DeleteReplicaAction' {_draRegionName = pRegionName_}

-- | The Region of the replica to be removed.
draRegionName :: Lens' DeleteReplicaAction Text
draRegionName = lens _draRegionName (\s a -> s {_draRegionName = a})

instance Hashable DeleteReplicaAction

instance NFData DeleteReplicaAction

instance ToJSON DeleteReplicaAction where
  toJSON DeleteReplicaAction' {..} =
    object (catMaybes [Just ("RegionName" .= _draRegionName)])
