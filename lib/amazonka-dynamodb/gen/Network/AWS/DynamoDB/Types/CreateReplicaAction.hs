{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.CreateReplicaAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.CreateReplicaAction where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a replica to be added.
--
--
--
-- /See:/ 'createReplicaAction' smart constructor.
newtype CreateReplicaAction = CreateReplicaAction'
  { _craRegionName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateReplicaAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'craRegionName' - The Region of the replica to be added.
createReplicaAction ::
  -- | 'craRegionName'
  Text ->
  CreateReplicaAction
createReplicaAction pRegionName_ =
  CreateReplicaAction' {_craRegionName = pRegionName_}

-- | The Region of the replica to be added.
craRegionName :: Lens' CreateReplicaAction Text
craRegionName = lens _craRegionName (\s a -> s {_craRegionName = a})

instance Hashable CreateReplicaAction

instance NFData CreateReplicaAction

instance ToJSON CreateReplicaAction where
  toJSON CreateReplicaAction' {..} =
    object (catMaybes [Just ("RegionName" .= _craRegionName)])
