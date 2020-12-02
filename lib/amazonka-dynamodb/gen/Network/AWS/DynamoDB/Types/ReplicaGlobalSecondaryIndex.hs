{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndex where

import Network.AWS.DynamoDB.Types.ProvisionedThroughputOverride
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the properties of a replica global secondary index.
--
--
--
-- /See:/ 'replicaGlobalSecondaryIndex' smart constructor.
data ReplicaGlobalSecondaryIndex = ReplicaGlobalSecondaryIndex'
  { _rgsiProvisionedThroughputOverride ::
      !( Maybe
           ProvisionedThroughputOverride
       ),
    _rgsiIndexName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReplicaGlobalSecondaryIndex' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgsiProvisionedThroughputOverride' - Replica table GSI-specific provisioned throughput. If not specified, uses the source table GSI's read capacity settings.
--
-- * 'rgsiIndexName' - The name of the global secondary index.
replicaGlobalSecondaryIndex ::
  -- | 'rgsiIndexName'
  Text ->
  ReplicaGlobalSecondaryIndex
replicaGlobalSecondaryIndex pIndexName_ =
  ReplicaGlobalSecondaryIndex'
    { _rgsiProvisionedThroughputOverride =
        Nothing,
      _rgsiIndexName = pIndexName_
    }

-- | Replica table GSI-specific provisioned throughput. If not specified, uses the source table GSI's read capacity settings.
rgsiProvisionedThroughputOverride :: Lens' ReplicaGlobalSecondaryIndex (Maybe ProvisionedThroughputOverride)
rgsiProvisionedThroughputOverride = lens _rgsiProvisionedThroughputOverride (\s a -> s {_rgsiProvisionedThroughputOverride = a})

-- | The name of the global secondary index.
rgsiIndexName :: Lens' ReplicaGlobalSecondaryIndex Text
rgsiIndexName = lens _rgsiIndexName (\s a -> s {_rgsiIndexName = a})

instance Hashable ReplicaGlobalSecondaryIndex

instance NFData ReplicaGlobalSecondaryIndex

instance ToJSON ReplicaGlobalSecondaryIndex where
  toJSON ReplicaGlobalSecondaryIndex' {..} =
    object
      ( catMaybes
          [ ("ProvisionedThroughputOverride" .=)
              <$> _rgsiProvisionedThroughputOverride,
            Just ("IndexName" .= _rgsiIndexName)
          ]
      )
