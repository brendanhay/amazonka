{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexDescription where

import Network.AWS.DynamoDB.Types.ProvisionedThroughputOverride
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the properties of a replica global secondary index.
--
--
--
-- /See:/ 'replicaGlobalSecondaryIndexDescription' smart constructor.
data ReplicaGlobalSecondaryIndexDescription = ReplicaGlobalSecondaryIndexDescription'
  { _rgsidProvisionedThroughputOverride ::
      !( Maybe
           ProvisionedThroughputOverride
       ),
    _rgsidIndexName ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReplicaGlobalSecondaryIndexDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgsidProvisionedThroughputOverride' - If not described, uses the source table GSI's read capacity settings.
--
-- * 'rgsidIndexName' - The name of the global secondary index.
replicaGlobalSecondaryIndexDescription ::
  ReplicaGlobalSecondaryIndexDescription
replicaGlobalSecondaryIndexDescription =
  ReplicaGlobalSecondaryIndexDescription'
    { _rgsidProvisionedThroughputOverride =
        Nothing,
      _rgsidIndexName = Nothing
    }

-- | If not described, uses the source table GSI's read capacity settings.
rgsidProvisionedThroughputOverride :: Lens' ReplicaGlobalSecondaryIndexDescription (Maybe ProvisionedThroughputOverride)
rgsidProvisionedThroughputOverride = lens _rgsidProvisionedThroughputOverride (\s a -> s {_rgsidProvisionedThroughputOverride = a})

-- | The name of the global secondary index.
rgsidIndexName :: Lens' ReplicaGlobalSecondaryIndexDescription (Maybe Text)
rgsidIndexName = lens _rgsidIndexName (\s a -> s {_rgsidIndexName = a})

instance FromJSON ReplicaGlobalSecondaryIndexDescription where
  parseJSON =
    withObject
      "ReplicaGlobalSecondaryIndexDescription"
      ( \x ->
          ReplicaGlobalSecondaryIndexDescription'
            <$> (x .:? "ProvisionedThroughputOverride") <*> (x .:? "IndexName")
      )

instance Hashable ReplicaGlobalSecondaryIndexDescription

instance NFData ReplicaGlobalSecondaryIndexDescription
