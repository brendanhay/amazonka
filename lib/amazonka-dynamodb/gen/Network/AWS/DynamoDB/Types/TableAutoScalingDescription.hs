{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.TableAutoScalingDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.TableAutoScalingDescription where

import Network.AWS.DynamoDB.Types.ReplicaAutoScalingDescription
import Network.AWS.DynamoDB.Types.TableStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the auto scaling configuration for a global table.
--
--
--
-- /See:/ 'tableAutoScalingDescription' smart constructor.
data TableAutoScalingDescription = TableAutoScalingDescription'
  { _tasdTableStatus ::
      !(Maybe TableStatus),
    _tasdReplicas ::
      !( Maybe
           [ReplicaAutoScalingDescription]
       ),
    _tasdTableName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TableAutoScalingDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tasdTableStatus' - The current state of the table:     * @CREATING@ - The table is being created.     * @UPDATING@ - The table is being updated.     * @DELETING@ - The table is being deleted.     * @ACTIVE@ - The table is ready for use.
--
-- * 'tasdReplicas' - Represents replicas of the global table.
--
-- * 'tasdTableName' - The name of the table.
tableAutoScalingDescription ::
  TableAutoScalingDescription
tableAutoScalingDescription =
  TableAutoScalingDescription'
    { _tasdTableStatus = Nothing,
      _tasdReplicas = Nothing,
      _tasdTableName = Nothing
    }

-- | The current state of the table:     * @CREATING@ - The table is being created.     * @UPDATING@ - The table is being updated.     * @DELETING@ - The table is being deleted.     * @ACTIVE@ - The table is ready for use.
tasdTableStatus :: Lens' TableAutoScalingDescription (Maybe TableStatus)
tasdTableStatus = lens _tasdTableStatus (\s a -> s {_tasdTableStatus = a})

-- | Represents replicas of the global table.
tasdReplicas :: Lens' TableAutoScalingDescription [ReplicaAutoScalingDescription]
tasdReplicas = lens _tasdReplicas (\s a -> s {_tasdReplicas = a}) . _Default . _Coerce

-- | The name of the table.
tasdTableName :: Lens' TableAutoScalingDescription (Maybe Text)
tasdTableName = lens _tasdTableName (\s a -> s {_tasdTableName = a})

instance FromJSON TableAutoScalingDescription where
  parseJSON =
    withObject
      "TableAutoScalingDescription"
      ( \x ->
          TableAutoScalingDescription'
            <$> (x .:? "TableStatus")
            <*> (x .:? "Replicas" .!= mempty)
            <*> (x .:? "TableName")
      )

instance Hashable TableAutoScalingDescription

instance NFData TableAutoScalingDescription
