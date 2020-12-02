{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.UpdateTableReplicaAutoScaling
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates auto scaling settings on your global tables at once.
module Network.AWS.DynamoDB.UpdateTableReplicaAutoScaling
  ( -- * Creating a Request
    updateTableReplicaAutoScaling,
    UpdateTableReplicaAutoScaling,

    -- * Request Lenses
    utrasReplicaUpdates,
    utrasProvisionedWriteCapacityAutoScalingUpdate,
    utrasGlobalSecondaryIndexUpdates,
    utrasTableName,

    -- * Destructuring the Response
    updateTableReplicaAutoScalingResponse,
    UpdateTableReplicaAutoScalingResponse,

    -- * Response Lenses
    utrasrsTableAutoScalingDescription,
    utrasrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateTableReplicaAutoScaling' smart constructor.
data UpdateTableReplicaAutoScaling = UpdateTableReplicaAutoScaling'
  { _utrasReplicaUpdates ::
      !( Maybe
           ( List1
               ReplicaAutoScalingUpdate
           )
       ),
    _utrasProvisionedWriteCapacityAutoScalingUpdate ::
      !( Maybe
           AutoScalingSettingsUpdate
       ),
    _utrasGlobalSecondaryIndexUpdates ::
      !( Maybe
           ( List1
               GlobalSecondaryIndexAutoScalingUpdate
           )
       ),
    _utrasTableName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateTableReplicaAutoScaling' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utrasReplicaUpdates' - Represents the auto scaling settings of replicas of the table that will be modified.
--
-- * 'utrasProvisionedWriteCapacityAutoScalingUpdate' - Undocumented member.
--
-- * 'utrasGlobalSecondaryIndexUpdates' - Represents the auto scaling settings of the global secondary indexes of the replica to be updated.
--
-- * 'utrasTableName' - The name of the global table to be updated.
updateTableReplicaAutoScaling ::
  -- | 'utrasTableName'
  Text ->
  UpdateTableReplicaAutoScaling
updateTableReplicaAutoScaling pTableName_ =
  UpdateTableReplicaAutoScaling'
    { _utrasReplicaUpdates = Nothing,
      _utrasProvisionedWriteCapacityAutoScalingUpdate = Nothing,
      _utrasGlobalSecondaryIndexUpdates = Nothing,
      _utrasTableName = pTableName_
    }

-- | Represents the auto scaling settings of replicas of the table that will be modified.
utrasReplicaUpdates :: Lens' UpdateTableReplicaAutoScaling (Maybe (NonEmpty ReplicaAutoScalingUpdate))
utrasReplicaUpdates = lens _utrasReplicaUpdates (\s a -> s {_utrasReplicaUpdates = a}) . mapping _List1

-- | Undocumented member.
utrasProvisionedWriteCapacityAutoScalingUpdate :: Lens' UpdateTableReplicaAutoScaling (Maybe AutoScalingSettingsUpdate)
utrasProvisionedWriteCapacityAutoScalingUpdate = lens _utrasProvisionedWriteCapacityAutoScalingUpdate (\s a -> s {_utrasProvisionedWriteCapacityAutoScalingUpdate = a})

-- | Represents the auto scaling settings of the global secondary indexes of the replica to be updated.
utrasGlobalSecondaryIndexUpdates :: Lens' UpdateTableReplicaAutoScaling (Maybe (NonEmpty GlobalSecondaryIndexAutoScalingUpdate))
utrasGlobalSecondaryIndexUpdates = lens _utrasGlobalSecondaryIndexUpdates (\s a -> s {_utrasGlobalSecondaryIndexUpdates = a}) . mapping _List1

-- | The name of the global table to be updated.
utrasTableName :: Lens' UpdateTableReplicaAutoScaling Text
utrasTableName = lens _utrasTableName (\s a -> s {_utrasTableName = a})

instance AWSRequest UpdateTableReplicaAutoScaling where
  type
    Rs UpdateTableReplicaAutoScaling =
      UpdateTableReplicaAutoScalingResponse
  request = postJSON dynamoDB
  response =
    receiveJSON
      ( \s h x ->
          UpdateTableReplicaAutoScalingResponse'
            <$> (x .?> "TableAutoScalingDescription") <*> (pure (fromEnum s))
      )

instance Hashable UpdateTableReplicaAutoScaling

instance NFData UpdateTableReplicaAutoScaling

instance ToHeaders UpdateTableReplicaAutoScaling where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DynamoDB_20120810.UpdateTableReplicaAutoScaling" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.0" :: ByteString)
          ]
      )

instance ToJSON UpdateTableReplicaAutoScaling where
  toJSON UpdateTableReplicaAutoScaling' {..} =
    object
      ( catMaybes
          [ ("ReplicaUpdates" .=) <$> _utrasReplicaUpdates,
            ("ProvisionedWriteCapacityAutoScalingUpdate" .=)
              <$> _utrasProvisionedWriteCapacityAutoScalingUpdate,
            ("GlobalSecondaryIndexUpdates" .=)
              <$> _utrasGlobalSecondaryIndexUpdates,
            Just ("TableName" .= _utrasTableName)
          ]
      )

instance ToPath UpdateTableReplicaAutoScaling where
  toPath = const "/"

instance ToQuery UpdateTableReplicaAutoScaling where
  toQuery = const mempty

-- | /See:/ 'updateTableReplicaAutoScalingResponse' smart constructor.
data UpdateTableReplicaAutoScalingResponse = UpdateTableReplicaAutoScalingResponse'
  { _utrasrsTableAutoScalingDescription ::
      !( Maybe
           TableAutoScalingDescription
       ),
    _utrasrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateTableReplicaAutoScalingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utrasrsTableAutoScalingDescription' - Returns information about the auto scaling settings of a table with replicas.
--
-- * 'utrasrsResponseStatus' - -- | The response status code.
updateTableReplicaAutoScalingResponse ::
  -- | 'utrasrsResponseStatus'
  Int ->
  UpdateTableReplicaAutoScalingResponse
updateTableReplicaAutoScalingResponse pResponseStatus_ =
  UpdateTableReplicaAutoScalingResponse'
    { _utrasrsTableAutoScalingDescription =
        Nothing,
      _utrasrsResponseStatus = pResponseStatus_
    }

-- | Returns information about the auto scaling settings of a table with replicas.
utrasrsTableAutoScalingDescription :: Lens' UpdateTableReplicaAutoScalingResponse (Maybe TableAutoScalingDescription)
utrasrsTableAutoScalingDescription = lens _utrasrsTableAutoScalingDescription (\s a -> s {_utrasrsTableAutoScalingDescription = a})

-- | -- | The response status code.
utrasrsResponseStatus :: Lens' UpdateTableReplicaAutoScalingResponse Int
utrasrsResponseStatus = lens _utrasrsResponseStatus (\s a -> s {_utrasrsResponseStatus = a})

instance NFData UpdateTableReplicaAutoScalingResponse
