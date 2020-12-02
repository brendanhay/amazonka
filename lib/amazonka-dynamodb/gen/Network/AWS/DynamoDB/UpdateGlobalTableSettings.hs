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
-- Module      : Network.AWS.DynamoDB.UpdateGlobalTableSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates settings for a global table.
module Network.AWS.DynamoDB.UpdateGlobalTableSettings
  ( -- * Creating a Request
    updateGlobalTableSettings,
    UpdateGlobalTableSettings,

    -- * Request Lenses
    ugtsGlobalTableProvisionedWriteCapacityAutoScalingSettingsUpdate,
    ugtsGlobalTableBillingMode,
    ugtsGlobalTableProvisionedWriteCapacityUnits,
    ugtsReplicaSettingsUpdate,
    ugtsGlobalTableGlobalSecondaryIndexSettingsUpdate,
    ugtsGlobalTableName,

    -- * Destructuring the Response
    updateGlobalTableSettingsResponse,
    UpdateGlobalTableSettingsResponse,

    -- * Response Lenses
    ugtsrsReplicaSettings,
    ugtsrsGlobalTableName,
    ugtsrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateGlobalTableSettings' smart constructor.
data UpdateGlobalTableSettings = UpdateGlobalTableSettings'
  { _ugtsGlobalTableProvisionedWriteCapacityAutoScalingSettingsUpdate ::
      !(Maybe AutoScalingSettingsUpdate),
    _ugtsGlobalTableBillingMode ::
      !(Maybe BillingMode),
    _ugtsGlobalTableProvisionedWriteCapacityUnits ::
      !(Maybe Nat),
    _ugtsReplicaSettingsUpdate ::
      !(Maybe (List1 ReplicaSettingsUpdate)),
    _ugtsGlobalTableGlobalSecondaryIndexSettingsUpdate ::
      !( Maybe
           ( List1
               GlobalTableGlobalSecondaryIndexSettingsUpdate
           )
       ),
    _ugtsGlobalTableName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateGlobalTableSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugtsGlobalTableProvisionedWriteCapacityAutoScalingSettingsUpdate' - Auto scaling settings for managing provisioned write capacity for the global table.
--
-- * 'ugtsGlobalTableBillingMode' - The billing mode of the global table. If @GlobalTableBillingMode@ is not specified, the global table defaults to @PROVISIONED@ capacity billing mode.     * @PROVISIONED@ - We recommend using @PROVISIONED@ for predictable workloads. @PROVISIONED@ sets the billing mode to <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.ProvisionedThroughput.Manual Provisioned Mode> .     * @PAY_PER_REQUEST@ - We recommend using @PAY_PER_REQUEST@ for unpredictable workloads. @PAY_PER_REQUEST@ sets the billing mode to <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.OnDemand On-Demand Mode> .
--
-- * 'ugtsGlobalTableProvisionedWriteCapacityUnits' - The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException.@
--
-- * 'ugtsReplicaSettingsUpdate' - Represents the settings for a global table in a Region that will be modified.
--
-- * 'ugtsGlobalTableGlobalSecondaryIndexSettingsUpdate' - Represents the settings of a global secondary index for a global table that will be modified.
--
-- * 'ugtsGlobalTableName' - The name of the global table
updateGlobalTableSettings ::
  -- | 'ugtsGlobalTableName'
  Text ->
  UpdateGlobalTableSettings
updateGlobalTableSettings pGlobalTableName_ =
  UpdateGlobalTableSettings'
    { _ugtsGlobalTableProvisionedWriteCapacityAutoScalingSettingsUpdate =
        Nothing,
      _ugtsGlobalTableBillingMode = Nothing,
      _ugtsGlobalTableProvisionedWriteCapacityUnits = Nothing,
      _ugtsReplicaSettingsUpdate = Nothing,
      _ugtsGlobalTableGlobalSecondaryIndexSettingsUpdate = Nothing,
      _ugtsGlobalTableName = pGlobalTableName_
    }

-- | Auto scaling settings for managing provisioned write capacity for the global table.
ugtsGlobalTableProvisionedWriteCapacityAutoScalingSettingsUpdate :: Lens' UpdateGlobalTableSettings (Maybe AutoScalingSettingsUpdate)
ugtsGlobalTableProvisionedWriteCapacityAutoScalingSettingsUpdate = lens _ugtsGlobalTableProvisionedWriteCapacityAutoScalingSettingsUpdate (\s a -> s {_ugtsGlobalTableProvisionedWriteCapacityAutoScalingSettingsUpdate = a})

-- | The billing mode of the global table. If @GlobalTableBillingMode@ is not specified, the global table defaults to @PROVISIONED@ capacity billing mode.     * @PROVISIONED@ - We recommend using @PROVISIONED@ for predictable workloads. @PROVISIONED@ sets the billing mode to <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.ProvisionedThroughput.Manual Provisioned Mode> .     * @PAY_PER_REQUEST@ - We recommend using @PAY_PER_REQUEST@ for unpredictable workloads. @PAY_PER_REQUEST@ sets the billing mode to <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.OnDemand On-Demand Mode> .
ugtsGlobalTableBillingMode :: Lens' UpdateGlobalTableSettings (Maybe BillingMode)
ugtsGlobalTableBillingMode = lens _ugtsGlobalTableBillingMode (\s a -> s {_ugtsGlobalTableBillingMode = a})

-- | The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException.@
ugtsGlobalTableProvisionedWriteCapacityUnits :: Lens' UpdateGlobalTableSettings (Maybe Natural)
ugtsGlobalTableProvisionedWriteCapacityUnits = lens _ugtsGlobalTableProvisionedWriteCapacityUnits (\s a -> s {_ugtsGlobalTableProvisionedWriteCapacityUnits = a}) . mapping _Nat

-- | Represents the settings for a global table in a Region that will be modified.
ugtsReplicaSettingsUpdate :: Lens' UpdateGlobalTableSettings (Maybe (NonEmpty ReplicaSettingsUpdate))
ugtsReplicaSettingsUpdate = lens _ugtsReplicaSettingsUpdate (\s a -> s {_ugtsReplicaSettingsUpdate = a}) . mapping _List1

-- | Represents the settings of a global secondary index for a global table that will be modified.
ugtsGlobalTableGlobalSecondaryIndexSettingsUpdate :: Lens' UpdateGlobalTableSettings (Maybe (NonEmpty GlobalTableGlobalSecondaryIndexSettingsUpdate))
ugtsGlobalTableGlobalSecondaryIndexSettingsUpdate = lens _ugtsGlobalTableGlobalSecondaryIndexSettingsUpdate (\s a -> s {_ugtsGlobalTableGlobalSecondaryIndexSettingsUpdate = a}) . mapping _List1

-- | The name of the global table
ugtsGlobalTableName :: Lens' UpdateGlobalTableSettings Text
ugtsGlobalTableName = lens _ugtsGlobalTableName (\s a -> s {_ugtsGlobalTableName = a})

instance AWSRequest UpdateGlobalTableSettings where
  type
    Rs UpdateGlobalTableSettings =
      UpdateGlobalTableSettingsResponse
  request = postJSON dynamoDB
  response =
    receiveJSON
      ( \s h x ->
          UpdateGlobalTableSettingsResponse'
            <$> (x .?> "ReplicaSettings" .!@ mempty)
            <*> (x .?> "GlobalTableName")
            <*> (pure (fromEnum s))
      )

instance Hashable UpdateGlobalTableSettings

instance NFData UpdateGlobalTableSettings

instance ToHeaders UpdateGlobalTableSettings where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DynamoDB_20120810.UpdateGlobalTableSettings" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.0" :: ByteString)
          ]
      )

instance ToJSON UpdateGlobalTableSettings where
  toJSON UpdateGlobalTableSettings' {..} =
    object
      ( catMaybes
          [ ( "GlobalTableProvisionedWriteCapacityAutoScalingSettingsUpdate"
                .=
            )
              <$> _ugtsGlobalTableProvisionedWriteCapacityAutoScalingSettingsUpdate,
            ("GlobalTableBillingMode" .=) <$> _ugtsGlobalTableBillingMode,
            ("GlobalTableProvisionedWriteCapacityUnits" .=)
              <$> _ugtsGlobalTableProvisionedWriteCapacityUnits,
            ("ReplicaSettingsUpdate" .=) <$> _ugtsReplicaSettingsUpdate,
            ("GlobalTableGlobalSecondaryIndexSettingsUpdate" .=)
              <$> _ugtsGlobalTableGlobalSecondaryIndexSettingsUpdate,
            Just ("GlobalTableName" .= _ugtsGlobalTableName)
          ]
      )

instance ToPath UpdateGlobalTableSettings where
  toPath = const "/"

instance ToQuery UpdateGlobalTableSettings where
  toQuery = const mempty

-- | /See:/ 'updateGlobalTableSettingsResponse' smart constructor.
data UpdateGlobalTableSettingsResponse = UpdateGlobalTableSettingsResponse'
  { _ugtsrsReplicaSettings ::
      !( Maybe
           [ReplicaSettingsDescription]
       ),
    _ugtsrsGlobalTableName ::
      !(Maybe Text),
    _ugtsrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateGlobalTableSettingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugtsrsReplicaSettings' - The Region-specific settings for the global table.
--
-- * 'ugtsrsGlobalTableName' - The name of the global table.
--
-- * 'ugtsrsResponseStatus' - -- | The response status code.
updateGlobalTableSettingsResponse ::
  -- | 'ugtsrsResponseStatus'
  Int ->
  UpdateGlobalTableSettingsResponse
updateGlobalTableSettingsResponse pResponseStatus_ =
  UpdateGlobalTableSettingsResponse'
    { _ugtsrsReplicaSettings =
        Nothing,
      _ugtsrsGlobalTableName = Nothing,
      _ugtsrsResponseStatus = pResponseStatus_
    }

-- | The Region-specific settings for the global table.
ugtsrsReplicaSettings :: Lens' UpdateGlobalTableSettingsResponse [ReplicaSettingsDescription]
ugtsrsReplicaSettings = lens _ugtsrsReplicaSettings (\s a -> s {_ugtsrsReplicaSettings = a}) . _Default . _Coerce

-- | The name of the global table.
ugtsrsGlobalTableName :: Lens' UpdateGlobalTableSettingsResponse (Maybe Text)
ugtsrsGlobalTableName = lens _ugtsrsGlobalTableName (\s a -> s {_ugtsrsGlobalTableName = a})

-- | -- | The response status code.
ugtsrsResponseStatus :: Lens' UpdateGlobalTableSettingsResponse Int
ugtsrsResponseStatus = lens _ugtsrsResponseStatus (\s a -> s {_ugtsrsResponseStatus = a})

instance NFData UpdateGlobalTableSettingsResponse
