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
-- Module      : Network.AWS.DynamoDB.RestoreTableToPointInTime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores the specified table to the specified point in time within @EarliestRestorableDateTime@ and @LatestRestorableDateTime@ . You can restore your table to any point in time during the last 35 days. Any number of users can execute up to 4 concurrent restores (any type of restore) in a given account.
--
--
-- When you restore using point in time recovery, DynamoDB restores your table data to the state based on the selected date and time (day:hour:minute:second) to a new table.
--
-- Along with data, the following are also included on the new restored table using point in time recovery:
--
--     * Global secondary indexes (GSIs)
--
--     * Local secondary indexes (LSIs)
--
--     * Provisioned read and write capacity
--
--     * Encryption settings
--
-- /Important:/ All these settings come from the current settings of the source table at the time of restore.
--
--
--
-- You must manually set up the following on the restored table:
--
--     * Auto scaling policies
--
--     * IAM policies
--
--     * Amazon CloudWatch metrics and alarms
--
--     * Tags
--
--     * Stream settings
--
--     * Time to Live (TTL) settings
--
--     * Point in time recovery settings
module Network.AWS.DynamoDB.RestoreTableToPointInTime
  ( -- * Creating a Request
    restoreTableToPointInTime,
    RestoreTableToPointInTime,

    -- * Request Lenses
    rttpitBillingModeOverride,
    rttpitUseLatestRestorableTime,
    rttpitGlobalSecondaryIndexOverride,
    rttpitProvisionedThroughputOverride,
    rttpitSourceTableARN,
    rttpitSSESpecificationOverride,
    rttpitSourceTableName,
    rttpitLocalSecondaryIndexOverride,
    rttpitRestoreDateTime,
    rttpitTargetTableName,

    -- * Destructuring the Response
    restoreTableToPointInTimeResponse,
    RestoreTableToPointInTimeResponse,

    -- * Response Lenses
    rttpitrsTableDescription,
    rttpitrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'restoreTableToPointInTime' smart constructor.
data RestoreTableToPointInTime = RestoreTableToPointInTime'
  { _rttpitBillingModeOverride ::
      !(Maybe BillingMode),
    _rttpitUseLatestRestorableTime ::
      !(Maybe Bool),
    _rttpitGlobalSecondaryIndexOverride ::
      !(Maybe [GlobalSecondaryIndex]),
    _rttpitProvisionedThroughputOverride ::
      !(Maybe ProvisionedThroughput),
    _rttpitSourceTableARN :: !(Maybe Text),
    _rttpitSSESpecificationOverride ::
      !(Maybe SSESpecification),
    _rttpitSourceTableName :: !(Maybe Text),
    _rttpitLocalSecondaryIndexOverride ::
      !(Maybe [LocalSecondaryIndex]),
    _rttpitRestoreDateTime ::
      !(Maybe POSIX),
    _rttpitTargetTableName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RestoreTableToPointInTime' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rttpitBillingModeOverride' - The billing mode of the restored table.
--
-- * 'rttpitUseLatestRestorableTime' - Restore the table to the latest possible time. @LatestRestorableDateTime@ is typically 5 minutes before the current time.
--
-- * 'rttpitGlobalSecondaryIndexOverride' - List of global secondary indexes for the restored table. The indexes provided should match existing secondary indexes. You can choose to exclude some or all of the indexes at the time of restore.
--
-- * 'rttpitProvisionedThroughputOverride' - Provisioned throughput settings for the restored table.
--
-- * 'rttpitSourceTableARN' - The DynamoDB table that will be restored. This value is an Amazon Resource Name (ARN).
--
-- * 'rttpitSSESpecificationOverride' - The new server-side encryption settings for the restored table.
--
-- * 'rttpitSourceTableName' - Name of the source table that is being restored.
--
-- * 'rttpitLocalSecondaryIndexOverride' - List of local secondary indexes for the restored table. The indexes provided should match existing secondary indexes. You can choose to exclude some or all of the indexes at the time of restore.
--
-- * 'rttpitRestoreDateTime' - Time in the past to restore the table to.
--
-- * 'rttpitTargetTableName' - The name of the new table to which it must be restored to.
restoreTableToPointInTime ::
  -- | 'rttpitTargetTableName'
  Text ->
  RestoreTableToPointInTime
restoreTableToPointInTime pTargetTableName_ =
  RestoreTableToPointInTime'
    { _rttpitBillingModeOverride = Nothing,
      _rttpitUseLatestRestorableTime = Nothing,
      _rttpitGlobalSecondaryIndexOverride = Nothing,
      _rttpitProvisionedThroughputOverride = Nothing,
      _rttpitSourceTableARN = Nothing,
      _rttpitSSESpecificationOverride = Nothing,
      _rttpitSourceTableName = Nothing,
      _rttpitLocalSecondaryIndexOverride = Nothing,
      _rttpitRestoreDateTime = Nothing,
      _rttpitTargetTableName = pTargetTableName_
    }

-- | The billing mode of the restored table.
rttpitBillingModeOverride :: Lens' RestoreTableToPointInTime (Maybe BillingMode)
rttpitBillingModeOverride = lens _rttpitBillingModeOverride (\s a -> s {_rttpitBillingModeOverride = a})

-- | Restore the table to the latest possible time. @LatestRestorableDateTime@ is typically 5 minutes before the current time.
rttpitUseLatestRestorableTime :: Lens' RestoreTableToPointInTime (Maybe Bool)
rttpitUseLatestRestorableTime = lens _rttpitUseLatestRestorableTime (\s a -> s {_rttpitUseLatestRestorableTime = a})

-- | List of global secondary indexes for the restored table. The indexes provided should match existing secondary indexes. You can choose to exclude some or all of the indexes at the time of restore.
rttpitGlobalSecondaryIndexOverride :: Lens' RestoreTableToPointInTime [GlobalSecondaryIndex]
rttpitGlobalSecondaryIndexOverride = lens _rttpitGlobalSecondaryIndexOverride (\s a -> s {_rttpitGlobalSecondaryIndexOverride = a}) . _Default . _Coerce

-- | Provisioned throughput settings for the restored table.
rttpitProvisionedThroughputOverride :: Lens' RestoreTableToPointInTime (Maybe ProvisionedThroughput)
rttpitProvisionedThroughputOverride = lens _rttpitProvisionedThroughputOverride (\s a -> s {_rttpitProvisionedThroughputOverride = a})

-- | The DynamoDB table that will be restored. This value is an Amazon Resource Name (ARN).
rttpitSourceTableARN :: Lens' RestoreTableToPointInTime (Maybe Text)
rttpitSourceTableARN = lens _rttpitSourceTableARN (\s a -> s {_rttpitSourceTableARN = a})

-- | The new server-side encryption settings for the restored table.
rttpitSSESpecificationOverride :: Lens' RestoreTableToPointInTime (Maybe SSESpecification)
rttpitSSESpecificationOverride = lens _rttpitSSESpecificationOverride (\s a -> s {_rttpitSSESpecificationOverride = a})

-- | Name of the source table that is being restored.
rttpitSourceTableName :: Lens' RestoreTableToPointInTime (Maybe Text)
rttpitSourceTableName = lens _rttpitSourceTableName (\s a -> s {_rttpitSourceTableName = a})

-- | List of local secondary indexes for the restored table. The indexes provided should match existing secondary indexes. You can choose to exclude some or all of the indexes at the time of restore.
rttpitLocalSecondaryIndexOverride :: Lens' RestoreTableToPointInTime [LocalSecondaryIndex]
rttpitLocalSecondaryIndexOverride = lens _rttpitLocalSecondaryIndexOverride (\s a -> s {_rttpitLocalSecondaryIndexOverride = a}) . _Default . _Coerce

-- | Time in the past to restore the table to.
rttpitRestoreDateTime :: Lens' RestoreTableToPointInTime (Maybe UTCTime)
rttpitRestoreDateTime = lens _rttpitRestoreDateTime (\s a -> s {_rttpitRestoreDateTime = a}) . mapping _Time

-- | The name of the new table to which it must be restored to.
rttpitTargetTableName :: Lens' RestoreTableToPointInTime Text
rttpitTargetTableName = lens _rttpitTargetTableName (\s a -> s {_rttpitTargetTableName = a})

instance AWSRequest RestoreTableToPointInTime where
  type
    Rs RestoreTableToPointInTime =
      RestoreTableToPointInTimeResponse
  request = postJSON dynamoDB
  response =
    receiveJSON
      ( \s h x ->
          RestoreTableToPointInTimeResponse'
            <$> (x .?> "TableDescription") <*> (pure (fromEnum s))
      )

instance Hashable RestoreTableToPointInTime

instance NFData RestoreTableToPointInTime

instance ToHeaders RestoreTableToPointInTime where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DynamoDB_20120810.RestoreTableToPointInTime" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.0" :: ByteString)
          ]
      )

instance ToJSON RestoreTableToPointInTime where
  toJSON RestoreTableToPointInTime' {..} =
    object
      ( catMaybes
          [ ("BillingModeOverride" .=) <$> _rttpitBillingModeOverride,
            ("UseLatestRestorableTime" .=) <$> _rttpitUseLatestRestorableTime,
            ("GlobalSecondaryIndexOverride" .=)
              <$> _rttpitGlobalSecondaryIndexOverride,
            ("ProvisionedThroughputOverride" .=)
              <$> _rttpitProvisionedThroughputOverride,
            ("SourceTableArn" .=) <$> _rttpitSourceTableARN,
            ("SSESpecificationOverride" .=)
              <$> _rttpitSSESpecificationOverride,
            ("SourceTableName" .=) <$> _rttpitSourceTableName,
            ("LocalSecondaryIndexOverride" .=)
              <$> _rttpitLocalSecondaryIndexOverride,
            ("RestoreDateTime" .=) <$> _rttpitRestoreDateTime,
            Just ("TargetTableName" .= _rttpitTargetTableName)
          ]
      )

instance ToPath RestoreTableToPointInTime where
  toPath = const "/"

instance ToQuery RestoreTableToPointInTime where
  toQuery = const mempty

-- | /See:/ 'restoreTableToPointInTimeResponse' smart constructor.
data RestoreTableToPointInTimeResponse = RestoreTableToPointInTimeResponse'
  { _rttpitrsTableDescription ::
      !( Maybe
           TableDescription
       ),
    _rttpitrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RestoreTableToPointInTimeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rttpitrsTableDescription' - Represents the properties of a table.
--
-- * 'rttpitrsResponseStatus' - -- | The response status code.
restoreTableToPointInTimeResponse ::
  -- | 'rttpitrsResponseStatus'
  Int ->
  RestoreTableToPointInTimeResponse
restoreTableToPointInTimeResponse pResponseStatus_ =
  RestoreTableToPointInTimeResponse'
    { _rttpitrsTableDescription =
        Nothing,
      _rttpitrsResponseStatus = pResponseStatus_
    }

-- | Represents the properties of a table.
rttpitrsTableDescription :: Lens' RestoreTableToPointInTimeResponse (Maybe TableDescription)
rttpitrsTableDescription = lens _rttpitrsTableDescription (\s a -> s {_rttpitrsTableDescription = a})

-- | -- | The response status code.
rttpitrsResponseStatus :: Lens' RestoreTableToPointInTimeResponse Int
rttpitrsResponseStatus = lens _rttpitrsResponseStatus (\s a -> s {_rttpitrsResponseStatus = a})

instance NFData RestoreTableToPointInTimeResponse
