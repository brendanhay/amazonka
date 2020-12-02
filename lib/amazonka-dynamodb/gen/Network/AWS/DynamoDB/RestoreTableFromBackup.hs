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
-- Module      : Network.AWS.DynamoDB.RestoreTableFromBackup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new table from an existing backup. Any number of users can execute up to 4 concurrent restores (any type of restore) in a given account.
--
--
-- You can call @RestoreTableFromBackup@ at a maximum rate of 10 times per second.
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
module Network.AWS.DynamoDB.RestoreTableFromBackup
  ( -- * Creating a Request
    restoreTableFromBackup,
    RestoreTableFromBackup,

    -- * Request Lenses
    rtfbBillingModeOverride,
    rtfbGlobalSecondaryIndexOverride,
    rtfbProvisionedThroughputOverride,
    rtfbSSESpecificationOverride,
    rtfbLocalSecondaryIndexOverride,
    rtfbTargetTableName,
    rtfbBackupARN,

    -- * Destructuring the Response
    restoreTableFromBackupResponse,
    RestoreTableFromBackupResponse,

    -- * Response Lenses
    rtfbrsTableDescription,
    rtfbrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'restoreTableFromBackup' smart constructor.
data RestoreTableFromBackup = RestoreTableFromBackup'
  { _rtfbBillingModeOverride ::
      !(Maybe BillingMode),
    _rtfbGlobalSecondaryIndexOverride ::
      !(Maybe [GlobalSecondaryIndex]),
    _rtfbProvisionedThroughputOverride ::
      !(Maybe ProvisionedThroughput),
    _rtfbSSESpecificationOverride ::
      !(Maybe SSESpecification),
    _rtfbLocalSecondaryIndexOverride ::
      !(Maybe [LocalSecondaryIndex]),
    _rtfbTargetTableName :: !Text,
    _rtfbBackupARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RestoreTableFromBackup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtfbBillingModeOverride' - The billing mode of the restored table.
--
-- * 'rtfbGlobalSecondaryIndexOverride' - List of global secondary indexes for the restored table. The indexes provided should match existing secondary indexes. You can choose to exclude some or all of the indexes at the time of restore.
--
-- * 'rtfbProvisionedThroughputOverride' - Provisioned throughput settings for the restored table.
--
-- * 'rtfbSSESpecificationOverride' - The new server-side encryption settings for the restored table.
--
-- * 'rtfbLocalSecondaryIndexOverride' - List of local secondary indexes for the restored table. The indexes provided should match existing secondary indexes. You can choose to exclude some or all of the indexes at the time of restore.
--
-- * 'rtfbTargetTableName' - The name of the new table to which the backup must be restored.
--
-- * 'rtfbBackupARN' - The Amazon Resource Name (ARN) associated with the backup.
restoreTableFromBackup ::
  -- | 'rtfbTargetTableName'
  Text ->
  -- | 'rtfbBackupARN'
  Text ->
  RestoreTableFromBackup
restoreTableFromBackup pTargetTableName_ pBackupARN_ =
  RestoreTableFromBackup'
    { _rtfbBillingModeOverride = Nothing,
      _rtfbGlobalSecondaryIndexOverride = Nothing,
      _rtfbProvisionedThroughputOverride = Nothing,
      _rtfbSSESpecificationOverride = Nothing,
      _rtfbLocalSecondaryIndexOverride = Nothing,
      _rtfbTargetTableName = pTargetTableName_,
      _rtfbBackupARN = pBackupARN_
    }

-- | The billing mode of the restored table.
rtfbBillingModeOverride :: Lens' RestoreTableFromBackup (Maybe BillingMode)
rtfbBillingModeOverride = lens _rtfbBillingModeOverride (\s a -> s {_rtfbBillingModeOverride = a})

-- | List of global secondary indexes for the restored table. The indexes provided should match existing secondary indexes. You can choose to exclude some or all of the indexes at the time of restore.
rtfbGlobalSecondaryIndexOverride :: Lens' RestoreTableFromBackup [GlobalSecondaryIndex]
rtfbGlobalSecondaryIndexOverride = lens _rtfbGlobalSecondaryIndexOverride (\s a -> s {_rtfbGlobalSecondaryIndexOverride = a}) . _Default . _Coerce

-- | Provisioned throughput settings for the restored table.
rtfbProvisionedThroughputOverride :: Lens' RestoreTableFromBackup (Maybe ProvisionedThroughput)
rtfbProvisionedThroughputOverride = lens _rtfbProvisionedThroughputOverride (\s a -> s {_rtfbProvisionedThroughputOverride = a})

-- | The new server-side encryption settings for the restored table.
rtfbSSESpecificationOverride :: Lens' RestoreTableFromBackup (Maybe SSESpecification)
rtfbSSESpecificationOverride = lens _rtfbSSESpecificationOverride (\s a -> s {_rtfbSSESpecificationOverride = a})

-- | List of local secondary indexes for the restored table. The indexes provided should match existing secondary indexes. You can choose to exclude some or all of the indexes at the time of restore.
rtfbLocalSecondaryIndexOverride :: Lens' RestoreTableFromBackup [LocalSecondaryIndex]
rtfbLocalSecondaryIndexOverride = lens _rtfbLocalSecondaryIndexOverride (\s a -> s {_rtfbLocalSecondaryIndexOverride = a}) . _Default . _Coerce

-- | The name of the new table to which the backup must be restored.
rtfbTargetTableName :: Lens' RestoreTableFromBackup Text
rtfbTargetTableName = lens _rtfbTargetTableName (\s a -> s {_rtfbTargetTableName = a})

-- | The Amazon Resource Name (ARN) associated with the backup.
rtfbBackupARN :: Lens' RestoreTableFromBackup Text
rtfbBackupARN = lens _rtfbBackupARN (\s a -> s {_rtfbBackupARN = a})

instance AWSRequest RestoreTableFromBackup where
  type Rs RestoreTableFromBackup = RestoreTableFromBackupResponse
  request = postJSON dynamoDB
  response =
    receiveJSON
      ( \s h x ->
          RestoreTableFromBackupResponse'
            <$> (x .?> "TableDescription") <*> (pure (fromEnum s))
      )

instance Hashable RestoreTableFromBackup

instance NFData RestoreTableFromBackup

instance ToHeaders RestoreTableFromBackup where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DynamoDB_20120810.RestoreTableFromBackup" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.0" :: ByteString)
          ]
      )

instance ToJSON RestoreTableFromBackup where
  toJSON RestoreTableFromBackup' {..} =
    object
      ( catMaybes
          [ ("BillingModeOverride" .=) <$> _rtfbBillingModeOverride,
            ("GlobalSecondaryIndexOverride" .=)
              <$> _rtfbGlobalSecondaryIndexOverride,
            ("ProvisionedThroughputOverride" .=)
              <$> _rtfbProvisionedThroughputOverride,
            ("SSESpecificationOverride" .=) <$> _rtfbSSESpecificationOverride,
            ("LocalSecondaryIndexOverride" .=)
              <$> _rtfbLocalSecondaryIndexOverride,
            Just ("TargetTableName" .= _rtfbTargetTableName),
            Just ("BackupArn" .= _rtfbBackupARN)
          ]
      )

instance ToPath RestoreTableFromBackup where
  toPath = const "/"

instance ToQuery RestoreTableFromBackup where
  toQuery = const mempty

-- | /See:/ 'restoreTableFromBackupResponse' smart constructor.
data RestoreTableFromBackupResponse = RestoreTableFromBackupResponse'
  { _rtfbrsTableDescription ::
      !(Maybe TableDescription),
    _rtfbrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RestoreTableFromBackupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtfbrsTableDescription' - The description of the table created from an existing backup.
--
-- * 'rtfbrsResponseStatus' - -- | The response status code.
restoreTableFromBackupResponse ::
  -- | 'rtfbrsResponseStatus'
  Int ->
  RestoreTableFromBackupResponse
restoreTableFromBackupResponse pResponseStatus_ =
  RestoreTableFromBackupResponse'
    { _rtfbrsTableDescription =
        Nothing,
      _rtfbrsResponseStatus = pResponseStatus_
    }

-- | The description of the table created from an existing backup.
rtfbrsTableDescription :: Lens' RestoreTableFromBackupResponse (Maybe TableDescription)
rtfbrsTableDescription = lens _rtfbrsTableDescription (\s a -> s {_rtfbrsTableDescription = a})

-- | -- | The response status code.
rtfbrsResponseStatus :: Lens' RestoreTableFromBackupResponse Int
rtfbrsResponseStatus = lens _rtfbrsResponseStatus (\s a -> s {_rtfbrsResponseStatus = a})

instance NFData RestoreTableFromBackupResponse
