{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.BulkDeploymentMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.BulkDeploymentMetrics where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Relevant metrics on input records processed during bulk deployment.
--
-- /See:/ 'bulkDeploymentMetrics' smart constructor.
data BulkDeploymentMetrics = BulkDeploymentMetrics'
  { _bdmRecordsProcessed ::
      !(Maybe Int),
    _bdmRetryAttempts :: !(Maybe Int),
    _bdmInvalidInputRecords :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BulkDeploymentMetrics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdmRecordsProcessed' - The total number of group records from the input file that have been processed so far, or attempted.
--
-- * 'bdmRetryAttempts' - The total number of deployment attempts that returned a retryable error. For example, a retry is triggered if the attempt to deploy a group returns a throttling error. ''StartBulkDeployment'' retries a group deployment up to five times.
--
-- * 'bdmInvalidInputRecords' - The total number of records that returned a non-retryable error. For example, this can occur if a group record from the input file uses an invalid format or specifies a nonexistent group version, or if the execution role doesn't grant permission to deploy a group or group version.
bulkDeploymentMetrics ::
  BulkDeploymentMetrics
bulkDeploymentMetrics =
  BulkDeploymentMetrics'
    { _bdmRecordsProcessed = Nothing,
      _bdmRetryAttempts = Nothing,
      _bdmInvalidInputRecords = Nothing
    }

-- | The total number of group records from the input file that have been processed so far, or attempted.
bdmRecordsProcessed :: Lens' BulkDeploymentMetrics (Maybe Int)
bdmRecordsProcessed = lens _bdmRecordsProcessed (\s a -> s {_bdmRecordsProcessed = a})

-- | The total number of deployment attempts that returned a retryable error. For example, a retry is triggered if the attempt to deploy a group returns a throttling error. ''StartBulkDeployment'' retries a group deployment up to five times.
bdmRetryAttempts :: Lens' BulkDeploymentMetrics (Maybe Int)
bdmRetryAttempts = lens _bdmRetryAttempts (\s a -> s {_bdmRetryAttempts = a})

-- | The total number of records that returned a non-retryable error. For example, this can occur if a group record from the input file uses an invalid format or specifies a nonexistent group version, or if the execution role doesn't grant permission to deploy a group or group version.
bdmInvalidInputRecords :: Lens' BulkDeploymentMetrics (Maybe Int)
bdmInvalidInputRecords = lens _bdmInvalidInputRecords (\s a -> s {_bdmInvalidInputRecords = a})

instance FromJSON BulkDeploymentMetrics where
  parseJSON =
    withObject
      "BulkDeploymentMetrics"
      ( \x ->
          BulkDeploymentMetrics'
            <$> (x .:? "RecordsProcessed")
            <*> (x .:? "RetryAttempts")
            <*> (x .:? "InvalidInputRecords")
      )

instance Hashable BulkDeploymentMetrics

instance NFData BulkDeploymentMetrics
