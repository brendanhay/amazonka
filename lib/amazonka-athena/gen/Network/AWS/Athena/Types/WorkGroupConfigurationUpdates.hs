{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.WorkGroupConfigurationUpdates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.WorkGroupConfigurationUpdates where

import Network.AWS.Athena.Types.ResultConfigurationUpdates
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The configuration information that will be updated for this workgroup, which includes the location in Amazon S3 where query results are stored, the encryption option, if any, used for query results, whether the Amazon CloudWatch Metrics are enabled for the workgroup, whether the workgroup settings override the client-side settings, and the data usage limit for the amount of bytes scanned per query, if it is specified.
--
--
--
-- /See:/ 'workGroupConfigurationUpdates' smart constructor.
data WorkGroupConfigurationUpdates = WorkGroupConfigurationUpdates'
  { _wgcuRequesterPaysEnabled ::
      !(Maybe Bool),
    _wgcuResultConfigurationUpdates ::
      !( Maybe
           ResultConfigurationUpdates
       ),
    _wgcuBytesScannedCutoffPerQuery ::
      !(Maybe Nat),
    _wgcuRemoveBytesScannedCutoffPerQuery ::
      !(Maybe Bool),
    _wgcuEnforceWorkGroupConfiguration ::
      !(Maybe Bool),
    _wgcuPublishCloudWatchMetricsEnabled ::
      !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WorkGroupConfigurationUpdates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wgcuRequesterPaysEnabled' - If set to @true@ , allows members assigned to a workgroup to specify Amazon S3 Requester Pays buckets in queries. If set to @false@ , workgroup members cannot query data from Requester Pays buckets, and queries that retrieve data from Requester Pays buckets cause an error. The default is @false@ . For more information about Requester Pays buckets, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/RequesterPaysBuckets.html Requester Pays Buckets> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- * 'wgcuResultConfigurationUpdates' - The result configuration information about the queries in this workgroup that will be updated. Includes the updated results location and an updated option for encrypting query results.
--
-- * 'wgcuBytesScannedCutoffPerQuery' - The upper limit (cutoff) for the amount of bytes a single query in a workgroup is allowed to scan.
--
-- * 'wgcuRemoveBytesScannedCutoffPerQuery' - Indicates that the data usage control limit per query is removed. 'WorkGroupConfiguration$BytesScannedCutoffPerQuery'
--
-- * 'wgcuEnforceWorkGroupConfiguration' - If set to "true", the settings for the workgroup override client-side settings. If set to "false" client-side settings are used. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
--
-- * 'wgcuPublishCloudWatchMetricsEnabled' - Indicates whether this workgroup enables publishing metrics to Amazon CloudWatch.
workGroupConfigurationUpdates ::
  WorkGroupConfigurationUpdates
workGroupConfigurationUpdates =
  WorkGroupConfigurationUpdates'
    { _wgcuRequesterPaysEnabled =
        Nothing,
      _wgcuResultConfigurationUpdates = Nothing,
      _wgcuBytesScannedCutoffPerQuery = Nothing,
      _wgcuRemoveBytesScannedCutoffPerQuery = Nothing,
      _wgcuEnforceWorkGroupConfiguration = Nothing,
      _wgcuPublishCloudWatchMetricsEnabled = Nothing
    }

-- | If set to @true@ , allows members assigned to a workgroup to specify Amazon S3 Requester Pays buckets in queries. If set to @false@ , workgroup members cannot query data from Requester Pays buckets, and queries that retrieve data from Requester Pays buckets cause an error. The default is @false@ . For more information about Requester Pays buckets, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/RequesterPaysBuckets.html Requester Pays Buckets> in the /Amazon Simple Storage Service Developer Guide/ .
wgcuRequesterPaysEnabled :: Lens' WorkGroupConfigurationUpdates (Maybe Bool)
wgcuRequesterPaysEnabled = lens _wgcuRequesterPaysEnabled (\s a -> s {_wgcuRequesterPaysEnabled = a})

-- | The result configuration information about the queries in this workgroup that will be updated. Includes the updated results location and an updated option for encrypting query results.
wgcuResultConfigurationUpdates :: Lens' WorkGroupConfigurationUpdates (Maybe ResultConfigurationUpdates)
wgcuResultConfigurationUpdates = lens _wgcuResultConfigurationUpdates (\s a -> s {_wgcuResultConfigurationUpdates = a})

-- | The upper limit (cutoff) for the amount of bytes a single query in a workgroup is allowed to scan.
wgcuBytesScannedCutoffPerQuery :: Lens' WorkGroupConfigurationUpdates (Maybe Natural)
wgcuBytesScannedCutoffPerQuery = lens _wgcuBytesScannedCutoffPerQuery (\s a -> s {_wgcuBytesScannedCutoffPerQuery = a}) . mapping _Nat

-- | Indicates that the data usage control limit per query is removed. 'WorkGroupConfiguration$BytesScannedCutoffPerQuery'
wgcuRemoveBytesScannedCutoffPerQuery :: Lens' WorkGroupConfigurationUpdates (Maybe Bool)
wgcuRemoveBytesScannedCutoffPerQuery = lens _wgcuRemoveBytesScannedCutoffPerQuery (\s a -> s {_wgcuRemoveBytesScannedCutoffPerQuery = a})

-- | If set to "true", the settings for the workgroup override client-side settings. If set to "false" client-side settings are used. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
wgcuEnforceWorkGroupConfiguration :: Lens' WorkGroupConfigurationUpdates (Maybe Bool)
wgcuEnforceWorkGroupConfiguration = lens _wgcuEnforceWorkGroupConfiguration (\s a -> s {_wgcuEnforceWorkGroupConfiguration = a})

-- | Indicates whether this workgroup enables publishing metrics to Amazon CloudWatch.
wgcuPublishCloudWatchMetricsEnabled :: Lens' WorkGroupConfigurationUpdates (Maybe Bool)
wgcuPublishCloudWatchMetricsEnabled = lens _wgcuPublishCloudWatchMetricsEnabled (\s a -> s {_wgcuPublishCloudWatchMetricsEnabled = a})

instance Hashable WorkGroupConfigurationUpdates

instance NFData WorkGroupConfigurationUpdates

instance ToJSON WorkGroupConfigurationUpdates where
  toJSON WorkGroupConfigurationUpdates' {..} =
    object
      ( catMaybes
          [ ("RequesterPaysEnabled" .=) <$> _wgcuRequesterPaysEnabled,
            ("ResultConfigurationUpdates" .=)
              <$> _wgcuResultConfigurationUpdates,
            ("BytesScannedCutoffPerQuery" .=)
              <$> _wgcuBytesScannedCutoffPerQuery,
            ("RemoveBytesScannedCutoffPerQuery" .=)
              <$> _wgcuRemoveBytesScannedCutoffPerQuery,
            ("EnforceWorkGroupConfiguration" .=)
              <$> _wgcuEnforceWorkGroupConfiguration,
            ("PublishCloudWatchMetricsEnabled" .=)
              <$> _wgcuPublishCloudWatchMetricsEnabled
          ]
      )
