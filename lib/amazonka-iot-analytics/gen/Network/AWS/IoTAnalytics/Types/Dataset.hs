{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.Dataset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.Dataset where

import Network.AWS.IoTAnalytics.Types.DatasetAction
import Network.AWS.IoTAnalytics.Types.DatasetContentDeliveryRule
import Network.AWS.IoTAnalytics.Types.DatasetStatus
import Network.AWS.IoTAnalytics.Types.DatasetTrigger
import Network.AWS.IoTAnalytics.Types.LateDataRule
import Network.AWS.IoTAnalytics.Types.RetentionPeriod
import Network.AWS.IoTAnalytics.Types.VersioningConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a data set.
--
--
--
-- /See:/ 'dataset' smart constructor.
data Dataset = Dataset'
  { _dCreationTime :: !(Maybe POSIX),
    _dStatus :: !(Maybe DatasetStatus),
    _dVersioningConfiguration :: !(Maybe VersioningConfiguration),
    _dArn :: !(Maybe Text),
    _dActions :: !(Maybe (List1 DatasetAction)),
    _dTriggers :: !(Maybe [DatasetTrigger]),
    _dRetentionPeriod :: !(Maybe RetentionPeriod),
    _dLateDataRules :: !(Maybe (List1 LateDataRule)),
    _dName :: !(Maybe Text),
    _dContentDeliveryRules :: !(Maybe [DatasetContentDeliveryRule]),
    _dLastUpdateTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Dataset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dCreationTime' - When the data set was created.
--
-- * 'dStatus' - The status of the data set.
--
-- * 'dVersioningConfiguration' - Optional. How many versions of dataset contents are kept. If not specified or set to null, only the latest version plus the latest succeeded version (if they are different) are kept for the time period specified by the @retentionPeriod@ parameter. For more information, see <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets> in the /AWS IoT Analytics User Guide/ .
--
-- * 'dArn' - The ARN of the data set.
--
-- * 'dActions' - The @DatasetAction@ objects that automatically create the data set contents.
--
-- * 'dTriggers' - The @DatasetTrigger@ objects that specify when the data set is automatically updated.
--
-- * 'dRetentionPeriod' - Optional. How long, in days, message data is kept for the data set.
--
-- * 'dLateDataRules' - A list of data rules that send notifications to Amazon CloudWatch, when data arrives late. To specify @lateDataRules@ , the dataset must use a <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer> filter.
--
-- * 'dName' - The name of the data set.
--
-- * 'dContentDeliveryRules' - When dataset contents are created they are delivered to destinations specified here.
--
-- * 'dLastUpdateTime' - The last time the data set was updated.
dataset ::
  Dataset
dataset =
  Dataset'
    { _dCreationTime = Nothing,
      _dStatus = Nothing,
      _dVersioningConfiguration = Nothing,
      _dArn = Nothing,
      _dActions = Nothing,
      _dTriggers = Nothing,
      _dRetentionPeriod = Nothing,
      _dLateDataRules = Nothing,
      _dName = Nothing,
      _dContentDeliveryRules = Nothing,
      _dLastUpdateTime = Nothing
    }

-- | When the data set was created.
dCreationTime :: Lens' Dataset (Maybe UTCTime)
dCreationTime = lens _dCreationTime (\s a -> s {_dCreationTime = a}) . mapping _Time

-- | The status of the data set.
dStatus :: Lens' Dataset (Maybe DatasetStatus)
dStatus = lens _dStatus (\s a -> s {_dStatus = a})

-- | Optional. How many versions of dataset contents are kept. If not specified or set to null, only the latest version plus the latest succeeded version (if they are different) are kept for the time period specified by the @retentionPeriod@ parameter. For more information, see <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets> in the /AWS IoT Analytics User Guide/ .
dVersioningConfiguration :: Lens' Dataset (Maybe VersioningConfiguration)
dVersioningConfiguration = lens _dVersioningConfiguration (\s a -> s {_dVersioningConfiguration = a})

-- | The ARN of the data set.
dArn :: Lens' Dataset (Maybe Text)
dArn = lens _dArn (\s a -> s {_dArn = a})

-- | The @DatasetAction@ objects that automatically create the data set contents.
dActions :: Lens' Dataset (Maybe (NonEmpty DatasetAction))
dActions = lens _dActions (\s a -> s {_dActions = a}) . mapping _List1

-- | The @DatasetTrigger@ objects that specify when the data set is automatically updated.
dTriggers :: Lens' Dataset [DatasetTrigger]
dTriggers = lens _dTriggers (\s a -> s {_dTriggers = a}) . _Default . _Coerce

-- | Optional. How long, in days, message data is kept for the data set.
dRetentionPeriod :: Lens' Dataset (Maybe RetentionPeriod)
dRetentionPeriod = lens _dRetentionPeriod (\s a -> s {_dRetentionPeriod = a})

-- | A list of data rules that send notifications to Amazon CloudWatch, when data arrives late. To specify @lateDataRules@ , the dataset must use a <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer> filter.
dLateDataRules :: Lens' Dataset (Maybe (NonEmpty LateDataRule))
dLateDataRules = lens _dLateDataRules (\s a -> s {_dLateDataRules = a}) . mapping _List1

-- | The name of the data set.
dName :: Lens' Dataset (Maybe Text)
dName = lens _dName (\s a -> s {_dName = a})

-- | When dataset contents are created they are delivered to destinations specified here.
dContentDeliveryRules :: Lens' Dataset [DatasetContentDeliveryRule]
dContentDeliveryRules = lens _dContentDeliveryRules (\s a -> s {_dContentDeliveryRules = a}) . _Default . _Coerce

-- | The last time the data set was updated.
dLastUpdateTime :: Lens' Dataset (Maybe UTCTime)
dLastUpdateTime = lens _dLastUpdateTime (\s a -> s {_dLastUpdateTime = a}) . mapping _Time

instance FromJSON Dataset where
  parseJSON =
    withObject
      "Dataset"
      ( \x ->
          Dataset'
            <$> (x .:? "creationTime")
            <*> (x .:? "status")
            <*> (x .:? "versioningConfiguration")
            <*> (x .:? "arn")
            <*> (x .:? "actions")
            <*> (x .:? "triggers" .!= mempty)
            <*> (x .:? "retentionPeriod")
            <*> (x .:? "lateDataRules")
            <*> (x .:? "name")
            <*> (x .:? "contentDeliveryRules" .!= mempty)
            <*> (x .:? "lastUpdateTime")
      )

instance Hashable Dataset

instance NFData Dataset
