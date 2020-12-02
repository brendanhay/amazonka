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
-- Module      : Network.AWS.IoTAnalytics.UpdateDataset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the settings of a data set.
module Network.AWS.IoTAnalytics.UpdateDataset
  ( -- * Creating a Request
    updateDataset,
    UpdateDataset,

    -- * Request Lenses
    udVersioningConfiguration,
    udTriggers,
    udRetentionPeriod,
    udLateDataRules,
    udContentDeliveryRules,
    udDatasetName,
    udActions,

    -- * Destructuring the Response
    updateDatasetResponse,
    UpdateDatasetResponse,
  )
where

import Network.AWS.IoTAnalytics.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateDataset' smart constructor.
data UpdateDataset = UpdateDataset'
  { _udVersioningConfiguration ::
      !(Maybe VersioningConfiguration),
    _udTriggers :: !(Maybe [DatasetTrigger]),
    _udRetentionPeriod :: !(Maybe RetentionPeriod),
    _udLateDataRules :: !(Maybe (List1 LateDataRule)),
    _udContentDeliveryRules ::
      !(Maybe [DatasetContentDeliveryRule]),
    _udDatasetName :: !Text,
    _udActions :: !(List1 DatasetAction)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateDataset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udVersioningConfiguration' - Optional. How many versions of dataset contents are kept. If not specified or set to null, only the latest version plus the latest succeeded version (if they are different) are kept for the time period specified by the @retentionPeriod@ parameter. For more information, see <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets> in the /AWS IoT Analytics User Guide/ .
--
-- * 'udTriggers' - A list of @DatasetTrigger@ objects. The list can be empty or can contain up to five @DatasetTrigger@ objects.
--
-- * 'udRetentionPeriod' - How long, in days, dataset contents are kept for the dataset.
--
-- * 'udLateDataRules' - A list of data rules that send notifications to Amazon CloudWatch, when data arrives late. To specify @lateDataRules@ , the dataset must use a <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer> filter.
--
-- * 'udContentDeliveryRules' - When dataset contents are created, they are delivered to destinations specified here.
--
-- * 'udDatasetName' - The name of the data set to update.
--
-- * 'udActions' - A list of @DatasetAction@ objects.
updateDataset ::
  -- | 'udDatasetName'
  Text ->
  -- | 'udActions'
  NonEmpty DatasetAction ->
  UpdateDataset
updateDataset pDatasetName_ pActions_ =
  UpdateDataset'
    { _udVersioningConfiguration = Nothing,
      _udTriggers = Nothing,
      _udRetentionPeriod = Nothing,
      _udLateDataRules = Nothing,
      _udContentDeliveryRules = Nothing,
      _udDatasetName = pDatasetName_,
      _udActions = _List1 # pActions_
    }

-- | Optional. How many versions of dataset contents are kept. If not specified or set to null, only the latest version plus the latest succeeded version (if they are different) are kept for the time period specified by the @retentionPeriod@ parameter. For more information, see <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets> in the /AWS IoT Analytics User Guide/ .
udVersioningConfiguration :: Lens' UpdateDataset (Maybe VersioningConfiguration)
udVersioningConfiguration = lens _udVersioningConfiguration (\s a -> s {_udVersioningConfiguration = a})

-- | A list of @DatasetTrigger@ objects. The list can be empty or can contain up to five @DatasetTrigger@ objects.
udTriggers :: Lens' UpdateDataset [DatasetTrigger]
udTriggers = lens _udTriggers (\s a -> s {_udTriggers = a}) . _Default . _Coerce

-- | How long, in days, dataset contents are kept for the dataset.
udRetentionPeriod :: Lens' UpdateDataset (Maybe RetentionPeriod)
udRetentionPeriod = lens _udRetentionPeriod (\s a -> s {_udRetentionPeriod = a})

-- | A list of data rules that send notifications to Amazon CloudWatch, when data arrives late. To specify @lateDataRules@ , the dataset must use a <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer> filter.
udLateDataRules :: Lens' UpdateDataset (Maybe (NonEmpty LateDataRule))
udLateDataRules = lens _udLateDataRules (\s a -> s {_udLateDataRules = a}) . mapping _List1

-- | When dataset contents are created, they are delivered to destinations specified here.
udContentDeliveryRules :: Lens' UpdateDataset [DatasetContentDeliveryRule]
udContentDeliveryRules = lens _udContentDeliveryRules (\s a -> s {_udContentDeliveryRules = a}) . _Default . _Coerce

-- | The name of the data set to update.
udDatasetName :: Lens' UpdateDataset Text
udDatasetName = lens _udDatasetName (\s a -> s {_udDatasetName = a})

-- | A list of @DatasetAction@ objects.
udActions :: Lens' UpdateDataset (NonEmpty DatasetAction)
udActions = lens _udActions (\s a -> s {_udActions = a}) . _List1

instance AWSRequest UpdateDataset where
  type Rs UpdateDataset = UpdateDatasetResponse
  request = putJSON ioTAnalytics
  response = receiveNull UpdateDatasetResponse'

instance Hashable UpdateDataset

instance NFData UpdateDataset

instance ToHeaders UpdateDataset where
  toHeaders = const mempty

instance ToJSON UpdateDataset where
  toJSON UpdateDataset' {..} =
    object
      ( catMaybes
          [ ("versioningConfiguration" .=) <$> _udVersioningConfiguration,
            ("triggers" .=) <$> _udTriggers,
            ("retentionPeriod" .=) <$> _udRetentionPeriod,
            ("lateDataRules" .=) <$> _udLateDataRules,
            ("contentDeliveryRules" .=) <$> _udContentDeliveryRules,
            Just ("actions" .= _udActions)
          ]
      )

instance ToPath UpdateDataset where
  toPath UpdateDataset' {..} =
    mconcat ["/datasets/", toBS _udDatasetName]

instance ToQuery UpdateDataset where
  toQuery = const mempty

-- | /See:/ 'updateDatasetResponse' smart constructor.
data UpdateDatasetResponse = UpdateDatasetResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateDatasetResponse' with the minimum fields required to make a request.
updateDatasetResponse ::
  UpdateDatasetResponse
updateDatasetResponse = UpdateDatasetResponse'

instance NFData UpdateDatasetResponse
