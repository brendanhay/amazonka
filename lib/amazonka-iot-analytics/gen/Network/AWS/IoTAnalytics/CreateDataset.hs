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
-- Module      : Network.AWS.IoTAnalytics.CreateDataset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a dataset. A dataset stores data retrieved from a data store by applying a @queryAction@ (a SQL query) or a @containerAction@ (executing a containerized application). This operation creates the skeleton of a dataset. The dataset can be populated manually by calling @CreateDatasetContent@ or automatically according to a trigger you specify.
module Network.AWS.IoTAnalytics.CreateDataset
  ( -- * Creating a Request
    createDataset,
    CreateDataset,

    -- * Request Lenses
    creVersioningConfiguration,
    creTriggers,
    creRetentionPeriod,
    creLateDataRules,
    creContentDeliveryRules,
    creTags,
    creDatasetName,
    creActions,

    -- * Destructuring the Response
    createDatasetResponse,
    CreateDatasetResponse,

    -- * Response Lenses
    crsDatasetARN,
    crsRetentionPeriod,
    crsDatasetName,
    crsResponseStatus,
  )
where

import Network.AWS.IoTAnalytics.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDataset' smart constructor.
data CreateDataset = CreateDataset'
  { _creVersioningConfiguration ::
      !(Maybe VersioningConfiguration),
    _creTriggers :: !(Maybe [DatasetTrigger]),
    _creRetentionPeriod :: !(Maybe RetentionPeriod),
    _creLateDataRules :: !(Maybe (List1 LateDataRule)),
    _creContentDeliveryRules ::
      !(Maybe [DatasetContentDeliveryRule]),
    _creTags :: !(Maybe (List1 Tag)),
    _creDatasetName :: !Text,
    _creActions :: !(List1 DatasetAction)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateDataset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'creVersioningConfiguration' - Optional. How many versions of dataset contents are kept. If not specified or set to null, only the latest version plus the latest succeeded version (if they are different) are kept for the time period specified by the @retentionPeriod@ parameter. For more information, see <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets> in the /AWS IoT Analytics User Guide/ .
--
-- * 'creTriggers' - A list of triggers. A trigger causes data set contents to be populated at a specified time interval or when another data set's contents are created. The list of triggers can be empty or contain up to five @DataSetTrigger@ objects.
--
-- * 'creRetentionPeriod' - Optional. How long, in days, versions of dataset contents are kept for the dataset. If not specified or set to @null@ , versions of dataset contents are retained for at most 90 days. The number of versions of dataset contents retained is determined by the @versioningConfiguration@ parameter. For more information, see <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets> in the /AWS IoT Analytics User Guide/ .
--
-- * 'creLateDataRules' - A list of data rules that send notifications to Amazon CloudWatch, when data arrives late. To specify @lateDataRules@ , the dataset must use a <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer> filter.
--
-- * 'creContentDeliveryRules' - When dataset contents are created, they are delivered to destinations specified here.
--
-- * 'creTags' - Metadata which can be used to manage the data set.
--
-- * 'creDatasetName' - The name of the data set.
--
-- * 'creActions' - A list of actions that create the data set contents.
createDataset ::
  -- | 'creDatasetName'
  Text ->
  -- | 'creActions'
  NonEmpty DatasetAction ->
  CreateDataset
createDataset pDatasetName_ pActions_ =
  CreateDataset'
    { _creVersioningConfiguration = Nothing,
      _creTriggers = Nothing,
      _creRetentionPeriod = Nothing,
      _creLateDataRules = Nothing,
      _creContentDeliveryRules = Nothing,
      _creTags = Nothing,
      _creDatasetName = pDatasetName_,
      _creActions = _List1 # pActions_
    }

-- | Optional. How many versions of dataset contents are kept. If not specified or set to null, only the latest version plus the latest succeeded version (if they are different) are kept for the time period specified by the @retentionPeriod@ parameter. For more information, see <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets> in the /AWS IoT Analytics User Guide/ .
creVersioningConfiguration :: Lens' CreateDataset (Maybe VersioningConfiguration)
creVersioningConfiguration = lens _creVersioningConfiguration (\s a -> s {_creVersioningConfiguration = a})

-- | A list of triggers. A trigger causes data set contents to be populated at a specified time interval or when another data set's contents are created. The list of triggers can be empty or contain up to five @DataSetTrigger@ objects.
creTriggers :: Lens' CreateDataset [DatasetTrigger]
creTriggers = lens _creTriggers (\s a -> s {_creTriggers = a}) . _Default . _Coerce

-- | Optional. How long, in days, versions of dataset contents are kept for the dataset. If not specified or set to @null@ , versions of dataset contents are retained for at most 90 days. The number of versions of dataset contents retained is determined by the @versioningConfiguration@ parameter. For more information, see <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets> in the /AWS IoT Analytics User Guide/ .
creRetentionPeriod :: Lens' CreateDataset (Maybe RetentionPeriod)
creRetentionPeriod = lens _creRetentionPeriod (\s a -> s {_creRetentionPeriod = a})

-- | A list of data rules that send notifications to Amazon CloudWatch, when data arrives late. To specify @lateDataRules@ , the dataset must use a <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer> filter.
creLateDataRules :: Lens' CreateDataset (Maybe (NonEmpty LateDataRule))
creLateDataRules = lens _creLateDataRules (\s a -> s {_creLateDataRules = a}) . mapping _List1

-- | When dataset contents are created, they are delivered to destinations specified here.
creContentDeliveryRules :: Lens' CreateDataset [DatasetContentDeliveryRule]
creContentDeliveryRules = lens _creContentDeliveryRules (\s a -> s {_creContentDeliveryRules = a}) . _Default . _Coerce

-- | Metadata which can be used to manage the data set.
creTags :: Lens' CreateDataset (Maybe (NonEmpty Tag))
creTags = lens _creTags (\s a -> s {_creTags = a}) . mapping _List1

-- | The name of the data set.
creDatasetName :: Lens' CreateDataset Text
creDatasetName = lens _creDatasetName (\s a -> s {_creDatasetName = a})

-- | A list of actions that create the data set contents.
creActions :: Lens' CreateDataset (NonEmpty DatasetAction)
creActions = lens _creActions (\s a -> s {_creActions = a}) . _List1

instance AWSRequest CreateDataset where
  type Rs CreateDataset = CreateDatasetResponse
  request = postJSON ioTAnalytics
  response =
    receiveJSON
      ( \s h x ->
          CreateDatasetResponse'
            <$> (x .?> "datasetArn")
            <*> (x .?> "retentionPeriod")
            <*> (x .?> "datasetName")
            <*> (pure (fromEnum s))
      )

instance Hashable CreateDataset

instance NFData CreateDataset

instance ToHeaders CreateDataset where
  toHeaders = const mempty

instance ToJSON CreateDataset where
  toJSON CreateDataset' {..} =
    object
      ( catMaybes
          [ ("versioningConfiguration" .=) <$> _creVersioningConfiguration,
            ("triggers" .=) <$> _creTriggers,
            ("retentionPeriod" .=) <$> _creRetentionPeriod,
            ("lateDataRules" .=) <$> _creLateDataRules,
            ("contentDeliveryRules" .=) <$> _creContentDeliveryRules,
            ("tags" .=) <$> _creTags,
            Just ("datasetName" .= _creDatasetName),
            Just ("actions" .= _creActions)
          ]
      )

instance ToPath CreateDataset where
  toPath = const "/datasets"

instance ToQuery CreateDataset where
  toQuery = const mempty

-- | /See:/ 'createDatasetResponse' smart constructor.
data CreateDatasetResponse = CreateDatasetResponse'
  { _crsDatasetARN ::
      !(Maybe Text),
    _crsRetentionPeriod :: !(Maybe RetentionPeriod),
    _crsDatasetName :: !(Maybe Text),
    _crsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateDatasetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsDatasetARN' - The ARN of the dataset.
--
-- * 'crsRetentionPeriod' - How long, in days, dataset contents are kept for the dataset.
--
-- * 'crsDatasetName' - The name of the dataset.
--
-- * 'crsResponseStatus' - -- | The response status code.
createDatasetResponse ::
  -- | 'crsResponseStatus'
  Int ->
  CreateDatasetResponse
createDatasetResponse pResponseStatus_ =
  CreateDatasetResponse'
    { _crsDatasetARN = Nothing,
      _crsRetentionPeriod = Nothing,
      _crsDatasetName = Nothing,
      _crsResponseStatus = pResponseStatus_
    }

-- | The ARN of the dataset.
crsDatasetARN :: Lens' CreateDatasetResponse (Maybe Text)
crsDatasetARN = lens _crsDatasetARN (\s a -> s {_crsDatasetARN = a})

-- | How long, in days, dataset contents are kept for the dataset.
crsRetentionPeriod :: Lens' CreateDatasetResponse (Maybe RetentionPeriod)
crsRetentionPeriod = lens _crsRetentionPeriod (\s a -> s {_crsRetentionPeriod = a})

-- | The name of the dataset.
crsDatasetName :: Lens' CreateDatasetResponse (Maybe Text)
crsDatasetName = lens _crsDatasetName (\s a -> s {_crsDatasetName = a})

-- | -- | The response status code.
crsResponseStatus :: Lens' CreateDatasetResponse Int
crsResponseStatus = lens _crsResponseStatus (\s a -> s {_crsResponseStatus = a})

instance NFData CreateDatasetResponse
