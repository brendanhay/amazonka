{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.CreateDataset
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a data set. A data set stores data retrieved from a data store by applying a "queryAction" (a SQL query) or a "containerAction" (executing a containerized application). This operation creates the skeleton of a data set. The data set can be populated manually by calling "CreateDatasetContent" or automatically according to a "trigger" you specify.
--
--
module Network.AWS.IoTAnalytics.CreateDataset
    (
    -- * Creating a Request
      createDataset
    , CreateDataset
    -- * Request Lenses
    , creTriggers
    , creRetentionPeriod
    , creContentDeliveryRules
    , creTags
    , creDatasetName
    , creActions

    -- * Destructuring the Response
    , createDatasetResponse
    , CreateDatasetResponse
    -- * Response Lenses
    , crsDatasetARN
    , crsRetentionPeriod
    , crsDatasetName
    , crsResponseStatus
    ) where

import Network.AWS.IoTAnalytics.Types
import Network.AWS.IoTAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDataset' smart constructor.
data CreateDataset = CreateDataset'
  { _creTriggers             :: !(Maybe [DatasetTrigger])
  , _creRetentionPeriod      :: !(Maybe RetentionPeriod)
  , _creContentDeliveryRules :: !(Maybe [DatasetContentDeliveryRule])
  , _creTags                 :: !(Maybe (List1 Tag))
  , _creDatasetName          :: !Text
  , _creActions              :: !(List1 DatasetAction)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDataset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'creTriggers' - A list of triggers. A trigger causes data set contents to be populated at a specified time interval or when another data set's contents are created. The list of triggers can be empty or contain up to five __DataSetTrigger__ objects.
--
-- * 'creRetentionPeriod' - [Optional] How long, in days, message data is kept for the data set. If not given or set to null, the latest version of the dataset content plus the latest succeeded version (if they are different) are retained for at most 90 days.
--
-- * 'creContentDeliveryRules' - When data set contents are created they are delivered to destinations specified here.
--
-- * 'creTags' - Metadata which can be used to manage the data set.
--
-- * 'creDatasetName' - The name of the data set.
--
-- * 'creActions' - A list of actions that create the data set contents.
createDataset
    :: Text -- ^ 'creDatasetName'
    -> NonEmpty DatasetAction -- ^ 'creActions'
    -> CreateDataset
createDataset pDatasetName_ pActions_ =
  CreateDataset'
    { _creTriggers = Nothing
    , _creRetentionPeriod = Nothing
    , _creContentDeliveryRules = Nothing
    , _creTags = Nothing
    , _creDatasetName = pDatasetName_
    , _creActions = _List1 # pActions_
    }


-- | A list of triggers. A trigger causes data set contents to be populated at a specified time interval or when another data set's contents are created. The list of triggers can be empty or contain up to five __DataSetTrigger__ objects.
creTriggers :: Lens' CreateDataset [DatasetTrigger]
creTriggers = lens _creTriggers (\ s a -> s{_creTriggers = a}) . _Default . _Coerce

-- | [Optional] How long, in days, message data is kept for the data set. If not given or set to null, the latest version of the dataset content plus the latest succeeded version (if they are different) are retained for at most 90 days.
creRetentionPeriod :: Lens' CreateDataset (Maybe RetentionPeriod)
creRetentionPeriod = lens _creRetentionPeriod (\ s a -> s{_creRetentionPeriod = a})

-- | When data set contents are created they are delivered to destinations specified here.
creContentDeliveryRules :: Lens' CreateDataset [DatasetContentDeliveryRule]
creContentDeliveryRules = lens _creContentDeliveryRules (\ s a -> s{_creContentDeliveryRules = a}) . _Default . _Coerce

-- | Metadata which can be used to manage the data set.
creTags :: Lens' CreateDataset (Maybe (NonEmpty Tag))
creTags = lens _creTags (\ s a -> s{_creTags = a}) . mapping _List1

-- | The name of the data set.
creDatasetName :: Lens' CreateDataset Text
creDatasetName = lens _creDatasetName (\ s a -> s{_creDatasetName = a})

-- | A list of actions that create the data set contents.
creActions :: Lens' CreateDataset (NonEmpty DatasetAction)
creActions = lens _creActions (\ s a -> s{_creActions = a}) . _List1

instance AWSRequest CreateDataset where
        type Rs CreateDataset = CreateDatasetResponse
        request = postJSON ioTAnalytics
        response
          = receiveJSON
              (\ s h x ->
                 CreateDatasetResponse' <$>
                   (x .?> "datasetArn") <*> (x .?> "retentionPeriod")
                     <*> (x .?> "datasetName")
                     <*> (pure (fromEnum s)))

instance Hashable CreateDataset where

instance NFData CreateDataset where

instance ToHeaders CreateDataset where
        toHeaders = const mempty

instance ToJSON CreateDataset where
        toJSON CreateDataset'{..}
          = object
              (catMaybes
                 [("triggers" .=) <$> _creTriggers,
                  ("retentionPeriod" .=) <$> _creRetentionPeriod,
                  ("contentDeliveryRules" .=) <$>
                    _creContentDeliveryRules,
                  ("tags" .=) <$> _creTags,
                  Just ("datasetName" .= _creDatasetName),
                  Just ("actions" .= _creActions)])

instance ToPath CreateDataset where
        toPath = const "/datasets"

instance ToQuery CreateDataset where
        toQuery = const mempty

-- | /See:/ 'createDatasetResponse' smart constructor.
data CreateDatasetResponse = CreateDatasetResponse'
  { _crsDatasetARN      :: !(Maybe Text)
  , _crsRetentionPeriod :: !(Maybe RetentionPeriod)
  , _crsDatasetName     :: !(Maybe Text)
  , _crsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDatasetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsDatasetARN' - The ARN of the data set.
--
-- * 'crsRetentionPeriod' - How long, in days, message data is kept for the data set.
--
-- * 'crsDatasetName' - The name of the data set.
--
-- * 'crsResponseStatus' - -- | The response status code.
createDatasetResponse
    :: Int -- ^ 'crsResponseStatus'
    -> CreateDatasetResponse
createDatasetResponse pResponseStatus_ =
  CreateDatasetResponse'
    { _crsDatasetARN = Nothing
    , _crsRetentionPeriod = Nothing
    , _crsDatasetName = Nothing
    , _crsResponseStatus = pResponseStatus_
    }


-- | The ARN of the data set.
crsDatasetARN :: Lens' CreateDatasetResponse (Maybe Text)
crsDatasetARN = lens _crsDatasetARN (\ s a -> s{_crsDatasetARN = a})

-- | How long, in days, message data is kept for the data set.
crsRetentionPeriod :: Lens' CreateDatasetResponse (Maybe RetentionPeriod)
crsRetentionPeriod = lens _crsRetentionPeriod (\ s a -> s{_crsRetentionPeriod = a})

-- | The name of the data set.
crsDatasetName :: Lens' CreateDatasetResponse (Maybe Text)
crsDatasetName = lens _crsDatasetName (\ s a -> s{_crsDatasetName = a})

-- | -- | The response status code.
crsResponseStatus :: Lens' CreateDatasetResponse Int
crsResponseStatus = lens _crsResponseStatus (\ s a -> s{_crsResponseStatus = a})

instance NFData CreateDatasetResponse where
