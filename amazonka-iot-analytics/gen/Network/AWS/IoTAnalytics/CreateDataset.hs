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
-- Creates a data set. A data set stores data retrieved from a data store by applying an SQL action.
--
--
module Network.AWS.IoTAnalytics.CreateDataset
    (
    -- * Creating a Request
      createDataset
    , CreateDataset
    -- * Request Lenses
    , cdTriggers
    , cdDatasetName
    , cdActions

    -- * Destructuring the Response
    , createDatasetResponse
    , CreateDatasetResponse
    -- * Response Lenses
    , crsDatasetARN
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
  { _cdTriggers    :: !(Maybe [DatasetTrigger])
  , _cdDatasetName :: !Text
  , _cdActions     :: !(List1 DatasetAction)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDataset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdTriggers' - A list of triggers. A trigger causes data set content to be populated at a specified time or time interval. The list of triggers can be empty or contain up to five __DataSetTrigger__ objects.
--
-- * 'cdDatasetName' - The name of the data set.
--
-- * 'cdActions' - A list of actions that create the data set. Only one action is supported at this time.
createDataset
    :: Text -- ^ 'cdDatasetName'
    -> NonEmpty DatasetAction -- ^ 'cdActions'
    -> CreateDataset
createDataset pDatasetName_ pActions_ =
  CreateDataset'
    { _cdTriggers = Nothing
    , _cdDatasetName = pDatasetName_
    , _cdActions = _List1 # pActions_
    }


-- | A list of triggers. A trigger causes data set content to be populated at a specified time or time interval. The list of triggers can be empty or contain up to five __DataSetTrigger__ objects.
cdTriggers :: Lens' CreateDataset [DatasetTrigger]
cdTriggers = lens _cdTriggers (\ s a -> s{_cdTriggers = a}) . _Default . _Coerce

-- | The name of the data set.
cdDatasetName :: Lens' CreateDataset Text
cdDatasetName = lens _cdDatasetName (\ s a -> s{_cdDatasetName = a})

-- | A list of actions that create the data set. Only one action is supported at this time.
cdActions :: Lens' CreateDataset (NonEmpty DatasetAction)
cdActions = lens _cdActions (\ s a -> s{_cdActions = a}) . _List1

instance AWSRequest CreateDataset where
        type Rs CreateDataset = CreateDatasetResponse
        request = postJSON ioTAnalytics
        response
          = receiveJSON
              (\ s h x ->
                 CreateDatasetResponse' <$>
                   (x .?> "datasetArn") <*> (x .?> "datasetName") <*>
                     (pure (fromEnum s)))

instance Hashable CreateDataset where

instance NFData CreateDataset where

instance ToHeaders CreateDataset where
        toHeaders = const mempty

instance ToJSON CreateDataset where
        toJSON CreateDataset'{..}
          = object
              (catMaybes
                 [("triggers" .=) <$> _cdTriggers,
                  Just ("datasetName" .= _cdDatasetName),
                  Just ("actions" .= _cdActions)])

instance ToPath CreateDataset where
        toPath = const "/datasets"

instance ToQuery CreateDataset where
        toQuery = const mempty

-- | /See:/ 'createDatasetResponse' smart constructor.
data CreateDatasetResponse = CreateDatasetResponse'
  { _crsDatasetARN     :: !(Maybe Text)
  , _crsDatasetName    :: !(Maybe Text)
  , _crsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDatasetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsDatasetARN' - The ARN of the data set.
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
    , _crsDatasetName = Nothing
    , _crsResponseStatus = pResponseStatus_
    }


-- | The ARN of the data set.
crsDatasetARN :: Lens' CreateDatasetResponse (Maybe Text)
crsDatasetARN = lens _crsDatasetARN (\ s a -> s{_crsDatasetARN = a})

-- | The name of the data set.
crsDatasetName :: Lens' CreateDatasetResponse (Maybe Text)
crsDatasetName = lens _crsDatasetName (\ s a -> s{_crsDatasetName = a})

-- | -- | The response status code.
crsResponseStatus :: Lens' CreateDatasetResponse Int
crsResponseStatus = lens _crsResponseStatus (\ s a -> s{_crsResponseStatus = a})

instance NFData CreateDatasetResponse where
