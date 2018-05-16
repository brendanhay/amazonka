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
-- Module      : Network.AWS.IoTAnalytics.DescribeDataset
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a data set.
--
--
module Network.AWS.IoTAnalytics.DescribeDataset
    (
    -- * Creating a Request
      describeDataset
    , DescribeDataset
    -- * Request Lenses
    , ddDatasetName

    -- * Destructuring the Response
    , describeDatasetResponse
    , DescribeDatasetResponse
    -- * Response Lenses
    , ddrsDataset
    , ddrsResponseStatus
    ) where

import Network.AWS.IoTAnalytics.Types
import Network.AWS.IoTAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeDataset' smart constructor.
newtype DescribeDataset = DescribeDataset'
  { _ddDatasetName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDataset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddDatasetName' - The name of the data set whose information is retrieved.
describeDataset
    :: Text -- ^ 'ddDatasetName'
    -> DescribeDataset
describeDataset pDatasetName_ =
  DescribeDataset' {_ddDatasetName = pDatasetName_}


-- | The name of the data set whose information is retrieved.
ddDatasetName :: Lens' DescribeDataset Text
ddDatasetName = lens _ddDatasetName (\ s a -> s{_ddDatasetName = a})

instance AWSRequest DescribeDataset where
        type Rs DescribeDataset = DescribeDatasetResponse
        request = get ioTAnalytics
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDatasetResponse' <$>
                   (x .?> "dataset") <*> (pure (fromEnum s)))

instance Hashable DescribeDataset where

instance NFData DescribeDataset where

instance ToHeaders DescribeDataset where
        toHeaders = const mempty

instance ToPath DescribeDataset where
        toPath DescribeDataset'{..}
          = mconcat ["/datasets/", toBS _ddDatasetName]

instance ToQuery DescribeDataset where
        toQuery = const mempty

-- | /See:/ 'describeDatasetResponse' smart constructor.
data DescribeDatasetResponse = DescribeDatasetResponse'
  { _ddrsDataset        :: !(Maybe Dataset)
  , _ddrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDatasetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddrsDataset' - An object that contains information about the data set.
--
-- * 'ddrsResponseStatus' - -- | The response status code.
describeDatasetResponse
    :: Int -- ^ 'ddrsResponseStatus'
    -> DescribeDatasetResponse
describeDatasetResponse pResponseStatus_ =
  DescribeDatasetResponse'
    {_ddrsDataset = Nothing, _ddrsResponseStatus = pResponseStatus_}


-- | An object that contains information about the data set.
ddrsDataset :: Lens' DescribeDatasetResponse (Maybe Dataset)
ddrsDataset = lens _ddrsDataset (\ s a -> s{_ddrsDataset = a})

-- | -- | The response status code.
ddrsResponseStatus :: Lens' DescribeDatasetResponse Int
ddrsResponseStatus = lens _ddrsResponseStatus (\ s a -> s{_ddrsResponseStatus = a})

instance NFData DescribeDatasetResponse where
