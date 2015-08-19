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
-- Module      : Network.AWS.EC2.DescribeExportTasks
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your export tasks.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeExportTasks.html AWS API Reference> for DescribeExportTasks.
module Network.AWS.EC2.DescribeExportTasks
    (
    -- * Creating a Request
      describeExportTasks
    , DescribeExportTasks
    -- * Request Lenses
    , detExportTaskIds

    -- * Destructuring the Response
    , describeExportTasksResponse
    , DescribeExportTasksResponse
    -- * Response Lenses
    , detrsExportTasks
    , detrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeExportTasks' smart constructor.
newtype DescribeExportTasks = DescribeExportTasks'
    { _detExportTaskIds :: Maybe [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeExportTasks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'detExportTaskIds'
describeExportTasks
    :: DescribeExportTasks
describeExportTasks =
    DescribeExportTasks'
    { _detExportTaskIds = Nothing
    }

-- | One or more export task IDs.
detExportTaskIds :: Lens' DescribeExportTasks [Text]
detExportTaskIds = lens _detExportTaskIds (\ s a -> s{_detExportTaskIds = a}) . _Default . _Coerce;

instance AWSRequest DescribeExportTasks where
        type Sv DescribeExportTasks = EC2
        type Rs DescribeExportTasks =
             DescribeExportTasksResponse
        request = postQuery
        response
          = receiveXML
              (\ s h x ->
                 DescribeExportTasksResponse' <$>
                   (x .@? "exportTaskSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeExportTasks where
        toHeaders = const mempty

instance ToPath DescribeExportTasks where
        toPath = const "/"

instance ToQuery DescribeExportTasks where
        toQuery DescribeExportTasks'{..}
          = mconcat
              ["Action" =: ("DescribeExportTasks" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery
                 (toQueryList "ExportTaskId" <$> _detExportTaskIds)]

-- | /See:/ 'describeExportTasksResponse' smart constructor.
data DescribeExportTasksResponse = DescribeExportTasksResponse'
    { _detrsExportTasks :: !(Maybe [ExportTask])
    , _detrsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeExportTasksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'detrsExportTasks'
--
-- * 'detrsStatus'
describeExportTasksResponse
    :: Int -- ^ 'detrsStatus'
    -> DescribeExportTasksResponse
describeExportTasksResponse pStatus_ =
    DescribeExportTasksResponse'
    { _detrsExportTasks = Nothing
    , _detrsStatus = pStatus_
    }

-- | Information about the export tasks.
detrsExportTasks :: Lens' DescribeExportTasksResponse [ExportTask]
detrsExportTasks = lens _detrsExportTasks (\ s a -> s{_detrsExportTasks = a}) . _Default . _Coerce;

-- | The response status code.
detrsStatus :: Lens' DescribeExportTasksResponse Int
detrsStatus = lens _detrsStatus (\ s a -> s{_detrsStatus = a});
