{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeExportTasks
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your export tasks.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeExportTasks.html>
module Network.AWS.EC2.DescribeExportTasks
    (
    -- * Request
      DescribeExportTasks
    -- ** Request constructor
    , describeExportTasks
    -- ** Request lenses
    , detExportTaskIds

    -- * Response
    , DescribeExportTasksResponse
    -- ** Response constructor
    , describeExportTasksResponse
    -- ** Response lenses
    , detrsExportTasks
    , detrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeExportTasks' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'detExportTaskIds'
newtype DescribeExportTasks = DescribeExportTasks'
    { _detExportTaskIds :: Maybe [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeExportTasks' smart constructor.
describeExportTasks :: DescribeExportTasks
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
        request = post
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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'detrsExportTasks'
--
-- * 'detrsStatus'
data DescribeExportTasksResponse = DescribeExportTasksResponse'
    { _detrsExportTasks :: !(Maybe [ExportTask])
    , _detrsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeExportTasksResponse' smart constructor.
describeExportTasksResponse :: Int -> DescribeExportTasksResponse
describeExportTasksResponse pStatus_ =
    DescribeExportTasksResponse'
    { _detrsExportTasks = Nothing
    , _detrsStatus = pStatus_
    }

-- | Information about the export tasks.
detrsExportTasks :: Lens' DescribeExportTasksResponse [ExportTask]
detrsExportTasks = lens _detrsExportTasks (\ s a -> s{_detrsExportTasks = a}) . _Default . _Coerce;

-- | FIXME: Undocumented member.
detrsStatus :: Lens' DescribeExportTasksResponse Int
detrsStatus = lens _detrsStatus (\ s a -> s{_detrsStatus = a});
