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
-- Module      : Network.AWS.EC2.DescribeImportImageTasks
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays details about an import virtual machine or import snapshot
-- tasks that are already created.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeImportImageTasks.html AWS API Reference> for DescribeImportImageTasks.
module Network.AWS.EC2.DescribeImportImageTasks
    (
    -- * Creating a Request
      describeImportImageTasks
    , DescribeImportImageTasks
    -- * Request Lenses
    , diitFilters
    , diitImportTaskIds
    , diitNextToken
    , diitDryRun
    , diitMaxResults

    -- * Destructuring the Response
    , describeImportImageTasksResponse
    , DescribeImportImageTasksResponse
    -- * Response Lenses
    , diitrsImportImageTasks
    , diitrsNextToken
    , diitrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeImportImageTasks' smart constructor.
data DescribeImportImageTasks = DescribeImportImageTasks'
    { _diitFilters       :: !(Maybe [Filter])
    , _diitImportTaskIds :: !(Maybe [Text])
    , _diitNextToken     :: !(Maybe Text)
    , _diitDryRun        :: !(Maybe Bool)
    , _diitMaxResults    :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeImportImageTasks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diitFilters'
--
-- * 'diitImportTaskIds'
--
-- * 'diitNextToken'
--
-- * 'diitDryRun'
--
-- * 'diitMaxResults'
describeImportImageTasks
    :: DescribeImportImageTasks
describeImportImageTasks =
    DescribeImportImageTasks'
    { _diitFilters = Nothing
    , _diitImportTaskIds = Nothing
    , _diitNextToken = Nothing
    , _diitDryRun = Nothing
    , _diitMaxResults = Nothing
    }

-- | One or more filters.
diitFilters :: Lens' DescribeImportImageTasks [Filter]
diitFilters = lens _diitFilters (\ s a -> s{_diitFilters = a}) . _Default . _Coerce;

-- | A list of import image task IDs.
diitImportTaskIds :: Lens' DescribeImportImageTasks [Text]
diitImportTaskIds = lens _diitImportTaskIds (\ s a -> s{_diitImportTaskIds = a}) . _Default . _Coerce;

-- | A token that indicates the next page of results.
diitNextToken :: Lens' DescribeImportImageTasks (Maybe Text)
diitNextToken = lens _diitNextToken (\ s a -> s{_diitNextToken = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
diitDryRun :: Lens' DescribeImportImageTasks (Maybe Bool)
diitDryRun = lens _diitDryRun (\ s a -> s{_diitDryRun = a});

-- | The maximum number of results to return in a single request.
diitMaxResults :: Lens' DescribeImportImageTasks (Maybe Int)
diitMaxResults = lens _diitMaxResults (\ s a -> s{_diitMaxResults = a});

instance AWSRequest DescribeImportImageTasks where
        type Rs DescribeImportImageTasks =
             DescribeImportImageTasksResponse
        request = postQuery eC2
        response
          = receiveXML
              (\ s h x ->
                 DescribeImportImageTasksResponse' <$>
                   (x .@? "importImageTaskSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "nextToken")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeImportImageTasks where
        toHeaders = const mempty

instance ToPath DescribeImportImageTasks where
        toPath = const "/"

instance ToQuery DescribeImportImageTasks where
        toQuery DescribeImportImageTasks'{..}
          = mconcat
              ["Action" =:
                 ("DescribeImportImageTasks" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "Filters" <$> _diitFilters),
               toQuery
                 (toQueryList "ImportTaskId" <$> _diitImportTaskIds),
               "NextToken" =: _diitNextToken,
               "DryRun" =: _diitDryRun,
               "MaxResults" =: _diitMaxResults]

-- | /See:/ 'describeImportImageTasksResponse' smart constructor.
data DescribeImportImageTasksResponse = DescribeImportImageTasksResponse'
    { _diitrsImportImageTasks :: !(Maybe [ImportImageTask])
    , _diitrsNextToken        :: !(Maybe Text)
    , _diitrsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeImportImageTasksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diitrsImportImageTasks'
--
-- * 'diitrsNextToken'
--
-- * 'diitrsStatus'
describeImportImageTasksResponse
    :: Int -- ^ 'diitrsStatus'
    -> DescribeImportImageTasksResponse
describeImportImageTasksResponse pStatus_ =
    DescribeImportImageTasksResponse'
    { _diitrsImportImageTasks = Nothing
    , _diitrsNextToken = Nothing
    , _diitrsStatus = pStatus_
    }

-- | A list of zero or more import image tasks that are currently active or
-- were completed or canceled in the previous 7 days.
diitrsImportImageTasks :: Lens' DescribeImportImageTasksResponse [ImportImageTask]
diitrsImportImageTasks = lens _diitrsImportImageTasks (\ s a -> s{_diitrsImportImageTasks = a}) . _Default . _Coerce;

-- | The token to use to get the next page of results. This value is 'null'
-- when there are no more results to return.
diitrsNextToken :: Lens' DescribeImportImageTasksResponse (Maybe Text)
diitrsNextToken = lens _diitrsNextToken (\ s a -> s{_diitrsNextToken = a});

-- | The response status code.
diitrsStatus :: Lens' DescribeImportImageTasksResponse Int
diitrsStatus = lens _diitrsStatus (\ s a -> s{_diitrsStatus = a});
