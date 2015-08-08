{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeConversionTasks
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your conversion tasks. For more information,
-- see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UploadingYourInstancesandVolumes.html Using the Command Line Tools to Import Your Virtual Machine to Amazon EC2>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeConversionTasks.html AWS API Reference> for DescribeConversionTasks.
module Network.AWS.EC2.DescribeConversionTasks
    (
    -- * Creating a Request
      DescribeConversionTasks
    , describeConversionTasks
    -- * Request Lenses
    , dctConversionTaskIds
    , dctFilters
    , dctDryRun

    -- * Destructuring the Response
    , DescribeConversionTasksResponse
    , describeConversionTasksResponse
    -- * Response Lenses
    , dctrsConversionTasks
    , dctrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeConversionTasks' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dctConversionTaskIds'
--
-- * 'dctFilters'
--
-- * 'dctDryRun'
data DescribeConversionTasks = DescribeConversionTasks'
    { _dctConversionTaskIds :: !(Maybe [Text])
    , _dctFilters           :: !(Maybe [Filter])
    , _dctDryRun            :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeConversionTasks' smart constructor.
describeConversionTasks :: DescribeConversionTasks
describeConversionTasks =
    DescribeConversionTasks'
    { _dctConversionTaskIds = Nothing
    , _dctFilters = Nothing
    , _dctDryRun = Nothing
    }

-- | One or more conversion task IDs.
dctConversionTaskIds :: Lens' DescribeConversionTasks [Text]
dctConversionTaskIds = lens _dctConversionTaskIds (\ s a -> s{_dctConversionTaskIds = a}) . _Default . _Coerce;

-- | One or more filters.
dctFilters :: Lens' DescribeConversionTasks [Filter]
dctFilters = lens _dctFilters (\ s a -> s{_dctFilters = a}) . _Default . _Coerce;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dctDryRun :: Lens' DescribeConversionTasks (Maybe Bool)
dctDryRun = lens _dctDryRun (\ s a -> s{_dctDryRun = a});

instance AWSRequest DescribeConversionTasks where
        type Sv DescribeConversionTasks = EC2
        type Rs DescribeConversionTasks =
             DescribeConversionTasksResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeConversionTasksResponse' <$>
                   (x .@? "conversionTasks" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeConversionTasks where
        toHeaders = const mempty

instance ToPath DescribeConversionTasks where
        toPath = const "/"

instance ToQuery DescribeConversionTasks where
        toQuery DescribeConversionTasks'{..}
          = mconcat
              ["Action" =:
                 ("DescribeConversionTasks" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery
                 (toQueryList "item" <$> _dctConversionTaskIds),
               toQuery (toQueryList "Filter" <$> _dctFilters),
               "DryRun" =: _dctDryRun]

-- | /See:/ 'describeConversionTasksResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dctrsConversionTasks'
--
-- * 'dctrsStatus'
data DescribeConversionTasksResponse = DescribeConversionTasksResponse'
    { _dctrsConversionTasks :: !(Maybe [ConversionTask])
    , _dctrsStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeConversionTasksResponse' smart constructor.
describeConversionTasksResponse :: Int -> DescribeConversionTasksResponse
describeConversionTasksResponse pStatus_ =
    DescribeConversionTasksResponse'
    { _dctrsConversionTasks = Nothing
    , _dctrsStatus = pStatus_
    }

-- | Information about the conversion tasks.
dctrsConversionTasks :: Lens' DescribeConversionTasksResponse [ConversionTask]
dctrsConversionTasks = lens _dctrsConversionTasks (\ s a -> s{_dctrsConversionTasks = a}) . _Default . _Coerce;

-- | Undocumented member.
dctrsStatus :: Lens' DescribeConversionTasksResponse Int
dctrsStatus = lens _dctrsStatus (\ s a -> s{_dctrsStatus = a});
