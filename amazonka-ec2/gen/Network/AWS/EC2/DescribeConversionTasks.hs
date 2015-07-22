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
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeConversionTasks.html>
module Network.AWS.EC2.DescribeConversionTasks
    (
    -- * Request
      DescribeConversionTasks
    -- ** Request constructor
    , describeConversionTasks
    -- ** Request lenses
    , dctrqConversionTaskIds
    , dctrqFilters
    , dctrqDryRun

    -- * Response
    , DescribeConversionTasksResponse
    -- ** Response constructor
    , describeConversionTasksResponse
    -- ** Response lenses
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
-- * 'dctrqConversionTaskIds'
--
-- * 'dctrqFilters'
--
-- * 'dctrqDryRun'
data DescribeConversionTasks = DescribeConversionTasks'
    { _dctrqConversionTaskIds :: !(Maybe [Text])
    , _dctrqFilters           :: !(Maybe [Filter])
    , _dctrqDryRun            :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeConversionTasks' smart constructor.
describeConversionTasks :: DescribeConversionTasks
describeConversionTasks =
    DescribeConversionTasks'
    { _dctrqConversionTaskIds = Nothing
    , _dctrqFilters = Nothing
    , _dctrqDryRun = Nothing
    }

-- | One or more conversion task IDs.
dctrqConversionTaskIds :: Lens' DescribeConversionTasks [Text]
dctrqConversionTaskIds = lens _dctrqConversionTaskIds (\ s a -> s{_dctrqConversionTaskIds = a}) . _Default;

-- | One or more filters.
dctrqFilters :: Lens' DescribeConversionTasks [Filter]
dctrqFilters = lens _dctrqFilters (\ s a -> s{_dctrqFilters = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dctrqDryRun :: Lens' DescribeConversionTasks (Maybe Bool)
dctrqDryRun = lens _dctrqDryRun (\ s a -> s{_dctrqDryRun = a});

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
                 (toQueryList "item" <$> _dctrqConversionTaskIds),
               toQuery (toQueryList "Filter" <$> _dctrqFilters),
               "DryRun" =: _dctrqDryRun]

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
describeConversionTasksResponse pStatus =
    DescribeConversionTasksResponse'
    { _dctrsConversionTasks = Nothing
    , _dctrsStatus = pStatus
    }

-- | Information about the conversion tasks.
dctrsConversionTasks :: Lens' DescribeConversionTasksResponse [ConversionTask]
dctrsConversionTasks = lens _dctrsConversionTasks (\ s a -> s{_dctrsConversionTasks = a}) . _Default;

-- | FIXME: Undocumented member.
dctrsStatus :: Lens' DescribeConversionTasksResponse Int
dctrsStatus = lens _dctrsStatus (\ s a -> s{_dctrsStatus = a});
