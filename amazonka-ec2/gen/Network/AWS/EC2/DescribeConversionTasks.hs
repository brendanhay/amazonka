{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeConversionTasks
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes one or more of your conversion tasks. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UploadingYourInstancesandVolumes.html Using the Command Line Tools to Import Your Virtual Machine to Amazon EC2> in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeConversionTasks.html>
module Network.AWS.EC2.DescribeConversionTasks
    (
    -- * Request
      DescribeConversionTasks
    -- ** Request constructor
    , describeConversionTasks
    -- ** Request lenses
    , dctConversionTaskIds
    , dctDryRun
    , dctFilters

    -- * Response
    , DescribeConversionTasksResponse
    -- ** Response constructor
    , describeConversionTasksResponse
    -- ** Response lenses
    , dctrConversionTasks
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeConversionTasks = DescribeConversionTasks
    { _dctConversionTaskIds :: List "item" Text
    , _dctDryRun            :: Maybe Bool
    , _dctFilters           :: List "Filter" Filter
    } deriving (Eq, Read, Show)

-- | 'DescribeConversionTasks' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dctConversionTaskIds' @::@ ['Text']
--
-- * 'dctDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dctFilters' @::@ ['Filter']
--
describeConversionTasks :: DescribeConversionTasks
describeConversionTasks = DescribeConversionTasks
    { _dctDryRun            = Nothing
    , _dctFilters           = mempty
    , _dctConversionTaskIds = mempty
    }

-- | One or more conversion task IDs.
dctConversionTaskIds :: Lens' DescribeConversionTasks [Text]
dctConversionTaskIds =
    lens _dctConversionTaskIds (\s a -> s { _dctConversionTaskIds = a })
        . _List

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
dctDryRun :: Lens' DescribeConversionTasks (Maybe Bool)
dctDryRun = lens _dctDryRun (\s a -> s { _dctDryRun = a })

-- | One or more filters.
dctFilters :: Lens' DescribeConversionTasks [Filter]
dctFilters = lens _dctFilters (\s a -> s { _dctFilters = a }) . _List

newtype DescribeConversionTasksResponse = DescribeConversionTasksResponse
    { _dctrConversionTasks :: List "item" ConversionTask
    } deriving (Eq, Read, Show, Monoid, Semigroup)

-- | 'DescribeConversionTasksResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dctrConversionTasks' @::@ ['ConversionTask']
--
describeConversionTasksResponse :: DescribeConversionTasksResponse
describeConversionTasksResponse = DescribeConversionTasksResponse
    { _dctrConversionTasks = mempty
    }

-- | Information about the conversion tasks.
dctrConversionTasks :: Lens' DescribeConversionTasksResponse [ConversionTask]
dctrConversionTasks =
    lens _dctrConversionTasks (\s a -> s { _dctrConversionTasks = a })
        . _List

instance ToPath DescribeConversionTasks where
    toPath = const "/"

instance ToQuery DescribeConversionTasks where
    toQuery DescribeConversionTasks{..} = mconcat
        [ "ConversionTaskId" `toQueryList` _dctConversionTaskIds
        , "DryRun"           =? _dctDryRun
        , "Filter"           `toQueryList` _dctFilters
        ]

instance ToHeaders DescribeConversionTasks

instance AWSRequest DescribeConversionTasks where
    type Sv DescribeConversionTasks = EC2
    type Rs DescribeConversionTasks = DescribeConversionTasksResponse

    request  = post "DescribeConversionTasks"
    response = xmlResponse

instance FromXML DescribeConversionTasksResponse where
    parseXML x = DescribeConversionTasksResponse
        <$> x .@? "conversionTasks" .!@ mempty
