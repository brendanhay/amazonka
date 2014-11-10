{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.EC2.DescribeExportTasks
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes one or more of your export tasks.
module Network.AWS.EC2.DescribeExportTasks
    (
    -- * Request
      DescribeExportTasks
    -- ** Request constructor
    , describeExportTasks
    -- ** Request lenses
    , detExportTaskIds

    -- * Response
    , DescribeExportTasksResult
    -- ** Response constructor
    , describeExportTasksResponse
    -- ** Response lenses
    , detrExportTasks
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

newtype DescribeExportTasks = DescribeExportTasks
    { _detExportTaskIds :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'DescribeExportTasks' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'detExportTaskIds' @::@ ['Text']
--
describeExportTasks :: DescribeExportTasks
describeExportTasks = DescribeExportTasks
    { _detExportTaskIds = mempty
    }

-- | One or more export task IDs.
detExportTaskIds :: Lens' DescribeExportTasks [Text]
detExportTaskIds = lens _detExportTaskIds (\s a -> s { _detExportTaskIds = a })

instance ToPath DescribeExportTasks where
    toPath = const "/"

instance ToQuery DescribeExportTasks

newtype DescribeExportTasksResult = DescribeExportTasksResult
    { _detrExportTasks :: [ExportTask]
    } deriving (Eq, Show, Generic, Monoid)

-- | 'DescribeExportTasksResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'detrExportTasks' @::@ ['ExportTask']
--
describeExportTasksResponse :: DescribeExportTasksResult
describeExportTasksResponse = DescribeExportTasksResult
    { _detrExportTasks = mempty
    }

detrExportTasks :: Lens' DescribeExportTasksResult [ExportTask]
detrExportTasks = lens _detrExportTasks (\s a -> s { _detrExportTasks = a })

instance AWSRequest DescribeExportTasks where
    type Sv DescribeExportTasks = EC2
    type Rs DescribeExportTasks = DescribeExportTasksResult

    request  = post "DescribeExportTasks"
    response = xmlResponse $ \h x -> DescribeExportTasksResult
        <$> x %| "exportTaskSet"
