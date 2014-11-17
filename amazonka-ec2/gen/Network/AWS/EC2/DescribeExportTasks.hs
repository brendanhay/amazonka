{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
    , detrExportTasks
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

newtype DescribeExportTasks = DescribeExportTasks
    { _detExportTaskIds :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeExportTasks where
    type Item DescribeExportTasks = Text

    fromList = DescribeExportTasks . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _detExportTaskIds

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

newtype DescribeExportTasksResponse = DescribeExportTasksResponse
    { _detrExportTasks :: [ExportTask]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeExportTasksResponse where
    type Item DescribeExportTasksResponse = ExportTask

    fromList = DescribeExportTasksResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _detrExportTasks

-- | 'DescribeExportTasksResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'detrExportTasks' @::@ ['ExportTask']
--
describeExportTasksResponse :: DescribeExportTasksResponse
describeExportTasksResponse = DescribeExportTasksResponse
    { _detrExportTasks = mempty
    }

detrExportTasks :: Lens' DescribeExportTasksResponse [ExportTask]
detrExportTasks = lens _detrExportTasks (\s a -> s { _detrExportTasks = a })

instance AWSRequest DescribeExportTasks where
    type Sv DescribeExportTasks = EC2
    type Rs DescribeExportTasks = DescribeExportTasksResponse

    request  = post "DescribeExportTasks"
    response = xmlResponse

instance FromXML DescribeExportTasksResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeExportTasksResponse"

instance ToPath DescribeExportTasks where
    toPath = const "/"

instance ToHeaders DescribeExportTasks

instance ToQuery DescribeExportTasks where
    toQuery rq = "exportTaskId" =? _detExportTaskIds rq

instance ToXML DescribeExportTasks where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "DescribeExportTasks"
