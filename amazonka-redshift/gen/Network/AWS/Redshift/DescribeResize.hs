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

-- Module      : Network.AWS.Redshift.DescribeResize
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about the last resize operation for the specified
-- cluster. If no resize operation has ever been initiated for the specified
-- cluster, a HTTP 404 error is returned. If a resize operation was initiated
-- and completed, the status of the resize remains as SUCCEEDED until the next
-- resize. A resize operation can be requested using ModifyCluster and
-- specifying a different number or type of nodes for the cluster.
module Network.AWS.Redshift.DescribeResize
    (
    -- * Request
      DescribeResizeMessage
    -- ** Request constructor
    , describeResizeMessage
    -- ** Request lenses
    , drmClusterIdentifier

    -- * Response
    , ResizeProgressMessage
    -- ** Response constructor
    , resizeProgressMessage
    -- ** Response lenses
    , rpmAvgResizeRateInMegaBytesPerSecond
    , rpmElapsedTimeInSeconds
    , rpmEstimatedTimeToCompletionInSeconds
    , rpmImportTablesCompleted
    , rpmImportTablesInProgress
    , rpmImportTablesNotStarted
    , rpmProgressInMegaBytes
    , rpmStatus
    , rpmTargetClusterType
    , rpmTargetNodeType
    , rpmTargetNumberOfNodes
    , rpmTotalResizeDataInMegaBytes
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

newtype DescribeResizeMessage = DescribeResizeMessage
    { _drmClusterIdentifier :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DescribeResizeMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drmClusterIdentifier' @::@ 'Text'
--
describeResizeMessage :: Text -- ^ 'drmClusterIdentifier'
                      -> DescribeResizeMessage
describeResizeMessage p1 = DescribeResizeMessage
    { _drmClusterIdentifier = p1
    }

-- | The unique identifier of a cluster whose resize progress you are
-- requesting. This parameter is case-sensitive. By default, resize
-- operations for all clusters defined for an AWS account are returned.
drmClusterIdentifier :: Lens' DescribeResizeMessage Text
drmClusterIdentifier =
    lens _drmClusterIdentifier (\s a -> s { _drmClusterIdentifier = a })

instance ToQuery DescribeResizeMessage

instance ToPath DescribeResizeMessage where
    toPath = const "/"

data ResizeProgressMessage = ResizeProgressMessage
    { _rpmAvgResizeRateInMegaBytesPerSecond  :: Maybe Double
    , _rpmElapsedTimeInSeconds               :: Maybe Integer
    , _rpmEstimatedTimeToCompletionInSeconds :: Maybe Integer
    , _rpmImportTablesCompleted              :: [Text]
    , _rpmImportTablesInProgress             :: [Text]
    , _rpmImportTablesNotStarted             :: [Text]
    , _rpmProgressInMegaBytes                :: Maybe Integer
    , _rpmStatus                             :: Maybe Text
    , _rpmTargetClusterType                  :: Maybe Text
    , _rpmTargetNodeType                     :: Maybe Text
    , _rpmTargetNumberOfNodes                :: Maybe Int
    , _rpmTotalResizeDataInMegaBytes         :: Maybe Integer
    } deriving (Eq, Ord, Show, Generic)

-- | 'ResizeProgressMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rpmAvgResizeRateInMegaBytesPerSecond' @::@ 'Maybe' 'Double'
--
-- * 'rpmElapsedTimeInSeconds' @::@ 'Maybe' 'Integer'
--
-- * 'rpmEstimatedTimeToCompletionInSeconds' @::@ 'Maybe' 'Integer'
--
-- * 'rpmImportTablesCompleted' @::@ ['Text']
--
-- * 'rpmImportTablesInProgress' @::@ ['Text']
--
-- * 'rpmImportTablesNotStarted' @::@ ['Text']
--
-- * 'rpmProgressInMegaBytes' @::@ 'Maybe' 'Integer'
--
-- * 'rpmStatus' @::@ 'Maybe' 'Text'
--
-- * 'rpmTargetClusterType' @::@ 'Maybe' 'Text'
--
-- * 'rpmTargetNodeType' @::@ 'Maybe' 'Text'
--
-- * 'rpmTargetNumberOfNodes' @::@ 'Maybe' 'Int'
--
-- * 'rpmTotalResizeDataInMegaBytes' @::@ 'Maybe' 'Integer'
--
resizeProgressMessage :: ResizeProgressMessage
resizeProgressMessage = ResizeProgressMessage
    { _rpmTargetNodeType                     = Nothing
    , _rpmTargetNumberOfNodes                = Nothing
    , _rpmTargetClusterType                  = Nothing
    , _rpmStatus                             = Nothing
    , _rpmImportTablesCompleted              = mempty
    , _rpmImportTablesInProgress             = mempty
    , _rpmImportTablesNotStarted             = mempty
    , _rpmAvgResizeRateInMegaBytesPerSecond  = Nothing
    , _rpmTotalResizeDataInMegaBytes         = Nothing
    , _rpmProgressInMegaBytes                = Nothing
    , _rpmElapsedTimeInSeconds               = Nothing
    , _rpmEstimatedTimeToCompletionInSeconds = Nothing
    }

-- | The average rate of the resize operation over the last few minutes,
-- measured in megabytes per second. After the resize operation completes,
-- this value shows the average rate of the entire resize operation.
rpmAvgResizeRateInMegaBytesPerSecond :: Lens' ResizeProgressMessage (Maybe Double)
rpmAvgResizeRateInMegaBytesPerSecond =
    lens _rpmAvgResizeRateInMegaBytesPerSecond
        (\s a -> s { _rpmAvgResizeRateInMegaBytesPerSecond = a })

-- | The amount of seconds that have elapsed since the resize operation began.
-- After the resize operation completes, this value shows the total actual
-- time, in seconds, for the resize operation.
rpmElapsedTimeInSeconds :: Lens' ResizeProgressMessage (Maybe Integer)
rpmElapsedTimeInSeconds =
    lens _rpmElapsedTimeInSeconds (\s a -> s { _rpmElapsedTimeInSeconds = a })

-- | The estimated time remaining, in seconds, until the resize operation is
-- complete. This value is calculated based on the average resize rate and
-- the estimated amount of data remaining to be processed. Once the resize
-- operation is complete, this value will be 0.
rpmEstimatedTimeToCompletionInSeconds :: Lens' ResizeProgressMessage (Maybe Integer)
rpmEstimatedTimeToCompletionInSeconds =
    lens _rpmEstimatedTimeToCompletionInSeconds
        (\s a -> s { _rpmEstimatedTimeToCompletionInSeconds = a })

-- | The names of tables that have been completely imported . Valid Values:
-- List of table names.
rpmImportTablesCompleted :: Lens' ResizeProgressMessage [Text]
rpmImportTablesCompleted =
    lens _rpmImportTablesCompleted
        (\s a -> s { _rpmImportTablesCompleted = a })

-- | The names of tables that are being currently imported. Valid Values: List
-- of table names.
rpmImportTablesInProgress :: Lens' ResizeProgressMessage [Text]
rpmImportTablesInProgress =
    lens _rpmImportTablesInProgress
        (\s a -> s { _rpmImportTablesInProgress = a })

-- | The names of tables that have not been yet imported. Valid Values: List
-- of table names.
rpmImportTablesNotStarted :: Lens' ResizeProgressMessage [Text]
rpmImportTablesNotStarted =
    lens _rpmImportTablesNotStarted
        (\s a -> s { _rpmImportTablesNotStarted = a })

-- | While the resize operation is in progress, this value shows the current
-- amount of data, in megabytes, that has been processed so far. When the
-- resize operation is complete, this value shows the total amount of data,
-- in megabytes, on the cluster, which may be more or less than
-- TotalResizeDataInMegaBytes (the estimated total amount of data before
-- resize).
rpmProgressInMegaBytes :: Lens' ResizeProgressMessage (Maybe Integer)
rpmProgressInMegaBytes =
    lens _rpmProgressInMegaBytes (\s a -> s { _rpmProgressInMegaBytes = a })

-- | The status of the resize operation. Valid Values: NONE | IN_PROGRESS |
-- FAILED | SUCCEEDED.
rpmStatus :: Lens' ResizeProgressMessage (Maybe Text)
rpmStatus = lens _rpmStatus (\s a -> s { _rpmStatus = a })

-- | The cluster type after the resize operation is complete. Valid Values:
-- multi-node | single-node.
rpmTargetClusterType :: Lens' ResizeProgressMessage (Maybe Text)
rpmTargetClusterType =
    lens _rpmTargetClusterType (\s a -> s { _rpmTargetClusterType = a })

-- | The node type that the cluster will have after the resize operation is
-- complete.
rpmTargetNodeType :: Lens' ResizeProgressMessage (Maybe Text)
rpmTargetNodeType =
    lens _rpmTargetNodeType (\s a -> s { _rpmTargetNodeType = a })

-- | The number of nodes that the cluster will have after the resize operation
-- is complete.
rpmTargetNumberOfNodes :: Lens' ResizeProgressMessage (Maybe Int)
rpmTargetNumberOfNodes =
    lens _rpmTargetNumberOfNodes (\s a -> s { _rpmTargetNumberOfNodes = a })

-- | The estimated total amount of data, in megabytes, on the cluster before
-- the resize operation began.
rpmTotalResizeDataInMegaBytes :: Lens' ResizeProgressMessage (Maybe Integer)
rpmTotalResizeDataInMegaBytes =
    lens _rpmTotalResizeDataInMegaBytes
        (\s a -> s { _rpmTotalResizeDataInMegaBytes = a })

instance FromXML ResizeProgressMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ResizeProgressMessage"

instance AWSRequest DescribeResizeMessage where
    type Sv DescribeResizeMessage = Redshift
    type Rs DescribeResizeMessage = ResizeProgressMessage

    request  = post "DescribeResize"
    response = xmlResponse $ \h x -> ResizeProgressMessage
        <$> x %| "AvgResizeRateInMegaBytesPerSecond"
        <*> x %| "ElapsedTimeInSeconds"
        <*> x %| "EstimatedTimeToCompletionInSeconds"
        <*> x %| "ImportTablesCompleted"
        <*> x %| "ImportTablesInProgress"
        <*> x %| "ImportTablesNotStarted"
        <*> x %| "ProgressInMegaBytes"
        <*> x %| "Status"
        <*> x %| "TargetClusterType"
        <*> x %| "TargetNodeType"
        <*> x %| "TargetNumberOfNodes"
        <*> x %| "TotalResizeDataInMegaBytes"
