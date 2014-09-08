{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.DescribeResize
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
-- https://redshift.us-east-1.amazonaws.com/ ?Action=DescribeResize
-- &ClusterIdentifier=examplecluster &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20121207/us-east-1/redshift/aws4_request
-- &x-amz-date=20121207T232427Z
-- &x-amz-signedheaders=content-type;host;x-amz-date multi-node SUCCEEDED
-- 6.5263 66922 0 users venue sales listing event date category 10254
-- dw1.xlarge 2 a6d59c61-a162-11e2-b2bc-fb54c9d11e09.
module Network.AWS.Redshift.V2012_12_01.DescribeResize
    (
    -- * Request
      DescribeResize
    -- ** Request constructor
    , mkDescribeResize
    -- ** Request lenses
    , drClusterIdentifier

    -- * Response
    , DescribeResizeResponse
    -- ** Response constructor
    , mkDescribeResizeResponse
    -- ** Response lenses
    , drrTargetNodeType
    , drrTargetNumberOfNodes
    , drrTargetClusterType
    , drrStatus
    , drrImportTablesCompleted
    , drrImportTablesInProgress
    , drrImportTablesNotStarted
    , drrAvgResizeRateInMegaBytesPerSecond
    , drrTotalResizeDataInMegaBytes
    , drrProgressInMegaBytes
    , drrElapsedTimeInSeconds
    , drrEstimatedTimeToCompletionInSeconds
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | 
newtype DescribeResize = DescribeResize
    { _drClusterIdentifier :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeResize' request.
mkDescribeResize :: Text -- ^ 'drClusterIdentifier'
                 -> DescribeResize
mkDescribeResize p1 = DescribeResize
    { _drClusterIdentifier = p1
    }

-- | The unique identifier of a cluster whose resize progress you are
-- requesting. This parameter isn't case-sensitive. By default, resize
-- operations for all clusters defined for an AWS account are returned.
drClusterIdentifier :: Lens' DescribeResize Text
drClusterIdentifier =
    lens _drClusterIdentifier (\s a -> s { _drClusterIdentifier = a })

instance ToQuery DescribeResize where
    toQuery = genericQuery def

-- | Describes the result of a cluster resize operation.
data DescribeResizeResponse = DescribeResizeResponse
    { _drrTargetNodeType :: Maybe Text
    , _drrTargetNumberOfNodes :: Maybe Integer
    , _drrTargetClusterType :: Maybe Text
    , _drrStatus :: Maybe Text
    , _drrImportTablesCompleted :: [Text]
    , _drrImportTablesInProgress :: [Text]
    , _drrImportTablesNotStarted :: [Text]
    , _drrAvgResizeRateInMegaBytesPerSecond :: Maybe Double
    , _drrTotalResizeDataInMegaBytes :: Maybe Integer
    , _drrProgressInMegaBytes :: Maybe Integer
    , _drrElapsedTimeInSeconds :: Maybe Integer
    , _drrEstimatedTimeToCompletionInSeconds :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeResizeResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDescribeResizeResponse :: DescribeResizeResponse
mkDescribeResizeResponse = DescribeResizeResponse
    { _drrTargetNodeType = Nothing
    , _drrTargetNumberOfNodes = Nothing
    , _drrTargetClusterType = Nothing
    , _drrStatus = Nothing
    , _drrImportTablesCompleted = mempty
    , _drrImportTablesInProgress = mempty
    , _drrImportTablesNotStarted = mempty
    , _drrAvgResizeRateInMegaBytesPerSecond = Nothing
    , _drrTotalResizeDataInMegaBytes = Nothing
    , _drrProgressInMegaBytes = Nothing
    , _drrElapsedTimeInSeconds = Nothing
    , _drrEstimatedTimeToCompletionInSeconds = Nothing
    }

-- | The node type that the cluster will have after the resize operation is
-- complete.
drrTargetNodeType :: Lens' DescribeResizeResponse (Maybe Text)
drrTargetNodeType =
    lens _drrTargetNodeType (\s a -> s { _drrTargetNodeType = a })

-- | The number of nodes that the cluster will have after the resize operation
-- is complete.
drrTargetNumberOfNodes :: Lens' DescribeResizeResponse (Maybe Integer)
drrTargetNumberOfNodes =
    lens _drrTargetNumberOfNodes (\s a -> s { _drrTargetNumberOfNodes = a })

-- | The cluster type after the resize operation is complete. Valid Values:
-- multi-node | single-node.
drrTargetClusterType :: Lens' DescribeResizeResponse (Maybe Text)
drrTargetClusterType =
    lens _drrTargetClusterType (\s a -> s { _drrTargetClusterType = a })

-- | The status of the resize operation. Valid Values: NONE | IN_PROGRESS |
-- FAILED | SUCCEEDED.
drrStatus :: Lens' DescribeResizeResponse (Maybe Text)
drrStatus = lens _drrStatus (\s a -> s { _drrStatus = a })

-- | The names of tables that have been completely imported . Valid Values: List
-- of table names.
drrImportTablesCompleted :: Lens' DescribeResizeResponse [Text]
drrImportTablesCompleted =
    lens _drrImportTablesCompleted
         (\s a -> s { _drrImportTablesCompleted = a })

-- | The names of tables that are being currently imported. Valid Values: List
-- of table names.
drrImportTablesInProgress :: Lens' DescribeResizeResponse [Text]
drrImportTablesInProgress =
    lens _drrImportTablesInProgress
         (\s a -> s { _drrImportTablesInProgress = a })

-- | The names of tables that have not been yet imported. Valid Values: List of
-- table names.
drrImportTablesNotStarted :: Lens' DescribeResizeResponse [Text]
drrImportTablesNotStarted =
    lens _drrImportTablesNotStarted
         (\s a -> s { _drrImportTablesNotStarted = a })

-- | The average rate of the resize operation over the last few minutes,
-- measured in megabytes per second. After the resize operation completes,
-- this value shows the average rate of the entire resize operation.
drrAvgResizeRateInMegaBytesPerSecond :: Lens' DescribeResizeResponse (Maybe Double)
drrAvgResizeRateInMegaBytesPerSecond =
    lens _drrAvgResizeRateInMegaBytesPerSecond
         (\s a -> s { _drrAvgResizeRateInMegaBytesPerSecond = a })

-- | The estimated total amount of data, in megabytes, on the cluster before the
-- resize operation began.
drrTotalResizeDataInMegaBytes :: Lens' DescribeResizeResponse (Maybe Integer)
drrTotalResizeDataInMegaBytes =
    lens _drrTotalResizeDataInMegaBytes
         (\s a -> s { _drrTotalResizeDataInMegaBytes = a })

-- | While the resize operation is in progress, this value shows the current
-- amount of data, in megabytes, that has been processed so far. When the
-- resize operation is complete, this value shows the total amount of data, in
-- megabytes, on the cluster, which may be more or less than
-- TotalResizeDataInMegaBytes (the estimated total amount of data before
-- resize).
drrProgressInMegaBytes :: Lens' DescribeResizeResponse (Maybe Integer)
drrProgressInMegaBytes =
    lens _drrProgressInMegaBytes (\s a -> s { _drrProgressInMegaBytes = a })

-- | The amount of seconds that have elapsed since the resize operation began.
-- After the resize operation completes, this value shows the total actual
-- time, in seconds, for the resize operation.
drrElapsedTimeInSeconds :: Lens' DescribeResizeResponse (Maybe Integer)
drrElapsedTimeInSeconds =
    lens _drrElapsedTimeInSeconds
         (\s a -> s { _drrElapsedTimeInSeconds = a })

-- | The estimated time remaining, in seconds, until the resize operation is
-- complete. This value is calculated based on the average resize rate and the
-- estimated amount of data remaining to be processed. Once the resize
-- operation is complete, this value will be 0.
drrEstimatedTimeToCompletionInSeconds :: Lens' DescribeResizeResponse (Maybe Integer)
drrEstimatedTimeToCompletionInSeconds =
    lens _drrEstimatedTimeToCompletionInSeconds
         (\s a -> s { _drrEstimatedTimeToCompletionInSeconds = a })

instance FromXML DescribeResizeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeResize where
    type Sv DescribeResize = Redshift
    type Rs DescribeResize = DescribeResizeResponse

    request = post "DescribeResize"
    response _ = xmlResponse
