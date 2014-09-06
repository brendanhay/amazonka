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
    -- ** Response lenses
    , drrsTargetNodeType
    , drrsTargetNumberOfNodes
    , drrsTargetClusterType
    , drrsStatus
    , drrsImportTablesCompleted
    , drrsImportTablesInProgress
    , drrsImportTablesNotStarted
    , drrsAvgResizeRateInMegaBytesPerSecond
    , drrsTotalResizeDataInMegaBytes
    , drrsProgressInMegaBytes
    , drrsElapsedTimeInSeconds
    , drrsEstimatedTimeToCompletionInSeconds
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
{-# INLINE mkDescribeResize #-}

-- | The unique identifier of a cluster whose resize progress you are
-- requesting. This parameter isn't case-sensitive. By default, resize
-- operations for all clusters defined for an AWS account are returned.
drClusterIdentifier :: Lens' DescribeResize Text
drClusterIdentifier =
    lens _drClusterIdentifier (\s a -> s { _drClusterIdentifier = a })
{-# INLINE drClusterIdentifier #-}

instance ToQuery DescribeResize where
    toQuery = genericQuery def

-- | Describes the result of a cluster resize operation.
data DescribeResizeResponse = DescribeResizeResponse
    { _drrsTargetNodeType :: Maybe Text
    , _drrsTargetNumberOfNodes :: Maybe Integer
    , _drrsTargetClusterType :: Maybe Text
    , _drrsStatus :: Maybe Text
    , _drrsImportTablesCompleted :: [Text]
    , _drrsImportTablesInProgress :: [Text]
    , _drrsImportTablesNotStarted :: [Text]
    , _drrsAvgResizeRateInMegaBytesPerSecond :: Maybe Double
    , _drrsTotalResizeDataInMegaBytes :: Maybe Integer
    , _drrsProgressInMegaBytes :: Maybe Integer
    , _drrsElapsedTimeInSeconds :: Maybe Integer
    , _drrsEstimatedTimeToCompletionInSeconds :: Maybe Integer
    } deriving (Show, Generic)

-- | The node type that the cluster will have after the resize operation is
-- complete.
drrsTargetNodeType :: Lens' DescribeResizeResponse (Maybe Text)
drrsTargetNodeType =
    lens _drrsTargetNodeType (\s a -> s { _drrsTargetNodeType = a })
{-# INLINE drrsTargetNodeType #-}

-- | The number of nodes that the cluster will have after the resize operation
-- is complete.
drrsTargetNumberOfNodes :: Lens' DescribeResizeResponse (Maybe Integer)
drrsTargetNumberOfNodes =
    lens _drrsTargetNumberOfNodes
         (\s a -> s { _drrsTargetNumberOfNodes = a })
{-# INLINE drrsTargetNumberOfNodes #-}

-- | The cluster type after the resize operation is complete. Valid Values:
-- multi-node | single-node.
drrsTargetClusterType :: Lens' DescribeResizeResponse (Maybe Text)
drrsTargetClusterType =
    lens _drrsTargetClusterType (\s a -> s { _drrsTargetClusterType = a })
{-# INLINE drrsTargetClusterType #-}

-- | The status of the resize operation. Valid Values: NONE | IN_PROGRESS |
-- FAILED | SUCCEEDED.
drrsStatus :: Lens' DescribeResizeResponse (Maybe Text)
drrsStatus = lens _drrsStatus (\s a -> s { _drrsStatus = a })
{-# INLINE drrsStatus #-}

-- | The names of tables that have been completely imported . Valid Values: List
-- of table names.
drrsImportTablesCompleted :: Lens' DescribeResizeResponse [Text]
drrsImportTablesCompleted =
    lens _drrsImportTablesCompleted
         (\s a -> s { _drrsImportTablesCompleted = a })
{-# INLINE drrsImportTablesCompleted #-}

-- | The names of tables that are being currently imported. Valid Values: List
-- of table names.
drrsImportTablesInProgress :: Lens' DescribeResizeResponse [Text]
drrsImportTablesInProgress =
    lens _drrsImportTablesInProgress
         (\s a -> s { _drrsImportTablesInProgress = a })
{-# INLINE drrsImportTablesInProgress #-}

-- | The names of tables that have not been yet imported. Valid Values: List of
-- table names.
drrsImportTablesNotStarted :: Lens' DescribeResizeResponse [Text]
drrsImportTablesNotStarted =
    lens _drrsImportTablesNotStarted
         (\s a -> s { _drrsImportTablesNotStarted = a })
{-# INLINE drrsImportTablesNotStarted #-}

-- | The average rate of the resize operation over the last few minutes,
-- measured in megabytes per second. After the resize operation completes,
-- this value shows the average rate of the entire resize operation.
drrsAvgResizeRateInMegaBytesPerSecond :: Lens' DescribeResizeResponse (Maybe Double)
drrsAvgResizeRateInMegaBytesPerSecond =
    lens _drrsAvgResizeRateInMegaBytesPerSecond
         (\s a -> s { _drrsAvgResizeRateInMegaBytesPerSecond = a })
{-# INLINE drrsAvgResizeRateInMegaBytesPerSecond #-}

-- | The estimated total amount of data, in megabytes, on the cluster before the
-- resize operation began.
drrsTotalResizeDataInMegaBytes :: Lens' DescribeResizeResponse (Maybe Integer)
drrsTotalResizeDataInMegaBytes =
    lens _drrsTotalResizeDataInMegaBytes
         (\s a -> s { _drrsTotalResizeDataInMegaBytes = a })
{-# INLINE drrsTotalResizeDataInMegaBytes #-}

-- | While the resize operation is in progress, this value shows the current
-- amount of data, in megabytes, that has been processed so far. When the
-- resize operation is complete, this value shows the total amount of data, in
-- megabytes, on the cluster, which may be more or less than
-- TotalResizeDataInMegaBytes (the estimated total amount of data before
-- resize).
drrsProgressInMegaBytes :: Lens' DescribeResizeResponse (Maybe Integer)
drrsProgressInMegaBytes =
    lens _drrsProgressInMegaBytes
         (\s a -> s { _drrsProgressInMegaBytes = a })
{-# INLINE drrsProgressInMegaBytes #-}

-- | The amount of seconds that have elapsed since the resize operation began.
-- After the resize operation completes, this value shows the total actual
-- time, in seconds, for the resize operation.
drrsElapsedTimeInSeconds :: Lens' DescribeResizeResponse (Maybe Integer)
drrsElapsedTimeInSeconds =
    lens _drrsElapsedTimeInSeconds
         (\s a -> s { _drrsElapsedTimeInSeconds = a })
{-# INLINE drrsElapsedTimeInSeconds #-}

-- | The estimated time remaining, in seconds, until the resize operation is
-- complete. This value is calculated based on the average resize rate and the
-- estimated amount of data remaining to be processed. Once the resize
-- operation is complete, this value will be 0.
drrsEstimatedTimeToCompletionInSeconds :: Lens' DescribeResizeResponse (Maybe Integer)
drrsEstimatedTimeToCompletionInSeconds =
    lens _drrsEstimatedTimeToCompletionInSeconds
         (\s a -> s { _drrsEstimatedTimeToCompletionInSeconds = a })
{-# INLINE drrsEstimatedTimeToCompletionInSeconds #-}

instance FromXML DescribeResizeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeResize where
    type Sv DescribeResize = Redshift
    type Rs DescribeResize = DescribeResizeResponse

    request = post "DescribeResize"
    response _ = xmlResponse
