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
-- Module      : Network.AWS.Redshift.DescribeResize
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the last resize operation for the specified cluster. If no resize operation has ever been initiated for the specified cluster, a @HTTP 404@ error is returned. If a resize operation was initiated and completed, the status of the resize remains as @SUCCEEDED@ until the next resize.
--
--
-- A resize operation can be requested using 'ModifyCluster' and specifying a different number or type of nodes for the cluster.
--
module Network.AWS.Redshift.DescribeResize
    (
    -- * Creating a Request
      describeResize
    , DescribeResize
    -- * Request Lenses
    , drClusterIdentifier

    -- * Destructuring the Response
    , describeResizeResponse
    , DescribeResizeResponse
    -- * Response Lenses
    , drrsImportTablesNotStarted
    , drrsStatus
    , drrsEstimatedTimeToCompletionInSeconds
    , drrsAvgResizeRateInMegaBytesPerSecond
    , drrsTargetNumberOfNodes
    , drrsTargetNodeType
    , drrsImportTablesInProgress
    , drrsImportTablesCompleted
    , drrsProgressInMegaBytes
    , drrsTotalResizeDataInMegaBytes
    , drrsTargetClusterType
    , drrsElapsedTimeInSeconds
    , drrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeResize' smart constructor.
newtype DescribeResize = DescribeResize'
  { _drClusterIdentifier :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeResize' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drClusterIdentifier' - The unique identifier of a cluster whose resize progress you are requesting. This parameter is case-sensitive. By default, resize operations for all clusters defined for an AWS account are returned.
describeResize
    :: Text -- ^ 'drClusterIdentifier'
    -> DescribeResize
describeResize pClusterIdentifier_ =
  DescribeResize' {_drClusterIdentifier = pClusterIdentifier_}


-- | The unique identifier of a cluster whose resize progress you are requesting. This parameter is case-sensitive. By default, resize operations for all clusters defined for an AWS account are returned.
drClusterIdentifier :: Lens' DescribeResize Text
drClusterIdentifier = lens _drClusterIdentifier (\ s a -> s{_drClusterIdentifier = a})

instance AWSRequest DescribeResize where
        type Rs DescribeResize = DescribeResizeResponse
        request = postQuery redshift
        response
          = receiveXMLWrapper "DescribeResizeResult"
              (\ s h x ->
                 DescribeResizeResponse' <$>
                   (x .@? "ImportTablesNotStarted" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "Status")
                     <*> (x .@? "EstimatedTimeToCompletionInSeconds")
                     <*> (x .@? "AvgResizeRateInMegaBytesPerSecond")
                     <*> (x .@? "TargetNumberOfNodes")
                     <*> (x .@? "TargetNodeType")
                     <*>
                     (x .@? "ImportTablesInProgress" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*>
                     (x .@? "ImportTablesCompleted" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (x .@? "ProgressInMegaBytes")
                     <*> (x .@? "TotalResizeDataInMegaBytes")
                     <*> (x .@? "TargetClusterType")
                     <*> (x .@? "ElapsedTimeInSeconds")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeResize where

instance NFData DescribeResize where

instance ToHeaders DescribeResize where
        toHeaders = const mempty

instance ToPath DescribeResize where
        toPath = const "/"

instance ToQuery DescribeResize where
        toQuery DescribeResize'{..}
          = mconcat
              ["Action" =: ("DescribeResize" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "ClusterIdentifier" =: _drClusterIdentifier]

-- | Describes the result of a cluster resize operation.
--
--
--
-- /See:/ 'describeResizeResponse' smart constructor.
data DescribeResizeResponse = DescribeResizeResponse'
  { _drrsImportTablesNotStarted             :: !(Maybe [Text])
  , _drrsStatus                             :: !(Maybe Text)
  , _drrsEstimatedTimeToCompletionInSeconds :: !(Maybe Integer)
  , _drrsAvgResizeRateInMegaBytesPerSecond  :: !(Maybe Double)
  , _drrsTargetNumberOfNodes                :: !(Maybe Int)
  , _drrsTargetNodeType                     :: !(Maybe Text)
  , _drrsImportTablesInProgress             :: !(Maybe [Text])
  , _drrsImportTablesCompleted              :: !(Maybe [Text])
  , _drrsProgressInMegaBytes                :: !(Maybe Integer)
  , _drrsTotalResizeDataInMegaBytes         :: !(Maybe Integer)
  , _drrsTargetClusterType                  :: !(Maybe Text)
  , _drrsElapsedTimeInSeconds               :: !(Maybe Integer)
  , _drrsResponseStatus                     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeResizeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drrsImportTablesNotStarted' - The names of tables that have not been yet imported. Valid Values: List of table names
--
-- * 'drrsStatus' - The status of the resize operation. Valid Values: @NONE@ | @IN_PROGRESS@ | @FAILED@ | @SUCCEEDED@
--
-- * 'drrsEstimatedTimeToCompletionInSeconds' - The estimated time remaining, in seconds, until the resize operation is complete. This value is calculated based on the average resize rate and the estimated amount of data remaining to be processed. Once the resize operation is complete, this value will be 0.
--
-- * 'drrsAvgResizeRateInMegaBytesPerSecond' - The average rate of the resize operation over the last few minutes, measured in megabytes per second. After the resize operation completes, this value shows the average rate of the entire resize operation.
--
-- * 'drrsTargetNumberOfNodes' - The number of nodes that the cluster will have after the resize operation is complete.
--
-- * 'drrsTargetNodeType' - The node type that the cluster will have after the resize operation is complete.
--
-- * 'drrsImportTablesInProgress' - The names of tables that are being currently imported. Valid Values: List of table names.
--
-- * 'drrsImportTablesCompleted' - The names of tables that have been completely imported . Valid Values: List of table names.
--
-- * 'drrsProgressInMegaBytes' - While the resize operation is in progress, this value shows the current amount of data, in megabytes, that has been processed so far. When the resize operation is complete, this value shows the total amount of data, in megabytes, on the cluster, which may be more or less than TotalResizeDataInMegaBytes (the estimated total amount of data before resize).
--
-- * 'drrsTotalResizeDataInMegaBytes' - The estimated total amount of data, in megabytes, on the cluster before the resize operation began.
--
-- * 'drrsTargetClusterType' - The cluster type after the resize operation is complete. Valid Values: @multi-node@ | @single-node@
--
-- * 'drrsElapsedTimeInSeconds' - The amount of seconds that have elapsed since the resize operation began. After the resize operation completes, this value shows the total actual time, in seconds, for the resize operation.
--
-- * 'drrsResponseStatus' - -- | The response status code.
describeResizeResponse
    :: Int -- ^ 'drrsResponseStatus'
    -> DescribeResizeResponse
describeResizeResponse pResponseStatus_ =
  DescribeResizeResponse'
    { _drrsImportTablesNotStarted = Nothing
    , _drrsStatus = Nothing
    , _drrsEstimatedTimeToCompletionInSeconds = Nothing
    , _drrsAvgResizeRateInMegaBytesPerSecond = Nothing
    , _drrsTargetNumberOfNodes = Nothing
    , _drrsTargetNodeType = Nothing
    , _drrsImportTablesInProgress = Nothing
    , _drrsImportTablesCompleted = Nothing
    , _drrsProgressInMegaBytes = Nothing
    , _drrsTotalResizeDataInMegaBytes = Nothing
    , _drrsTargetClusterType = Nothing
    , _drrsElapsedTimeInSeconds = Nothing
    , _drrsResponseStatus = pResponseStatus_
    }


-- | The names of tables that have not been yet imported. Valid Values: List of table names
drrsImportTablesNotStarted :: Lens' DescribeResizeResponse [Text]
drrsImportTablesNotStarted = lens _drrsImportTablesNotStarted (\ s a -> s{_drrsImportTablesNotStarted = a}) . _Default . _Coerce

-- | The status of the resize operation. Valid Values: @NONE@ | @IN_PROGRESS@ | @FAILED@ | @SUCCEEDED@
drrsStatus :: Lens' DescribeResizeResponse (Maybe Text)
drrsStatus = lens _drrsStatus (\ s a -> s{_drrsStatus = a})

-- | The estimated time remaining, in seconds, until the resize operation is complete. This value is calculated based on the average resize rate and the estimated amount of data remaining to be processed. Once the resize operation is complete, this value will be 0.
drrsEstimatedTimeToCompletionInSeconds :: Lens' DescribeResizeResponse (Maybe Integer)
drrsEstimatedTimeToCompletionInSeconds = lens _drrsEstimatedTimeToCompletionInSeconds (\ s a -> s{_drrsEstimatedTimeToCompletionInSeconds = a})

-- | The average rate of the resize operation over the last few minutes, measured in megabytes per second. After the resize operation completes, this value shows the average rate of the entire resize operation.
drrsAvgResizeRateInMegaBytesPerSecond :: Lens' DescribeResizeResponse (Maybe Double)
drrsAvgResizeRateInMegaBytesPerSecond = lens _drrsAvgResizeRateInMegaBytesPerSecond (\ s a -> s{_drrsAvgResizeRateInMegaBytesPerSecond = a})

-- | The number of nodes that the cluster will have after the resize operation is complete.
drrsTargetNumberOfNodes :: Lens' DescribeResizeResponse (Maybe Int)
drrsTargetNumberOfNodes = lens _drrsTargetNumberOfNodes (\ s a -> s{_drrsTargetNumberOfNodes = a})

-- | The node type that the cluster will have after the resize operation is complete.
drrsTargetNodeType :: Lens' DescribeResizeResponse (Maybe Text)
drrsTargetNodeType = lens _drrsTargetNodeType (\ s a -> s{_drrsTargetNodeType = a})

-- | The names of tables that are being currently imported. Valid Values: List of table names.
drrsImportTablesInProgress :: Lens' DescribeResizeResponse [Text]
drrsImportTablesInProgress = lens _drrsImportTablesInProgress (\ s a -> s{_drrsImportTablesInProgress = a}) . _Default . _Coerce

-- | The names of tables that have been completely imported . Valid Values: List of table names.
drrsImportTablesCompleted :: Lens' DescribeResizeResponse [Text]
drrsImportTablesCompleted = lens _drrsImportTablesCompleted (\ s a -> s{_drrsImportTablesCompleted = a}) . _Default . _Coerce

-- | While the resize operation is in progress, this value shows the current amount of data, in megabytes, that has been processed so far. When the resize operation is complete, this value shows the total amount of data, in megabytes, on the cluster, which may be more or less than TotalResizeDataInMegaBytes (the estimated total amount of data before resize).
drrsProgressInMegaBytes :: Lens' DescribeResizeResponse (Maybe Integer)
drrsProgressInMegaBytes = lens _drrsProgressInMegaBytes (\ s a -> s{_drrsProgressInMegaBytes = a})

-- | The estimated total amount of data, in megabytes, on the cluster before the resize operation began.
drrsTotalResizeDataInMegaBytes :: Lens' DescribeResizeResponse (Maybe Integer)
drrsTotalResizeDataInMegaBytes = lens _drrsTotalResizeDataInMegaBytes (\ s a -> s{_drrsTotalResizeDataInMegaBytes = a})

-- | The cluster type after the resize operation is complete. Valid Values: @multi-node@ | @single-node@
drrsTargetClusterType :: Lens' DescribeResizeResponse (Maybe Text)
drrsTargetClusterType = lens _drrsTargetClusterType (\ s a -> s{_drrsTargetClusterType = a})

-- | The amount of seconds that have elapsed since the resize operation began. After the resize operation completes, this value shows the total actual time, in seconds, for the resize operation.
drrsElapsedTimeInSeconds :: Lens' DescribeResizeResponse (Maybe Integer)
drrsElapsedTimeInSeconds = lens _drrsElapsedTimeInSeconds (\ s a -> s{_drrsElapsedTimeInSeconds = a})

-- | -- | The response status code.
drrsResponseStatus :: Lens' DescribeResizeResponse Int
drrsResponseStatus = lens _drrsResponseStatus (\ s a -> s{_drrsResponseStatus = a})

instance NFData DescribeResizeResponse where
