{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ResizeProgressMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ResizeProgressMessage where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

-- | Describes the result of a cluster resize operation.
--
--
--
-- /See:/ 'resizeProgressMessage' smart constructor.
data ResizeProgressMessage = ResizeProgressMessage'
  { _rpmImportTablesNotStarted ::
      !(Maybe [Text]),
    _rpmStatus :: !(Maybe Text),
    _rpmEstimatedTimeToCompletionInSeconds ::
      !(Maybe Integer),
    _rpmAvgResizeRateInMegaBytesPerSecond ::
      !(Maybe Double),
    _rpmTargetNumberOfNodes :: !(Maybe Int),
    _rpmTargetEncryptionType :: !(Maybe Text),
    _rpmTargetNodeType :: !(Maybe Text),
    _rpmImportTablesInProgress :: !(Maybe [Text]),
    _rpmResizeType :: !(Maybe Text),
    _rpmImportTablesCompleted :: !(Maybe [Text]),
    _rpmProgressInMegaBytes :: !(Maybe Integer),
    _rpmDataTransferProgressPercent ::
      !(Maybe Double),
    _rpmTotalResizeDataInMegaBytes ::
      !(Maybe Integer),
    _rpmTargetClusterType :: !(Maybe Text),
    _rpmMessage :: !(Maybe Text),
    _rpmElapsedTimeInSeconds :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResizeProgressMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpmImportTablesNotStarted' - The names of tables that have not been yet imported. Valid Values: List of table names
--
-- * 'rpmStatus' - The status of the resize operation. Valid Values: @NONE@ | @IN_PROGRESS@ | @FAILED@ | @SUCCEEDED@ | @CANCELLING@
--
-- * 'rpmEstimatedTimeToCompletionInSeconds' - The estimated time remaining, in seconds, until the resize operation is complete. This value is calculated based on the average resize rate and the estimated amount of data remaining to be processed. Once the resize operation is complete, this value will be 0.
--
-- * 'rpmAvgResizeRateInMegaBytesPerSecond' - The average rate of the resize operation over the last few minutes, measured in megabytes per second. After the resize operation completes, this value shows the average rate of the entire resize operation.
--
-- * 'rpmTargetNumberOfNodes' - The number of nodes that the cluster will have after the resize operation is complete.
--
-- * 'rpmTargetEncryptionType' - The type of encryption for the cluster after the resize is complete. Possible values are @KMS@ and @None@ .
--
-- * 'rpmTargetNodeType' - The node type that the cluster will have after the resize operation is complete.
--
-- * 'rpmImportTablesInProgress' - The names of tables that are being currently imported. Valid Values: List of table names.
--
-- * 'rpmResizeType' - An enum with possible values of @ClassicResize@ and @ElasticResize@ . These values describe the type of resize operation being performed.
--
-- * 'rpmImportTablesCompleted' - The names of tables that have been completely imported . Valid Values: List of table names.
--
-- * 'rpmProgressInMegaBytes' - While the resize operation is in progress, this value shows the current amount of data, in megabytes, that has been processed so far. When the resize operation is complete, this value shows the total amount of data, in megabytes, on the cluster, which may be more or less than TotalResizeDataInMegaBytes (the estimated total amount of data before resize).
--
-- * 'rpmDataTransferProgressPercent' - The percent of data transferred from source cluster to target cluster.
--
-- * 'rpmTotalResizeDataInMegaBytes' - The estimated total amount of data, in megabytes, on the cluster before the resize operation began.
--
-- * 'rpmTargetClusterType' - The cluster type after the resize operation is complete. Valid Values: @multi-node@ | @single-node@
--
-- * 'rpmMessage' - An optional string to provide additional details about the resize action.
--
-- * 'rpmElapsedTimeInSeconds' - The amount of seconds that have elapsed since the resize operation began. After the resize operation completes, this value shows the total actual time, in seconds, for the resize operation.
resizeProgressMessage ::
  ResizeProgressMessage
resizeProgressMessage =
  ResizeProgressMessage'
    { _rpmImportTablesNotStarted = Nothing,
      _rpmStatus = Nothing,
      _rpmEstimatedTimeToCompletionInSeconds = Nothing,
      _rpmAvgResizeRateInMegaBytesPerSecond = Nothing,
      _rpmTargetNumberOfNodes = Nothing,
      _rpmTargetEncryptionType = Nothing,
      _rpmTargetNodeType = Nothing,
      _rpmImportTablesInProgress = Nothing,
      _rpmResizeType = Nothing,
      _rpmImportTablesCompleted = Nothing,
      _rpmProgressInMegaBytes = Nothing,
      _rpmDataTransferProgressPercent = Nothing,
      _rpmTotalResizeDataInMegaBytes = Nothing,
      _rpmTargetClusterType = Nothing,
      _rpmMessage = Nothing,
      _rpmElapsedTimeInSeconds = Nothing
    }

-- | The names of tables that have not been yet imported. Valid Values: List of table names
rpmImportTablesNotStarted :: Lens' ResizeProgressMessage [Text]
rpmImportTablesNotStarted = lens _rpmImportTablesNotStarted (\s a -> s {_rpmImportTablesNotStarted = a}) . _Default . _Coerce

-- | The status of the resize operation. Valid Values: @NONE@ | @IN_PROGRESS@ | @FAILED@ | @SUCCEEDED@ | @CANCELLING@
rpmStatus :: Lens' ResizeProgressMessage (Maybe Text)
rpmStatus = lens _rpmStatus (\s a -> s {_rpmStatus = a})

-- | The estimated time remaining, in seconds, until the resize operation is complete. This value is calculated based on the average resize rate and the estimated amount of data remaining to be processed. Once the resize operation is complete, this value will be 0.
rpmEstimatedTimeToCompletionInSeconds :: Lens' ResizeProgressMessage (Maybe Integer)
rpmEstimatedTimeToCompletionInSeconds = lens _rpmEstimatedTimeToCompletionInSeconds (\s a -> s {_rpmEstimatedTimeToCompletionInSeconds = a})

-- | The average rate of the resize operation over the last few minutes, measured in megabytes per second. After the resize operation completes, this value shows the average rate of the entire resize operation.
rpmAvgResizeRateInMegaBytesPerSecond :: Lens' ResizeProgressMessage (Maybe Double)
rpmAvgResizeRateInMegaBytesPerSecond = lens _rpmAvgResizeRateInMegaBytesPerSecond (\s a -> s {_rpmAvgResizeRateInMegaBytesPerSecond = a})

-- | The number of nodes that the cluster will have after the resize operation is complete.
rpmTargetNumberOfNodes :: Lens' ResizeProgressMessage (Maybe Int)
rpmTargetNumberOfNodes = lens _rpmTargetNumberOfNodes (\s a -> s {_rpmTargetNumberOfNodes = a})

-- | The type of encryption for the cluster after the resize is complete. Possible values are @KMS@ and @None@ .
rpmTargetEncryptionType :: Lens' ResizeProgressMessage (Maybe Text)
rpmTargetEncryptionType = lens _rpmTargetEncryptionType (\s a -> s {_rpmTargetEncryptionType = a})

-- | The node type that the cluster will have after the resize operation is complete.
rpmTargetNodeType :: Lens' ResizeProgressMessage (Maybe Text)
rpmTargetNodeType = lens _rpmTargetNodeType (\s a -> s {_rpmTargetNodeType = a})

-- | The names of tables that are being currently imported. Valid Values: List of table names.
rpmImportTablesInProgress :: Lens' ResizeProgressMessage [Text]
rpmImportTablesInProgress = lens _rpmImportTablesInProgress (\s a -> s {_rpmImportTablesInProgress = a}) . _Default . _Coerce

-- | An enum with possible values of @ClassicResize@ and @ElasticResize@ . These values describe the type of resize operation being performed.
rpmResizeType :: Lens' ResizeProgressMessage (Maybe Text)
rpmResizeType = lens _rpmResizeType (\s a -> s {_rpmResizeType = a})

-- | The names of tables that have been completely imported . Valid Values: List of table names.
rpmImportTablesCompleted :: Lens' ResizeProgressMessage [Text]
rpmImportTablesCompleted = lens _rpmImportTablesCompleted (\s a -> s {_rpmImportTablesCompleted = a}) . _Default . _Coerce

-- | While the resize operation is in progress, this value shows the current amount of data, in megabytes, that has been processed so far. When the resize operation is complete, this value shows the total amount of data, in megabytes, on the cluster, which may be more or less than TotalResizeDataInMegaBytes (the estimated total amount of data before resize).
rpmProgressInMegaBytes :: Lens' ResizeProgressMessage (Maybe Integer)
rpmProgressInMegaBytes = lens _rpmProgressInMegaBytes (\s a -> s {_rpmProgressInMegaBytes = a})

-- | The percent of data transferred from source cluster to target cluster.
rpmDataTransferProgressPercent :: Lens' ResizeProgressMessage (Maybe Double)
rpmDataTransferProgressPercent = lens _rpmDataTransferProgressPercent (\s a -> s {_rpmDataTransferProgressPercent = a})

-- | The estimated total amount of data, in megabytes, on the cluster before the resize operation began.
rpmTotalResizeDataInMegaBytes :: Lens' ResizeProgressMessage (Maybe Integer)
rpmTotalResizeDataInMegaBytes = lens _rpmTotalResizeDataInMegaBytes (\s a -> s {_rpmTotalResizeDataInMegaBytes = a})

-- | The cluster type after the resize operation is complete. Valid Values: @multi-node@ | @single-node@
rpmTargetClusterType :: Lens' ResizeProgressMessage (Maybe Text)
rpmTargetClusterType = lens _rpmTargetClusterType (\s a -> s {_rpmTargetClusterType = a})

-- | An optional string to provide additional details about the resize action.
rpmMessage :: Lens' ResizeProgressMessage (Maybe Text)
rpmMessage = lens _rpmMessage (\s a -> s {_rpmMessage = a})

-- | The amount of seconds that have elapsed since the resize operation began. After the resize operation completes, this value shows the total actual time, in seconds, for the resize operation.
rpmElapsedTimeInSeconds :: Lens' ResizeProgressMessage (Maybe Integer)
rpmElapsedTimeInSeconds = lens _rpmElapsedTimeInSeconds (\s a -> s {_rpmElapsedTimeInSeconds = a})

instance FromXML ResizeProgressMessage where
  parseXML x =
    ResizeProgressMessage'
      <$> ( x .@? "ImportTablesNotStarted" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> (x .@? "Status")
      <*> (x .@? "EstimatedTimeToCompletionInSeconds")
      <*> (x .@? "AvgResizeRateInMegaBytesPerSecond")
      <*> (x .@? "TargetNumberOfNodes")
      <*> (x .@? "TargetEncryptionType")
      <*> (x .@? "TargetNodeType")
      <*> ( x .@? "ImportTablesInProgress" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> (x .@? "ResizeType")
      <*> ( x .@? "ImportTablesCompleted" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> (x .@? "ProgressInMegaBytes")
      <*> (x .@? "DataTransferProgressPercent")
      <*> (x .@? "TotalResizeDataInMegaBytes")
      <*> (x .@? "TargetClusterType")
      <*> (x .@? "Message")
      <*> (x .@? "ElapsedTimeInSeconds")

instance Hashable ResizeProgressMessage

instance NFData ResizeProgressMessage
