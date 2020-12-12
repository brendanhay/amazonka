{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ResizeProgressMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ResizeProgressMessage
  ( ResizeProgressMessage (..),

    -- * Smart constructor
    mkResizeProgressMessage,

    -- * Lenses
    rpmImportTablesNotStarted,
    rpmStatus,
    rpmEstimatedTimeToCompletionInSeconds,
    rpmAvgResizeRateInMegaBytesPerSecond,
    rpmTargetNumberOfNodes,
    rpmTargetEncryptionType,
    rpmTargetNodeType,
    rpmImportTablesInProgress,
    rpmResizeType,
    rpmImportTablesCompleted,
    rpmProgressInMegaBytes,
    rpmDataTransferProgressPercent,
    rpmTotalResizeDataInMegaBytes,
    rpmTargetClusterType,
    rpmMessage,
    rpmElapsedTimeInSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

-- | Describes the result of a cluster resize operation.
--
-- /See:/ 'mkResizeProgressMessage' smart constructor.
data ResizeProgressMessage = ResizeProgressMessage'
  { importTablesNotStarted ::
      Lude.Maybe [Lude.Text],
    status :: Lude.Maybe Lude.Text,
    estimatedTimeToCompletionInSeconds ::
      Lude.Maybe Lude.Integer,
    avgResizeRateInMegaBytesPerSecond ::
      Lude.Maybe Lude.Double,
    targetNumberOfNodes :: Lude.Maybe Lude.Int,
    targetEncryptionType :: Lude.Maybe Lude.Text,
    targetNodeType :: Lude.Maybe Lude.Text,
    importTablesInProgress ::
      Lude.Maybe [Lude.Text],
    resizeType :: Lude.Maybe Lude.Text,
    importTablesCompleted :: Lude.Maybe [Lude.Text],
    progressInMegaBytes :: Lude.Maybe Lude.Integer,
    dataTransferProgressPercent ::
      Lude.Maybe Lude.Double,
    totalResizeDataInMegaBytes ::
      Lude.Maybe Lude.Integer,
    targetClusterType :: Lude.Maybe Lude.Text,
    message :: Lude.Maybe Lude.Text,
    elapsedTimeInSeconds :: Lude.Maybe Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResizeProgressMessage' with the minimum fields required to make a request.
--
-- * 'avgResizeRateInMegaBytesPerSecond' - The average rate of the resize operation over the last few minutes, measured in megabytes per second. After the resize operation completes, this value shows the average rate of the entire resize operation.
-- * 'dataTransferProgressPercent' - The percent of data transferred from source cluster to target cluster.
-- * 'elapsedTimeInSeconds' - The amount of seconds that have elapsed since the resize operation began. After the resize operation completes, this value shows the total actual time, in seconds, for the resize operation.
-- * 'estimatedTimeToCompletionInSeconds' - The estimated time remaining, in seconds, until the resize operation is complete. This value is calculated based on the average resize rate and the estimated amount of data remaining to be processed. Once the resize operation is complete, this value will be 0.
-- * 'importTablesCompleted' - The names of tables that have been completely imported .
--
-- Valid Values: List of table names.
-- * 'importTablesInProgress' - The names of tables that are being currently imported.
--
-- Valid Values: List of table names.
-- * 'importTablesNotStarted' - The names of tables that have not been yet imported.
--
-- Valid Values: List of table names
-- * 'message' - An optional string to provide additional details about the resize action.
-- * 'progressInMegaBytes' - While the resize operation is in progress, this value shows the current amount of data, in megabytes, that has been processed so far. When the resize operation is complete, this value shows the total amount of data, in megabytes, on the cluster, which may be more or less than TotalResizeDataInMegaBytes (the estimated total amount of data before resize).
-- * 'resizeType' - An enum with possible values of @ClassicResize@ and @ElasticResize@ . These values describe the type of resize operation being performed.
-- * 'status' - The status of the resize operation.
--
-- Valid Values: @NONE@ | @IN_PROGRESS@ | @FAILED@ | @SUCCEEDED@ | @CANCELLING@
-- * 'targetClusterType' - The cluster type after the resize operation is complete.
--
-- Valid Values: @multi-node@ | @single-node@
-- * 'targetEncryptionType' - The type of encryption for the cluster after the resize is complete.
--
-- Possible values are @KMS@ and @None@ .
-- * 'targetNodeType' - The node type that the cluster will have after the resize operation is complete.
-- * 'targetNumberOfNodes' - The number of nodes that the cluster will have after the resize operation is complete.
-- * 'totalResizeDataInMegaBytes' - The estimated total amount of data, in megabytes, on the cluster before the resize operation began.
mkResizeProgressMessage ::
  ResizeProgressMessage
mkResizeProgressMessage =
  ResizeProgressMessage'
    { importTablesNotStarted = Lude.Nothing,
      status = Lude.Nothing,
      estimatedTimeToCompletionInSeconds = Lude.Nothing,
      avgResizeRateInMegaBytesPerSecond = Lude.Nothing,
      targetNumberOfNodes = Lude.Nothing,
      targetEncryptionType = Lude.Nothing,
      targetNodeType = Lude.Nothing,
      importTablesInProgress = Lude.Nothing,
      resizeType = Lude.Nothing,
      importTablesCompleted = Lude.Nothing,
      progressInMegaBytes = Lude.Nothing,
      dataTransferProgressPercent = Lude.Nothing,
      totalResizeDataInMegaBytes = Lude.Nothing,
      targetClusterType = Lude.Nothing,
      message = Lude.Nothing,
      elapsedTimeInSeconds = Lude.Nothing
    }

-- | The names of tables that have not been yet imported.
--
-- Valid Values: List of table names
--
-- /Note:/ Consider using 'importTablesNotStarted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmImportTablesNotStarted :: Lens.Lens' ResizeProgressMessage (Lude.Maybe [Lude.Text])
rpmImportTablesNotStarted = Lens.lens (importTablesNotStarted :: ResizeProgressMessage -> Lude.Maybe [Lude.Text]) (\s a -> s {importTablesNotStarted = a} :: ResizeProgressMessage)
{-# DEPRECATED rpmImportTablesNotStarted "Use generic-lens or generic-optics with 'importTablesNotStarted' instead." #-}

-- | The status of the resize operation.
--
-- Valid Values: @NONE@ | @IN_PROGRESS@ | @FAILED@ | @SUCCEEDED@ | @CANCELLING@
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmStatus :: Lens.Lens' ResizeProgressMessage (Lude.Maybe Lude.Text)
rpmStatus = Lens.lens (status :: ResizeProgressMessage -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: ResizeProgressMessage)
{-# DEPRECATED rpmStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The estimated time remaining, in seconds, until the resize operation is complete. This value is calculated based on the average resize rate and the estimated amount of data remaining to be processed. Once the resize operation is complete, this value will be 0.
--
-- /Note:/ Consider using 'estimatedTimeToCompletionInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmEstimatedTimeToCompletionInSeconds :: Lens.Lens' ResizeProgressMessage (Lude.Maybe Lude.Integer)
rpmEstimatedTimeToCompletionInSeconds = Lens.lens (estimatedTimeToCompletionInSeconds :: ResizeProgressMessage -> Lude.Maybe Lude.Integer) (\s a -> s {estimatedTimeToCompletionInSeconds = a} :: ResizeProgressMessage)
{-# DEPRECATED rpmEstimatedTimeToCompletionInSeconds "Use generic-lens or generic-optics with 'estimatedTimeToCompletionInSeconds' instead." #-}

-- | The average rate of the resize operation over the last few minutes, measured in megabytes per second. After the resize operation completes, this value shows the average rate of the entire resize operation.
--
-- /Note:/ Consider using 'avgResizeRateInMegaBytesPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmAvgResizeRateInMegaBytesPerSecond :: Lens.Lens' ResizeProgressMessage (Lude.Maybe Lude.Double)
rpmAvgResizeRateInMegaBytesPerSecond = Lens.lens (avgResizeRateInMegaBytesPerSecond :: ResizeProgressMessage -> Lude.Maybe Lude.Double) (\s a -> s {avgResizeRateInMegaBytesPerSecond = a} :: ResizeProgressMessage)
{-# DEPRECATED rpmAvgResizeRateInMegaBytesPerSecond "Use generic-lens or generic-optics with 'avgResizeRateInMegaBytesPerSecond' instead." #-}

-- | The number of nodes that the cluster will have after the resize operation is complete.
--
-- /Note:/ Consider using 'targetNumberOfNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmTargetNumberOfNodes :: Lens.Lens' ResizeProgressMessage (Lude.Maybe Lude.Int)
rpmTargetNumberOfNodes = Lens.lens (targetNumberOfNodes :: ResizeProgressMessage -> Lude.Maybe Lude.Int) (\s a -> s {targetNumberOfNodes = a} :: ResizeProgressMessage)
{-# DEPRECATED rpmTargetNumberOfNodes "Use generic-lens or generic-optics with 'targetNumberOfNodes' instead." #-}

-- | The type of encryption for the cluster after the resize is complete.
--
-- Possible values are @KMS@ and @None@ .
--
-- /Note:/ Consider using 'targetEncryptionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmTargetEncryptionType :: Lens.Lens' ResizeProgressMessage (Lude.Maybe Lude.Text)
rpmTargetEncryptionType = Lens.lens (targetEncryptionType :: ResizeProgressMessage -> Lude.Maybe Lude.Text) (\s a -> s {targetEncryptionType = a} :: ResizeProgressMessage)
{-# DEPRECATED rpmTargetEncryptionType "Use generic-lens or generic-optics with 'targetEncryptionType' instead." #-}

-- | The node type that the cluster will have after the resize operation is complete.
--
-- /Note:/ Consider using 'targetNodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmTargetNodeType :: Lens.Lens' ResizeProgressMessage (Lude.Maybe Lude.Text)
rpmTargetNodeType = Lens.lens (targetNodeType :: ResizeProgressMessage -> Lude.Maybe Lude.Text) (\s a -> s {targetNodeType = a} :: ResizeProgressMessage)
{-# DEPRECATED rpmTargetNodeType "Use generic-lens or generic-optics with 'targetNodeType' instead." #-}

-- | The names of tables that are being currently imported.
--
-- Valid Values: List of table names.
--
-- /Note:/ Consider using 'importTablesInProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmImportTablesInProgress :: Lens.Lens' ResizeProgressMessage (Lude.Maybe [Lude.Text])
rpmImportTablesInProgress = Lens.lens (importTablesInProgress :: ResizeProgressMessage -> Lude.Maybe [Lude.Text]) (\s a -> s {importTablesInProgress = a} :: ResizeProgressMessage)
{-# DEPRECATED rpmImportTablesInProgress "Use generic-lens or generic-optics with 'importTablesInProgress' instead." #-}

-- | An enum with possible values of @ClassicResize@ and @ElasticResize@ . These values describe the type of resize operation being performed.
--
-- /Note:/ Consider using 'resizeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmResizeType :: Lens.Lens' ResizeProgressMessage (Lude.Maybe Lude.Text)
rpmResizeType = Lens.lens (resizeType :: ResizeProgressMessage -> Lude.Maybe Lude.Text) (\s a -> s {resizeType = a} :: ResizeProgressMessage)
{-# DEPRECATED rpmResizeType "Use generic-lens or generic-optics with 'resizeType' instead." #-}

-- | The names of tables that have been completely imported .
--
-- Valid Values: List of table names.
--
-- /Note:/ Consider using 'importTablesCompleted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmImportTablesCompleted :: Lens.Lens' ResizeProgressMessage (Lude.Maybe [Lude.Text])
rpmImportTablesCompleted = Lens.lens (importTablesCompleted :: ResizeProgressMessage -> Lude.Maybe [Lude.Text]) (\s a -> s {importTablesCompleted = a} :: ResizeProgressMessage)
{-# DEPRECATED rpmImportTablesCompleted "Use generic-lens or generic-optics with 'importTablesCompleted' instead." #-}

-- | While the resize operation is in progress, this value shows the current amount of data, in megabytes, that has been processed so far. When the resize operation is complete, this value shows the total amount of data, in megabytes, on the cluster, which may be more or less than TotalResizeDataInMegaBytes (the estimated total amount of data before resize).
--
-- /Note:/ Consider using 'progressInMegaBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmProgressInMegaBytes :: Lens.Lens' ResizeProgressMessage (Lude.Maybe Lude.Integer)
rpmProgressInMegaBytes = Lens.lens (progressInMegaBytes :: ResizeProgressMessage -> Lude.Maybe Lude.Integer) (\s a -> s {progressInMegaBytes = a} :: ResizeProgressMessage)
{-# DEPRECATED rpmProgressInMegaBytes "Use generic-lens or generic-optics with 'progressInMegaBytes' instead." #-}

-- | The percent of data transferred from source cluster to target cluster.
--
-- /Note:/ Consider using 'dataTransferProgressPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmDataTransferProgressPercent :: Lens.Lens' ResizeProgressMessage (Lude.Maybe Lude.Double)
rpmDataTransferProgressPercent = Lens.lens (dataTransferProgressPercent :: ResizeProgressMessage -> Lude.Maybe Lude.Double) (\s a -> s {dataTransferProgressPercent = a} :: ResizeProgressMessage)
{-# DEPRECATED rpmDataTransferProgressPercent "Use generic-lens or generic-optics with 'dataTransferProgressPercent' instead." #-}

-- | The estimated total amount of data, in megabytes, on the cluster before the resize operation began.
--
-- /Note:/ Consider using 'totalResizeDataInMegaBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmTotalResizeDataInMegaBytes :: Lens.Lens' ResizeProgressMessage (Lude.Maybe Lude.Integer)
rpmTotalResizeDataInMegaBytes = Lens.lens (totalResizeDataInMegaBytes :: ResizeProgressMessage -> Lude.Maybe Lude.Integer) (\s a -> s {totalResizeDataInMegaBytes = a} :: ResizeProgressMessage)
{-# DEPRECATED rpmTotalResizeDataInMegaBytes "Use generic-lens or generic-optics with 'totalResizeDataInMegaBytes' instead." #-}

-- | The cluster type after the resize operation is complete.
--
-- Valid Values: @multi-node@ | @single-node@
--
-- /Note:/ Consider using 'targetClusterType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmTargetClusterType :: Lens.Lens' ResizeProgressMessage (Lude.Maybe Lude.Text)
rpmTargetClusterType = Lens.lens (targetClusterType :: ResizeProgressMessage -> Lude.Maybe Lude.Text) (\s a -> s {targetClusterType = a} :: ResizeProgressMessage)
{-# DEPRECATED rpmTargetClusterType "Use generic-lens or generic-optics with 'targetClusterType' instead." #-}

-- | An optional string to provide additional details about the resize action.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmMessage :: Lens.Lens' ResizeProgressMessage (Lude.Maybe Lude.Text)
rpmMessage = Lens.lens (message :: ResizeProgressMessage -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: ResizeProgressMessage)
{-# DEPRECATED rpmMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The amount of seconds that have elapsed since the resize operation began. After the resize operation completes, this value shows the total actual time, in seconds, for the resize operation.
--
-- /Note:/ Consider using 'elapsedTimeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmElapsedTimeInSeconds :: Lens.Lens' ResizeProgressMessage (Lude.Maybe Lude.Integer)
rpmElapsedTimeInSeconds = Lens.lens (elapsedTimeInSeconds :: ResizeProgressMessage -> Lude.Maybe Lude.Integer) (\s a -> s {elapsedTimeInSeconds = a} :: ResizeProgressMessage)
{-# DEPRECATED rpmElapsedTimeInSeconds "Use generic-lens or generic-optics with 'elapsedTimeInSeconds' instead." #-}

instance Lude.FromXML ResizeProgressMessage where
  parseXML x =
    ResizeProgressMessage'
      Lude.<$> ( x Lude..@? "ImportTablesNotStarted" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "EstimatedTimeToCompletionInSeconds")
      Lude.<*> (x Lude..@? "AvgResizeRateInMegaBytesPerSecond")
      Lude.<*> (x Lude..@? "TargetNumberOfNodes")
      Lude.<*> (x Lude..@? "TargetEncryptionType")
      Lude.<*> (x Lude..@? "TargetNodeType")
      Lude.<*> ( x Lude..@? "ImportTablesInProgress" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "ResizeType")
      Lude.<*> ( x Lude..@? "ImportTablesCompleted" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "ProgressInMegaBytes")
      Lude.<*> (x Lude..@? "DataTransferProgressPercent")
      Lude.<*> (x Lude..@? "TotalResizeDataInMegaBytes")
      Lude.<*> (x Lude..@? "TargetClusterType")
      Lude.<*> (x Lude..@? "Message")
      Lude.<*> (x Lude..@? "ElapsedTimeInSeconds")
