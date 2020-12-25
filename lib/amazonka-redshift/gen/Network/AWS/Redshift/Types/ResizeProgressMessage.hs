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
    rpmAvgResizeRateInMegaBytesPerSecond,
    rpmDataTransferProgressPercent,
    rpmElapsedTimeInSeconds,
    rpmEstimatedTimeToCompletionInSeconds,
    rpmImportTablesCompleted,
    rpmImportTablesInProgress,
    rpmImportTablesNotStarted,
    rpmMessage,
    rpmProgressInMegaBytes,
    rpmResizeType,
    rpmStatus,
    rpmTargetClusterType,
    rpmTargetEncryptionType,
    rpmTargetNodeType,
    rpmTargetNumberOfNodes,
    rpmTotalResizeDataInMegaBytes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.Message as Types
import qualified Network.AWS.Redshift.Types.ResizeType as Types
import qualified Network.AWS.Redshift.Types.Status as Types
import qualified Network.AWS.Redshift.Types.String as Types
import qualified Network.AWS.Redshift.Types.TargetClusterType as Types
import qualified Network.AWS.Redshift.Types.TargetEncryptionType as Types
import qualified Network.AWS.Redshift.Types.TargetNodeType as Types

-- | Describes the result of a cluster resize operation.
--
-- /See:/ 'mkResizeProgressMessage' smart constructor.
data ResizeProgressMessage = ResizeProgressMessage'
  { -- | The average rate of the resize operation over the last few minutes, measured in megabytes per second. After the resize operation completes, this value shows the average rate of the entire resize operation.
    avgResizeRateInMegaBytesPerSecond :: Core.Maybe Core.Double,
    -- | The percent of data transferred from source cluster to target cluster.
    dataTransferProgressPercent :: Core.Maybe Core.Double,
    -- | The amount of seconds that have elapsed since the resize operation began. After the resize operation completes, this value shows the total actual time, in seconds, for the resize operation.
    elapsedTimeInSeconds :: Core.Maybe Core.Integer,
    -- | The estimated time remaining, in seconds, until the resize operation is complete. This value is calculated based on the average resize rate and the estimated amount of data remaining to be processed. Once the resize operation is complete, this value will be 0.
    estimatedTimeToCompletionInSeconds :: Core.Maybe Core.Integer,
    -- | The names of tables that have been completely imported .
    --
    -- Valid Values: List of table names.
    importTablesCompleted :: Core.Maybe [Types.String],
    -- | The names of tables that are being currently imported.
    --
    -- Valid Values: List of table names.
    importTablesInProgress :: Core.Maybe [Types.String],
    -- | The names of tables that have not been yet imported.
    --
    -- Valid Values: List of table names
    importTablesNotStarted :: Core.Maybe [Types.String],
    -- | An optional string to provide additional details about the resize action.
    message :: Core.Maybe Types.Message,
    -- | While the resize operation is in progress, this value shows the current amount of data, in megabytes, that has been processed so far. When the resize operation is complete, this value shows the total amount of data, in megabytes, on the cluster, which may be more or less than TotalResizeDataInMegaBytes (the estimated total amount of data before resize).
    progressInMegaBytes :: Core.Maybe Core.Integer,
    -- | An enum with possible values of @ClassicResize@ and @ElasticResize@ . These values describe the type of resize operation being performed.
    resizeType :: Core.Maybe Types.ResizeType,
    -- | The status of the resize operation.
    --
    -- Valid Values: @NONE@ | @IN_PROGRESS@ | @FAILED@ | @SUCCEEDED@ | @CANCELLING@
    status :: Core.Maybe Types.Status,
    -- | The cluster type after the resize operation is complete.
    --
    -- Valid Values: @multi-node@ | @single-node@
    targetClusterType :: Core.Maybe Types.TargetClusterType,
    -- | The type of encryption for the cluster after the resize is complete.
    --
    -- Possible values are @KMS@ and @None@ .
    targetEncryptionType :: Core.Maybe Types.TargetEncryptionType,
    -- | The node type that the cluster will have after the resize operation is complete.
    targetNodeType :: Core.Maybe Types.TargetNodeType,
    -- | The number of nodes that the cluster will have after the resize operation is complete.
    targetNumberOfNodes :: Core.Maybe Core.Int,
    -- | The estimated total amount of data, in megabytes, on the cluster before the resize operation began.
    totalResizeDataInMegaBytes :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResizeProgressMessage' value with any optional fields omitted.
mkResizeProgressMessage ::
  ResizeProgressMessage
mkResizeProgressMessage =
  ResizeProgressMessage'
    { avgResizeRateInMegaBytesPerSecond =
        Core.Nothing,
      dataTransferProgressPercent = Core.Nothing,
      elapsedTimeInSeconds = Core.Nothing,
      estimatedTimeToCompletionInSeconds = Core.Nothing,
      importTablesCompleted = Core.Nothing,
      importTablesInProgress = Core.Nothing,
      importTablesNotStarted = Core.Nothing,
      message = Core.Nothing,
      progressInMegaBytes = Core.Nothing,
      resizeType = Core.Nothing,
      status = Core.Nothing,
      targetClusterType = Core.Nothing,
      targetEncryptionType = Core.Nothing,
      targetNodeType = Core.Nothing,
      targetNumberOfNodes = Core.Nothing,
      totalResizeDataInMegaBytes = Core.Nothing
    }

-- | The average rate of the resize operation over the last few minutes, measured in megabytes per second. After the resize operation completes, this value shows the average rate of the entire resize operation.
--
-- /Note:/ Consider using 'avgResizeRateInMegaBytesPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmAvgResizeRateInMegaBytesPerSecond :: Lens.Lens' ResizeProgressMessage (Core.Maybe Core.Double)
rpmAvgResizeRateInMegaBytesPerSecond = Lens.field @"avgResizeRateInMegaBytesPerSecond"
{-# DEPRECATED rpmAvgResizeRateInMegaBytesPerSecond "Use generic-lens or generic-optics with 'avgResizeRateInMegaBytesPerSecond' instead." #-}

-- | The percent of data transferred from source cluster to target cluster.
--
-- /Note:/ Consider using 'dataTransferProgressPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmDataTransferProgressPercent :: Lens.Lens' ResizeProgressMessage (Core.Maybe Core.Double)
rpmDataTransferProgressPercent = Lens.field @"dataTransferProgressPercent"
{-# DEPRECATED rpmDataTransferProgressPercent "Use generic-lens or generic-optics with 'dataTransferProgressPercent' instead." #-}

-- | The amount of seconds that have elapsed since the resize operation began. After the resize operation completes, this value shows the total actual time, in seconds, for the resize operation.
--
-- /Note:/ Consider using 'elapsedTimeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmElapsedTimeInSeconds :: Lens.Lens' ResizeProgressMessage (Core.Maybe Core.Integer)
rpmElapsedTimeInSeconds = Lens.field @"elapsedTimeInSeconds"
{-# DEPRECATED rpmElapsedTimeInSeconds "Use generic-lens or generic-optics with 'elapsedTimeInSeconds' instead." #-}

-- | The estimated time remaining, in seconds, until the resize operation is complete. This value is calculated based on the average resize rate and the estimated amount of data remaining to be processed. Once the resize operation is complete, this value will be 0.
--
-- /Note:/ Consider using 'estimatedTimeToCompletionInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmEstimatedTimeToCompletionInSeconds :: Lens.Lens' ResizeProgressMessage (Core.Maybe Core.Integer)
rpmEstimatedTimeToCompletionInSeconds = Lens.field @"estimatedTimeToCompletionInSeconds"
{-# DEPRECATED rpmEstimatedTimeToCompletionInSeconds "Use generic-lens or generic-optics with 'estimatedTimeToCompletionInSeconds' instead." #-}

-- | The names of tables that have been completely imported .
--
-- Valid Values: List of table names.
--
-- /Note:/ Consider using 'importTablesCompleted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmImportTablesCompleted :: Lens.Lens' ResizeProgressMessage (Core.Maybe [Types.String])
rpmImportTablesCompleted = Lens.field @"importTablesCompleted"
{-# DEPRECATED rpmImportTablesCompleted "Use generic-lens or generic-optics with 'importTablesCompleted' instead." #-}

-- | The names of tables that are being currently imported.
--
-- Valid Values: List of table names.
--
-- /Note:/ Consider using 'importTablesInProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmImportTablesInProgress :: Lens.Lens' ResizeProgressMessage (Core.Maybe [Types.String])
rpmImportTablesInProgress = Lens.field @"importTablesInProgress"
{-# DEPRECATED rpmImportTablesInProgress "Use generic-lens or generic-optics with 'importTablesInProgress' instead." #-}

-- | The names of tables that have not been yet imported.
--
-- Valid Values: List of table names
--
-- /Note:/ Consider using 'importTablesNotStarted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmImportTablesNotStarted :: Lens.Lens' ResizeProgressMessage (Core.Maybe [Types.String])
rpmImportTablesNotStarted = Lens.field @"importTablesNotStarted"
{-# DEPRECATED rpmImportTablesNotStarted "Use generic-lens or generic-optics with 'importTablesNotStarted' instead." #-}

-- | An optional string to provide additional details about the resize action.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmMessage :: Lens.Lens' ResizeProgressMessage (Core.Maybe Types.Message)
rpmMessage = Lens.field @"message"
{-# DEPRECATED rpmMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | While the resize operation is in progress, this value shows the current amount of data, in megabytes, that has been processed so far. When the resize operation is complete, this value shows the total amount of data, in megabytes, on the cluster, which may be more or less than TotalResizeDataInMegaBytes (the estimated total amount of data before resize).
--
-- /Note:/ Consider using 'progressInMegaBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmProgressInMegaBytes :: Lens.Lens' ResizeProgressMessage (Core.Maybe Core.Integer)
rpmProgressInMegaBytes = Lens.field @"progressInMegaBytes"
{-# DEPRECATED rpmProgressInMegaBytes "Use generic-lens or generic-optics with 'progressInMegaBytes' instead." #-}

-- | An enum with possible values of @ClassicResize@ and @ElasticResize@ . These values describe the type of resize operation being performed.
--
-- /Note:/ Consider using 'resizeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmResizeType :: Lens.Lens' ResizeProgressMessage (Core.Maybe Types.ResizeType)
rpmResizeType = Lens.field @"resizeType"
{-# DEPRECATED rpmResizeType "Use generic-lens or generic-optics with 'resizeType' instead." #-}

-- | The status of the resize operation.
--
-- Valid Values: @NONE@ | @IN_PROGRESS@ | @FAILED@ | @SUCCEEDED@ | @CANCELLING@
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmStatus :: Lens.Lens' ResizeProgressMessage (Core.Maybe Types.Status)
rpmStatus = Lens.field @"status"
{-# DEPRECATED rpmStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The cluster type after the resize operation is complete.
--
-- Valid Values: @multi-node@ | @single-node@
--
-- /Note:/ Consider using 'targetClusterType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmTargetClusterType :: Lens.Lens' ResizeProgressMessage (Core.Maybe Types.TargetClusterType)
rpmTargetClusterType = Lens.field @"targetClusterType"
{-# DEPRECATED rpmTargetClusterType "Use generic-lens or generic-optics with 'targetClusterType' instead." #-}

-- | The type of encryption for the cluster after the resize is complete.
--
-- Possible values are @KMS@ and @None@ .
--
-- /Note:/ Consider using 'targetEncryptionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmTargetEncryptionType :: Lens.Lens' ResizeProgressMessage (Core.Maybe Types.TargetEncryptionType)
rpmTargetEncryptionType = Lens.field @"targetEncryptionType"
{-# DEPRECATED rpmTargetEncryptionType "Use generic-lens or generic-optics with 'targetEncryptionType' instead." #-}

-- | The node type that the cluster will have after the resize operation is complete.
--
-- /Note:/ Consider using 'targetNodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmTargetNodeType :: Lens.Lens' ResizeProgressMessage (Core.Maybe Types.TargetNodeType)
rpmTargetNodeType = Lens.field @"targetNodeType"
{-# DEPRECATED rpmTargetNodeType "Use generic-lens or generic-optics with 'targetNodeType' instead." #-}

-- | The number of nodes that the cluster will have after the resize operation is complete.
--
-- /Note:/ Consider using 'targetNumberOfNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmTargetNumberOfNodes :: Lens.Lens' ResizeProgressMessage (Core.Maybe Core.Int)
rpmTargetNumberOfNodes = Lens.field @"targetNumberOfNodes"
{-# DEPRECATED rpmTargetNumberOfNodes "Use generic-lens or generic-optics with 'targetNumberOfNodes' instead." #-}

-- | The estimated total amount of data, in megabytes, on the cluster before the resize operation began.
--
-- /Note:/ Consider using 'totalResizeDataInMegaBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmTotalResizeDataInMegaBytes :: Lens.Lens' ResizeProgressMessage (Core.Maybe Core.Integer)
rpmTotalResizeDataInMegaBytes = Lens.field @"totalResizeDataInMegaBytes"
{-# DEPRECATED rpmTotalResizeDataInMegaBytes "Use generic-lens or generic-optics with 'totalResizeDataInMegaBytes' instead." #-}

instance Core.FromXML ResizeProgressMessage where
  parseXML x =
    ResizeProgressMessage'
      Core.<$> (x Core..@? "AvgResizeRateInMegaBytesPerSecond")
      Core.<*> (x Core..@? "DataTransferProgressPercent")
      Core.<*> (x Core..@? "ElapsedTimeInSeconds")
      Core.<*> (x Core..@? "EstimatedTimeToCompletionInSeconds")
      Core.<*> ( x Core..@? "ImportTablesCompleted"
                   Core..<@> Core.parseXMLList "member"
               )
      Core.<*> ( x Core..@? "ImportTablesInProgress"
                   Core..<@> Core.parseXMLList "member"
               )
      Core.<*> ( x Core..@? "ImportTablesNotStarted"
                   Core..<@> Core.parseXMLList "member"
               )
      Core.<*> (x Core..@? "Message")
      Core.<*> (x Core..@? "ProgressInMegaBytes")
      Core.<*> (x Core..@? "ResizeType")
      Core.<*> (x Core..@? "Status")
      Core.<*> (x Core..@? "TargetClusterType")
      Core.<*> (x Core..@? "TargetEncryptionType")
      Core.<*> (x Core..@? "TargetNodeType")
      Core.<*> (x Core..@? "TargetNumberOfNodes")
      Core.<*> (x Core..@? "TotalResizeDataInMegaBytes")
