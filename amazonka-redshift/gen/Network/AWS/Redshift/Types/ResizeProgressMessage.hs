{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ResizeProgressMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ResizeProgressMessage where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal

-- | Describes the result of a cluster resize operation.
--
-- /See:/ 'newResizeProgressMessage' smart constructor.
data ResizeProgressMessage = ResizeProgressMessage'
  { -- | The status of the resize operation.
    --
    -- Valid Values: @NONE@ | @IN_PROGRESS@ | @FAILED@ | @SUCCEEDED@ |
    -- @CANCELLING@
    status :: Core.Maybe Core.Text,
    -- | The estimated time remaining, in seconds, until the resize operation is
    -- complete. This value is calculated based on the average resize rate and
    -- the estimated amount of data remaining to be processed. Once the resize
    -- operation is complete, this value will be 0.
    estimatedTimeToCompletionInSeconds :: Core.Maybe Core.Integer,
    -- | The names of tables that have not been yet imported.
    --
    -- Valid Values: List of table names
    importTablesNotStarted :: Core.Maybe [Core.Text],
    -- | The node type that the cluster will have after the resize operation is
    -- complete.
    targetNodeType :: Core.Maybe Core.Text,
    -- | An optional string to provide additional details about the resize
    -- action.
    message :: Core.Maybe Core.Text,
    -- | The cluster type after the resize operation is complete.
    --
    -- Valid Values: @multi-node@ | @single-node@
    targetClusterType :: Core.Maybe Core.Text,
    -- | The average rate of the resize operation over the last few minutes,
    -- measured in megabytes per second. After the resize operation completes,
    -- this value shows the average rate of the entire resize operation.
    avgResizeRateInMegaBytesPerSecond :: Core.Maybe Core.Double,
    -- | The type of encryption for the cluster after the resize is complete.
    --
    -- Possible values are @KMS@ and @None@.
    targetEncryptionType :: Core.Maybe Core.Text,
    -- | The amount of seconds that have elapsed since the resize operation
    -- began. After the resize operation completes, this value shows the total
    -- actual time, in seconds, for the resize operation.
    elapsedTimeInSeconds :: Core.Maybe Core.Integer,
    -- | The number of nodes that the cluster will have after the resize
    -- operation is complete.
    targetNumberOfNodes :: Core.Maybe Core.Int,
    -- | The estimated total amount of data, in megabytes, on the cluster before
    -- the resize operation began.
    totalResizeDataInMegaBytes :: Core.Maybe Core.Integer,
    -- | While the resize operation is in progress, this value shows the current
    -- amount of data, in megabytes, that has been processed so far. When the
    -- resize operation is complete, this value shows the total amount of data,
    -- in megabytes, on the cluster, which may be more or less than
    -- TotalResizeDataInMegaBytes (the estimated total amount of data before
    -- resize).
    progressInMegaBytes :: Core.Maybe Core.Integer,
    -- | The percent of data transferred from source cluster to target cluster.
    dataTransferProgressPercent :: Core.Maybe Core.Double,
    -- | The names of tables that have been completely imported .
    --
    -- Valid Values: List of table names.
    importTablesCompleted :: Core.Maybe [Core.Text],
    -- | The names of tables that are being currently imported.
    --
    -- Valid Values: List of table names.
    importTablesInProgress :: Core.Maybe [Core.Text],
    -- | An enum with possible values of @ClassicResize@ and @ElasticResize@.
    -- These values describe the type of resize operation being performed.
    resizeType :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResizeProgressMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'resizeProgressMessage_status' - The status of the resize operation.
--
-- Valid Values: @NONE@ | @IN_PROGRESS@ | @FAILED@ | @SUCCEEDED@ |
-- @CANCELLING@
--
-- 'estimatedTimeToCompletionInSeconds', 'resizeProgressMessage_estimatedTimeToCompletionInSeconds' - The estimated time remaining, in seconds, until the resize operation is
-- complete. This value is calculated based on the average resize rate and
-- the estimated amount of data remaining to be processed. Once the resize
-- operation is complete, this value will be 0.
--
-- 'importTablesNotStarted', 'resizeProgressMessage_importTablesNotStarted' - The names of tables that have not been yet imported.
--
-- Valid Values: List of table names
--
-- 'targetNodeType', 'resizeProgressMessage_targetNodeType' - The node type that the cluster will have after the resize operation is
-- complete.
--
-- 'message', 'resizeProgressMessage_message' - An optional string to provide additional details about the resize
-- action.
--
-- 'targetClusterType', 'resizeProgressMessage_targetClusterType' - The cluster type after the resize operation is complete.
--
-- Valid Values: @multi-node@ | @single-node@
--
-- 'avgResizeRateInMegaBytesPerSecond', 'resizeProgressMessage_avgResizeRateInMegaBytesPerSecond' - The average rate of the resize operation over the last few minutes,
-- measured in megabytes per second. After the resize operation completes,
-- this value shows the average rate of the entire resize operation.
--
-- 'targetEncryptionType', 'resizeProgressMessage_targetEncryptionType' - The type of encryption for the cluster after the resize is complete.
--
-- Possible values are @KMS@ and @None@.
--
-- 'elapsedTimeInSeconds', 'resizeProgressMessage_elapsedTimeInSeconds' - The amount of seconds that have elapsed since the resize operation
-- began. After the resize operation completes, this value shows the total
-- actual time, in seconds, for the resize operation.
--
-- 'targetNumberOfNodes', 'resizeProgressMessage_targetNumberOfNodes' - The number of nodes that the cluster will have after the resize
-- operation is complete.
--
-- 'totalResizeDataInMegaBytes', 'resizeProgressMessage_totalResizeDataInMegaBytes' - The estimated total amount of data, in megabytes, on the cluster before
-- the resize operation began.
--
-- 'progressInMegaBytes', 'resizeProgressMessage_progressInMegaBytes' - While the resize operation is in progress, this value shows the current
-- amount of data, in megabytes, that has been processed so far. When the
-- resize operation is complete, this value shows the total amount of data,
-- in megabytes, on the cluster, which may be more or less than
-- TotalResizeDataInMegaBytes (the estimated total amount of data before
-- resize).
--
-- 'dataTransferProgressPercent', 'resizeProgressMessage_dataTransferProgressPercent' - The percent of data transferred from source cluster to target cluster.
--
-- 'importTablesCompleted', 'resizeProgressMessage_importTablesCompleted' - The names of tables that have been completely imported .
--
-- Valid Values: List of table names.
--
-- 'importTablesInProgress', 'resizeProgressMessage_importTablesInProgress' - The names of tables that are being currently imported.
--
-- Valid Values: List of table names.
--
-- 'resizeType', 'resizeProgressMessage_resizeType' - An enum with possible values of @ClassicResize@ and @ElasticResize@.
-- These values describe the type of resize operation being performed.
newResizeProgressMessage ::
  ResizeProgressMessage
newResizeProgressMessage =
  ResizeProgressMessage'
    { status = Core.Nothing,
      estimatedTimeToCompletionInSeconds = Core.Nothing,
      importTablesNotStarted = Core.Nothing,
      targetNodeType = Core.Nothing,
      message = Core.Nothing,
      targetClusterType = Core.Nothing,
      avgResizeRateInMegaBytesPerSecond = Core.Nothing,
      targetEncryptionType = Core.Nothing,
      elapsedTimeInSeconds = Core.Nothing,
      targetNumberOfNodes = Core.Nothing,
      totalResizeDataInMegaBytes = Core.Nothing,
      progressInMegaBytes = Core.Nothing,
      dataTransferProgressPercent = Core.Nothing,
      importTablesCompleted = Core.Nothing,
      importTablesInProgress = Core.Nothing,
      resizeType = Core.Nothing
    }

-- | The status of the resize operation.
--
-- Valid Values: @NONE@ | @IN_PROGRESS@ | @FAILED@ | @SUCCEEDED@ |
-- @CANCELLING@
resizeProgressMessage_status :: Lens.Lens' ResizeProgressMessage (Core.Maybe Core.Text)
resizeProgressMessage_status = Lens.lens (\ResizeProgressMessage' {status} -> status) (\s@ResizeProgressMessage' {} a -> s {status = a} :: ResizeProgressMessage)

-- | The estimated time remaining, in seconds, until the resize operation is
-- complete. This value is calculated based on the average resize rate and
-- the estimated amount of data remaining to be processed. Once the resize
-- operation is complete, this value will be 0.
resizeProgressMessage_estimatedTimeToCompletionInSeconds :: Lens.Lens' ResizeProgressMessage (Core.Maybe Core.Integer)
resizeProgressMessage_estimatedTimeToCompletionInSeconds = Lens.lens (\ResizeProgressMessage' {estimatedTimeToCompletionInSeconds} -> estimatedTimeToCompletionInSeconds) (\s@ResizeProgressMessage' {} a -> s {estimatedTimeToCompletionInSeconds = a} :: ResizeProgressMessage)

-- | The names of tables that have not been yet imported.
--
-- Valid Values: List of table names
resizeProgressMessage_importTablesNotStarted :: Lens.Lens' ResizeProgressMessage (Core.Maybe [Core.Text])
resizeProgressMessage_importTablesNotStarted = Lens.lens (\ResizeProgressMessage' {importTablesNotStarted} -> importTablesNotStarted) (\s@ResizeProgressMessage' {} a -> s {importTablesNotStarted = a} :: ResizeProgressMessage) Core.. Lens.mapping Lens._Coerce

-- | The node type that the cluster will have after the resize operation is
-- complete.
resizeProgressMessage_targetNodeType :: Lens.Lens' ResizeProgressMessage (Core.Maybe Core.Text)
resizeProgressMessage_targetNodeType = Lens.lens (\ResizeProgressMessage' {targetNodeType} -> targetNodeType) (\s@ResizeProgressMessage' {} a -> s {targetNodeType = a} :: ResizeProgressMessage)

-- | An optional string to provide additional details about the resize
-- action.
resizeProgressMessage_message :: Lens.Lens' ResizeProgressMessage (Core.Maybe Core.Text)
resizeProgressMessage_message = Lens.lens (\ResizeProgressMessage' {message} -> message) (\s@ResizeProgressMessage' {} a -> s {message = a} :: ResizeProgressMessage)

-- | The cluster type after the resize operation is complete.
--
-- Valid Values: @multi-node@ | @single-node@
resizeProgressMessage_targetClusterType :: Lens.Lens' ResizeProgressMessage (Core.Maybe Core.Text)
resizeProgressMessage_targetClusterType = Lens.lens (\ResizeProgressMessage' {targetClusterType} -> targetClusterType) (\s@ResizeProgressMessage' {} a -> s {targetClusterType = a} :: ResizeProgressMessage)

-- | The average rate of the resize operation over the last few minutes,
-- measured in megabytes per second. After the resize operation completes,
-- this value shows the average rate of the entire resize operation.
resizeProgressMessage_avgResizeRateInMegaBytesPerSecond :: Lens.Lens' ResizeProgressMessage (Core.Maybe Core.Double)
resizeProgressMessage_avgResizeRateInMegaBytesPerSecond = Lens.lens (\ResizeProgressMessage' {avgResizeRateInMegaBytesPerSecond} -> avgResizeRateInMegaBytesPerSecond) (\s@ResizeProgressMessage' {} a -> s {avgResizeRateInMegaBytesPerSecond = a} :: ResizeProgressMessage)

-- | The type of encryption for the cluster after the resize is complete.
--
-- Possible values are @KMS@ and @None@.
resizeProgressMessage_targetEncryptionType :: Lens.Lens' ResizeProgressMessage (Core.Maybe Core.Text)
resizeProgressMessage_targetEncryptionType = Lens.lens (\ResizeProgressMessage' {targetEncryptionType} -> targetEncryptionType) (\s@ResizeProgressMessage' {} a -> s {targetEncryptionType = a} :: ResizeProgressMessage)

-- | The amount of seconds that have elapsed since the resize operation
-- began. After the resize operation completes, this value shows the total
-- actual time, in seconds, for the resize operation.
resizeProgressMessage_elapsedTimeInSeconds :: Lens.Lens' ResizeProgressMessage (Core.Maybe Core.Integer)
resizeProgressMessage_elapsedTimeInSeconds = Lens.lens (\ResizeProgressMessage' {elapsedTimeInSeconds} -> elapsedTimeInSeconds) (\s@ResizeProgressMessage' {} a -> s {elapsedTimeInSeconds = a} :: ResizeProgressMessage)

-- | The number of nodes that the cluster will have after the resize
-- operation is complete.
resizeProgressMessage_targetNumberOfNodes :: Lens.Lens' ResizeProgressMessage (Core.Maybe Core.Int)
resizeProgressMessage_targetNumberOfNodes = Lens.lens (\ResizeProgressMessage' {targetNumberOfNodes} -> targetNumberOfNodes) (\s@ResizeProgressMessage' {} a -> s {targetNumberOfNodes = a} :: ResizeProgressMessage)

-- | The estimated total amount of data, in megabytes, on the cluster before
-- the resize operation began.
resizeProgressMessage_totalResizeDataInMegaBytes :: Lens.Lens' ResizeProgressMessage (Core.Maybe Core.Integer)
resizeProgressMessage_totalResizeDataInMegaBytes = Lens.lens (\ResizeProgressMessage' {totalResizeDataInMegaBytes} -> totalResizeDataInMegaBytes) (\s@ResizeProgressMessage' {} a -> s {totalResizeDataInMegaBytes = a} :: ResizeProgressMessage)

-- | While the resize operation is in progress, this value shows the current
-- amount of data, in megabytes, that has been processed so far. When the
-- resize operation is complete, this value shows the total amount of data,
-- in megabytes, on the cluster, which may be more or less than
-- TotalResizeDataInMegaBytes (the estimated total amount of data before
-- resize).
resizeProgressMessage_progressInMegaBytes :: Lens.Lens' ResizeProgressMessage (Core.Maybe Core.Integer)
resizeProgressMessage_progressInMegaBytes = Lens.lens (\ResizeProgressMessage' {progressInMegaBytes} -> progressInMegaBytes) (\s@ResizeProgressMessage' {} a -> s {progressInMegaBytes = a} :: ResizeProgressMessage)

-- | The percent of data transferred from source cluster to target cluster.
resizeProgressMessage_dataTransferProgressPercent :: Lens.Lens' ResizeProgressMessage (Core.Maybe Core.Double)
resizeProgressMessage_dataTransferProgressPercent = Lens.lens (\ResizeProgressMessage' {dataTransferProgressPercent} -> dataTransferProgressPercent) (\s@ResizeProgressMessage' {} a -> s {dataTransferProgressPercent = a} :: ResizeProgressMessage)

-- | The names of tables that have been completely imported .
--
-- Valid Values: List of table names.
resizeProgressMessage_importTablesCompleted :: Lens.Lens' ResizeProgressMessage (Core.Maybe [Core.Text])
resizeProgressMessage_importTablesCompleted = Lens.lens (\ResizeProgressMessage' {importTablesCompleted} -> importTablesCompleted) (\s@ResizeProgressMessage' {} a -> s {importTablesCompleted = a} :: ResizeProgressMessage) Core.. Lens.mapping Lens._Coerce

-- | The names of tables that are being currently imported.
--
-- Valid Values: List of table names.
resizeProgressMessage_importTablesInProgress :: Lens.Lens' ResizeProgressMessage (Core.Maybe [Core.Text])
resizeProgressMessage_importTablesInProgress = Lens.lens (\ResizeProgressMessage' {importTablesInProgress} -> importTablesInProgress) (\s@ResizeProgressMessage' {} a -> s {importTablesInProgress = a} :: ResizeProgressMessage) Core.. Lens.mapping Lens._Coerce

-- | An enum with possible values of @ClassicResize@ and @ElasticResize@.
-- These values describe the type of resize operation being performed.
resizeProgressMessage_resizeType :: Lens.Lens' ResizeProgressMessage (Core.Maybe Core.Text)
resizeProgressMessage_resizeType = Lens.lens (\ResizeProgressMessage' {resizeType} -> resizeType) (\s@ResizeProgressMessage' {} a -> s {resizeType = a} :: ResizeProgressMessage)

instance Core.FromXML ResizeProgressMessage where
  parseXML x =
    ResizeProgressMessage'
      Core.<$> (x Core..@? "Status")
      Core.<*> (x Core..@? "EstimatedTimeToCompletionInSeconds")
      Core.<*> ( x Core..@? "ImportTablesNotStarted"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "TargetNodeType")
      Core.<*> (x Core..@? "Message")
      Core.<*> (x Core..@? "TargetClusterType")
      Core.<*> (x Core..@? "AvgResizeRateInMegaBytesPerSecond")
      Core.<*> (x Core..@? "TargetEncryptionType")
      Core.<*> (x Core..@? "ElapsedTimeInSeconds")
      Core.<*> (x Core..@? "TargetNumberOfNodes")
      Core.<*> (x Core..@? "TotalResizeDataInMegaBytes")
      Core.<*> (x Core..@? "ProgressInMegaBytes")
      Core.<*> (x Core..@? "DataTransferProgressPercent")
      Core.<*> ( x Core..@? "ImportTablesCompleted"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> ( x Core..@? "ImportTablesInProgress"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "ResizeType")

instance Core.Hashable ResizeProgressMessage

instance Core.NFData ResizeProgressMessage
