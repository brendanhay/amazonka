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
-- Module      : Amazonka.Redshift.Types.ResizeProgressMessage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.ResizeProgressMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

-- | Describes the result of a cluster resize operation.
--
-- /See:/ 'newResizeProgressMessage' smart constructor.
data ResizeProgressMessage = ResizeProgressMessage'
  { -- | The average rate of the resize operation over the last few minutes,
    -- measured in megabytes per second. After the resize operation completes,
    -- this value shows the average rate of the entire resize operation.
    avgResizeRateInMegaBytesPerSecond :: Prelude.Maybe Prelude.Double,
    -- | The percent of data transferred from source cluster to target cluster.
    dataTransferProgressPercent :: Prelude.Maybe Prelude.Double,
    -- | The amount of seconds that have elapsed since the resize operation
    -- began. After the resize operation completes, this value shows the total
    -- actual time, in seconds, for the resize operation.
    elapsedTimeInSeconds :: Prelude.Maybe Prelude.Integer,
    -- | The estimated time remaining, in seconds, until the resize operation is
    -- complete. This value is calculated based on the average resize rate and
    -- the estimated amount of data remaining to be processed. Once the resize
    -- operation is complete, this value will be 0.
    estimatedTimeToCompletionInSeconds :: Prelude.Maybe Prelude.Integer,
    -- | The names of tables that have been completely imported .
    --
    -- Valid Values: List of table names.
    importTablesCompleted :: Prelude.Maybe [Prelude.Text],
    -- | The names of tables that are being currently imported.
    --
    -- Valid Values: List of table names.
    importTablesInProgress :: Prelude.Maybe [Prelude.Text],
    -- | The names of tables that have not been yet imported.
    --
    -- Valid Values: List of table names
    importTablesNotStarted :: Prelude.Maybe [Prelude.Text],
    -- | An optional string to provide additional details about the resize
    -- action.
    message :: Prelude.Maybe Prelude.Text,
    -- | While the resize operation is in progress, this value shows the current
    -- amount of data, in megabytes, that has been processed so far. When the
    -- resize operation is complete, this value shows the total amount of data,
    -- in megabytes, on the cluster, which may be more or less than
    -- TotalResizeDataInMegaBytes (the estimated total amount of data before
    -- resize).
    progressInMegaBytes :: Prelude.Maybe Prelude.Integer,
    -- | An enum with possible values of @ClassicResize@ and @ElasticResize@.
    -- These values describe the type of resize operation being performed.
    resizeType :: Prelude.Maybe Prelude.Text,
    -- | The status of the resize operation.
    --
    -- Valid Values: @NONE@ | @IN_PROGRESS@ | @FAILED@ | @SUCCEEDED@ |
    -- @CANCELLING@
    status :: Prelude.Maybe Prelude.Text,
    -- | The cluster type after the resize operation is complete.
    --
    -- Valid Values: @multi-node@ | @single-node@
    targetClusterType :: Prelude.Maybe Prelude.Text,
    -- | The type of encryption for the cluster after the resize is complete.
    --
    -- Possible values are @KMS@ and @None@.
    targetEncryptionType :: Prelude.Maybe Prelude.Text,
    -- | The node type that the cluster will have after the resize operation is
    -- complete.
    targetNodeType :: Prelude.Maybe Prelude.Text,
    -- | The number of nodes that the cluster will have after the resize
    -- operation is complete.
    targetNumberOfNodes :: Prelude.Maybe Prelude.Int,
    -- | The estimated total amount of data, in megabytes, on the cluster before
    -- the resize operation began.
    totalResizeDataInMegaBytes :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResizeProgressMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'avgResizeRateInMegaBytesPerSecond', 'resizeProgressMessage_avgResizeRateInMegaBytesPerSecond' - The average rate of the resize operation over the last few minutes,
-- measured in megabytes per second. After the resize operation completes,
-- this value shows the average rate of the entire resize operation.
--
-- 'dataTransferProgressPercent', 'resizeProgressMessage_dataTransferProgressPercent' - The percent of data transferred from source cluster to target cluster.
--
-- 'elapsedTimeInSeconds', 'resizeProgressMessage_elapsedTimeInSeconds' - The amount of seconds that have elapsed since the resize operation
-- began. After the resize operation completes, this value shows the total
-- actual time, in seconds, for the resize operation.
--
-- 'estimatedTimeToCompletionInSeconds', 'resizeProgressMessage_estimatedTimeToCompletionInSeconds' - The estimated time remaining, in seconds, until the resize operation is
-- complete. This value is calculated based on the average resize rate and
-- the estimated amount of data remaining to be processed. Once the resize
-- operation is complete, this value will be 0.
--
-- 'importTablesCompleted', 'resizeProgressMessage_importTablesCompleted' - The names of tables that have been completely imported .
--
-- Valid Values: List of table names.
--
-- 'importTablesInProgress', 'resizeProgressMessage_importTablesInProgress' - The names of tables that are being currently imported.
--
-- Valid Values: List of table names.
--
-- 'importTablesNotStarted', 'resizeProgressMessage_importTablesNotStarted' - The names of tables that have not been yet imported.
--
-- Valid Values: List of table names
--
-- 'message', 'resizeProgressMessage_message' - An optional string to provide additional details about the resize
-- action.
--
-- 'progressInMegaBytes', 'resizeProgressMessage_progressInMegaBytes' - While the resize operation is in progress, this value shows the current
-- amount of data, in megabytes, that has been processed so far. When the
-- resize operation is complete, this value shows the total amount of data,
-- in megabytes, on the cluster, which may be more or less than
-- TotalResizeDataInMegaBytes (the estimated total amount of data before
-- resize).
--
-- 'resizeType', 'resizeProgressMessage_resizeType' - An enum with possible values of @ClassicResize@ and @ElasticResize@.
-- These values describe the type of resize operation being performed.
--
-- 'status', 'resizeProgressMessage_status' - The status of the resize operation.
--
-- Valid Values: @NONE@ | @IN_PROGRESS@ | @FAILED@ | @SUCCEEDED@ |
-- @CANCELLING@
--
-- 'targetClusterType', 'resizeProgressMessage_targetClusterType' - The cluster type after the resize operation is complete.
--
-- Valid Values: @multi-node@ | @single-node@
--
-- 'targetEncryptionType', 'resizeProgressMessage_targetEncryptionType' - The type of encryption for the cluster after the resize is complete.
--
-- Possible values are @KMS@ and @None@.
--
-- 'targetNodeType', 'resizeProgressMessage_targetNodeType' - The node type that the cluster will have after the resize operation is
-- complete.
--
-- 'targetNumberOfNodes', 'resizeProgressMessage_targetNumberOfNodes' - The number of nodes that the cluster will have after the resize
-- operation is complete.
--
-- 'totalResizeDataInMegaBytes', 'resizeProgressMessage_totalResizeDataInMegaBytes' - The estimated total amount of data, in megabytes, on the cluster before
-- the resize operation began.
newResizeProgressMessage ::
  ResizeProgressMessage
newResizeProgressMessage =
  ResizeProgressMessage'
    { avgResizeRateInMegaBytesPerSecond =
        Prelude.Nothing,
      dataTransferProgressPercent = Prelude.Nothing,
      elapsedTimeInSeconds = Prelude.Nothing,
      estimatedTimeToCompletionInSeconds = Prelude.Nothing,
      importTablesCompleted = Prelude.Nothing,
      importTablesInProgress = Prelude.Nothing,
      importTablesNotStarted = Prelude.Nothing,
      message = Prelude.Nothing,
      progressInMegaBytes = Prelude.Nothing,
      resizeType = Prelude.Nothing,
      status = Prelude.Nothing,
      targetClusterType = Prelude.Nothing,
      targetEncryptionType = Prelude.Nothing,
      targetNodeType = Prelude.Nothing,
      targetNumberOfNodes = Prelude.Nothing,
      totalResizeDataInMegaBytes = Prelude.Nothing
    }

-- | The average rate of the resize operation over the last few minutes,
-- measured in megabytes per second. After the resize operation completes,
-- this value shows the average rate of the entire resize operation.
resizeProgressMessage_avgResizeRateInMegaBytesPerSecond :: Lens.Lens' ResizeProgressMessage (Prelude.Maybe Prelude.Double)
resizeProgressMessage_avgResizeRateInMegaBytesPerSecond = Lens.lens (\ResizeProgressMessage' {avgResizeRateInMegaBytesPerSecond} -> avgResizeRateInMegaBytesPerSecond) (\s@ResizeProgressMessage' {} a -> s {avgResizeRateInMegaBytesPerSecond = a} :: ResizeProgressMessage)

-- | The percent of data transferred from source cluster to target cluster.
resizeProgressMessage_dataTransferProgressPercent :: Lens.Lens' ResizeProgressMessage (Prelude.Maybe Prelude.Double)
resizeProgressMessage_dataTransferProgressPercent = Lens.lens (\ResizeProgressMessage' {dataTransferProgressPercent} -> dataTransferProgressPercent) (\s@ResizeProgressMessage' {} a -> s {dataTransferProgressPercent = a} :: ResizeProgressMessage)

-- | The amount of seconds that have elapsed since the resize operation
-- began. After the resize operation completes, this value shows the total
-- actual time, in seconds, for the resize operation.
resizeProgressMessage_elapsedTimeInSeconds :: Lens.Lens' ResizeProgressMessage (Prelude.Maybe Prelude.Integer)
resizeProgressMessage_elapsedTimeInSeconds = Lens.lens (\ResizeProgressMessage' {elapsedTimeInSeconds} -> elapsedTimeInSeconds) (\s@ResizeProgressMessage' {} a -> s {elapsedTimeInSeconds = a} :: ResizeProgressMessage)

-- | The estimated time remaining, in seconds, until the resize operation is
-- complete. This value is calculated based on the average resize rate and
-- the estimated amount of data remaining to be processed. Once the resize
-- operation is complete, this value will be 0.
resizeProgressMessage_estimatedTimeToCompletionInSeconds :: Lens.Lens' ResizeProgressMessage (Prelude.Maybe Prelude.Integer)
resizeProgressMessage_estimatedTimeToCompletionInSeconds = Lens.lens (\ResizeProgressMessage' {estimatedTimeToCompletionInSeconds} -> estimatedTimeToCompletionInSeconds) (\s@ResizeProgressMessage' {} a -> s {estimatedTimeToCompletionInSeconds = a} :: ResizeProgressMessage)

-- | The names of tables that have been completely imported .
--
-- Valid Values: List of table names.
resizeProgressMessage_importTablesCompleted :: Lens.Lens' ResizeProgressMessage (Prelude.Maybe [Prelude.Text])
resizeProgressMessage_importTablesCompleted = Lens.lens (\ResizeProgressMessage' {importTablesCompleted} -> importTablesCompleted) (\s@ResizeProgressMessage' {} a -> s {importTablesCompleted = a} :: ResizeProgressMessage) Prelude.. Lens.mapping Lens.coerced

-- | The names of tables that are being currently imported.
--
-- Valid Values: List of table names.
resizeProgressMessage_importTablesInProgress :: Lens.Lens' ResizeProgressMessage (Prelude.Maybe [Prelude.Text])
resizeProgressMessage_importTablesInProgress = Lens.lens (\ResizeProgressMessage' {importTablesInProgress} -> importTablesInProgress) (\s@ResizeProgressMessage' {} a -> s {importTablesInProgress = a} :: ResizeProgressMessage) Prelude.. Lens.mapping Lens.coerced

-- | The names of tables that have not been yet imported.
--
-- Valid Values: List of table names
resizeProgressMessage_importTablesNotStarted :: Lens.Lens' ResizeProgressMessage (Prelude.Maybe [Prelude.Text])
resizeProgressMessage_importTablesNotStarted = Lens.lens (\ResizeProgressMessage' {importTablesNotStarted} -> importTablesNotStarted) (\s@ResizeProgressMessage' {} a -> s {importTablesNotStarted = a} :: ResizeProgressMessage) Prelude.. Lens.mapping Lens.coerced

-- | An optional string to provide additional details about the resize
-- action.
resizeProgressMessage_message :: Lens.Lens' ResizeProgressMessage (Prelude.Maybe Prelude.Text)
resizeProgressMessage_message = Lens.lens (\ResizeProgressMessage' {message} -> message) (\s@ResizeProgressMessage' {} a -> s {message = a} :: ResizeProgressMessage)

-- | While the resize operation is in progress, this value shows the current
-- amount of data, in megabytes, that has been processed so far. When the
-- resize operation is complete, this value shows the total amount of data,
-- in megabytes, on the cluster, which may be more or less than
-- TotalResizeDataInMegaBytes (the estimated total amount of data before
-- resize).
resizeProgressMessage_progressInMegaBytes :: Lens.Lens' ResizeProgressMessage (Prelude.Maybe Prelude.Integer)
resizeProgressMessage_progressInMegaBytes = Lens.lens (\ResizeProgressMessage' {progressInMegaBytes} -> progressInMegaBytes) (\s@ResizeProgressMessage' {} a -> s {progressInMegaBytes = a} :: ResizeProgressMessage)

-- | An enum with possible values of @ClassicResize@ and @ElasticResize@.
-- These values describe the type of resize operation being performed.
resizeProgressMessage_resizeType :: Lens.Lens' ResizeProgressMessage (Prelude.Maybe Prelude.Text)
resizeProgressMessage_resizeType = Lens.lens (\ResizeProgressMessage' {resizeType} -> resizeType) (\s@ResizeProgressMessage' {} a -> s {resizeType = a} :: ResizeProgressMessage)

-- | The status of the resize operation.
--
-- Valid Values: @NONE@ | @IN_PROGRESS@ | @FAILED@ | @SUCCEEDED@ |
-- @CANCELLING@
resizeProgressMessage_status :: Lens.Lens' ResizeProgressMessage (Prelude.Maybe Prelude.Text)
resizeProgressMessage_status = Lens.lens (\ResizeProgressMessage' {status} -> status) (\s@ResizeProgressMessage' {} a -> s {status = a} :: ResizeProgressMessage)

-- | The cluster type after the resize operation is complete.
--
-- Valid Values: @multi-node@ | @single-node@
resizeProgressMessage_targetClusterType :: Lens.Lens' ResizeProgressMessage (Prelude.Maybe Prelude.Text)
resizeProgressMessage_targetClusterType = Lens.lens (\ResizeProgressMessage' {targetClusterType} -> targetClusterType) (\s@ResizeProgressMessage' {} a -> s {targetClusterType = a} :: ResizeProgressMessage)

-- | The type of encryption for the cluster after the resize is complete.
--
-- Possible values are @KMS@ and @None@.
resizeProgressMessage_targetEncryptionType :: Lens.Lens' ResizeProgressMessage (Prelude.Maybe Prelude.Text)
resizeProgressMessage_targetEncryptionType = Lens.lens (\ResizeProgressMessage' {targetEncryptionType} -> targetEncryptionType) (\s@ResizeProgressMessage' {} a -> s {targetEncryptionType = a} :: ResizeProgressMessage)

-- | The node type that the cluster will have after the resize operation is
-- complete.
resizeProgressMessage_targetNodeType :: Lens.Lens' ResizeProgressMessage (Prelude.Maybe Prelude.Text)
resizeProgressMessage_targetNodeType = Lens.lens (\ResizeProgressMessage' {targetNodeType} -> targetNodeType) (\s@ResizeProgressMessage' {} a -> s {targetNodeType = a} :: ResizeProgressMessage)

-- | The number of nodes that the cluster will have after the resize
-- operation is complete.
resizeProgressMessage_targetNumberOfNodes :: Lens.Lens' ResizeProgressMessage (Prelude.Maybe Prelude.Int)
resizeProgressMessage_targetNumberOfNodes = Lens.lens (\ResizeProgressMessage' {targetNumberOfNodes} -> targetNumberOfNodes) (\s@ResizeProgressMessage' {} a -> s {targetNumberOfNodes = a} :: ResizeProgressMessage)

-- | The estimated total amount of data, in megabytes, on the cluster before
-- the resize operation began.
resizeProgressMessage_totalResizeDataInMegaBytes :: Lens.Lens' ResizeProgressMessage (Prelude.Maybe Prelude.Integer)
resizeProgressMessage_totalResizeDataInMegaBytes = Lens.lens (\ResizeProgressMessage' {totalResizeDataInMegaBytes} -> totalResizeDataInMegaBytes) (\s@ResizeProgressMessage' {} a -> s {totalResizeDataInMegaBytes = a} :: ResizeProgressMessage)

instance Data.FromXML ResizeProgressMessage where
  parseXML x =
    ResizeProgressMessage'
      Prelude.<$> (x Data..@? "AvgResizeRateInMegaBytesPerSecond")
      Prelude.<*> (x Data..@? "DataTransferProgressPercent")
      Prelude.<*> (x Data..@? "ElapsedTimeInSeconds")
      Prelude.<*> (x Data..@? "EstimatedTimeToCompletionInSeconds")
      Prelude.<*> ( x
                      Data..@? "ImportTablesCompleted"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> ( x
                      Data..@? "ImportTablesInProgress"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> ( x
                      Data..@? "ImportTablesNotStarted"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "Message")
      Prelude.<*> (x Data..@? "ProgressInMegaBytes")
      Prelude.<*> (x Data..@? "ResizeType")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> (x Data..@? "TargetClusterType")
      Prelude.<*> (x Data..@? "TargetEncryptionType")
      Prelude.<*> (x Data..@? "TargetNodeType")
      Prelude.<*> (x Data..@? "TargetNumberOfNodes")
      Prelude.<*> (x Data..@? "TotalResizeDataInMegaBytes")

instance Prelude.Hashable ResizeProgressMessage where
  hashWithSalt _salt ResizeProgressMessage' {..} =
    _salt
      `Prelude.hashWithSalt` avgResizeRateInMegaBytesPerSecond
      `Prelude.hashWithSalt` dataTransferProgressPercent
      `Prelude.hashWithSalt` elapsedTimeInSeconds
      `Prelude.hashWithSalt` estimatedTimeToCompletionInSeconds
      `Prelude.hashWithSalt` importTablesCompleted
      `Prelude.hashWithSalt` importTablesInProgress
      `Prelude.hashWithSalt` importTablesNotStarted
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` progressInMegaBytes
      `Prelude.hashWithSalt` resizeType
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` targetClusterType
      `Prelude.hashWithSalt` targetEncryptionType
      `Prelude.hashWithSalt` targetNodeType
      `Prelude.hashWithSalt` targetNumberOfNodes
      `Prelude.hashWithSalt` totalResizeDataInMegaBytes

instance Prelude.NFData ResizeProgressMessage where
  rnf ResizeProgressMessage' {..} =
    Prelude.rnf avgResizeRateInMegaBytesPerSecond
      `Prelude.seq` Prelude.rnf dataTransferProgressPercent
      `Prelude.seq` Prelude.rnf elapsedTimeInSeconds
      `Prelude.seq` Prelude.rnf estimatedTimeToCompletionInSeconds
      `Prelude.seq` Prelude.rnf importTablesCompleted
      `Prelude.seq` Prelude.rnf importTablesInProgress
      `Prelude.seq` Prelude.rnf importTablesNotStarted
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf progressInMegaBytes
      `Prelude.seq` Prelude.rnf resizeType
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf targetClusterType
      `Prelude.seq` Prelude.rnf targetEncryptionType
      `Prelude.seq` Prelude.rnf targetNodeType
      `Prelude.seq` Prelude.rnf targetNumberOfNodes
      `Prelude.seq` Prelude.rnf totalResizeDataInMegaBytes
