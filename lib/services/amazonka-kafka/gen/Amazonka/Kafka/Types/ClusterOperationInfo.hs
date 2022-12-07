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
-- Module      : Amazonka.Kafka.Types.ClusterOperationInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.ClusterOperationInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types.ClusterOperationStep
import Amazonka.Kafka.Types.ErrorInfo
import Amazonka.Kafka.Types.MutableClusterInfo
import qualified Amazonka.Prelude as Prelude

-- | Returns information about a cluster operation.
--
-- /See:/ 'newClusterOperationInfo' smart constructor.
data ClusterOperationInfo = ClusterOperationInfo'
  { -- | ARN of the cluster.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | Type of the cluster operation.
    operationType :: Prelude.Maybe Prelude.Text,
    -- | ARN of the cluster operation.
    operationArn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the operation finished.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | Information about cluster attributes before a cluster is updated.
    sourceClusterInfo :: Prelude.Maybe MutableClusterInfo,
    -- | Information about cluster attributes after a cluster is updated.
    targetClusterInfo :: Prelude.Maybe MutableClusterInfo,
    -- | The ID of the API request that triggered this operation.
    clientRequestId :: Prelude.Maybe Prelude.Text,
    -- | The time that the operation was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | State of the cluster operation.
    operationState :: Prelude.Maybe Prelude.Text,
    -- | Describes the error if the operation fails.
    errorInfo :: Prelude.Maybe ErrorInfo,
    -- | Steps completed during the operation.
    operationSteps :: Prelude.Maybe [ClusterOperationStep]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClusterOperationInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'clusterOperationInfo_clusterArn' - ARN of the cluster.
--
-- 'operationType', 'clusterOperationInfo_operationType' - Type of the cluster operation.
--
-- 'operationArn', 'clusterOperationInfo_operationArn' - ARN of the cluster operation.
--
-- 'endTime', 'clusterOperationInfo_endTime' - The time at which the operation finished.
--
-- 'sourceClusterInfo', 'clusterOperationInfo_sourceClusterInfo' - Information about cluster attributes before a cluster is updated.
--
-- 'targetClusterInfo', 'clusterOperationInfo_targetClusterInfo' - Information about cluster attributes after a cluster is updated.
--
-- 'clientRequestId', 'clusterOperationInfo_clientRequestId' - The ID of the API request that triggered this operation.
--
-- 'creationTime', 'clusterOperationInfo_creationTime' - The time that the operation was created.
--
-- 'operationState', 'clusterOperationInfo_operationState' - State of the cluster operation.
--
-- 'errorInfo', 'clusterOperationInfo_errorInfo' - Describes the error if the operation fails.
--
-- 'operationSteps', 'clusterOperationInfo_operationSteps' - Steps completed during the operation.
newClusterOperationInfo ::
  ClusterOperationInfo
newClusterOperationInfo =
  ClusterOperationInfo'
    { clusterArn = Prelude.Nothing,
      operationType = Prelude.Nothing,
      operationArn = Prelude.Nothing,
      endTime = Prelude.Nothing,
      sourceClusterInfo = Prelude.Nothing,
      targetClusterInfo = Prelude.Nothing,
      clientRequestId = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      operationState = Prelude.Nothing,
      errorInfo = Prelude.Nothing,
      operationSteps = Prelude.Nothing
    }

-- | ARN of the cluster.
clusterOperationInfo_clusterArn :: Lens.Lens' ClusterOperationInfo (Prelude.Maybe Prelude.Text)
clusterOperationInfo_clusterArn = Lens.lens (\ClusterOperationInfo' {clusterArn} -> clusterArn) (\s@ClusterOperationInfo' {} a -> s {clusterArn = a} :: ClusterOperationInfo)

-- | Type of the cluster operation.
clusterOperationInfo_operationType :: Lens.Lens' ClusterOperationInfo (Prelude.Maybe Prelude.Text)
clusterOperationInfo_operationType = Lens.lens (\ClusterOperationInfo' {operationType} -> operationType) (\s@ClusterOperationInfo' {} a -> s {operationType = a} :: ClusterOperationInfo)

-- | ARN of the cluster operation.
clusterOperationInfo_operationArn :: Lens.Lens' ClusterOperationInfo (Prelude.Maybe Prelude.Text)
clusterOperationInfo_operationArn = Lens.lens (\ClusterOperationInfo' {operationArn} -> operationArn) (\s@ClusterOperationInfo' {} a -> s {operationArn = a} :: ClusterOperationInfo)

-- | The time at which the operation finished.
clusterOperationInfo_endTime :: Lens.Lens' ClusterOperationInfo (Prelude.Maybe Prelude.UTCTime)
clusterOperationInfo_endTime = Lens.lens (\ClusterOperationInfo' {endTime} -> endTime) (\s@ClusterOperationInfo' {} a -> s {endTime = a} :: ClusterOperationInfo) Prelude.. Lens.mapping Data._Time

-- | Information about cluster attributes before a cluster is updated.
clusterOperationInfo_sourceClusterInfo :: Lens.Lens' ClusterOperationInfo (Prelude.Maybe MutableClusterInfo)
clusterOperationInfo_sourceClusterInfo = Lens.lens (\ClusterOperationInfo' {sourceClusterInfo} -> sourceClusterInfo) (\s@ClusterOperationInfo' {} a -> s {sourceClusterInfo = a} :: ClusterOperationInfo)

-- | Information about cluster attributes after a cluster is updated.
clusterOperationInfo_targetClusterInfo :: Lens.Lens' ClusterOperationInfo (Prelude.Maybe MutableClusterInfo)
clusterOperationInfo_targetClusterInfo = Lens.lens (\ClusterOperationInfo' {targetClusterInfo} -> targetClusterInfo) (\s@ClusterOperationInfo' {} a -> s {targetClusterInfo = a} :: ClusterOperationInfo)

-- | The ID of the API request that triggered this operation.
clusterOperationInfo_clientRequestId :: Lens.Lens' ClusterOperationInfo (Prelude.Maybe Prelude.Text)
clusterOperationInfo_clientRequestId = Lens.lens (\ClusterOperationInfo' {clientRequestId} -> clientRequestId) (\s@ClusterOperationInfo' {} a -> s {clientRequestId = a} :: ClusterOperationInfo)

-- | The time that the operation was created.
clusterOperationInfo_creationTime :: Lens.Lens' ClusterOperationInfo (Prelude.Maybe Prelude.UTCTime)
clusterOperationInfo_creationTime = Lens.lens (\ClusterOperationInfo' {creationTime} -> creationTime) (\s@ClusterOperationInfo' {} a -> s {creationTime = a} :: ClusterOperationInfo) Prelude.. Lens.mapping Data._Time

-- | State of the cluster operation.
clusterOperationInfo_operationState :: Lens.Lens' ClusterOperationInfo (Prelude.Maybe Prelude.Text)
clusterOperationInfo_operationState = Lens.lens (\ClusterOperationInfo' {operationState} -> operationState) (\s@ClusterOperationInfo' {} a -> s {operationState = a} :: ClusterOperationInfo)

-- | Describes the error if the operation fails.
clusterOperationInfo_errorInfo :: Lens.Lens' ClusterOperationInfo (Prelude.Maybe ErrorInfo)
clusterOperationInfo_errorInfo = Lens.lens (\ClusterOperationInfo' {errorInfo} -> errorInfo) (\s@ClusterOperationInfo' {} a -> s {errorInfo = a} :: ClusterOperationInfo)

-- | Steps completed during the operation.
clusterOperationInfo_operationSteps :: Lens.Lens' ClusterOperationInfo (Prelude.Maybe [ClusterOperationStep])
clusterOperationInfo_operationSteps = Lens.lens (\ClusterOperationInfo' {operationSteps} -> operationSteps) (\s@ClusterOperationInfo' {} a -> s {operationSteps = a} :: ClusterOperationInfo) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ClusterOperationInfo where
  parseJSON =
    Data.withObject
      "ClusterOperationInfo"
      ( \x ->
          ClusterOperationInfo'
            Prelude.<$> (x Data..:? "clusterArn")
            Prelude.<*> (x Data..:? "operationType")
            Prelude.<*> (x Data..:? "operationArn")
            Prelude.<*> (x Data..:? "endTime")
            Prelude.<*> (x Data..:? "sourceClusterInfo")
            Prelude.<*> (x Data..:? "targetClusterInfo")
            Prelude.<*> (x Data..:? "clientRequestId")
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "operationState")
            Prelude.<*> (x Data..:? "errorInfo")
            Prelude.<*> ( x Data..:? "operationSteps"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ClusterOperationInfo where
  hashWithSalt _salt ClusterOperationInfo' {..} =
    _salt `Prelude.hashWithSalt` clusterArn
      `Prelude.hashWithSalt` operationType
      `Prelude.hashWithSalt` operationArn
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` sourceClusterInfo
      `Prelude.hashWithSalt` targetClusterInfo
      `Prelude.hashWithSalt` clientRequestId
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` operationState
      `Prelude.hashWithSalt` errorInfo
      `Prelude.hashWithSalt` operationSteps

instance Prelude.NFData ClusterOperationInfo where
  rnf ClusterOperationInfo' {..} =
    Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf operationType
      `Prelude.seq` Prelude.rnf operationArn
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf sourceClusterInfo
      `Prelude.seq` Prelude.rnf targetClusterInfo
      `Prelude.seq` Prelude.rnf clientRequestId
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf operationState
      `Prelude.seq` Prelude.rnf errorInfo
      `Prelude.seq` Prelude.rnf operationSteps
