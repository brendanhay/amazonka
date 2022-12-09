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
-- Module      : Amazonka.DrS.Types.RecoveryInstance
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.RecoveryInstance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types.EC2InstanceState
import Amazonka.DrS.Types.OriginEnvironment
import Amazonka.DrS.Types.RecoveryInstanceDataReplicationInfo
import Amazonka.DrS.Types.RecoveryInstanceFailback
import Amazonka.DrS.Types.RecoveryInstanceProperties
import qualified Amazonka.Prelude as Prelude

-- | A Recovery Instance is a replica of a Source Server running on EC2.
--
-- /See:/ 'newRecoveryInstance' smart constructor.
data RecoveryInstance = RecoveryInstance'
  { -- | The ARN of the Recovery Instance.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Data Replication Info of the Recovery Instance.
    dataReplicationInfo :: Prelude.Maybe RecoveryInstanceDataReplicationInfo,
    -- | The EC2 instance ID of the Recovery Instance.
    ec2InstanceID :: Prelude.Maybe Prelude.Text,
    -- | The state of the EC2 instance for this Recovery Instance.
    ec2InstanceState :: Prelude.Maybe EC2InstanceState,
    -- | An object representing failback related information of the Recovery
    -- Instance.
    failback :: Prelude.Maybe RecoveryInstanceFailback,
    -- | Whether this Recovery Instance was created for a drill or for an actual
    -- Recovery event.
    isDrill :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Job that created the Recovery Instance.
    jobID :: Prelude.Maybe Prelude.Text,
    -- | Environment (On Premises \/ AWS) of the instance that the recovery
    -- instance originated from.
    originEnvironment :: Prelude.Maybe OriginEnvironment,
    -- | The date and time of the Point in Time (PIT) snapshot that this Recovery
    -- Instance was launched from.
    pointInTimeSnapshotDateTime :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Recovery Instance.
    recoveryInstanceID :: Prelude.Maybe Prelude.Text,
    -- | Properties of the Recovery Instance machine.
    recoveryInstanceProperties :: Prelude.Maybe RecoveryInstanceProperties,
    -- | The Source Server ID that this Recovery Instance is associated with.
    sourceServerID :: Prelude.Maybe Prelude.Text,
    -- | An array of tags that are associated with the Recovery Instance.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text))
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecoveryInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'recoveryInstance_arn' - The ARN of the Recovery Instance.
--
-- 'dataReplicationInfo', 'recoveryInstance_dataReplicationInfo' - The Data Replication Info of the Recovery Instance.
--
-- 'ec2InstanceID', 'recoveryInstance_ec2InstanceID' - The EC2 instance ID of the Recovery Instance.
--
-- 'ec2InstanceState', 'recoveryInstance_ec2InstanceState' - The state of the EC2 instance for this Recovery Instance.
--
-- 'failback', 'recoveryInstance_failback' - An object representing failback related information of the Recovery
-- Instance.
--
-- 'isDrill', 'recoveryInstance_isDrill' - Whether this Recovery Instance was created for a drill or for an actual
-- Recovery event.
--
-- 'jobID', 'recoveryInstance_jobID' - The ID of the Job that created the Recovery Instance.
--
-- 'originEnvironment', 'recoveryInstance_originEnvironment' - Environment (On Premises \/ AWS) of the instance that the recovery
-- instance originated from.
--
-- 'pointInTimeSnapshotDateTime', 'recoveryInstance_pointInTimeSnapshotDateTime' - The date and time of the Point in Time (PIT) snapshot that this Recovery
-- Instance was launched from.
--
-- 'recoveryInstanceID', 'recoveryInstance_recoveryInstanceID' - The ID of the Recovery Instance.
--
-- 'recoveryInstanceProperties', 'recoveryInstance_recoveryInstanceProperties' - Properties of the Recovery Instance machine.
--
-- 'sourceServerID', 'recoveryInstance_sourceServerID' - The Source Server ID that this Recovery Instance is associated with.
--
-- 'tags', 'recoveryInstance_tags' - An array of tags that are associated with the Recovery Instance.
newRecoveryInstance ::
  RecoveryInstance
newRecoveryInstance =
  RecoveryInstance'
    { arn = Prelude.Nothing,
      dataReplicationInfo = Prelude.Nothing,
      ec2InstanceID = Prelude.Nothing,
      ec2InstanceState = Prelude.Nothing,
      failback = Prelude.Nothing,
      isDrill = Prelude.Nothing,
      jobID = Prelude.Nothing,
      originEnvironment = Prelude.Nothing,
      pointInTimeSnapshotDateTime = Prelude.Nothing,
      recoveryInstanceID = Prelude.Nothing,
      recoveryInstanceProperties = Prelude.Nothing,
      sourceServerID = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The ARN of the Recovery Instance.
recoveryInstance_arn :: Lens.Lens' RecoveryInstance (Prelude.Maybe Prelude.Text)
recoveryInstance_arn = Lens.lens (\RecoveryInstance' {arn} -> arn) (\s@RecoveryInstance' {} a -> s {arn = a} :: RecoveryInstance)

-- | The Data Replication Info of the Recovery Instance.
recoveryInstance_dataReplicationInfo :: Lens.Lens' RecoveryInstance (Prelude.Maybe RecoveryInstanceDataReplicationInfo)
recoveryInstance_dataReplicationInfo = Lens.lens (\RecoveryInstance' {dataReplicationInfo} -> dataReplicationInfo) (\s@RecoveryInstance' {} a -> s {dataReplicationInfo = a} :: RecoveryInstance)

-- | The EC2 instance ID of the Recovery Instance.
recoveryInstance_ec2InstanceID :: Lens.Lens' RecoveryInstance (Prelude.Maybe Prelude.Text)
recoveryInstance_ec2InstanceID = Lens.lens (\RecoveryInstance' {ec2InstanceID} -> ec2InstanceID) (\s@RecoveryInstance' {} a -> s {ec2InstanceID = a} :: RecoveryInstance)

-- | The state of the EC2 instance for this Recovery Instance.
recoveryInstance_ec2InstanceState :: Lens.Lens' RecoveryInstance (Prelude.Maybe EC2InstanceState)
recoveryInstance_ec2InstanceState = Lens.lens (\RecoveryInstance' {ec2InstanceState} -> ec2InstanceState) (\s@RecoveryInstance' {} a -> s {ec2InstanceState = a} :: RecoveryInstance)

-- | An object representing failback related information of the Recovery
-- Instance.
recoveryInstance_failback :: Lens.Lens' RecoveryInstance (Prelude.Maybe RecoveryInstanceFailback)
recoveryInstance_failback = Lens.lens (\RecoveryInstance' {failback} -> failback) (\s@RecoveryInstance' {} a -> s {failback = a} :: RecoveryInstance)

-- | Whether this Recovery Instance was created for a drill or for an actual
-- Recovery event.
recoveryInstance_isDrill :: Lens.Lens' RecoveryInstance (Prelude.Maybe Prelude.Bool)
recoveryInstance_isDrill = Lens.lens (\RecoveryInstance' {isDrill} -> isDrill) (\s@RecoveryInstance' {} a -> s {isDrill = a} :: RecoveryInstance)

-- | The ID of the Job that created the Recovery Instance.
recoveryInstance_jobID :: Lens.Lens' RecoveryInstance (Prelude.Maybe Prelude.Text)
recoveryInstance_jobID = Lens.lens (\RecoveryInstance' {jobID} -> jobID) (\s@RecoveryInstance' {} a -> s {jobID = a} :: RecoveryInstance)

-- | Environment (On Premises \/ AWS) of the instance that the recovery
-- instance originated from.
recoveryInstance_originEnvironment :: Lens.Lens' RecoveryInstance (Prelude.Maybe OriginEnvironment)
recoveryInstance_originEnvironment = Lens.lens (\RecoveryInstance' {originEnvironment} -> originEnvironment) (\s@RecoveryInstance' {} a -> s {originEnvironment = a} :: RecoveryInstance)

-- | The date and time of the Point in Time (PIT) snapshot that this Recovery
-- Instance was launched from.
recoveryInstance_pointInTimeSnapshotDateTime :: Lens.Lens' RecoveryInstance (Prelude.Maybe Prelude.Text)
recoveryInstance_pointInTimeSnapshotDateTime = Lens.lens (\RecoveryInstance' {pointInTimeSnapshotDateTime} -> pointInTimeSnapshotDateTime) (\s@RecoveryInstance' {} a -> s {pointInTimeSnapshotDateTime = a} :: RecoveryInstance)

-- | The ID of the Recovery Instance.
recoveryInstance_recoveryInstanceID :: Lens.Lens' RecoveryInstance (Prelude.Maybe Prelude.Text)
recoveryInstance_recoveryInstanceID = Lens.lens (\RecoveryInstance' {recoveryInstanceID} -> recoveryInstanceID) (\s@RecoveryInstance' {} a -> s {recoveryInstanceID = a} :: RecoveryInstance)

-- | Properties of the Recovery Instance machine.
recoveryInstance_recoveryInstanceProperties :: Lens.Lens' RecoveryInstance (Prelude.Maybe RecoveryInstanceProperties)
recoveryInstance_recoveryInstanceProperties = Lens.lens (\RecoveryInstance' {recoveryInstanceProperties} -> recoveryInstanceProperties) (\s@RecoveryInstance' {} a -> s {recoveryInstanceProperties = a} :: RecoveryInstance)

-- | The Source Server ID that this Recovery Instance is associated with.
recoveryInstance_sourceServerID :: Lens.Lens' RecoveryInstance (Prelude.Maybe Prelude.Text)
recoveryInstance_sourceServerID = Lens.lens (\RecoveryInstance' {sourceServerID} -> sourceServerID) (\s@RecoveryInstance' {} a -> s {sourceServerID = a} :: RecoveryInstance)

-- | An array of tags that are associated with the Recovery Instance.
recoveryInstance_tags :: Lens.Lens' RecoveryInstance (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
recoveryInstance_tags = Lens.lens (\RecoveryInstance' {tags} -> tags) (\s@RecoveryInstance' {} a -> s {tags = a} :: RecoveryInstance) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

instance Data.FromJSON RecoveryInstance where
  parseJSON =
    Data.withObject
      "RecoveryInstance"
      ( \x ->
          RecoveryInstance'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "dataReplicationInfo")
            Prelude.<*> (x Data..:? "ec2InstanceID")
            Prelude.<*> (x Data..:? "ec2InstanceState")
            Prelude.<*> (x Data..:? "failback")
            Prelude.<*> (x Data..:? "isDrill")
            Prelude.<*> (x Data..:? "jobID")
            Prelude.<*> (x Data..:? "originEnvironment")
            Prelude.<*> (x Data..:? "pointInTimeSnapshotDateTime")
            Prelude.<*> (x Data..:? "recoveryInstanceID")
            Prelude.<*> (x Data..:? "recoveryInstanceProperties")
            Prelude.<*> (x Data..:? "sourceServerID")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable RecoveryInstance where
  hashWithSalt _salt RecoveryInstance' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` dataReplicationInfo
      `Prelude.hashWithSalt` ec2InstanceID
      `Prelude.hashWithSalt` ec2InstanceState
      `Prelude.hashWithSalt` failback
      `Prelude.hashWithSalt` isDrill
      `Prelude.hashWithSalt` jobID
      `Prelude.hashWithSalt` originEnvironment
      `Prelude.hashWithSalt` pointInTimeSnapshotDateTime
      `Prelude.hashWithSalt` recoveryInstanceID
      `Prelude.hashWithSalt` recoveryInstanceProperties
      `Prelude.hashWithSalt` sourceServerID
      `Prelude.hashWithSalt` tags

instance Prelude.NFData RecoveryInstance where
  rnf RecoveryInstance' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf dataReplicationInfo
      `Prelude.seq` Prelude.rnf ec2InstanceID
      `Prelude.seq` Prelude.rnf ec2InstanceState
      `Prelude.seq` Prelude.rnf failback
      `Prelude.seq` Prelude.rnf isDrill
      `Prelude.seq` Prelude.rnf jobID
      `Prelude.seq` Prelude.rnf originEnvironment
      `Prelude.seq` Prelude.rnf pointInTimeSnapshotDateTime
      `Prelude.seq` Prelude.rnf recoveryInstanceID
      `Prelude.seq` Prelude.rnf recoveryInstanceProperties
      `Prelude.seq` Prelude.rnf sourceServerID
      `Prelude.seq` Prelude.rnf tags
