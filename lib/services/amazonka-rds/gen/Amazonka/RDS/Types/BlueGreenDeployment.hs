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
-- Module      : Amazonka.RDS.Types.BlueGreenDeployment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.BlueGreenDeployment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types.BlueGreenDeploymentTask
import Amazonka.RDS.Types.SwitchoverDetail
import Amazonka.RDS.Types.Tag

-- | Contains the details about a blue\/green deployment.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/blue-green-deployments.html Using Amazon RDS Blue\/Green Deployments for database updates>
-- in the /Amazon RDS User Guide/ and
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/blue-green-deployments.html Using Amazon RDS Blue\/Green Deployments for database updates>
-- in the /Amazon Aurora User Guide/.
--
-- /See:/ 'newBlueGreenDeployment' smart constructor.
data BlueGreenDeployment = BlueGreenDeployment'
  { -- | The system-generated identifier of the blue\/green deployment.
    blueGreenDeploymentIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The user-supplied name of the blue\/green deployment.
    blueGreenDeploymentName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the time when the blue\/green deployment was created, in
    -- Universal Coordinated Time (UTC).
    createTime :: Prelude.Maybe Data.ISO8601,
    -- | Specifies the time when the blue\/green deployment was deleted, in
    -- Universal Coordinated Time (UTC).
    deleteTime :: Prelude.Maybe Data.ISO8601,
    -- | The source database for the blue\/green deployment.
    --
    -- Before switchover, the source database is the production database in the
    -- blue environment.
    source :: Prelude.Maybe Prelude.Text,
    -- | The status of the blue\/green deployment.
    --
    -- Values:
    --
    -- -   @PROVISIONING@ - Resources are being created in the green
    --     environment.
    --
    -- -   @AVAILABLE@ - Resources are available in the green environment.
    --
    -- -   @SWITCHOVER_IN_PROGRESS@ - The deployment is being switched from the
    --     blue environment to the green environment.
    --
    -- -   @SWITCHOVER_COMPLETED@ - Switchover from the blue environment to the
    --     green environment is complete.
    --
    -- -   @INVALID_CONFIGURATION@ - Resources in the green environment are
    --     invalid, so switchover isn\'t possible.
    --
    -- -   @SWITCHOVER_FAILED@ - Switchover was attempted but failed.
    --
    -- -   @DELETING@ - The blue\/green deployment is being deleted.
    status :: Prelude.Maybe Prelude.Text,
    -- | Additional information about the status of the blue\/green deployment.
    statusDetails :: Prelude.Maybe Prelude.Text,
    -- | The details about each source and target resource in the blue\/green
    -- deployment.
    switchoverDetails :: Prelude.Maybe [SwitchoverDetail],
    tagList :: Prelude.Maybe [Tag],
    -- | The target database for the blue\/green deployment.
    --
    -- Before switchover, the target database is the clone database in the
    -- green environment.
    target :: Prelude.Maybe Prelude.Text,
    -- | Either tasks to be performed or tasks that have been completed on the
    -- target database before switchover.
    tasks :: Prelude.Maybe [BlueGreenDeploymentTask]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BlueGreenDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blueGreenDeploymentIdentifier', 'blueGreenDeployment_blueGreenDeploymentIdentifier' - The system-generated identifier of the blue\/green deployment.
--
-- 'blueGreenDeploymentName', 'blueGreenDeployment_blueGreenDeploymentName' - The user-supplied name of the blue\/green deployment.
--
-- 'createTime', 'blueGreenDeployment_createTime' - Specifies the time when the blue\/green deployment was created, in
-- Universal Coordinated Time (UTC).
--
-- 'deleteTime', 'blueGreenDeployment_deleteTime' - Specifies the time when the blue\/green deployment was deleted, in
-- Universal Coordinated Time (UTC).
--
-- 'source', 'blueGreenDeployment_source' - The source database for the blue\/green deployment.
--
-- Before switchover, the source database is the production database in the
-- blue environment.
--
-- 'status', 'blueGreenDeployment_status' - The status of the blue\/green deployment.
--
-- Values:
--
-- -   @PROVISIONING@ - Resources are being created in the green
--     environment.
--
-- -   @AVAILABLE@ - Resources are available in the green environment.
--
-- -   @SWITCHOVER_IN_PROGRESS@ - The deployment is being switched from the
--     blue environment to the green environment.
--
-- -   @SWITCHOVER_COMPLETED@ - Switchover from the blue environment to the
--     green environment is complete.
--
-- -   @INVALID_CONFIGURATION@ - Resources in the green environment are
--     invalid, so switchover isn\'t possible.
--
-- -   @SWITCHOVER_FAILED@ - Switchover was attempted but failed.
--
-- -   @DELETING@ - The blue\/green deployment is being deleted.
--
-- 'statusDetails', 'blueGreenDeployment_statusDetails' - Additional information about the status of the blue\/green deployment.
--
-- 'switchoverDetails', 'blueGreenDeployment_switchoverDetails' - The details about each source and target resource in the blue\/green
-- deployment.
--
-- 'tagList', 'blueGreenDeployment_tagList' - Undocumented member.
--
-- 'target', 'blueGreenDeployment_target' - The target database for the blue\/green deployment.
--
-- Before switchover, the target database is the clone database in the
-- green environment.
--
-- 'tasks', 'blueGreenDeployment_tasks' - Either tasks to be performed or tasks that have been completed on the
-- target database before switchover.
newBlueGreenDeployment ::
  BlueGreenDeployment
newBlueGreenDeployment =
  BlueGreenDeployment'
    { blueGreenDeploymentIdentifier =
        Prelude.Nothing,
      blueGreenDeploymentName = Prelude.Nothing,
      createTime = Prelude.Nothing,
      deleteTime = Prelude.Nothing,
      source = Prelude.Nothing,
      status = Prelude.Nothing,
      statusDetails = Prelude.Nothing,
      switchoverDetails = Prelude.Nothing,
      tagList = Prelude.Nothing,
      target = Prelude.Nothing,
      tasks = Prelude.Nothing
    }

-- | The system-generated identifier of the blue\/green deployment.
blueGreenDeployment_blueGreenDeploymentIdentifier :: Lens.Lens' BlueGreenDeployment (Prelude.Maybe Prelude.Text)
blueGreenDeployment_blueGreenDeploymentIdentifier = Lens.lens (\BlueGreenDeployment' {blueGreenDeploymentIdentifier} -> blueGreenDeploymentIdentifier) (\s@BlueGreenDeployment' {} a -> s {blueGreenDeploymentIdentifier = a} :: BlueGreenDeployment)

-- | The user-supplied name of the blue\/green deployment.
blueGreenDeployment_blueGreenDeploymentName :: Lens.Lens' BlueGreenDeployment (Prelude.Maybe Prelude.Text)
blueGreenDeployment_blueGreenDeploymentName = Lens.lens (\BlueGreenDeployment' {blueGreenDeploymentName} -> blueGreenDeploymentName) (\s@BlueGreenDeployment' {} a -> s {blueGreenDeploymentName = a} :: BlueGreenDeployment)

-- | Specifies the time when the blue\/green deployment was created, in
-- Universal Coordinated Time (UTC).
blueGreenDeployment_createTime :: Lens.Lens' BlueGreenDeployment (Prelude.Maybe Prelude.UTCTime)
blueGreenDeployment_createTime = Lens.lens (\BlueGreenDeployment' {createTime} -> createTime) (\s@BlueGreenDeployment' {} a -> s {createTime = a} :: BlueGreenDeployment) Prelude.. Lens.mapping Data._Time

-- | Specifies the time when the blue\/green deployment was deleted, in
-- Universal Coordinated Time (UTC).
blueGreenDeployment_deleteTime :: Lens.Lens' BlueGreenDeployment (Prelude.Maybe Prelude.UTCTime)
blueGreenDeployment_deleteTime = Lens.lens (\BlueGreenDeployment' {deleteTime} -> deleteTime) (\s@BlueGreenDeployment' {} a -> s {deleteTime = a} :: BlueGreenDeployment) Prelude.. Lens.mapping Data._Time

-- | The source database for the blue\/green deployment.
--
-- Before switchover, the source database is the production database in the
-- blue environment.
blueGreenDeployment_source :: Lens.Lens' BlueGreenDeployment (Prelude.Maybe Prelude.Text)
blueGreenDeployment_source = Lens.lens (\BlueGreenDeployment' {source} -> source) (\s@BlueGreenDeployment' {} a -> s {source = a} :: BlueGreenDeployment)

-- | The status of the blue\/green deployment.
--
-- Values:
--
-- -   @PROVISIONING@ - Resources are being created in the green
--     environment.
--
-- -   @AVAILABLE@ - Resources are available in the green environment.
--
-- -   @SWITCHOVER_IN_PROGRESS@ - The deployment is being switched from the
--     blue environment to the green environment.
--
-- -   @SWITCHOVER_COMPLETED@ - Switchover from the blue environment to the
--     green environment is complete.
--
-- -   @INVALID_CONFIGURATION@ - Resources in the green environment are
--     invalid, so switchover isn\'t possible.
--
-- -   @SWITCHOVER_FAILED@ - Switchover was attempted but failed.
--
-- -   @DELETING@ - The blue\/green deployment is being deleted.
blueGreenDeployment_status :: Lens.Lens' BlueGreenDeployment (Prelude.Maybe Prelude.Text)
blueGreenDeployment_status = Lens.lens (\BlueGreenDeployment' {status} -> status) (\s@BlueGreenDeployment' {} a -> s {status = a} :: BlueGreenDeployment)

-- | Additional information about the status of the blue\/green deployment.
blueGreenDeployment_statusDetails :: Lens.Lens' BlueGreenDeployment (Prelude.Maybe Prelude.Text)
blueGreenDeployment_statusDetails = Lens.lens (\BlueGreenDeployment' {statusDetails} -> statusDetails) (\s@BlueGreenDeployment' {} a -> s {statusDetails = a} :: BlueGreenDeployment)

-- | The details about each source and target resource in the blue\/green
-- deployment.
blueGreenDeployment_switchoverDetails :: Lens.Lens' BlueGreenDeployment (Prelude.Maybe [SwitchoverDetail])
blueGreenDeployment_switchoverDetails = Lens.lens (\BlueGreenDeployment' {switchoverDetails} -> switchoverDetails) (\s@BlueGreenDeployment' {} a -> s {switchoverDetails = a} :: BlueGreenDeployment) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
blueGreenDeployment_tagList :: Lens.Lens' BlueGreenDeployment (Prelude.Maybe [Tag])
blueGreenDeployment_tagList = Lens.lens (\BlueGreenDeployment' {tagList} -> tagList) (\s@BlueGreenDeployment' {} a -> s {tagList = a} :: BlueGreenDeployment) Prelude.. Lens.mapping Lens.coerced

-- | The target database for the blue\/green deployment.
--
-- Before switchover, the target database is the clone database in the
-- green environment.
blueGreenDeployment_target :: Lens.Lens' BlueGreenDeployment (Prelude.Maybe Prelude.Text)
blueGreenDeployment_target = Lens.lens (\BlueGreenDeployment' {target} -> target) (\s@BlueGreenDeployment' {} a -> s {target = a} :: BlueGreenDeployment)

-- | Either tasks to be performed or tasks that have been completed on the
-- target database before switchover.
blueGreenDeployment_tasks :: Lens.Lens' BlueGreenDeployment (Prelude.Maybe [BlueGreenDeploymentTask])
blueGreenDeployment_tasks = Lens.lens (\BlueGreenDeployment' {tasks} -> tasks) (\s@BlueGreenDeployment' {} a -> s {tasks = a} :: BlueGreenDeployment) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML BlueGreenDeployment where
  parseXML x =
    BlueGreenDeployment'
      Prelude.<$> (x Data..@? "BlueGreenDeploymentIdentifier")
      Prelude.<*> (x Data..@? "BlueGreenDeploymentName")
      Prelude.<*> (x Data..@? "CreateTime")
      Prelude.<*> (x Data..@? "DeleteTime")
      Prelude.<*> (x Data..@? "Source")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> (x Data..@? "StatusDetails")
      Prelude.<*> ( x
                      Data..@? "SwitchoverDetails"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> ( x Data..@? "TagList" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "Tag")
                  )
      Prelude.<*> (x Data..@? "Target")
      Prelude.<*> ( x Data..@? "Tasks" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )

instance Prelude.Hashable BlueGreenDeployment where
  hashWithSalt _salt BlueGreenDeployment' {..} =
    _salt
      `Prelude.hashWithSalt` blueGreenDeploymentIdentifier
      `Prelude.hashWithSalt` blueGreenDeploymentName
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` deleteTime
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusDetails
      `Prelude.hashWithSalt` switchoverDetails
      `Prelude.hashWithSalt` tagList
      `Prelude.hashWithSalt` target
      `Prelude.hashWithSalt` tasks

instance Prelude.NFData BlueGreenDeployment where
  rnf BlueGreenDeployment' {..} =
    Prelude.rnf blueGreenDeploymentIdentifier `Prelude.seq`
      Prelude.rnf blueGreenDeploymentName `Prelude.seq`
        Prelude.rnf createTime `Prelude.seq`
          Prelude.rnf deleteTime `Prelude.seq`
            Prelude.rnf source `Prelude.seq`
              Prelude.rnf status `Prelude.seq`
                Prelude.rnf statusDetails `Prelude.seq`
                  Prelude.rnf switchoverDetails `Prelude.seq`
                    Prelude.rnf tagList `Prelude.seq`
                      Prelude.rnf target `Prelude.seq`
                        Prelude.rnf tasks
