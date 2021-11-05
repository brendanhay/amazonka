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
-- Module      : Amazonka.SSM.Types.Association
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.Association where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.AssociationOverview
import Amazonka.SSM.Types.Target

-- | Describes an association of a Amazon Web Services Systems Manager
-- document (SSM document) and an instance.
--
-- /See:/ 'newAssociation' smart constructor.
data Association = Association'
  { -- | The ID created by the system when you create an association. An
    -- association is a binding between a document and a set of targets with a
    -- schedule.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The instance ID.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | Information about the association.
    overview :: Prelude.Maybe AssociationOverview,
    -- | The date on which the association was last run.
    lastExecutionDate :: Prelude.Maybe Core.POSIX,
    -- | A cron expression that specifies a schedule when the association runs.
    -- The schedule runs in Coordinated Universal Time (UTC).
    scheduleExpression :: Prelude.Maybe Prelude.Text,
    -- | The name of the SSM document.
    name :: Prelude.Maybe Prelude.Text,
    -- | The instances targeted by the request to create an association. You can
    -- target all instances in an Amazon Web Services account by specifying the
    -- @InstanceIds@ key with a value of @*@.
    targets :: Prelude.Maybe [Target],
    -- | The version of the document used in the association.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | The association version.
    associationVersion :: Prelude.Maybe Prelude.Text,
    -- | The association name.
    associationName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Association' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationId', 'association_associationId' - The ID created by the system when you create an association. An
-- association is a binding between a document and a set of targets with a
-- schedule.
--
-- 'instanceId', 'association_instanceId' - The instance ID.
--
-- 'overview', 'association_overview' - Information about the association.
--
-- 'lastExecutionDate', 'association_lastExecutionDate' - The date on which the association was last run.
--
-- 'scheduleExpression', 'association_scheduleExpression' - A cron expression that specifies a schedule when the association runs.
-- The schedule runs in Coordinated Universal Time (UTC).
--
-- 'name', 'association_name' - The name of the SSM document.
--
-- 'targets', 'association_targets' - The instances targeted by the request to create an association. You can
-- target all instances in an Amazon Web Services account by specifying the
-- @InstanceIds@ key with a value of @*@.
--
-- 'documentVersion', 'association_documentVersion' - The version of the document used in the association.
--
-- 'associationVersion', 'association_associationVersion' - The association version.
--
-- 'associationName', 'association_associationName' - The association name.
newAssociation ::
  Association
newAssociation =
  Association'
    { associationId = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      overview = Prelude.Nothing,
      lastExecutionDate = Prelude.Nothing,
      scheduleExpression = Prelude.Nothing,
      name = Prelude.Nothing,
      targets = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      associationVersion = Prelude.Nothing,
      associationName = Prelude.Nothing
    }

-- | The ID created by the system when you create an association. An
-- association is a binding between a document and a set of targets with a
-- schedule.
association_associationId :: Lens.Lens' Association (Prelude.Maybe Prelude.Text)
association_associationId = Lens.lens (\Association' {associationId} -> associationId) (\s@Association' {} a -> s {associationId = a} :: Association)

-- | The instance ID.
association_instanceId :: Lens.Lens' Association (Prelude.Maybe Prelude.Text)
association_instanceId = Lens.lens (\Association' {instanceId} -> instanceId) (\s@Association' {} a -> s {instanceId = a} :: Association)

-- | Information about the association.
association_overview :: Lens.Lens' Association (Prelude.Maybe AssociationOverview)
association_overview = Lens.lens (\Association' {overview} -> overview) (\s@Association' {} a -> s {overview = a} :: Association)

-- | The date on which the association was last run.
association_lastExecutionDate :: Lens.Lens' Association (Prelude.Maybe Prelude.UTCTime)
association_lastExecutionDate = Lens.lens (\Association' {lastExecutionDate} -> lastExecutionDate) (\s@Association' {} a -> s {lastExecutionDate = a} :: Association) Prelude.. Lens.mapping Core._Time

-- | A cron expression that specifies a schedule when the association runs.
-- The schedule runs in Coordinated Universal Time (UTC).
association_scheduleExpression :: Lens.Lens' Association (Prelude.Maybe Prelude.Text)
association_scheduleExpression = Lens.lens (\Association' {scheduleExpression} -> scheduleExpression) (\s@Association' {} a -> s {scheduleExpression = a} :: Association)

-- | The name of the SSM document.
association_name :: Lens.Lens' Association (Prelude.Maybe Prelude.Text)
association_name = Lens.lens (\Association' {name} -> name) (\s@Association' {} a -> s {name = a} :: Association)

-- | The instances targeted by the request to create an association. You can
-- target all instances in an Amazon Web Services account by specifying the
-- @InstanceIds@ key with a value of @*@.
association_targets :: Lens.Lens' Association (Prelude.Maybe [Target])
association_targets = Lens.lens (\Association' {targets} -> targets) (\s@Association' {} a -> s {targets = a} :: Association) Prelude.. Lens.mapping Lens.coerced

-- | The version of the document used in the association.
association_documentVersion :: Lens.Lens' Association (Prelude.Maybe Prelude.Text)
association_documentVersion = Lens.lens (\Association' {documentVersion} -> documentVersion) (\s@Association' {} a -> s {documentVersion = a} :: Association)

-- | The association version.
association_associationVersion :: Lens.Lens' Association (Prelude.Maybe Prelude.Text)
association_associationVersion = Lens.lens (\Association' {associationVersion} -> associationVersion) (\s@Association' {} a -> s {associationVersion = a} :: Association)

-- | The association name.
association_associationName :: Lens.Lens' Association (Prelude.Maybe Prelude.Text)
association_associationName = Lens.lens (\Association' {associationName} -> associationName) (\s@Association' {} a -> s {associationName = a} :: Association)

instance Core.FromJSON Association where
  parseJSON =
    Core.withObject
      "Association"
      ( \x ->
          Association'
            Prelude.<$> (x Core..:? "AssociationId")
            Prelude.<*> (x Core..:? "InstanceId")
            Prelude.<*> (x Core..:? "Overview")
            Prelude.<*> (x Core..:? "LastExecutionDate")
            Prelude.<*> (x Core..:? "ScheduleExpression")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Targets" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "DocumentVersion")
            Prelude.<*> (x Core..:? "AssociationVersion")
            Prelude.<*> (x Core..:? "AssociationName")
      )

instance Prelude.Hashable Association

instance Prelude.NFData Association
