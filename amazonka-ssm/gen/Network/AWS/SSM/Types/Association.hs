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
-- Module      : Network.AWS.SSM.Types.Association
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.Association where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.AssociationOverview
import Network.AWS.SSM.Types.Target

-- | Describes an association of a Systems Manager document and an instance.
--
-- /See:/ 'newAssociation' smart constructor.
data Association = Association'
  { -- | The date on which the association was last run.
    lastExecutionDate :: Core.Maybe Core.POSIX,
    -- | The ID of the instance.
    instanceId :: Core.Maybe Core.Text,
    -- | Information about the association.
    overview :: Core.Maybe AssociationOverview,
    -- | The instances targeted by the request to create an association.
    targets :: Core.Maybe [Target],
    -- | A cron expression that specifies a schedule when the association runs.
    scheduleExpression :: Core.Maybe Core.Text,
    -- | The name of the Systems Manager document.
    name :: Core.Maybe Core.Text,
    -- | The ID created by the system when you create an association. An
    -- association is a binding between a document and a set of targets with a
    -- schedule.
    associationId :: Core.Maybe Core.Text,
    -- | The association name.
    associationName :: Core.Maybe Core.Text,
    -- | The association version.
    associationVersion :: Core.Maybe Core.Text,
    -- | The version of the document used in the association.
    documentVersion :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Association' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastExecutionDate', 'association_lastExecutionDate' - The date on which the association was last run.
--
-- 'instanceId', 'association_instanceId' - The ID of the instance.
--
-- 'overview', 'association_overview' - Information about the association.
--
-- 'targets', 'association_targets' - The instances targeted by the request to create an association.
--
-- 'scheduleExpression', 'association_scheduleExpression' - A cron expression that specifies a schedule when the association runs.
--
-- 'name', 'association_name' - The name of the Systems Manager document.
--
-- 'associationId', 'association_associationId' - The ID created by the system when you create an association. An
-- association is a binding between a document and a set of targets with a
-- schedule.
--
-- 'associationName', 'association_associationName' - The association name.
--
-- 'associationVersion', 'association_associationVersion' - The association version.
--
-- 'documentVersion', 'association_documentVersion' - The version of the document used in the association.
newAssociation ::
  Association
newAssociation =
  Association'
    { lastExecutionDate = Core.Nothing,
      instanceId = Core.Nothing,
      overview = Core.Nothing,
      targets = Core.Nothing,
      scheduleExpression = Core.Nothing,
      name = Core.Nothing,
      associationId = Core.Nothing,
      associationName = Core.Nothing,
      associationVersion = Core.Nothing,
      documentVersion = Core.Nothing
    }

-- | The date on which the association was last run.
association_lastExecutionDate :: Lens.Lens' Association (Core.Maybe Core.UTCTime)
association_lastExecutionDate = Lens.lens (\Association' {lastExecutionDate} -> lastExecutionDate) (\s@Association' {} a -> s {lastExecutionDate = a} :: Association) Core.. Lens.mapping Core._Time

-- | The ID of the instance.
association_instanceId :: Lens.Lens' Association (Core.Maybe Core.Text)
association_instanceId = Lens.lens (\Association' {instanceId} -> instanceId) (\s@Association' {} a -> s {instanceId = a} :: Association)

-- | Information about the association.
association_overview :: Lens.Lens' Association (Core.Maybe AssociationOverview)
association_overview = Lens.lens (\Association' {overview} -> overview) (\s@Association' {} a -> s {overview = a} :: Association)

-- | The instances targeted by the request to create an association.
association_targets :: Lens.Lens' Association (Core.Maybe [Target])
association_targets = Lens.lens (\Association' {targets} -> targets) (\s@Association' {} a -> s {targets = a} :: Association) Core.. Lens.mapping Lens._Coerce

-- | A cron expression that specifies a schedule when the association runs.
association_scheduleExpression :: Lens.Lens' Association (Core.Maybe Core.Text)
association_scheduleExpression = Lens.lens (\Association' {scheduleExpression} -> scheduleExpression) (\s@Association' {} a -> s {scheduleExpression = a} :: Association)

-- | The name of the Systems Manager document.
association_name :: Lens.Lens' Association (Core.Maybe Core.Text)
association_name = Lens.lens (\Association' {name} -> name) (\s@Association' {} a -> s {name = a} :: Association)

-- | The ID created by the system when you create an association. An
-- association is a binding between a document and a set of targets with a
-- schedule.
association_associationId :: Lens.Lens' Association (Core.Maybe Core.Text)
association_associationId = Lens.lens (\Association' {associationId} -> associationId) (\s@Association' {} a -> s {associationId = a} :: Association)

-- | The association name.
association_associationName :: Lens.Lens' Association (Core.Maybe Core.Text)
association_associationName = Lens.lens (\Association' {associationName} -> associationName) (\s@Association' {} a -> s {associationName = a} :: Association)

-- | The association version.
association_associationVersion :: Lens.Lens' Association (Core.Maybe Core.Text)
association_associationVersion = Lens.lens (\Association' {associationVersion} -> associationVersion) (\s@Association' {} a -> s {associationVersion = a} :: Association)

-- | The version of the document used in the association.
association_documentVersion :: Lens.Lens' Association (Core.Maybe Core.Text)
association_documentVersion = Lens.lens (\Association' {documentVersion} -> documentVersion) (\s@Association' {} a -> s {documentVersion = a} :: Association)

instance Core.FromJSON Association where
  parseJSON =
    Core.withObject
      "Association"
      ( \x ->
          Association'
            Core.<$> (x Core..:? "LastExecutionDate")
            Core.<*> (x Core..:? "InstanceId")
            Core.<*> (x Core..:? "Overview")
            Core.<*> (x Core..:? "Targets" Core..!= Core.mempty)
            Core.<*> (x Core..:? "ScheduleExpression")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "AssociationId")
            Core.<*> (x Core..:? "AssociationName")
            Core.<*> (x Core..:? "AssociationVersion")
            Core.<*> (x Core..:? "DocumentVersion")
      )

instance Core.Hashable Association

instance Core.NFData Association
