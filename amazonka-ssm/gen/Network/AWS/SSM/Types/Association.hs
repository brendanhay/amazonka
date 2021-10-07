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
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.AssociationOverview
import Network.AWS.SSM.Types.Target

-- | Describes an association of a Amazon Web Services Systems Manager
-- document (SSM document) and an instance.
--
-- /See:/ 'newAssociation' smart constructor.
data Association = Association'
  { -- | The instance ID.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The date on which the association was last run.
    lastExecutionDate :: Prelude.Maybe Core.POSIX,
    -- | Information about the association.
    overview :: Prelude.Maybe AssociationOverview,
    -- | The instances targeted by the request to create an association.
    targets :: Prelude.Maybe [Target],
    -- | The name of the SSM document.
    name :: Prelude.Maybe Prelude.Text,
    -- | A cron expression that specifies a schedule when the association runs.
    -- The schedule runs in Coordinated Universal Time (UTC).
    scheduleExpression :: Prelude.Maybe Prelude.Text,
    -- | The ID created by the system when you create an association. An
    -- association is a binding between a document and a set of targets with a
    -- schedule.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The association name.
    associationName :: Prelude.Maybe Prelude.Text,
    -- | The association version.
    associationVersion :: Prelude.Maybe Prelude.Text,
    -- | The version of the document used in the association.
    documentVersion :: Prelude.Maybe Prelude.Text
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
-- 'instanceId', 'association_instanceId' - The instance ID.
--
-- 'lastExecutionDate', 'association_lastExecutionDate' - The date on which the association was last run.
--
-- 'overview', 'association_overview' - Information about the association.
--
-- 'targets', 'association_targets' - The instances targeted by the request to create an association.
--
-- 'name', 'association_name' - The name of the SSM document.
--
-- 'scheduleExpression', 'association_scheduleExpression' - A cron expression that specifies a schedule when the association runs.
-- The schedule runs in Coordinated Universal Time (UTC).
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
    { instanceId = Prelude.Nothing,
      lastExecutionDate = Prelude.Nothing,
      overview = Prelude.Nothing,
      targets = Prelude.Nothing,
      name = Prelude.Nothing,
      scheduleExpression = Prelude.Nothing,
      associationId = Prelude.Nothing,
      associationName = Prelude.Nothing,
      associationVersion = Prelude.Nothing,
      documentVersion = Prelude.Nothing
    }

-- | The instance ID.
association_instanceId :: Lens.Lens' Association (Prelude.Maybe Prelude.Text)
association_instanceId = Lens.lens (\Association' {instanceId} -> instanceId) (\s@Association' {} a -> s {instanceId = a} :: Association)

-- | The date on which the association was last run.
association_lastExecutionDate :: Lens.Lens' Association (Prelude.Maybe Prelude.UTCTime)
association_lastExecutionDate = Lens.lens (\Association' {lastExecutionDate} -> lastExecutionDate) (\s@Association' {} a -> s {lastExecutionDate = a} :: Association) Prelude.. Lens.mapping Core._Time

-- | Information about the association.
association_overview :: Lens.Lens' Association (Prelude.Maybe AssociationOverview)
association_overview = Lens.lens (\Association' {overview} -> overview) (\s@Association' {} a -> s {overview = a} :: Association)

-- | The instances targeted by the request to create an association.
association_targets :: Lens.Lens' Association (Prelude.Maybe [Target])
association_targets = Lens.lens (\Association' {targets} -> targets) (\s@Association' {} a -> s {targets = a} :: Association) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the SSM document.
association_name :: Lens.Lens' Association (Prelude.Maybe Prelude.Text)
association_name = Lens.lens (\Association' {name} -> name) (\s@Association' {} a -> s {name = a} :: Association)

-- | A cron expression that specifies a schedule when the association runs.
-- The schedule runs in Coordinated Universal Time (UTC).
association_scheduleExpression :: Lens.Lens' Association (Prelude.Maybe Prelude.Text)
association_scheduleExpression = Lens.lens (\Association' {scheduleExpression} -> scheduleExpression) (\s@Association' {} a -> s {scheduleExpression = a} :: Association)

-- | The ID created by the system when you create an association. An
-- association is a binding between a document and a set of targets with a
-- schedule.
association_associationId :: Lens.Lens' Association (Prelude.Maybe Prelude.Text)
association_associationId = Lens.lens (\Association' {associationId} -> associationId) (\s@Association' {} a -> s {associationId = a} :: Association)

-- | The association name.
association_associationName :: Lens.Lens' Association (Prelude.Maybe Prelude.Text)
association_associationName = Lens.lens (\Association' {associationName} -> associationName) (\s@Association' {} a -> s {associationName = a} :: Association)

-- | The association version.
association_associationVersion :: Lens.Lens' Association (Prelude.Maybe Prelude.Text)
association_associationVersion = Lens.lens (\Association' {associationVersion} -> associationVersion) (\s@Association' {} a -> s {associationVersion = a} :: Association)

-- | The version of the document used in the association.
association_documentVersion :: Lens.Lens' Association (Prelude.Maybe Prelude.Text)
association_documentVersion = Lens.lens (\Association' {documentVersion} -> documentVersion) (\s@Association' {} a -> s {documentVersion = a} :: Association)

instance Core.FromJSON Association where
  parseJSON =
    Core.withObject
      "Association"
      ( \x ->
          Association'
            Prelude.<$> (x Core..:? "InstanceId")
            Prelude.<*> (x Core..:? "LastExecutionDate")
            Prelude.<*> (x Core..:? "Overview")
            Prelude.<*> (x Core..:? "Targets" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "ScheduleExpression")
            Prelude.<*> (x Core..:? "AssociationId")
            Prelude.<*> (x Core..:? "AssociationName")
            Prelude.<*> (x Core..:? "AssociationVersion")
            Prelude.<*> (x Core..:? "DocumentVersion")
      )

instance Prelude.Hashable Association

instance Prelude.NFData Association
