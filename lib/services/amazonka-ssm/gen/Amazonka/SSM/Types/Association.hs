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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.Association where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.AssociationOverview
import Amazonka.SSM.Types.Target

-- | Describes an association of a Amazon Web Services Systems Manager
-- document (SSM document) and a managed node.
--
-- /See:/ 'newAssociation' smart constructor.
data Association = Association'
  { -- | The ID created by the system when you create an association. An
    -- association is a binding between a document and a set of targets with a
    -- schedule.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The association name.
    associationName :: Prelude.Maybe Prelude.Text,
    -- | The association version.
    associationVersion :: Prelude.Maybe Prelude.Text,
    -- | The version of the document used in the association. If you change a
    -- document version for a State Manager association, Systems Manager
    -- immediately runs the association unless you previously specifed the
    -- @apply-only-at-cron-interval@ parameter.
    --
    -- State Manager doesn\'t support running associations that use a new
    -- version of a document if that document is shared from another account.
    -- State Manager always runs the @default@ version of a document if shared
    -- from another account, even though the Systems Manager console shows that
    -- a new version was processed. If you want to run an association using a
    -- new version of a document shared form another account, you must set the
    -- document version to @default@.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | The managed node ID.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The date on which the association was last run.
    lastExecutionDate :: Prelude.Maybe Data.POSIX,
    -- | The name of the SSM document.
    name :: Prelude.Maybe Prelude.Text,
    -- | Information about the association.
    overview :: Prelude.Maybe AssociationOverview,
    -- | A cron expression that specifies a schedule when the association runs.
    -- The schedule runs in Coordinated Universal Time (UTC).
    scheduleExpression :: Prelude.Maybe Prelude.Text,
    -- | Number of days to wait after the scheduled day to run an association.
    scheduleOffset :: Prelude.Maybe Prelude.Natural,
    -- | A key-value mapping of document parameters to target resources. Both
    -- Targets and TargetMaps can\'t be specified together.
    targetMaps :: Prelude.Maybe [Prelude.HashMap Prelude.Text [Prelude.Text]],
    -- | The managed nodes targeted by the request to create an association. You
    -- can target all managed nodes in an Amazon Web Services account by
    -- specifying the @InstanceIds@ key with a value of @*@.
    targets :: Prelude.Maybe [Target]
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
-- 'associationName', 'association_associationName' - The association name.
--
-- 'associationVersion', 'association_associationVersion' - The association version.
--
-- 'documentVersion', 'association_documentVersion' - The version of the document used in the association. If you change a
-- document version for a State Manager association, Systems Manager
-- immediately runs the association unless you previously specifed the
-- @apply-only-at-cron-interval@ parameter.
--
-- State Manager doesn\'t support running associations that use a new
-- version of a document if that document is shared from another account.
-- State Manager always runs the @default@ version of a document if shared
-- from another account, even though the Systems Manager console shows that
-- a new version was processed. If you want to run an association using a
-- new version of a document shared form another account, you must set the
-- document version to @default@.
--
-- 'instanceId', 'association_instanceId' - The managed node ID.
--
-- 'lastExecutionDate', 'association_lastExecutionDate' - The date on which the association was last run.
--
-- 'name', 'association_name' - The name of the SSM document.
--
-- 'overview', 'association_overview' - Information about the association.
--
-- 'scheduleExpression', 'association_scheduleExpression' - A cron expression that specifies a schedule when the association runs.
-- The schedule runs in Coordinated Universal Time (UTC).
--
-- 'scheduleOffset', 'association_scheduleOffset' - Number of days to wait after the scheduled day to run an association.
--
-- 'targetMaps', 'association_targetMaps' - A key-value mapping of document parameters to target resources. Both
-- Targets and TargetMaps can\'t be specified together.
--
-- 'targets', 'association_targets' - The managed nodes targeted by the request to create an association. You
-- can target all managed nodes in an Amazon Web Services account by
-- specifying the @InstanceIds@ key with a value of @*@.
newAssociation ::
  Association
newAssociation =
  Association'
    { associationId = Prelude.Nothing,
      associationName = Prelude.Nothing,
      associationVersion = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      lastExecutionDate = Prelude.Nothing,
      name = Prelude.Nothing,
      overview = Prelude.Nothing,
      scheduleExpression = Prelude.Nothing,
      scheduleOffset = Prelude.Nothing,
      targetMaps = Prelude.Nothing,
      targets = Prelude.Nothing
    }

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

-- | The version of the document used in the association. If you change a
-- document version for a State Manager association, Systems Manager
-- immediately runs the association unless you previously specifed the
-- @apply-only-at-cron-interval@ parameter.
--
-- State Manager doesn\'t support running associations that use a new
-- version of a document if that document is shared from another account.
-- State Manager always runs the @default@ version of a document if shared
-- from another account, even though the Systems Manager console shows that
-- a new version was processed. If you want to run an association using a
-- new version of a document shared form another account, you must set the
-- document version to @default@.
association_documentVersion :: Lens.Lens' Association (Prelude.Maybe Prelude.Text)
association_documentVersion = Lens.lens (\Association' {documentVersion} -> documentVersion) (\s@Association' {} a -> s {documentVersion = a} :: Association)

-- | The managed node ID.
association_instanceId :: Lens.Lens' Association (Prelude.Maybe Prelude.Text)
association_instanceId = Lens.lens (\Association' {instanceId} -> instanceId) (\s@Association' {} a -> s {instanceId = a} :: Association)

-- | The date on which the association was last run.
association_lastExecutionDate :: Lens.Lens' Association (Prelude.Maybe Prelude.UTCTime)
association_lastExecutionDate = Lens.lens (\Association' {lastExecutionDate} -> lastExecutionDate) (\s@Association' {} a -> s {lastExecutionDate = a} :: Association) Prelude.. Lens.mapping Data._Time

-- | The name of the SSM document.
association_name :: Lens.Lens' Association (Prelude.Maybe Prelude.Text)
association_name = Lens.lens (\Association' {name} -> name) (\s@Association' {} a -> s {name = a} :: Association)

-- | Information about the association.
association_overview :: Lens.Lens' Association (Prelude.Maybe AssociationOverview)
association_overview = Lens.lens (\Association' {overview} -> overview) (\s@Association' {} a -> s {overview = a} :: Association)

-- | A cron expression that specifies a schedule when the association runs.
-- The schedule runs in Coordinated Universal Time (UTC).
association_scheduleExpression :: Lens.Lens' Association (Prelude.Maybe Prelude.Text)
association_scheduleExpression = Lens.lens (\Association' {scheduleExpression} -> scheduleExpression) (\s@Association' {} a -> s {scheduleExpression = a} :: Association)

-- | Number of days to wait after the scheduled day to run an association.
association_scheduleOffset :: Lens.Lens' Association (Prelude.Maybe Prelude.Natural)
association_scheduleOffset = Lens.lens (\Association' {scheduleOffset} -> scheduleOffset) (\s@Association' {} a -> s {scheduleOffset = a} :: Association)

-- | A key-value mapping of document parameters to target resources. Both
-- Targets and TargetMaps can\'t be specified together.
association_targetMaps :: Lens.Lens' Association (Prelude.Maybe [Prelude.HashMap Prelude.Text [Prelude.Text]])
association_targetMaps = Lens.lens (\Association' {targetMaps} -> targetMaps) (\s@Association' {} a -> s {targetMaps = a} :: Association) Prelude.. Lens.mapping Lens.coerced

-- | The managed nodes targeted by the request to create an association. You
-- can target all managed nodes in an Amazon Web Services account by
-- specifying the @InstanceIds@ key with a value of @*@.
association_targets :: Lens.Lens' Association (Prelude.Maybe [Target])
association_targets = Lens.lens (\Association' {targets} -> targets) (\s@Association' {} a -> s {targets = a} :: Association) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Association where
  parseJSON =
    Data.withObject
      "Association"
      ( \x ->
          Association'
            Prelude.<$> (x Data..:? "AssociationId")
            Prelude.<*> (x Data..:? "AssociationName")
            Prelude.<*> (x Data..:? "AssociationVersion")
            Prelude.<*> (x Data..:? "DocumentVersion")
            Prelude.<*> (x Data..:? "InstanceId")
            Prelude.<*> (x Data..:? "LastExecutionDate")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Overview")
            Prelude.<*> (x Data..:? "ScheduleExpression")
            Prelude.<*> (x Data..:? "ScheduleOffset")
            Prelude.<*> (x Data..:? "TargetMaps" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Targets" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Association where
  hashWithSalt _salt Association' {..} =
    _salt `Prelude.hashWithSalt` associationId
      `Prelude.hashWithSalt` associationName
      `Prelude.hashWithSalt` associationVersion
      `Prelude.hashWithSalt` documentVersion
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` lastExecutionDate
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` overview
      `Prelude.hashWithSalt` scheduleExpression
      `Prelude.hashWithSalt` scheduleOffset
      `Prelude.hashWithSalt` targetMaps
      `Prelude.hashWithSalt` targets

instance Prelude.NFData Association where
  rnf Association' {..} =
    Prelude.rnf associationId
      `Prelude.seq` Prelude.rnf associationName
      `Prelude.seq` Prelude.rnf associationVersion
      `Prelude.seq` Prelude.rnf documentVersion
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf lastExecutionDate
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf overview
      `Prelude.seq` Prelude.rnf scheduleExpression
      `Prelude.seq` Prelude.rnf scheduleOffset
      `Prelude.seq` Prelude.rnf targetMaps
      `Prelude.seq` Prelude.rnf targets
