{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateSnapshotSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a snapshot schedule that can be associated to a cluster and which
-- overrides the default system backup schedule.
module Network.AWS.Redshift.CreateSnapshotSchedule
  ( -- * Creating a Request
    CreateSnapshotSchedule (..),
    newCreateSnapshotSchedule,

    -- * Request Lenses
    createSnapshotSchedule_nextInvocations,
    createSnapshotSchedule_dryRun,
    createSnapshotSchedule_scheduleIdentifier,
    createSnapshotSchedule_scheduleDescription,
    createSnapshotSchedule_scheduleDefinitions,
    createSnapshotSchedule_tags,

    -- * Destructuring the Response
    SnapshotSchedule (..),
    newSnapshotSchedule,

    -- * Response Lenses
    snapshotSchedule_nextInvocations,
    snapshotSchedule_associatedClusters,
    snapshotSchedule_scheduleIdentifier,
    snapshotSchedule_scheduleDescription,
    snapshotSchedule_scheduleDefinitions,
    snapshotSchedule_associatedClusterCount,
    snapshotSchedule_tags,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateSnapshotSchedule' smart constructor.
data CreateSnapshotSchedule = CreateSnapshotSchedule'
  { nextInvocations :: Core.Maybe Core.Int,
    dryRun :: Core.Maybe Core.Bool,
    -- | A unique identifier for a snapshot schedule. Only alphanumeric
    -- characters are allowed for the identifier.
    scheduleIdentifier :: Core.Maybe Core.Text,
    -- | The description of the snapshot schedule.
    scheduleDescription :: Core.Maybe Core.Text,
    -- | The definition of the snapshot schedule. The definition is made up of
    -- schedule expressions, for example \"cron(30 12 *)\" or \"rate(12
    -- hours)\".
    scheduleDefinitions :: Core.Maybe [Core.Text],
    -- | An optional set of tags you can use to search for the schedule.
    tags :: Core.Maybe [Tag]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateSnapshotSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextInvocations', 'createSnapshotSchedule_nextInvocations' -
--
-- 'dryRun', 'createSnapshotSchedule_dryRun' -
--
-- 'scheduleIdentifier', 'createSnapshotSchedule_scheduleIdentifier' - A unique identifier for a snapshot schedule. Only alphanumeric
-- characters are allowed for the identifier.
--
-- 'scheduleDescription', 'createSnapshotSchedule_scheduleDescription' - The description of the snapshot schedule.
--
-- 'scheduleDefinitions', 'createSnapshotSchedule_scheduleDefinitions' - The definition of the snapshot schedule. The definition is made up of
-- schedule expressions, for example \"cron(30 12 *)\" or \"rate(12
-- hours)\".
--
-- 'tags', 'createSnapshotSchedule_tags' - An optional set of tags you can use to search for the schedule.
newCreateSnapshotSchedule ::
  CreateSnapshotSchedule
newCreateSnapshotSchedule =
  CreateSnapshotSchedule'
    { nextInvocations =
        Core.Nothing,
      dryRun = Core.Nothing,
      scheduleIdentifier = Core.Nothing,
      scheduleDescription = Core.Nothing,
      scheduleDefinitions = Core.Nothing,
      tags = Core.Nothing
    }

-- |
createSnapshotSchedule_nextInvocations :: Lens.Lens' CreateSnapshotSchedule (Core.Maybe Core.Int)
createSnapshotSchedule_nextInvocations = Lens.lens (\CreateSnapshotSchedule' {nextInvocations} -> nextInvocations) (\s@CreateSnapshotSchedule' {} a -> s {nextInvocations = a} :: CreateSnapshotSchedule)

-- |
createSnapshotSchedule_dryRun :: Lens.Lens' CreateSnapshotSchedule (Core.Maybe Core.Bool)
createSnapshotSchedule_dryRun = Lens.lens (\CreateSnapshotSchedule' {dryRun} -> dryRun) (\s@CreateSnapshotSchedule' {} a -> s {dryRun = a} :: CreateSnapshotSchedule)

-- | A unique identifier for a snapshot schedule. Only alphanumeric
-- characters are allowed for the identifier.
createSnapshotSchedule_scheduleIdentifier :: Lens.Lens' CreateSnapshotSchedule (Core.Maybe Core.Text)
createSnapshotSchedule_scheduleIdentifier = Lens.lens (\CreateSnapshotSchedule' {scheduleIdentifier} -> scheduleIdentifier) (\s@CreateSnapshotSchedule' {} a -> s {scheduleIdentifier = a} :: CreateSnapshotSchedule)

-- | The description of the snapshot schedule.
createSnapshotSchedule_scheduleDescription :: Lens.Lens' CreateSnapshotSchedule (Core.Maybe Core.Text)
createSnapshotSchedule_scheduleDescription = Lens.lens (\CreateSnapshotSchedule' {scheduleDescription} -> scheduleDescription) (\s@CreateSnapshotSchedule' {} a -> s {scheduleDescription = a} :: CreateSnapshotSchedule)

-- | The definition of the snapshot schedule. The definition is made up of
-- schedule expressions, for example \"cron(30 12 *)\" or \"rate(12
-- hours)\".
createSnapshotSchedule_scheduleDefinitions :: Lens.Lens' CreateSnapshotSchedule (Core.Maybe [Core.Text])
createSnapshotSchedule_scheduleDefinitions = Lens.lens (\CreateSnapshotSchedule' {scheduleDefinitions} -> scheduleDefinitions) (\s@CreateSnapshotSchedule' {} a -> s {scheduleDefinitions = a} :: CreateSnapshotSchedule) Core.. Lens.mapping Lens._Coerce

-- | An optional set of tags you can use to search for the schedule.
createSnapshotSchedule_tags :: Lens.Lens' CreateSnapshotSchedule (Core.Maybe [Tag])
createSnapshotSchedule_tags = Lens.lens (\CreateSnapshotSchedule' {tags} -> tags) (\s@CreateSnapshotSchedule' {} a -> s {tags = a} :: CreateSnapshotSchedule) Core.. Lens.mapping Lens._Coerce

instance Core.AWSRequest CreateSnapshotSchedule where
  type
    AWSResponse CreateSnapshotSchedule =
      SnapshotSchedule
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateSnapshotScheduleResult"
      (\s h x -> Core.parseXML x)

instance Core.Hashable CreateSnapshotSchedule

instance Core.NFData CreateSnapshotSchedule

instance Core.ToHeaders CreateSnapshotSchedule where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateSnapshotSchedule where
  toPath = Core.const "/"

instance Core.ToQuery CreateSnapshotSchedule where
  toQuery CreateSnapshotSchedule' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreateSnapshotSchedule" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "NextInvocations" Core.=: nextInvocations,
        "DryRun" Core.=: dryRun,
        "ScheduleIdentifier" Core.=: scheduleIdentifier,
        "ScheduleDescription" Core.=: scheduleDescription,
        "ScheduleDefinitions"
          Core.=: Core.toQuery
            ( Core.toQueryList "ScheduleDefinition"
                Core.<$> scheduleDefinitions
            ),
        "Tags"
          Core.=: Core.toQuery (Core.toQueryList "Tag" Core.<$> tags)
      ]
