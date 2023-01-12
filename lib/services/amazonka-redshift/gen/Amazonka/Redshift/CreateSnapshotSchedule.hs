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
-- Module      : Amazonka.Redshift.CreateSnapshotSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a snapshot schedule that can be associated to a cluster and which
-- overrides the default system backup schedule.
module Amazonka.Redshift.CreateSnapshotSchedule
  ( -- * Creating a Request
    CreateSnapshotSchedule (..),
    newCreateSnapshotSchedule,

    -- * Request Lenses
    createSnapshotSchedule_dryRun,
    createSnapshotSchedule_nextInvocations,
    createSnapshotSchedule_scheduleDefinitions,
    createSnapshotSchedule_scheduleDescription,
    createSnapshotSchedule_scheduleIdentifier,
    createSnapshotSchedule_tags,

    -- * Destructuring the Response
    SnapshotSchedule (..),
    newSnapshotSchedule,

    -- * Response Lenses
    snapshotSchedule_associatedClusterCount,
    snapshotSchedule_associatedClusters,
    snapshotSchedule_nextInvocations,
    snapshotSchedule_scheduleDefinitions,
    snapshotSchedule_scheduleDescription,
    snapshotSchedule_scheduleIdentifier,
    snapshotSchedule_tags,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSnapshotSchedule' smart constructor.
data CreateSnapshotSchedule = CreateSnapshotSchedule'
  { dryRun :: Prelude.Maybe Prelude.Bool,
    nextInvocations :: Prelude.Maybe Prelude.Int,
    -- | The definition of the snapshot schedule. The definition is made up of
    -- schedule expressions, for example \"cron(30 12 *)\" or \"rate(12
    -- hours)\".
    scheduleDefinitions :: Prelude.Maybe [Prelude.Text],
    -- | The description of the snapshot schedule.
    scheduleDescription :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a snapshot schedule. Only alphanumeric
    -- characters are allowed for the identifier.
    scheduleIdentifier :: Prelude.Maybe Prelude.Text,
    -- | An optional set of tags you can use to search for the schedule.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSnapshotSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'createSnapshotSchedule_dryRun' -
--
-- 'nextInvocations', 'createSnapshotSchedule_nextInvocations' -
--
-- 'scheduleDefinitions', 'createSnapshotSchedule_scheduleDefinitions' - The definition of the snapshot schedule. The definition is made up of
-- schedule expressions, for example \"cron(30 12 *)\" or \"rate(12
-- hours)\".
--
-- 'scheduleDescription', 'createSnapshotSchedule_scheduleDescription' - The description of the snapshot schedule.
--
-- 'scheduleIdentifier', 'createSnapshotSchedule_scheduleIdentifier' - A unique identifier for a snapshot schedule. Only alphanumeric
-- characters are allowed for the identifier.
--
-- 'tags', 'createSnapshotSchedule_tags' - An optional set of tags you can use to search for the schedule.
newCreateSnapshotSchedule ::
  CreateSnapshotSchedule
newCreateSnapshotSchedule =
  CreateSnapshotSchedule'
    { dryRun = Prelude.Nothing,
      nextInvocations = Prelude.Nothing,
      scheduleDefinitions = Prelude.Nothing,
      scheduleDescription = Prelude.Nothing,
      scheduleIdentifier = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- |
createSnapshotSchedule_dryRun :: Lens.Lens' CreateSnapshotSchedule (Prelude.Maybe Prelude.Bool)
createSnapshotSchedule_dryRun = Lens.lens (\CreateSnapshotSchedule' {dryRun} -> dryRun) (\s@CreateSnapshotSchedule' {} a -> s {dryRun = a} :: CreateSnapshotSchedule)

-- |
createSnapshotSchedule_nextInvocations :: Lens.Lens' CreateSnapshotSchedule (Prelude.Maybe Prelude.Int)
createSnapshotSchedule_nextInvocations = Lens.lens (\CreateSnapshotSchedule' {nextInvocations} -> nextInvocations) (\s@CreateSnapshotSchedule' {} a -> s {nextInvocations = a} :: CreateSnapshotSchedule)

-- | The definition of the snapshot schedule. The definition is made up of
-- schedule expressions, for example \"cron(30 12 *)\" or \"rate(12
-- hours)\".
createSnapshotSchedule_scheduleDefinitions :: Lens.Lens' CreateSnapshotSchedule (Prelude.Maybe [Prelude.Text])
createSnapshotSchedule_scheduleDefinitions = Lens.lens (\CreateSnapshotSchedule' {scheduleDefinitions} -> scheduleDefinitions) (\s@CreateSnapshotSchedule' {} a -> s {scheduleDefinitions = a} :: CreateSnapshotSchedule) Prelude.. Lens.mapping Lens.coerced

-- | The description of the snapshot schedule.
createSnapshotSchedule_scheduleDescription :: Lens.Lens' CreateSnapshotSchedule (Prelude.Maybe Prelude.Text)
createSnapshotSchedule_scheduleDescription = Lens.lens (\CreateSnapshotSchedule' {scheduleDescription} -> scheduleDescription) (\s@CreateSnapshotSchedule' {} a -> s {scheduleDescription = a} :: CreateSnapshotSchedule)

-- | A unique identifier for a snapshot schedule. Only alphanumeric
-- characters are allowed for the identifier.
createSnapshotSchedule_scheduleIdentifier :: Lens.Lens' CreateSnapshotSchedule (Prelude.Maybe Prelude.Text)
createSnapshotSchedule_scheduleIdentifier = Lens.lens (\CreateSnapshotSchedule' {scheduleIdentifier} -> scheduleIdentifier) (\s@CreateSnapshotSchedule' {} a -> s {scheduleIdentifier = a} :: CreateSnapshotSchedule)

-- | An optional set of tags you can use to search for the schedule.
createSnapshotSchedule_tags :: Lens.Lens' CreateSnapshotSchedule (Prelude.Maybe [Tag])
createSnapshotSchedule_tags = Lens.lens (\CreateSnapshotSchedule' {tags} -> tags) (\s@CreateSnapshotSchedule' {} a -> s {tags = a} :: CreateSnapshotSchedule) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest CreateSnapshotSchedule where
  type
    AWSResponse CreateSnapshotSchedule =
      SnapshotSchedule
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateSnapshotScheduleResult"
      (\s h x -> Data.parseXML x)

instance Prelude.Hashable CreateSnapshotSchedule where
  hashWithSalt _salt CreateSnapshotSchedule' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` nextInvocations
      `Prelude.hashWithSalt` scheduleDefinitions
      `Prelude.hashWithSalt` scheduleDescription
      `Prelude.hashWithSalt` scheduleIdentifier
      `Prelude.hashWithSalt` tags

instance Prelude.NFData CreateSnapshotSchedule where
  rnf CreateSnapshotSchedule' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf nextInvocations
      `Prelude.seq` Prelude.rnf scheduleDefinitions
      `Prelude.seq` Prelude.rnf scheduleDescription
      `Prelude.seq` Prelude.rnf scheduleIdentifier
      `Prelude.seq` Prelude.rnf tags

instance Data.ToHeaders CreateSnapshotSchedule where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateSnapshotSchedule where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateSnapshotSchedule where
  toQuery CreateSnapshotSchedule' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateSnapshotSchedule" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "NextInvocations" Data.=: nextInvocations,
        "ScheduleDefinitions"
          Data.=: Data.toQuery
            ( Data.toQueryList "ScheduleDefinition"
                Prelude.<$> scheduleDefinitions
            ),
        "ScheduleDescription" Data.=: scheduleDescription,
        "ScheduleIdentifier" Data.=: scheduleIdentifier,
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "Tag" Prelude.<$> tags)
      ]
