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
-- Module      : Amazonka.IotTwinMaker.Types.WorkspaceSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.WorkspaceSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that contains information about a workspace.
--
-- /See:/ 'newWorkspaceSummary' smart constructor.
data WorkspaceSummary = WorkspaceSummary'
  { -- | The description of the workspace.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the workspace.
    workspaceId :: Prelude.Text,
    -- | The ARN of the workspace.
    arn :: Prelude.Text,
    -- | The date and time when the workspace was created.
    creationDateTime :: Core.POSIX,
    -- | The date and time when the workspace was last updated.
    updateDateTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkspaceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'workspaceSummary_description' - The description of the workspace.
--
-- 'workspaceId', 'workspaceSummary_workspaceId' - The ID of the workspace.
--
-- 'arn', 'workspaceSummary_arn' - The ARN of the workspace.
--
-- 'creationDateTime', 'workspaceSummary_creationDateTime' - The date and time when the workspace was created.
--
-- 'updateDateTime', 'workspaceSummary_updateDateTime' - The date and time when the workspace was last updated.
newWorkspaceSummary ::
  -- | 'workspaceId'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'creationDateTime'
  Prelude.UTCTime ->
  -- | 'updateDateTime'
  Prelude.UTCTime ->
  WorkspaceSummary
newWorkspaceSummary
  pWorkspaceId_
  pArn_
  pCreationDateTime_
  pUpdateDateTime_ =
    WorkspaceSummary'
      { description = Prelude.Nothing,
        workspaceId = pWorkspaceId_,
        arn = pArn_,
        creationDateTime =
          Core._Time Lens.# pCreationDateTime_,
        updateDateTime = Core._Time Lens.# pUpdateDateTime_
      }

-- | The description of the workspace.
workspaceSummary_description :: Lens.Lens' WorkspaceSummary (Prelude.Maybe Prelude.Text)
workspaceSummary_description = Lens.lens (\WorkspaceSummary' {description} -> description) (\s@WorkspaceSummary' {} a -> s {description = a} :: WorkspaceSummary)

-- | The ID of the workspace.
workspaceSummary_workspaceId :: Lens.Lens' WorkspaceSummary Prelude.Text
workspaceSummary_workspaceId = Lens.lens (\WorkspaceSummary' {workspaceId} -> workspaceId) (\s@WorkspaceSummary' {} a -> s {workspaceId = a} :: WorkspaceSummary)

-- | The ARN of the workspace.
workspaceSummary_arn :: Lens.Lens' WorkspaceSummary Prelude.Text
workspaceSummary_arn = Lens.lens (\WorkspaceSummary' {arn} -> arn) (\s@WorkspaceSummary' {} a -> s {arn = a} :: WorkspaceSummary)

-- | The date and time when the workspace was created.
workspaceSummary_creationDateTime :: Lens.Lens' WorkspaceSummary Prelude.UTCTime
workspaceSummary_creationDateTime = Lens.lens (\WorkspaceSummary' {creationDateTime} -> creationDateTime) (\s@WorkspaceSummary' {} a -> s {creationDateTime = a} :: WorkspaceSummary) Prelude.. Core._Time

-- | The date and time when the workspace was last updated.
workspaceSummary_updateDateTime :: Lens.Lens' WorkspaceSummary Prelude.UTCTime
workspaceSummary_updateDateTime = Lens.lens (\WorkspaceSummary' {updateDateTime} -> updateDateTime) (\s@WorkspaceSummary' {} a -> s {updateDateTime = a} :: WorkspaceSummary) Prelude.. Core._Time

instance Core.FromJSON WorkspaceSummary where
  parseJSON =
    Core.withObject
      "WorkspaceSummary"
      ( \x ->
          WorkspaceSummary'
            Prelude.<$> (x Core..:? "description")
            Prelude.<*> (x Core..: "workspaceId")
            Prelude.<*> (x Core..: "arn")
            Prelude.<*> (x Core..: "creationDateTime")
            Prelude.<*> (x Core..: "updateDateTime")
      )

instance Prelude.Hashable WorkspaceSummary where
  hashWithSalt _salt WorkspaceSummary' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` workspaceId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` updateDateTime

instance Prelude.NFData WorkspaceSummary where
  rnf WorkspaceSummary' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf workspaceId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf updateDateTime
