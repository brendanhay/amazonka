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
-- Module      : Amazonka.WorkSpaces.Types.PendingCreateStandbyWorkspacesRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.PendingCreateStandbyWorkspacesRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkSpaces.Types.WorkspaceState

-- | Information about the Standby WorkSpace.
--
-- /See:/ 'newPendingCreateStandbyWorkspacesRequest' smart constructor.
data PendingCreateStandbyWorkspacesRequest = PendingCreateStandbyWorkspacesRequest'
  { -- | The identifier of the directory for the Standby WorkSpace.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | Describes the Standby WorkSpace that was created.
    --
    -- Because this operation is asynchronous, the identifier returned is not
    -- immediately available for use with other operations. For example, if you
    -- call
    -- <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaces.html DescribeWorkspaces>
    -- before the WorkSpace is created, the information returned can be
    -- incomplete.
    userName :: Prelude.Maybe Prelude.Text,
    -- | The operational state of the Standby WorkSpace.
    state :: Prelude.Maybe WorkspaceState,
    -- | The identifier of the Standby WorkSpace.
    workspaceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PendingCreateStandbyWorkspacesRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'pendingCreateStandbyWorkspacesRequest_directoryId' - The identifier of the directory for the Standby WorkSpace.
--
-- 'userName', 'pendingCreateStandbyWorkspacesRequest_userName' - Describes the Standby WorkSpace that was created.
--
-- Because this operation is asynchronous, the identifier returned is not
-- immediately available for use with other operations. For example, if you
-- call
-- <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaces.html DescribeWorkspaces>
-- before the WorkSpace is created, the information returned can be
-- incomplete.
--
-- 'state', 'pendingCreateStandbyWorkspacesRequest_state' - The operational state of the Standby WorkSpace.
--
-- 'workspaceId', 'pendingCreateStandbyWorkspacesRequest_workspaceId' - The identifier of the Standby WorkSpace.
newPendingCreateStandbyWorkspacesRequest ::
  PendingCreateStandbyWorkspacesRequest
newPendingCreateStandbyWorkspacesRequest =
  PendingCreateStandbyWorkspacesRequest'
    { directoryId =
        Prelude.Nothing,
      userName = Prelude.Nothing,
      state = Prelude.Nothing,
      workspaceId = Prelude.Nothing
    }

-- | The identifier of the directory for the Standby WorkSpace.
pendingCreateStandbyWorkspacesRequest_directoryId :: Lens.Lens' PendingCreateStandbyWorkspacesRequest (Prelude.Maybe Prelude.Text)
pendingCreateStandbyWorkspacesRequest_directoryId = Lens.lens (\PendingCreateStandbyWorkspacesRequest' {directoryId} -> directoryId) (\s@PendingCreateStandbyWorkspacesRequest' {} a -> s {directoryId = a} :: PendingCreateStandbyWorkspacesRequest)

-- | Describes the Standby WorkSpace that was created.
--
-- Because this operation is asynchronous, the identifier returned is not
-- immediately available for use with other operations. For example, if you
-- call
-- <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaces.html DescribeWorkspaces>
-- before the WorkSpace is created, the information returned can be
-- incomplete.
pendingCreateStandbyWorkspacesRequest_userName :: Lens.Lens' PendingCreateStandbyWorkspacesRequest (Prelude.Maybe Prelude.Text)
pendingCreateStandbyWorkspacesRequest_userName = Lens.lens (\PendingCreateStandbyWorkspacesRequest' {userName} -> userName) (\s@PendingCreateStandbyWorkspacesRequest' {} a -> s {userName = a} :: PendingCreateStandbyWorkspacesRequest)

-- | The operational state of the Standby WorkSpace.
pendingCreateStandbyWorkspacesRequest_state :: Lens.Lens' PendingCreateStandbyWorkspacesRequest (Prelude.Maybe WorkspaceState)
pendingCreateStandbyWorkspacesRequest_state = Lens.lens (\PendingCreateStandbyWorkspacesRequest' {state} -> state) (\s@PendingCreateStandbyWorkspacesRequest' {} a -> s {state = a} :: PendingCreateStandbyWorkspacesRequest)

-- | The identifier of the Standby WorkSpace.
pendingCreateStandbyWorkspacesRequest_workspaceId :: Lens.Lens' PendingCreateStandbyWorkspacesRequest (Prelude.Maybe Prelude.Text)
pendingCreateStandbyWorkspacesRequest_workspaceId = Lens.lens (\PendingCreateStandbyWorkspacesRequest' {workspaceId} -> workspaceId) (\s@PendingCreateStandbyWorkspacesRequest' {} a -> s {workspaceId = a} :: PendingCreateStandbyWorkspacesRequest)

instance
  Core.FromJSON
    PendingCreateStandbyWorkspacesRequest
  where
  parseJSON =
    Core.withObject
      "PendingCreateStandbyWorkspacesRequest"
      ( \x ->
          PendingCreateStandbyWorkspacesRequest'
            Prelude.<$> (x Core..:? "DirectoryId")
            Prelude.<*> (x Core..:? "UserName")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "WorkspaceId")
      )

instance
  Prelude.Hashable
    PendingCreateStandbyWorkspacesRequest
  where
  hashWithSalt
    _salt
    PendingCreateStandbyWorkspacesRequest' {..} =
      _salt `Prelude.hashWithSalt` directoryId
        `Prelude.hashWithSalt` userName
        `Prelude.hashWithSalt` state
        `Prelude.hashWithSalt` workspaceId

instance
  Prelude.NFData
    PendingCreateStandbyWorkspacesRequest
  where
  rnf PendingCreateStandbyWorkspacesRequest' {..} =
    Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf workspaceId
