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
-- Module      : Amazonka.WorkSpaces.Types.FailedWorkspaceChangeRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.FailedWorkspaceChangeRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a WorkSpace that could not be rebooted. (RebootWorkspaces),
-- rebuilt (RebuildWorkspaces), restored (RestoreWorkspace), terminated
-- (TerminateWorkspaces), started (StartWorkspaces), or stopped
-- (StopWorkspaces).
--
-- /See:/ 'newFailedWorkspaceChangeRequest' smart constructor.
data FailedWorkspaceChangeRequest = FailedWorkspaceChangeRequest'
  { -- | The text of the error message that is returned if the WorkSpace cannot
    -- be rebooted.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the WorkSpace.
    workspaceId :: Prelude.Maybe Prelude.Text,
    -- | The error code that is returned if the WorkSpace cannot be rebooted.
    errorCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailedWorkspaceChangeRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorMessage', 'failedWorkspaceChangeRequest_errorMessage' - The text of the error message that is returned if the WorkSpace cannot
-- be rebooted.
--
-- 'workspaceId', 'failedWorkspaceChangeRequest_workspaceId' - The identifier of the WorkSpace.
--
-- 'errorCode', 'failedWorkspaceChangeRequest_errorCode' - The error code that is returned if the WorkSpace cannot be rebooted.
newFailedWorkspaceChangeRequest ::
  FailedWorkspaceChangeRequest
newFailedWorkspaceChangeRequest =
  FailedWorkspaceChangeRequest'
    { errorMessage =
        Prelude.Nothing,
      workspaceId = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | The text of the error message that is returned if the WorkSpace cannot
-- be rebooted.
failedWorkspaceChangeRequest_errorMessage :: Lens.Lens' FailedWorkspaceChangeRequest (Prelude.Maybe Prelude.Text)
failedWorkspaceChangeRequest_errorMessage = Lens.lens (\FailedWorkspaceChangeRequest' {errorMessage} -> errorMessage) (\s@FailedWorkspaceChangeRequest' {} a -> s {errorMessage = a} :: FailedWorkspaceChangeRequest)

-- | The identifier of the WorkSpace.
failedWorkspaceChangeRequest_workspaceId :: Lens.Lens' FailedWorkspaceChangeRequest (Prelude.Maybe Prelude.Text)
failedWorkspaceChangeRequest_workspaceId = Lens.lens (\FailedWorkspaceChangeRequest' {workspaceId} -> workspaceId) (\s@FailedWorkspaceChangeRequest' {} a -> s {workspaceId = a} :: FailedWorkspaceChangeRequest)

-- | The error code that is returned if the WorkSpace cannot be rebooted.
failedWorkspaceChangeRequest_errorCode :: Lens.Lens' FailedWorkspaceChangeRequest (Prelude.Maybe Prelude.Text)
failedWorkspaceChangeRequest_errorCode = Lens.lens (\FailedWorkspaceChangeRequest' {errorCode} -> errorCode) (\s@FailedWorkspaceChangeRequest' {} a -> s {errorCode = a} :: FailedWorkspaceChangeRequest)

instance Core.FromJSON FailedWorkspaceChangeRequest where
  parseJSON =
    Core.withObject
      "FailedWorkspaceChangeRequest"
      ( \x ->
          FailedWorkspaceChangeRequest'
            Prelude.<$> (x Core..:? "ErrorMessage")
            Prelude.<*> (x Core..:? "WorkspaceId")
            Prelude.<*> (x Core..:? "ErrorCode")
      )

instance
  Prelude.Hashable
    FailedWorkspaceChangeRequest
  where
  hashWithSalt _salt FailedWorkspaceChangeRequest' {..} =
    _salt `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` workspaceId
      `Prelude.hashWithSalt` errorCode

instance Prelude.NFData FailedWorkspaceChangeRequest where
  rnf FailedWorkspaceChangeRequest' {..} =
    Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf workspaceId
      `Prelude.seq` Prelude.rnf errorCode
