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
-- Module      : Network.AWS.WorkSpaces.Types.FailedCreateWorkspaceRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.FailedCreateWorkspaceRequest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.WorkSpaces.Types.WorkspaceRequest

-- | Describes a WorkSpace that cannot be created.
--
-- /See:/ 'newFailedCreateWorkspaceRequest' smart constructor.
data FailedCreateWorkspaceRequest = FailedCreateWorkspaceRequest'
  { -- | Information about the WorkSpace.
    workspaceRequest :: Core.Maybe WorkspaceRequest,
    -- | The text of the error message that is returned if the WorkSpace cannot
    -- be created.
    errorMessage :: Core.Maybe Core.Text,
    -- | The error code that is returned if the WorkSpace cannot be created.
    errorCode :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FailedCreateWorkspaceRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workspaceRequest', 'failedCreateWorkspaceRequest_workspaceRequest' - Information about the WorkSpace.
--
-- 'errorMessage', 'failedCreateWorkspaceRequest_errorMessage' - The text of the error message that is returned if the WorkSpace cannot
-- be created.
--
-- 'errorCode', 'failedCreateWorkspaceRequest_errorCode' - The error code that is returned if the WorkSpace cannot be created.
newFailedCreateWorkspaceRequest ::
  FailedCreateWorkspaceRequest
newFailedCreateWorkspaceRequest =
  FailedCreateWorkspaceRequest'
    { workspaceRequest =
        Core.Nothing,
      errorMessage = Core.Nothing,
      errorCode = Core.Nothing
    }

-- | Information about the WorkSpace.
failedCreateWorkspaceRequest_workspaceRequest :: Lens.Lens' FailedCreateWorkspaceRequest (Core.Maybe WorkspaceRequest)
failedCreateWorkspaceRequest_workspaceRequest = Lens.lens (\FailedCreateWorkspaceRequest' {workspaceRequest} -> workspaceRequest) (\s@FailedCreateWorkspaceRequest' {} a -> s {workspaceRequest = a} :: FailedCreateWorkspaceRequest)

-- | The text of the error message that is returned if the WorkSpace cannot
-- be created.
failedCreateWorkspaceRequest_errorMessage :: Lens.Lens' FailedCreateWorkspaceRequest (Core.Maybe Core.Text)
failedCreateWorkspaceRequest_errorMessage = Lens.lens (\FailedCreateWorkspaceRequest' {errorMessage} -> errorMessage) (\s@FailedCreateWorkspaceRequest' {} a -> s {errorMessage = a} :: FailedCreateWorkspaceRequest)

-- | The error code that is returned if the WorkSpace cannot be created.
failedCreateWorkspaceRequest_errorCode :: Lens.Lens' FailedCreateWorkspaceRequest (Core.Maybe Core.Text)
failedCreateWorkspaceRequest_errorCode = Lens.lens (\FailedCreateWorkspaceRequest' {errorCode} -> errorCode) (\s@FailedCreateWorkspaceRequest' {} a -> s {errorCode = a} :: FailedCreateWorkspaceRequest)

instance Core.FromJSON FailedCreateWorkspaceRequest where
  parseJSON =
    Core.withObject
      "FailedCreateWorkspaceRequest"
      ( \x ->
          FailedCreateWorkspaceRequest'
            Core.<$> (x Core..:? "WorkspaceRequest")
            Core.<*> (x Core..:? "ErrorMessage")
            Core.<*> (x Core..:? "ErrorCode")
      )

instance Core.Hashable FailedCreateWorkspaceRequest

instance Core.NFData FailedCreateWorkspaceRequest
