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
-- Module      : Network.AWS.AMP.Types.WorkspaceStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AMP.Types.WorkspaceStatus where

import Network.AWS.AMP.Types.WorkspaceStatusCode
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the status of a workspace.
--
-- /See:/ 'newWorkspaceStatus' smart constructor.
data WorkspaceStatus = WorkspaceStatus'
  { -- | Status code of this workspace.
    statusCode :: WorkspaceStatusCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkspaceStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusCode', 'workspaceStatus_statusCode' - Status code of this workspace.
newWorkspaceStatus ::
  -- | 'statusCode'
  WorkspaceStatusCode ->
  WorkspaceStatus
newWorkspaceStatus pStatusCode_ =
  WorkspaceStatus' {statusCode = pStatusCode_}

-- | Status code of this workspace.
workspaceStatus_statusCode :: Lens.Lens' WorkspaceStatus WorkspaceStatusCode
workspaceStatus_statusCode = Lens.lens (\WorkspaceStatus' {statusCode} -> statusCode) (\s@WorkspaceStatus' {} a -> s {statusCode = a} :: WorkspaceStatus)

instance Core.FromJSON WorkspaceStatus where
  parseJSON =
    Core.withObject
      "WorkspaceStatus"
      ( \x ->
          WorkspaceStatus'
            Prelude.<$> (x Core..: "statusCode")
      )

instance Prelude.Hashable WorkspaceStatus

instance Prelude.NFData WorkspaceStatus
