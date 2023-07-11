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
-- Module      : Amazonka.AMP.Types.WorkspaceStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AMP.Types.WorkspaceStatus where

import Amazonka.AMP.Types.WorkspaceStatusCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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

instance Data.FromJSON WorkspaceStatus where
  parseJSON =
    Data.withObject
      "WorkspaceStatus"
      ( \x ->
          WorkspaceStatus'
            Prelude.<$> (x Data..: "statusCode")
      )

instance Prelude.Hashable WorkspaceStatus where
  hashWithSalt _salt WorkspaceStatus' {..} =
    _salt `Prelude.hashWithSalt` statusCode

instance Prelude.NFData WorkspaceStatus where
  rnf WorkspaceStatus' {..} = Prelude.rnf statusCode
