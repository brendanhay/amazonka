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
-- Module      : Amazonka.WorkSpaces.Types.TerminateRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.TerminateRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the information used to terminate a WorkSpace.
--
-- /See:/ 'newTerminateRequest' smart constructor.
data TerminateRequest = TerminateRequest'
  { -- | The identifier of the WorkSpace.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TerminateRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workspaceId', 'terminateRequest_workspaceId' - The identifier of the WorkSpace.
newTerminateRequest ::
  -- | 'workspaceId'
  Prelude.Text ->
  TerminateRequest
newTerminateRequest pWorkspaceId_ =
  TerminateRequest' {workspaceId = pWorkspaceId_}

-- | The identifier of the WorkSpace.
terminateRequest_workspaceId :: Lens.Lens' TerminateRequest Prelude.Text
terminateRequest_workspaceId = Lens.lens (\TerminateRequest' {workspaceId} -> workspaceId) (\s@TerminateRequest' {} a -> s {workspaceId = a} :: TerminateRequest)

instance Prelude.Hashable TerminateRequest where
  hashWithSalt _salt TerminateRequest' {..} =
    _salt `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData TerminateRequest where
  rnf TerminateRequest' {..} = Prelude.rnf workspaceId

instance Data.ToJSON TerminateRequest where
  toJSON TerminateRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("WorkspaceId" Data..= workspaceId)]
      )
