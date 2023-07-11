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
-- Module      : Amazonka.WorkSpaces.Types.RebuildRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.RebuildRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the information used to rebuild a WorkSpace.
--
-- /See:/ 'newRebuildRequest' smart constructor.
data RebuildRequest = RebuildRequest'
  { -- | The identifier of the WorkSpace.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RebuildRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workspaceId', 'rebuildRequest_workspaceId' - The identifier of the WorkSpace.
newRebuildRequest ::
  -- | 'workspaceId'
  Prelude.Text ->
  RebuildRequest
newRebuildRequest pWorkspaceId_ =
  RebuildRequest' {workspaceId = pWorkspaceId_}

-- | The identifier of the WorkSpace.
rebuildRequest_workspaceId :: Lens.Lens' RebuildRequest Prelude.Text
rebuildRequest_workspaceId = Lens.lens (\RebuildRequest' {workspaceId} -> workspaceId) (\s@RebuildRequest' {} a -> s {workspaceId = a} :: RebuildRequest)

instance Prelude.Hashable RebuildRequest where
  hashWithSalt _salt RebuildRequest' {..} =
    _salt `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData RebuildRequest where
  rnf RebuildRequest' {..} = Prelude.rnf workspaceId

instance Data.ToJSON RebuildRequest where
  toJSON RebuildRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("WorkspaceId" Data..= workspaceId)]
      )
