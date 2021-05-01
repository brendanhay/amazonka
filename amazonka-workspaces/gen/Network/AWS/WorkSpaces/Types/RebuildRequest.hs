{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.WorkSpaces.Types.RebuildRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.RebuildRequest where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the information used to rebuild a WorkSpace.
--
-- /See:/ 'newRebuildRequest' smart constructor.
data RebuildRequest = RebuildRequest'
  { -- | The identifier of the WorkSpace.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.Hashable RebuildRequest

instance Prelude.NFData RebuildRequest

instance Prelude.ToJSON RebuildRequest where
  toJSON RebuildRequest' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("WorkspaceId" Prelude..= workspaceId)
          ]
      )
