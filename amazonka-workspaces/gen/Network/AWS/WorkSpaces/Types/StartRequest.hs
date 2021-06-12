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
-- Module      : Network.AWS.WorkSpaces.Types.StartRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.StartRequest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information used to start a WorkSpace.
--
-- /See:/ 'newStartRequest' smart constructor.
data StartRequest = StartRequest'
  { -- | The identifier of the WorkSpace.
    workspaceId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workspaceId', 'startRequest_workspaceId' - The identifier of the WorkSpace.
newStartRequest ::
  StartRequest
newStartRequest =
  StartRequest' {workspaceId = Core.Nothing}

-- | The identifier of the WorkSpace.
startRequest_workspaceId :: Lens.Lens' StartRequest (Core.Maybe Core.Text)
startRequest_workspaceId = Lens.lens (\StartRequest' {workspaceId} -> workspaceId) (\s@StartRequest' {} a -> s {workspaceId = a} :: StartRequest)

instance Core.Hashable StartRequest

instance Core.NFData StartRequest

instance Core.ToJSON StartRequest where
  toJSON StartRequest' {..} =
    Core.object
      ( Core.catMaybes
          [("WorkspaceId" Core..=) Core.<$> workspaceId]
      )
