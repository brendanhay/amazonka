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
-- Module      : Network.AWS.WorkSpaces.Types.StopRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.StopRequest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the information used to stop a WorkSpace.
--
-- /See:/ 'newStopRequest' smart constructor.
data StopRequest = StopRequest'
  { -- | The identifier of the WorkSpace.
    workspaceId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workspaceId', 'stopRequest_workspaceId' - The identifier of the WorkSpace.
newStopRequest ::
  StopRequest
newStopRequest =
  StopRequest' {workspaceId = Core.Nothing}

-- | The identifier of the WorkSpace.
stopRequest_workspaceId :: Lens.Lens' StopRequest (Core.Maybe Core.Text)
stopRequest_workspaceId = Lens.lens (\StopRequest' {workspaceId} -> workspaceId) (\s@StopRequest' {} a -> s {workspaceId = a} :: StopRequest)

instance Core.Hashable StopRequest

instance Core.NFData StopRequest

instance Core.ToJSON StopRequest where
  toJSON StopRequest' {..} =
    Core.object
      ( Core.catMaybes
          [("WorkspaceId" Core..=) Core.<$> workspaceId]
      )
