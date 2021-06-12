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
-- Module      : Network.AWS.WorkSpaces.Types.RebootRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.RebootRequest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the information used to reboot a WorkSpace.
--
-- /See:/ 'newRebootRequest' smart constructor.
data RebootRequest = RebootRequest'
  { -- | The identifier of the WorkSpace.
    workspaceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RebootRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workspaceId', 'rebootRequest_workspaceId' - The identifier of the WorkSpace.
newRebootRequest ::
  -- | 'workspaceId'
  Core.Text ->
  RebootRequest
newRebootRequest pWorkspaceId_ =
  RebootRequest' {workspaceId = pWorkspaceId_}

-- | The identifier of the WorkSpace.
rebootRequest_workspaceId :: Lens.Lens' RebootRequest Core.Text
rebootRequest_workspaceId = Lens.lens (\RebootRequest' {workspaceId} -> workspaceId) (\s@RebootRequest' {} a -> s {workspaceId = a} :: RebootRequest)

instance Core.Hashable RebootRequest

instance Core.NFData RebootRequest

instance Core.ToJSON RebootRequest where
  toJSON RebootRequest' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("WorkspaceId" Core..= workspaceId)]
      )
