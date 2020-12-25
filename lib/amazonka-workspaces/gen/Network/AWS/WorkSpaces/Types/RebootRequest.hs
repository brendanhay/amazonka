{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.RebootRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.RebootRequest
  ( RebootRequest (..),

    -- * Smart constructor
    mkRebootRequest,

    -- * Lenses
    rWorkspaceId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkSpaces.Types.WorkspaceId as Types

-- | Describes the information used to reboot a WorkSpace.
--
-- /See:/ 'mkRebootRequest' smart constructor.
newtype RebootRequest = RebootRequest'
  { -- | The identifier of the WorkSpace.
    workspaceId :: Types.WorkspaceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RebootRequest' value with any optional fields omitted.
mkRebootRequest ::
  -- | 'workspaceId'
  Types.WorkspaceId ->
  RebootRequest
mkRebootRequest workspaceId = RebootRequest' {workspaceId}

-- | The identifier of the WorkSpace.
--
-- /Note:/ Consider using 'workspaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rWorkspaceId :: Lens.Lens' RebootRequest Types.WorkspaceId
rWorkspaceId = Lens.field @"workspaceId"
{-# DEPRECATED rWorkspaceId "Use generic-lens or generic-optics with 'workspaceId' instead." #-}

instance Core.FromJSON RebootRequest where
  toJSON RebootRequest {..} =
    Core.object
      (Core.catMaybes [Core.Just ("WorkspaceId" Core..= workspaceId)])
