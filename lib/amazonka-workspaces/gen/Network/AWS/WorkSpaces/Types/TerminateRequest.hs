{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.TerminateRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.TerminateRequest
  ( TerminateRequest (..),

    -- * Smart constructor
    mkTerminateRequest,

    -- * Lenses
    trWorkspaceId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkSpaces.Types.WorkspaceId as Types

-- | Describes the information used to terminate a WorkSpace.
--
-- /See:/ 'mkTerminateRequest' smart constructor.
newtype TerminateRequest = TerminateRequest'
  { -- | The identifier of the WorkSpace.
    workspaceId :: Types.WorkspaceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TerminateRequest' value with any optional fields omitted.
mkTerminateRequest ::
  -- | 'workspaceId'
  Types.WorkspaceId ->
  TerminateRequest
mkTerminateRequest workspaceId = TerminateRequest' {workspaceId}

-- | The identifier of the WorkSpace.
--
-- /Note:/ Consider using 'workspaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trWorkspaceId :: Lens.Lens' TerminateRequest Types.WorkspaceId
trWorkspaceId = Lens.field @"workspaceId"
{-# DEPRECATED trWorkspaceId "Use generic-lens or generic-optics with 'workspaceId' instead." #-}

instance Core.FromJSON TerminateRequest where
  toJSON TerminateRequest {..} =
    Core.object
      (Core.catMaybes [Core.Just ("WorkspaceId" Core..= workspaceId)])
