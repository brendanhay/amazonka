{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.FailedCreateWorkspaceRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.FailedCreateWorkspaceRequest
  ( FailedCreateWorkspaceRequest (..),

    -- * Smart constructor
    mkFailedCreateWorkspaceRequest,

    -- * Lenses
    fcwrErrorCode,
    fcwrErrorMessage,
    fcwrWorkspaceRequest,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkSpaces.Types.ErrorCode as Types
import qualified Network.AWS.WorkSpaces.Types.ErrorMessage as Types
import qualified Network.AWS.WorkSpaces.Types.WorkspaceRequest as Types

-- | Describes a WorkSpace that cannot be created.
--
-- /See:/ 'mkFailedCreateWorkspaceRequest' smart constructor.
data FailedCreateWorkspaceRequest = FailedCreateWorkspaceRequest'
  { -- | The error code that is returned if the WorkSpace cannot be created.
    errorCode :: Core.Maybe Types.ErrorCode,
    -- | The text of the error message that is returned if the WorkSpace cannot be created.
    errorMessage :: Core.Maybe Types.ErrorMessage,
    -- | Information about the WorkSpace.
    workspaceRequest :: Core.Maybe Types.WorkspaceRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FailedCreateWorkspaceRequest' value with any optional fields omitted.
mkFailedCreateWorkspaceRequest ::
  FailedCreateWorkspaceRequest
mkFailedCreateWorkspaceRequest =
  FailedCreateWorkspaceRequest'
    { errorCode = Core.Nothing,
      errorMessage = Core.Nothing,
      workspaceRequest = Core.Nothing
    }

-- | The error code that is returned if the WorkSpace cannot be created.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcwrErrorCode :: Lens.Lens' FailedCreateWorkspaceRequest (Core.Maybe Types.ErrorCode)
fcwrErrorCode = Lens.field @"errorCode"
{-# DEPRECATED fcwrErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The text of the error message that is returned if the WorkSpace cannot be created.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcwrErrorMessage :: Lens.Lens' FailedCreateWorkspaceRequest (Core.Maybe Types.ErrorMessage)
fcwrErrorMessage = Lens.field @"errorMessage"
{-# DEPRECATED fcwrErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | Information about the WorkSpace.
--
-- /Note:/ Consider using 'workspaceRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcwrWorkspaceRequest :: Lens.Lens' FailedCreateWorkspaceRequest (Core.Maybe Types.WorkspaceRequest)
fcwrWorkspaceRequest = Lens.field @"workspaceRequest"
{-# DEPRECATED fcwrWorkspaceRequest "Use generic-lens or generic-optics with 'workspaceRequest' instead." #-}

instance Core.FromJSON FailedCreateWorkspaceRequest where
  parseJSON =
    Core.withObject "FailedCreateWorkspaceRequest" Core.$
      \x ->
        FailedCreateWorkspaceRequest'
          Core.<$> (x Core..:? "ErrorCode")
          Core.<*> (x Core..:? "ErrorMessage")
          Core.<*> (x Core..:? "WorkspaceRequest")
