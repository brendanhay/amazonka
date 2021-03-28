{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.FailedWorkspaceChangeRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.FailedWorkspaceChangeRequest
  ( FailedWorkspaceChangeRequest (..)
  -- * Smart constructor
  , mkFailedWorkspaceChangeRequest
  -- * Lenses
  , fwcrErrorCode
  , fwcrErrorMessage
  , fwcrWorkspaceId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkSpaces.Types.ErrorCode as Types
import qualified Network.AWS.WorkSpaces.Types.ErrorMessage as Types
import qualified Network.AWS.WorkSpaces.Types.WorkspaceId as Types

-- | Describes a WorkSpace that could not be rebooted. ('RebootWorkspaces' ), rebuilt ('RebuildWorkspaces' ), restored ('RestoreWorkspace' ), terminated ('TerminateWorkspaces' ), started ('StartWorkspaces' ), or stopped ('StopWorkspaces' ).
--
-- /See:/ 'mkFailedWorkspaceChangeRequest' smart constructor.
data FailedWorkspaceChangeRequest = FailedWorkspaceChangeRequest'
  { errorCode :: Core.Maybe Types.ErrorCode
    -- ^ The error code that is returned if the WorkSpace cannot be rebooted.
  , errorMessage :: Core.Maybe Types.ErrorMessage
    -- ^ The text of the error message that is returned if the WorkSpace cannot be rebooted.
  , workspaceId :: Core.Maybe Types.WorkspaceId
    -- ^ The identifier of the WorkSpace.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FailedWorkspaceChangeRequest' value with any optional fields omitted.
mkFailedWorkspaceChangeRequest
    :: FailedWorkspaceChangeRequest
mkFailedWorkspaceChangeRequest
  = FailedWorkspaceChangeRequest'{errorCode = Core.Nothing,
                                  errorMessage = Core.Nothing, workspaceId = Core.Nothing}

-- | The error code that is returned if the WorkSpace cannot be rebooted.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fwcrErrorCode :: Lens.Lens' FailedWorkspaceChangeRequest (Core.Maybe Types.ErrorCode)
fwcrErrorCode = Lens.field @"errorCode"
{-# INLINEABLE fwcrErrorCode #-}
{-# DEPRECATED errorCode "Use generic-lens or generic-optics with 'errorCode' instead"  #-}

-- | The text of the error message that is returned if the WorkSpace cannot be rebooted.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fwcrErrorMessage :: Lens.Lens' FailedWorkspaceChangeRequest (Core.Maybe Types.ErrorMessage)
fwcrErrorMessage = Lens.field @"errorMessage"
{-# INLINEABLE fwcrErrorMessage #-}
{-# DEPRECATED errorMessage "Use generic-lens or generic-optics with 'errorMessage' instead"  #-}

-- | The identifier of the WorkSpace.
--
-- /Note:/ Consider using 'workspaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fwcrWorkspaceId :: Lens.Lens' FailedWorkspaceChangeRequest (Core.Maybe Types.WorkspaceId)
fwcrWorkspaceId = Lens.field @"workspaceId"
{-# INLINEABLE fwcrWorkspaceId #-}
{-# DEPRECATED workspaceId "Use generic-lens or generic-optics with 'workspaceId' instead"  #-}

instance Core.FromJSON FailedWorkspaceChangeRequest where
        parseJSON
          = Core.withObject "FailedWorkspaceChangeRequest" Core.$
              \ x ->
                FailedWorkspaceChangeRequest' Core.<$>
                  (x Core..:? "ErrorCode") Core.<*> x Core..:? "ErrorMessage"
                    Core.<*> x Core..:? "WorkspaceId"
