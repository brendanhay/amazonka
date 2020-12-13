{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.FailedWorkspaceChangeRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.FailedWorkspaceChangeRequest
  ( FailedWorkspaceChangeRequest (..),

    -- * Smart constructor
    mkFailedWorkspaceChangeRequest,

    -- * Lenses
    fwcrErrorCode,
    fwcrWorkspaceId,
    fwcrErrorMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a WorkSpace that could not be rebooted. ('RebootWorkspaces' ), rebuilt ('RebuildWorkspaces' ), restored ('RestoreWorkspace' ), terminated ('TerminateWorkspaces' ), started ('StartWorkspaces' ), or stopped ('StopWorkspaces' ).
--
-- /See:/ 'mkFailedWorkspaceChangeRequest' smart constructor.
data FailedWorkspaceChangeRequest = FailedWorkspaceChangeRequest'
  { -- | The error code that is returned if the WorkSpace cannot be rebooted.
    errorCode :: Lude.Maybe Lude.Text,
    -- | The identifier of the WorkSpace.
    workspaceId :: Lude.Maybe Lude.Text,
    -- | The text of the error message that is returned if the WorkSpace cannot be rebooted.
    errorMessage :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FailedWorkspaceChangeRequest' with the minimum fields required to make a request.
--
-- * 'errorCode' - The error code that is returned if the WorkSpace cannot be rebooted.
-- * 'workspaceId' - The identifier of the WorkSpace.
-- * 'errorMessage' - The text of the error message that is returned if the WorkSpace cannot be rebooted.
mkFailedWorkspaceChangeRequest ::
  FailedWorkspaceChangeRequest
mkFailedWorkspaceChangeRequest =
  FailedWorkspaceChangeRequest'
    { errorCode = Lude.Nothing,
      workspaceId = Lude.Nothing,
      errorMessage = Lude.Nothing
    }

-- | The error code that is returned if the WorkSpace cannot be rebooted.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fwcrErrorCode :: Lens.Lens' FailedWorkspaceChangeRequest (Lude.Maybe Lude.Text)
fwcrErrorCode = Lens.lens (errorCode :: FailedWorkspaceChangeRequest -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: FailedWorkspaceChangeRequest)
{-# DEPRECATED fwcrErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The identifier of the WorkSpace.
--
-- /Note:/ Consider using 'workspaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fwcrWorkspaceId :: Lens.Lens' FailedWorkspaceChangeRequest (Lude.Maybe Lude.Text)
fwcrWorkspaceId = Lens.lens (workspaceId :: FailedWorkspaceChangeRequest -> Lude.Maybe Lude.Text) (\s a -> s {workspaceId = a} :: FailedWorkspaceChangeRequest)
{-# DEPRECATED fwcrWorkspaceId "Use generic-lens or generic-optics with 'workspaceId' instead." #-}

-- | The text of the error message that is returned if the WorkSpace cannot be rebooted.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fwcrErrorMessage :: Lens.Lens' FailedWorkspaceChangeRequest (Lude.Maybe Lude.Text)
fwcrErrorMessage = Lens.lens (errorMessage :: FailedWorkspaceChangeRequest -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: FailedWorkspaceChangeRequest)
{-# DEPRECATED fwcrErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Lude.FromJSON FailedWorkspaceChangeRequest where
  parseJSON =
    Lude.withObject
      "FailedWorkspaceChangeRequest"
      ( \x ->
          FailedWorkspaceChangeRequest'
            Lude.<$> (x Lude..:? "ErrorCode")
            Lude.<*> (x Lude..:? "WorkspaceId")
            Lude.<*> (x Lude..:? "ErrorMessage")
      )
