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
    fcwrWorkspaceRequest,
    fcwrErrorCode,
    fcwrErrorMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkSpaces.Types.WorkspaceRequest

-- | Describes a WorkSpace that cannot be created.
--
-- /See:/ 'mkFailedCreateWorkspaceRequest' smart constructor.
data FailedCreateWorkspaceRequest = FailedCreateWorkspaceRequest'
  { workspaceRequest ::
      Lude.Maybe WorkspaceRequest,
    errorCode :: Lude.Maybe Lude.Text,
    errorMessage ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FailedCreateWorkspaceRequest' with the minimum fields required to make a request.
--
-- * 'errorCode' - The error code that is returned if the WorkSpace cannot be created.
-- * 'errorMessage' - The text of the error message that is returned if the WorkSpace cannot be created.
-- * 'workspaceRequest' - Information about the WorkSpace.
mkFailedCreateWorkspaceRequest ::
  FailedCreateWorkspaceRequest
mkFailedCreateWorkspaceRequest =
  FailedCreateWorkspaceRequest'
    { workspaceRequest = Lude.Nothing,
      errorCode = Lude.Nothing,
      errorMessage = Lude.Nothing
    }

-- | Information about the WorkSpace.
--
-- /Note:/ Consider using 'workspaceRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcwrWorkspaceRequest :: Lens.Lens' FailedCreateWorkspaceRequest (Lude.Maybe WorkspaceRequest)
fcwrWorkspaceRequest = Lens.lens (workspaceRequest :: FailedCreateWorkspaceRequest -> Lude.Maybe WorkspaceRequest) (\s a -> s {workspaceRequest = a} :: FailedCreateWorkspaceRequest)
{-# DEPRECATED fcwrWorkspaceRequest "Use generic-lens or generic-optics with 'workspaceRequest' instead." #-}

-- | The error code that is returned if the WorkSpace cannot be created.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcwrErrorCode :: Lens.Lens' FailedCreateWorkspaceRequest (Lude.Maybe Lude.Text)
fcwrErrorCode = Lens.lens (errorCode :: FailedCreateWorkspaceRequest -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: FailedCreateWorkspaceRequest)
{-# DEPRECATED fcwrErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The text of the error message that is returned if the WorkSpace cannot be created.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcwrErrorMessage :: Lens.Lens' FailedCreateWorkspaceRequest (Lude.Maybe Lude.Text)
fcwrErrorMessage = Lens.lens (errorMessage :: FailedCreateWorkspaceRequest -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: FailedCreateWorkspaceRequest)
{-# DEPRECATED fcwrErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Lude.FromJSON FailedCreateWorkspaceRequest where
  parseJSON =
    Lude.withObject
      "FailedCreateWorkspaceRequest"
      ( \x ->
          FailedCreateWorkspaceRequest'
            Lude.<$> (x Lude..:? "WorkspaceRequest")
            Lude.<*> (x Lude..:? "ErrorCode")
            Lude.<*> (x Lude..:? "ErrorMessage")
      )
