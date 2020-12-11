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
import qualified Network.AWS.Prelude as Lude

-- | Describes the information used to reboot a WorkSpace.
--
-- /See:/ 'mkRebootRequest' smart constructor.
newtype RebootRequest = RebootRequest' {workspaceId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RebootRequest' with the minimum fields required to make a request.
--
-- * 'workspaceId' - The identifier of the WorkSpace.
mkRebootRequest ::
  -- | 'workspaceId'
  Lude.Text ->
  RebootRequest
mkRebootRequest pWorkspaceId_ =
  RebootRequest' {workspaceId = pWorkspaceId_}

-- | The identifier of the WorkSpace.
--
-- /Note:/ Consider using 'workspaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rWorkspaceId :: Lens.Lens' RebootRequest Lude.Text
rWorkspaceId = Lens.lens (workspaceId :: RebootRequest -> Lude.Text) (\s a -> s {workspaceId = a} :: RebootRequest)
{-# DEPRECATED rWorkspaceId "Use generic-lens or generic-optics with 'workspaceId' instead." #-}

instance Lude.ToJSON RebootRequest where
  toJSON RebootRequest' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("WorkspaceId" Lude..= workspaceId)])
