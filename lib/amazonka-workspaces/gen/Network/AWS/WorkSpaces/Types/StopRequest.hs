{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.StopRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.StopRequest
  ( StopRequest (..),

    -- * Smart constructor
    mkStopRequest,

    -- * Lenses
    srWorkspaceId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the information used to stop a WorkSpace.
--
-- /See:/ 'mkStopRequest' smart constructor.
newtype StopRequest = StopRequest'
  { -- | The identifier of the WorkSpace.
    workspaceId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopRequest' with the minimum fields required to make a request.
--
-- * 'workspaceId' - The identifier of the WorkSpace.
mkStopRequest ::
  StopRequest
mkStopRequest = StopRequest' {workspaceId = Lude.Nothing}

-- | The identifier of the WorkSpace.
--
-- /Note:/ Consider using 'workspaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srWorkspaceId :: Lens.Lens' StopRequest (Lude.Maybe Lude.Text)
srWorkspaceId = Lens.lens (workspaceId :: StopRequest -> Lude.Maybe Lude.Text) (\s a -> s {workspaceId = a} :: StopRequest)
{-# DEPRECATED srWorkspaceId "Use generic-lens or generic-optics with 'workspaceId' instead." #-}

instance Lude.ToJSON StopRequest where
  toJSON StopRequest' {..} =
    Lude.object
      (Lude.catMaybes [("WorkspaceId" Lude..=) Lude.<$> workspaceId])
