{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.RebuildRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.RebuildRequest
  ( RebuildRequest (..),

    -- * Smart constructor
    mkRebuildRequest,

    -- * Lenses
    rrWorkspaceId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the information used to rebuild a WorkSpace.
--
-- /See:/ 'mkRebuildRequest' smart constructor.
newtype RebuildRequest = RebuildRequest'
  { -- | The identifier of the WorkSpace.
    workspaceId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RebuildRequest' with the minimum fields required to make a request.
--
-- * 'workspaceId' - The identifier of the WorkSpace.
mkRebuildRequest ::
  -- | 'workspaceId'
  Lude.Text ->
  RebuildRequest
mkRebuildRequest pWorkspaceId_ =
  RebuildRequest' {workspaceId = pWorkspaceId_}

-- | The identifier of the WorkSpace.
--
-- /Note:/ Consider using 'workspaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrWorkspaceId :: Lens.Lens' RebuildRequest Lude.Text
rrWorkspaceId = Lens.lens (workspaceId :: RebuildRequest -> Lude.Text) (\s a -> s {workspaceId = a} :: RebuildRequest)
{-# DEPRECATED rrWorkspaceId "Use generic-lens or generic-optics with 'workspaceId' instead." #-}

instance Lude.ToJSON RebuildRequest where
  toJSON RebuildRequest' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("WorkspaceId" Lude..= workspaceId)])
