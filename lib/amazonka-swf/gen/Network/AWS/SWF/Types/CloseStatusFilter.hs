{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.CloseStatusFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.CloseStatusFilter
  ( CloseStatusFilter (..),

    -- * Smart constructor
    mkCloseStatusFilter,

    -- * Lenses
    csfStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SWF.Types.CloseStatus

-- | Used to filter the closed workflow executions in visibility APIs by their close status.
--
-- /See:/ 'mkCloseStatusFilter' smart constructor.
newtype CloseStatusFilter = CloseStatusFilter'
  { -- | The close status that must match the close status of an execution for it to meet the criteria of this filter.
    status :: CloseStatus
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CloseStatusFilter' with the minimum fields required to make a request.
--
-- * 'status' - The close status that must match the close status of an execution for it to meet the criteria of this filter.
mkCloseStatusFilter ::
  -- | 'status'
  CloseStatus ->
  CloseStatusFilter
mkCloseStatusFilter pStatus_ =
  CloseStatusFilter' {status = pStatus_}

-- | The close status that must match the close status of an execution for it to meet the criteria of this filter.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfStatus :: Lens.Lens' CloseStatusFilter CloseStatus
csfStatus = Lens.lens (status :: CloseStatusFilter -> CloseStatus) (\s a -> s {status = a} :: CloseStatusFilter)
{-# DEPRECATED csfStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.ToJSON CloseStatusFilter where
  toJSON CloseStatusFilter' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("status" Lude..= status)])
