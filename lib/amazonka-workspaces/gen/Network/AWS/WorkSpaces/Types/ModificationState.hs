{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.ModificationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.ModificationState
  ( ModificationState (..),

    -- * Smart constructor
    mkModificationState,

    -- * Lenses
    msState,
    msResource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkSpaces.Types.ModificationResourceEnum
import Network.AWS.WorkSpaces.Types.ModificationStateEnum

-- | Describes a WorkSpace modification.
--
-- /See:/ 'mkModificationState' smart constructor.
data ModificationState = ModificationState'
  { state ::
      Lude.Maybe ModificationStateEnum,
    resource :: Lude.Maybe ModificationResourceEnum
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModificationState' with the minimum fields required to make a request.
--
-- * 'resource' - The resource.
-- * 'state' - The modification state.
mkModificationState ::
  ModificationState
mkModificationState =
  ModificationState' {state = Lude.Nothing, resource = Lude.Nothing}

-- | The modification state.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msState :: Lens.Lens' ModificationState (Lude.Maybe ModificationStateEnum)
msState = Lens.lens (state :: ModificationState -> Lude.Maybe ModificationStateEnum) (\s a -> s {state = a} :: ModificationState)
{-# DEPRECATED msState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The resource.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msResource :: Lens.Lens' ModificationState (Lude.Maybe ModificationResourceEnum)
msResource = Lens.lens (resource :: ModificationState -> Lude.Maybe ModificationResourceEnum) (\s a -> s {resource = a} :: ModificationState)
{-# DEPRECATED msResource "Use generic-lens or generic-optics with 'resource' instead." #-}

instance Lude.FromJSON ModificationState where
  parseJSON =
    Lude.withObject
      "ModificationState"
      ( \x ->
          ModificationState'
            Lude.<$> (x Lude..:? "State") Lude.<*> (x Lude..:? "Resource")
      )
