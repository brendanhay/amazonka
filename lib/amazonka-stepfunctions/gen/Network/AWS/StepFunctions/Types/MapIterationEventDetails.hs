{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.MapIterationEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.MapIterationEventDetails
  ( MapIterationEventDetails (..),

    -- * Smart constructor
    mkMapIterationEventDetails,

    -- * Lenses
    miedName,
    miedIndex,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains details about an iteration of a Map state.
--
-- /See:/ 'mkMapIterationEventDetails' smart constructor.
data MapIterationEventDetails = MapIterationEventDetails'
  { name ::
      Lude.Maybe Lude.Text,
    index :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MapIterationEventDetails' with the minimum fields required to make a request.
--
-- * 'index' - The index of the array belonging to the Map state iteration.
-- * 'name' - The name of the iteration’s parent Map state.
mkMapIterationEventDetails ::
  MapIterationEventDetails
mkMapIterationEventDetails =
  MapIterationEventDetails'
    { name = Lude.Nothing,
      index = Lude.Nothing
    }

-- | The name of the iteration’s parent Map state.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miedName :: Lens.Lens' MapIterationEventDetails (Lude.Maybe Lude.Text)
miedName = Lens.lens (name :: MapIterationEventDetails -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: MapIterationEventDetails)
{-# DEPRECATED miedName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The index of the array belonging to the Map state iteration.
--
-- /Note:/ Consider using 'index' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miedIndex :: Lens.Lens' MapIterationEventDetails (Lude.Maybe Lude.Natural)
miedIndex = Lens.lens (index :: MapIterationEventDetails -> Lude.Maybe Lude.Natural) (\s a -> s {index = a} :: MapIterationEventDetails)
{-# DEPRECATED miedIndex "Use generic-lens or generic-optics with 'index' instead." #-}

instance Lude.FromJSON MapIterationEventDetails where
  parseJSON =
    Lude.withObject
      "MapIterationEventDetails"
      ( \x ->
          MapIterationEventDetails'
            Lude.<$> (x Lude..:? "name") Lude.<*> (x Lude..:? "index")
      )
