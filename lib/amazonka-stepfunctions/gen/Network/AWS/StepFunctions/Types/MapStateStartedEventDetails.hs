{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.MapStateStartedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.MapStateStartedEventDetails
  ( MapStateStartedEventDetails (..),

    -- * Smart constructor
    mkMapStateStartedEventDetails,

    -- * Lenses
    mssedLength,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details about a Map state that was started.
--
-- /See:/ 'mkMapStateStartedEventDetails' smart constructor.
newtype MapStateStartedEventDetails = MapStateStartedEventDetails'
  { length ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MapStateStartedEventDetails' with the minimum fields required to make a request.
--
-- * 'length' - The size of the array for Map state iterations.
mkMapStateStartedEventDetails ::
  MapStateStartedEventDetails
mkMapStateStartedEventDetails =
  MapStateStartedEventDetails' {length = Lude.Nothing}

-- | The size of the array for Map state iterations.
--
-- /Note:/ Consider using 'length' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssedLength :: Lens.Lens' MapStateStartedEventDetails (Lude.Maybe Lude.Natural)
mssedLength = Lens.lens (length :: MapStateStartedEventDetails -> Lude.Maybe Lude.Natural) (\s a -> s {length = a} :: MapStateStartedEventDetails)
{-# DEPRECATED mssedLength "Use generic-lens or generic-optics with 'length' instead." #-}

instance Lude.FromJSON MapStateStartedEventDetails where
  parseJSON =
    Lude.withObject
      "MapStateStartedEventDetails"
      ( \x ->
          MapStateStartedEventDetails' Lude.<$> (x Lude..:? "length")
      )
