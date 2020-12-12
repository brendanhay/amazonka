{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.OnFailure
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.OnFailure
  ( OnFailure (..),

    -- * Smart constructor
    mkOnFailure,

    -- * Lenses
    ofDestination,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A destination for events that failed processing.
--
-- /See:/ 'mkOnFailure' smart constructor.
newtype OnFailure = OnFailure' {destination :: Lude.Maybe Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OnFailure' with the minimum fields required to make a request.
--
-- * 'destination' - The Amazon Resource Name (ARN) of the destination resource.
mkOnFailure ::
  OnFailure
mkOnFailure = OnFailure' {destination = Lude.Nothing}

-- | The Amazon Resource Name (ARN) of the destination resource.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ofDestination :: Lens.Lens' OnFailure (Lude.Maybe Lude.Text)
ofDestination = Lens.lens (destination :: OnFailure -> Lude.Maybe Lude.Text) (\s a -> s {destination = a} :: OnFailure)
{-# DEPRECATED ofDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

instance Lude.FromJSON OnFailure where
  parseJSON =
    Lude.withObject
      "OnFailure"
      (\x -> OnFailure' Lude.<$> (x Lude..:? "Destination"))

instance Lude.ToJSON OnFailure where
  toJSON OnFailure' {..} =
    Lude.object
      (Lude.catMaybes [("Destination" Lude..=) Lude.<$> destination])
