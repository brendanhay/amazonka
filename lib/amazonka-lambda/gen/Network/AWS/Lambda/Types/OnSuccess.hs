{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.OnSuccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.OnSuccess
  ( OnSuccess (..),

    -- * Smart constructor
    mkOnSuccess,

    -- * Lenses
    osDestination,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A destination for events that were processed successfully.
--
-- /See:/ 'mkOnSuccess' smart constructor.
newtype OnSuccess = OnSuccess'
  { -- | The Amazon Resource Name (ARN) of the destination resource.
    destination :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OnSuccess' with the minimum fields required to make a request.
--
-- * 'destination' - The Amazon Resource Name (ARN) of the destination resource.
mkOnSuccess ::
  OnSuccess
mkOnSuccess = OnSuccess' {destination = Lude.Nothing}

-- | The Amazon Resource Name (ARN) of the destination resource.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osDestination :: Lens.Lens' OnSuccess (Lude.Maybe Lude.Text)
osDestination = Lens.lens (destination :: OnSuccess -> Lude.Maybe Lude.Text) (\s a -> s {destination = a} :: OnSuccess)
{-# DEPRECATED osDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

instance Lude.FromJSON OnSuccess where
  parseJSON =
    Lude.withObject
      "OnSuccess"
      (\x -> OnSuccess' Lude.<$> (x Lude..:? "Destination"))

instance Lude.ToJSON OnSuccess where
  toJSON OnSuccess' {..} =
    Lude.object
      (Lude.catMaybes [("Destination" Lude..=) Lude.<$> destination])
