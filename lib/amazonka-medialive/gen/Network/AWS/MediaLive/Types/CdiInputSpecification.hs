{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.CdiInputSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.CdiInputSpecification
  ( CdiInputSpecification (..),

    -- * Smart constructor
    mkCdiInputSpecification,

    -- * Lenses
    cisResolution,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.CdiInputResolution
import qualified Network.AWS.Prelude as Lude

-- | Placeholder documentation for CdiInputSpecification
--
-- /See:/ 'mkCdiInputSpecification' smart constructor.
newtype CdiInputSpecification = CdiInputSpecification'
  { -- | Maximum CDI input resolution
    resolution :: Lude.Maybe CdiInputResolution
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CdiInputSpecification' with the minimum fields required to make a request.
--
-- * 'resolution' - Maximum CDI input resolution
mkCdiInputSpecification ::
  CdiInputSpecification
mkCdiInputSpecification =
  CdiInputSpecification' {resolution = Lude.Nothing}

-- | Maximum CDI input resolution
--
-- /Note:/ Consider using 'resolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisResolution :: Lens.Lens' CdiInputSpecification (Lude.Maybe CdiInputResolution)
cisResolution = Lens.lens (resolution :: CdiInputSpecification -> Lude.Maybe CdiInputResolution) (\s a -> s {resolution = a} :: CdiInputSpecification)
{-# DEPRECATED cisResolution "Use generic-lens or generic-optics with 'resolution' instead." #-}

instance Lude.FromJSON CdiInputSpecification where
  parseJSON =
    Lude.withObject
      "CdiInputSpecification"
      (\x -> CdiInputSpecification' Lude.<$> (x Lude..:? "resolution"))

instance Lude.ToJSON CdiInputSpecification where
  toJSON CdiInputSpecification' {..} =
    Lude.object
      (Lude.catMaybes [("resolution" Lude..=) Lude.<$> resolution])
