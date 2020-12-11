-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.TrailInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.TrailInfo
  ( TrailInfo (..),

    -- * Smart constructor
    mkTrailInfo,

    -- * Lenses
    tiTrailARN,
    tiHomeRegion,
    tiName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a CloudTrail trail, including the trail's name, home region, and Amazon Resource Name (ARN).
--
-- /See:/ 'mkTrailInfo' smart constructor.
data TrailInfo = TrailInfo'
  { trailARN :: Lude.Maybe Lude.Text,
    homeRegion :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TrailInfo' with the minimum fields required to make a request.
--
-- * 'homeRegion' - The AWS region in which a trail was created.
-- * 'name' - The name of a trail.
-- * 'trailARN' - The ARN of a trail.
mkTrailInfo ::
  TrailInfo
mkTrailInfo =
  TrailInfo'
    { trailARN = Lude.Nothing,
      homeRegion = Lude.Nothing,
      name = Lude.Nothing
    }

-- | The ARN of a trail.
--
-- /Note:/ Consider using 'trailARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiTrailARN :: Lens.Lens' TrailInfo (Lude.Maybe Lude.Text)
tiTrailARN = Lens.lens (trailARN :: TrailInfo -> Lude.Maybe Lude.Text) (\s a -> s {trailARN = a} :: TrailInfo)
{-# DEPRECATED tiTrailARN "Use generic-lens or generic-optics with 'trailARN' instead." #-}

-- | The AWS region in which a trail was created.
--
-- /Note:/ Consider using 'homeRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiHomeRegion :: Lens.Lens' TrailInfo (Lude.Maybe Lude.Text)
tiHomeRegion = Lens.lens (homeRegion :: TrailInfo -> Lude.Maybe Lude.Text) (\s a -> s {homeRegion = a} :: TrailInfo)
{-# DEPRECATED tiHomeRegion "Use generic-lens or generic-optics with 'homeRegion' instead." #-}

-- | The name of a trail.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiName :: Lens.Lens' TrailInfo (Lude.Maybe Lude.Text)
tiName = Lens.lens (name :: TrailInfo -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: TrailInfo)
{-# DEPRECATED tiName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON TrailInfo where
  parseJSON =
    Lude.withObject
      "TrailInfo"
      ( \x ->
          TrailInfo'
            Lude.<$> (x Lude..:? "TrailARN")
            Lude.<*> (x Lude..:? "HomeRegion")
            Lude.<*> (x Lude..:? "Name")
      )
