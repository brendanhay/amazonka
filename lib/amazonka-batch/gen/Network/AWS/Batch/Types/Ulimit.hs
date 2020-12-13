{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.Ulimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.Ulimit
  ( Ulimit (..),

    -- * Smart constructor
    mkUlimit,

    -- * Lenses
    uName,
    uHardLimit,
    uSoftLimit,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The @ulimit@ settings to pass to the container.
--
-- /See:/ 'mkUlimit' smart constructor.
data Ulimit = Ulimit'
  { -- | The @type@ of the @ulimit@ .
    name :: Lude.Text,
    -- | The hard limit for the @ulimit@ type.
    hardLimit :: Lude.Int,
    -- | The soft limit for the @ulimit@ type.
    softLimit :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Ulimit' with the minimum fields required to make a request.
--
-- * 'name' - The @type@ of the @ulimit@ .
-- * 'hardLimit' - The hard limit for the @ulimit@ type.
-- * 'softLimit' - The soft limit for the @ulimit@ type.
mkUlimit ::
  -- | 'name'
  Lude.Text ->
  -- | 'hardLimit'
  Lude.Int ->
  -- | 'softLimit'
  Lude.Int ->
  Ulimit
mkUlimit pName_ pHardLimit_ pSoftLimit_ =
  Ulimit'
    { name = pName_,
      hardLimit = pHardLimit_,
      softLimit = pSoftLimit_
    }

-- | The @type@ of the @ulimit@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uName :: Lens.Lens' Ulimit Lude.Text
uName = Lens.lens (name :: Ulimit -> Lude.Text) (\s a -> s {name = a} :: Ulimit)
{-# DEPRECATED uName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The hard limit for the @ulimit@ type.
--
-- /Note:/ Consider using 'hardLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uHardLimit :: Lens.Lens' Ulimit Lude.Int
uHardLimit = Lens.lens (hardLimit :: Ulimit -> Lude.Int) (\s a -> s {hardLimit = a} :: Ulimit)
{-# DEPRECATED uHardLimit "Use generic-lens or generic-optics with 'hardLimit' instead." #-}

-- | The soft limit for the @ulimit@ type.
--
-- /Note:/ Consider using 'softLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uSoftLimit :: Lens.Lens' Ulimit Lude.Int
uSoftLimit = Lens.lens (softLimit :: Ulimit -> Lude.Int) (\s a -> s {softLimit = a} :: Ulimit)
{-# DEPRECATED uSoftLimit "Use generic-lens or generic-optics with 'softLimit' instead." #-}

instance Lude.FromJSON Ulimit where
  parseJSON =
    Lude.withObject
      "Ulimit"
      ( \x ->
          Ulimit'
            Lude.<$> (x Lude..: "name")
            Lude.<*> (x Lude..: "hardLimit")
            Lude.<*> (x Lude..: "softLimit")
      )

instance Lude.ToJSON Ulimit where
  toJSON Ulimit' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("name" Lude..= name),
            Lude.Just ("hardLimit" Lude..= hardLimit),
            Lude.Just ("softLimit" Lude..= softLimit)
          ]
      )
