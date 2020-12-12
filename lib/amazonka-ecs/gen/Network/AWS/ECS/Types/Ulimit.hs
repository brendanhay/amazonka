{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Ulimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Ulimit
  ( Ulimit (..),

    -- * Smart constructor
    mkUlimit,

    -- * Lenses
    uName,
    uSoftLimit,
    uHardLimit,
  )
where

import Network.AWS.ECS.Types.UlimitName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The @ulimit@ settings to pass to the container.
--
-- /See:/ 'mkUlimit' smart constructor.
data Ulimit = Ulimit'
  { name :: UlimitName,
    softLimit :: Lude.Int,
    hardLimit :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Ulimit' with the minimum fields required to make a request.
--
-- * 'hardLimit' - The hard limit for the ulimit type.
-- * 'name' - The @type@ of the @ulimit@ .
-- * 'softLimit' - The soft limit for the ulimit type.
mkUlimit ::
  -- | 'name'
  UlimitName ->
  -- | 'softLimit'
  Lude.Int ->
  -- | 'hardLimit'
  Lude.Int ->
  Ulimit
mkUlimit pName_ pSoftLimit_ pHardLimit_ =
  Ulimit'
    { name = pName_,
      softLimit = pSoftLimit_,
      hardLimit = pHardLimit_
    }

-- | The @type@ of the @ulimit@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uName :: Lens.Lens' Ulimit UlimitName
uName = Lens.lens (name :: Ulimit -> UlimitName) (\s a -> s {name = a} :: Ulimit)
{-# DEPRECATED uName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The soft limit for the ulimit type.
--
-- /Note:/ Consider using 'softLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uSoftLimit :: Lens.Lens' Ulimit Lude.Int
uSoftLimit = Lens.lens (softLimit :: Ulimit -> Lude.Int) (\s a -> s {softLimit = a} :: Ulimit)
{-# DEPRECATED uSoftLimit "Use generic-lens or generic-optics with 'softLimit' instead." #-}

-- | The hard limit for the ulimit type.
--
-- /Note:/ Consider using 'hardLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uHardLimit :: Lens.Lens' Ulimit Lude.Int
uHardLimit = Lens.lens (hardLimit :: Ulimit -> Lude.Int) (\s a -> s {hardLimit = a} :: Ulimit)
{-# DEPRECATED uHardLimit "Use generic-lens or generic-optics with 'hardLimit' instead." #-}

instance Lude.FromJSON Ulimit where
  parseJSON =
    Lude.withObject
      "Ulimit"
      ( \x ->
          Ulimit'
            Lude.<$> (x Lude..: "name")
            Lude.<*> (x Lude..: "softLimit")
            Lude.<*> (x Lude..: "hardLimit")
      )

instance Lude.ToJSON Ulimit where
  toJSON Ulimit' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("name" Lude..= name),
            Lude.Just ("softLimit" Lude..= softLimit),
            Lude.Just ("hardLimit" Lude..= hardLimit)
          ]
      )
