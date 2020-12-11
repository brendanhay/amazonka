-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.LaunchPath
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.LaunchPath
  ( LaunchPath (..),

    -- * Smart constructor
    mkLaunchPath,

    -- * Lenses
    lpName,
    lpId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A launch path object.
--
-- /See:/ 'mkLaunchPath' smart constructor.
data LaunchPath = LaunchPath'
  { name :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchPath' with the minimum fields required to make a request.
--
-- * 'id' - The identifier of the launch path.
-- * 'name' - The name of the launch path.
mkLaunchPath ::
  LaunchPath
mkLaunchPath = LaunchPath' {name = Lude.Nothing, id = Lude.Nothing}

-- | The name of the launch path.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpName :: Lens.Lens' LaunchPath (Lude.Maybe Lude.Text)
lpName = Lens.lens (name :: LaunchPath -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: LaunchPath)
{-# DEPRECATED lpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier of the launch path.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpId :: Lens.Lens' LaunchPath (Lude.Maybe Lude.Text)
lpId = Lens.lens (id :: LaunchPath -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: LaunchPath)
{-# DEPRECATED lpId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON LaunchPath where
  parseJSON =
    Lude.withObject
      "LaunchPath"
      ( \x ->
          LaunchPath'
            Lude.<$> (x Lude..:? "Name") Lude.<*> (x Lude..:? "Id")
      )
