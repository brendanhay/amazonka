{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.PlatformFramework
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.PlatformFramework
  ( PlatformFramework (..),

    -- * Smart constructor
    mkPlatformFramework,

    -- * Lenses
    pfName,
    pfVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A framework supported by the platform.
--
-- /See:/ 'mkPlatformFramework' smart constructor.
data PlatformFramework = PlatformFramework'
  { -- | The name of the framework.
    name :: Lude.Maybe Lude.Text,
    -- | The version of the framework.
    version :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PlatformFramework' with the minimum fields required to make a request.
--
-- * 'name' - The name of the framework.
-- * 'version' - The version of the framework.
mkPlatformFramework ::
  PlatformFramework
mkPlatformFramework =
  PlatformFramework' {name = Lude.Nothing, version = Lude.Nothing}

-- | The name of the framework.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfName :: Lens.Lens' PlatformFramework (Lude.Maybe Lude.Text)
pfName = Lens.lens (name :: PlatformFramework -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: PlatformFramework)
{-# DEPRECATED pfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the framework.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfVersion :: Lens.Lens' PlatformFramework (Lude.Maybe Lude.Text)
pfVersion = Lens.lens (version :: PlatformFramework -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: PlatformFramework)
{-# DEPRECATED pfVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.FromXML PlatformFramework where
  parseXML x =
    PlatformFramework'
      Lude.<$> (x Lude..@? "Name") Lude.<*> (x Lude..@? "Version")
