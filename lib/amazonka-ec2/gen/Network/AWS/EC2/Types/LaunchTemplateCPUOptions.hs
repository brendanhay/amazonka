-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateCPUOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateCPUOptions
  ( LaunchTemplateCPUOptions (..),

    -- * Smart constructor
    mkLaunchTemplateCPUOptions,

    -- * Lenses
    ltcoCoreCount,
    ltcoThreadsPerCore,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The CPU options for the instance.
--
-- /See:/ 'mkLaunchTemplateCPUOptions' smart constructor.
data LaunchTemplateCPUOptions = LaunchTemplateCPUOptions'
  { coreCount ::
      Lude.Maybe Lude.Int,
    threadsPerCore :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchTemplateCPUOptions' with the minimum fields required to make a request.
--
-- * 'coreCount' - The number of CPU cores for the instance.
-- * 'threadsPerCore' - The number of threads per CPU core.
mkLaunchTemplateCPUOptions ::
  LaunchTemplateCPUOptions
mkLaunchTemplateCPUOptions =
  LaunchTemplateCPUOptions'
    { coreCount = Lude.Nothing,
      threadsPerCore = Lude.Nothing
    }

-- | The number of CPU cores for the instance.
--
-- /Note:/ Consider using 'coreCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcoCoreCount :: Lens.Lens' LaunchTemplateCPUOptions (Lude.Maybe Lude.Int)
ltcoCoreCount = Lens.lens (coreCount :: LaunchTemplateCPUOptions -> Lude.Maybe Lude.Int) (\s a -> s {coreCount = a} :: LaunchTemplateCPUOptions)
{-# DEPRECATED ltcoCoreCount "Use generic-lens or generic-optics with 'coreCount' instead." #-}

-- | The number of threads per CPU core.
--
-- /Note:/ Consider using 'threadsPerCore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcoThreadsPerCore :: Lens.Lens' LaunchTemplateCPUOptions (Lude.Maybe Lude.Int)
ltcoThreadsPerCore = Lens.lens (threadsPerCore :: LaunchTemplateCPUOptions -> Lude.Maybe Lude.Int) (\s a -> s {threadsPerCore = a} :: LaunchTemplateCPUOptions)
{-# DEPRECATED ltcoThreadsPerCore "Use generic-lens or generic-optics with 'threadsPerCore' instead." #-}

instance Lude.FromXML LaunchTemplateCPUOptions where
  parseXML x =
    LaunchTemplateCPUOptions'
      Lude.<$> (x Lude..@? "coreCount") Lude.<*> (x Lude..@? "threadsPerCore")
