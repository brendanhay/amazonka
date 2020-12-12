{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CPUOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CPUOptions
  ( CPUOptions (..),

    -- * Smart constructor
    mkCPUOptions,

    -- * Lenses
    coCoreCount,
    coThreadsPerCore,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The CPU options for the instance.
--
-- /See:/ 'mkCPUOptions' smart constructor.
data CPUOptions = CPUOptions'
  { coreCount :: Lude.Maybe Lude.Int,
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

-- | Creates a value of 'CPUOptions' with the minimum fields required to make a request.
--
-- * 'coreCount' - The number of CPU cores for the instance.
-- * 'threadsPerCore' - The number of threads per CPU core.
mkCPUOptions ::
  CPUOptions
mkCPUOptions =
  CPUOptions'
    { coreCount = Lude.Nothing,
      threadsPerCore = Lude.Nothing
    }

-- | The number of CPU cores for the instance.
--
-- /Note:/ Consider using 'coreCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coCoreCount :: Lens.Lens' CPUOptions (Lude.Maybe Lude.Int)
coCoreCount = Lens.lens (coreCount :: CPUOptions -> Lude.Maybe Lude.Int) (\s a -> s {coreCount = a} :: CPUOptions)
{-# DEPRECATED coCoreCount "Use generic-lens or generic-optics with 'coreCount' instead." #-}

-- | The number of threads per CPU core.
--
-- /Note:/ Consider using 'threadsPerCore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coThreadsPerCore :: Lens.Lens' CPUOptions (Lude.Maybe Lude.Int)
coThreadsPerCore = Lens.lens (threadsPerCore :: CPUOptions -> Lude.Maybe Lude.Int) (\s a -> s {threadsPerCore = a} :: CPUOptions)
{-# DEPRECATED coThreadsPerCore "Use generic-lens or generic-optics with 'threadsPerCore' instead." #-}

instance Lude.FromXML CPUOptions where
  parseXML x =
    CPUOptions'
      Lude.<$> (x Lude..@? "coreCount") Lude.<*> (x Lude..@? "threadsPerCore")
