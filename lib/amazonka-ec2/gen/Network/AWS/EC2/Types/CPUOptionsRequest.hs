{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CPUOptionsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CPUOptionsRequest
  ( CPUOptionsRequest (..),

    -- * Smart constructor
    mkCPUOptionsRequest,

    -- * Lenses
    corCoreCount,
    corThreadsPerCore,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The CPU options for the instance. Both the core count and threads per core must be specified in the request.
--
-- /See:/ 'mkCPUOptionsRequest' smart constructor.
data CPUOptionsRequest = CPUOptionsRequest'
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

-- | Creates a value of 'CPUOptionsRequest' with the minimum fields required to make a request.
--
-- * 'coreCount' - The number of CPU cores for the instance.
-- * 'threadsPerCore' - The number of threads per CPU core. To disable multithreading for the instance, specify a value of @1@ . Otherwise, specify the default value of @2@ .
mkCPUOptionsRequest ::
  CPUOptionsRequest
mkCPUOptionsRequest =
  CPUOptionsRequest'
    { coreCount = Lude.Nothing,
      threadsPerCore = Lude.Nothing
    }

-- | The number of CPU cores for the instance.
--
-- /Note:/ Consider using 'coreCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corCoreCount :: Lens.Lens' CPUOptionsRequest (Lude.Maybe Lude.Int)
corCoreCount = Lens.lens (coreCount :: CPUOptionsRequest -> Lude.Maybe Lude.Int) (\s a -> s {coreCount = a} :: CPUOptionsRequest)
{-# DEPRECATED corCoreCount "Use generic-lens or generic-optics with 'coreCount' instead." #-}

-- | The number of threads per CPU core. To disable multithreading for the instance, specify a value of @1@ . Otherwise, specify the default value of @2@ .
--
-- /Note:/ Consider using 'threadsPerCore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corThreadsPerCore :: Lens.Lens' CPUOptionsRequest (Lude.Maybe Lude.Int)
corThreadsPerCore = Lens.lens (threadsPerCore :: CPUOptionsRequest -> Lude.Maybe Lude.Int) (\s a -> s {threadsPerCore = a} :: CPUOptionsRequest)
{-# DEPRECATED corThreadsPerCore "Use generic-lens or generic-optics with 'threadsPerCore' instead." #-}

instance Lude.ToQuery CPUOptionsRequest where
  toQuery CPUOptionsRequest' {..} =
    Lude.mconcat
      [ "CoreCount" Lude.=: coreCount,
        "ThreadsPerCore" Lude.=: threadsPerCore
      ]
