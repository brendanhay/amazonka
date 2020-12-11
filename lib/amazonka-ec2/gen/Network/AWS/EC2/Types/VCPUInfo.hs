-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VCPUInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VCPUInfo
  ( VCPUInfo (..),

    -- * Smart constructor
    mkVCPUInfo,

    -- * Lenses
    vciValidThreadsPerCore,
    vciDefaultThreadsPerCore,
    vciDefaultVCPUs,
    vciDefaultCores,
    vciValidCores,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the vCPU configurations for the instance type.
--
-- /See:/ 'mkVCPUInfo' smart constructor.
data VCPUInfo = VCPUInfo'
  { validThreadsPerCore ::
      Lude.Maybe [Lude.Int],
    defaultThreadsPerCore :: Lude.Maybe Lude.Int,
    defaultVCPUs :: Lude.Maybe Lude.Int,
    defaultCores :: Lude.Maybe Lude.Int,
    validCores :: Lude.Maybe [Lude.Int]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VCPUInfo' with the minimum fields required to make a request.
--
-- * 'defaultCores' - The default number of cores for the instance type.
-- * 'defaultThreadsPerCore' - The default number of threads per core for the instance type.
-- * 'defaultVCPUs' - The default number of vCPUs for the instance type.
-- * 'validCores' - The valid number of cores that can be configured for the instance type.
-- * 'validThreadsPerCore' - The valid number of threads per core that can be configured for the instance type.
mkVCPUInfo ::
  VCPUInfo
mkVCPUInfo =
  VCPUInfo'
    { validThreadsPerCore = Lude.Nothing,
      defaultThreadsPerCore = Lude.Nothing,
      defaultVCPUs = Lude.Nothing,
      defaultCores = Lude.Nothing,
      validCores = Lude.Nothing
    }

-- | The valid number of threads per core that can be configured for the instance type.
--
-- /Note:/ Consider using 'validThreadsPerCore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vciValidThreadsPerCore :: Lens.Lens' VCPUInfo (Lude.Maybe [Lude.Int])
vciValidThreadsPerCore = Lens.lens (validThreadsPerCore :: VCPUInfo -> Lude.Maybe [Lude.Int]) (\s a -> s {validThreadsPerCore = a} :: VCPUInfo)
{-# DEPRECATED vciValidThreadsPerCore "Use generic-lens or generic-optics with 'validThreadsPerCore' instead." #-}

-- | The default number of threads per core for the instance type.
--
-- /Note:/ Consider using 'defaultThreadsPerCore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vciDefaultThreadsPerCore :: Lens.Lens' VCPUInfo (Lude.Maybe Lude.Int)
vciDefaultThreadsPerCore = Lens.lens (defaultThreadsPerCore :: VCPUInfo -> Lude.Maybe Lude.Int) (\s a -> s {defaultThreadsPerCore = a} :: VCPUInfo)
{-# DEPRECATED vciDefaultThreadsPerCore "Use generic-lens or generic-optics with 'defaultThreadsPerCore' instead." #-}

-- | The default number of vCPUs for the instance type.
--
-- /Note:/ Consider using 'defaultVCPUs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vciDefaultVCPUs :: Lens.Lens' VCPUInfo (Lude.Maybe Lude.Int)
vciDefaultVCPUs = Lens.lens (defaultVCPUs :: VCPUInfo -> Lude.Maybe Lude.Int) (\s a -> s {defaultVCPUs = a} :: VCPUInfo)
{-# DEPRECATED vciDefaultVCPUs "Use generic-lens or generic-optics with 'defaultVCPUs' instead." #-}

-- | The default number of cores for the instance type.
--
-- /Note:/ Consider using 'defaultCores' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vciDefaultCores :: Lens.Lens' VCPUInfo (Lude.Maybe Lude.Int)
vciDefaultCores = Lens.lens (defaultCores :: VCPUInfo -> Lude.Maybe Lude.Int) (\s a -> s {defaultCores = a} :: VCPUInfo)
{-# DEPRECATED vciDefaultCores "Use generic-lens or generic-optics with 'defaultCores' instead." #-}

-- | The valid number of cores that can be configured for the instance type.
--
-- /Note:/ Consider using 'validCores' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vciValidCores :: Lens.Lens' VCPUInfo (Lude.Maybe [Lude.Int])
vciValidCores = Lens.lens (validCores :: VCPUInfo -> Lude.Maybe [Lude.Int]) (\s a -> s {validCores = a} :: VCPUInfo)
{-# DEPRECATED vciValidCores "Use generic-lens or generic-optics with 'validCores' instead." #-}

instance Lude.FromXML VCPUInfo where
  parseXML x =
    VCPUInfo'
      Lude.<$> ( x Lude..@? "validThreadsPerCore" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "defaultThreadsPerCore")
      Lude.<*> (x Lude..@? "defaultVCpus")
      Lude.<*> (x Lude..@? "defaultCores")
      Lude.<*> ( x Lude..@? "validCores" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
