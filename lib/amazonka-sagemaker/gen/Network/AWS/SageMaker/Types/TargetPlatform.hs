{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TargetPlatform
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.TargetPlatform
  ( TargetPlatform (..)
  -- * Smart constructor
  , mkTargetPlatform
  -- * Lenses
  , tpOs
  , tpArch
  , tpAccelerator
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.TargetPlatformAccelerator as Types
import qualified Network.AWS.SageMaker.Types.TargetPlatformArch as Types
import qualified Network.AWS.SageMaker.Types.TargetPlatformOs as Types

-- | Contains information about a target platform that you want your model to run on, such as OS, architecture, and accelerators. It is an alternative of @TargetDevice@ .
--
-- /See:/ 'mkTargetPlatform' smart constructor.
data TargetPlatform = TargetPlatform'
  { os :: Types.TargetPlatformOs
    -- ^ Specifies a target platform OS.
--
--
--     * @LINUX@ : Linux-based operating systems.
--
--
--     * @ANDROID@ : Android operating systems. Android API level can be specified using the @ANDROID_PLATFORM@ compiler option. For example, @"CompilerOptions": {'ANDROID_PLATFORM': 28}@ 
--
--
  , arch :: Types.TargetPlatformArch
    -- ^ Specifies a target platform architecture.
--
--
--     * @X86_64@ : 64-bit version of the x86 instruction set.
--
--
--     * @X86@ : 32-bit version of the x86 instruction set.
--
--
--     * @ARM64@ : ARMv8 64-bit CPU.
--
--
--     * @ARM_EABIHF@ : ARMv7 32-bit, Hard Float.
--
--
--     * @ARM_EABI@ : ARMv7 32-bit, Soft Float. Used by Android 32-bit ARM platform.
--
--
  , accelerator :: Core.Maybe Types.TargetPlatformAccelerator
    -- ^ Specifies a target platform accelerator (optional).
--
--
--     * @NVIDIA@ : Nvidia graphics processing unit. It also requires @gpu-code@ , @trt-ver@ , @cuda-ver@ compiler options
--
--
--     * @MALI@ : ARM Mali graphics processor
--
--
--     * @INTEL_GRAPHICS@ : Integrated Intel graphics
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TargetPlatform' value with any optional fields omitted.
mkTargetPlatform
    :: Types.TargetPlatformOs -- ^ 'os'
    -> Types.TargetPlatformArch -- ^ 'arch'
    -> TargetPlatform
mkTargetPlatform os arch
  = TargetPlatform'{os, arch, accelerator = Core.Nothing}

-- | Specifies a target platform OS.
--
--
--     * @LINUX@ : Linux-based operating systems.
--
--
--     * @ANDROID@ : Android operating systems. Android API level can be specified using the @ANDROID_PLATFORM@ compiler option. For example, @"CompilerOptions": {'ANDROID_PLATFORM': 28}@ 
--
--
--
-- /Note:/ Consider using 'os' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpOs :: Lens.Lens' TargetPlatform Types.TargetPlatformOs
tpOs = Lens.field @"os"
{-# INLINEABLE tpOs #-}
{-# DEPRECATED os "Use generic-lens or generic-optics with 'os' instead"  #-}

-- | Specifies a target platform architecture.
--
--
--     * @X86_64@ : 64-bit version of the x86 instruction set.
--
--
--     * @X86@ : 32-bit version of the x86 instruction set.
--
--
--     * @ARM64@ : ARMv8 64-bit CPU.
--
--
--     * @ARM_EABIHF@ : ARMv7 32-bit, Hard Float.
--
--
--     * @ARM_EABI@ : ARMv7 32-bit, Soft Float. Used by Android 32-bit ARM platform.
--
--
--
-- /Note:/ Consider using 'arch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpArch :: Lens.Lens' TargetPlatform Types.TargetPlatformArch
tpArch = Lens.field @"arch"
{-# INLINEABLE tpArch #-}
{-# DEPRECATED arch "Use generic-lens or generic-optics with 'arch' instead"  #-}

-- | Specifies a target platform accelerator (optional).
--
--
--     * @NVIDIA@ : Nvidia graphics processing unit. It also requires @gpu-code@ , @trt-ver@ , @cuda-ver@ compiler options
--
--
--     * @MALI@ : ARM Mali graphics processor
--
--
--     * @INTEL_GRAPHICS@ : Integrated Intel graphics
--
--
--
-- /Note:/ Consider using 'accelerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpAccelerator :: Lens.Lens' TargetPlatform (Core.Maybe Types.TargetPlatformAccelerator)
tpAccelerator = Lens.field @"accelerator"
{-# INLINEABLE tpAccelerator #-}
{-# DEPRECATED accelerator "Use generic-lens or generic-optics with 'accelerator' instead"  #-}

instance Core.FromJSON TargetPlatform where
        toJSON TargetPlatform{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Os" Core..= os), Core.Just ("Arch" Core..= arch),
                  ("Accelerator" Core..=) Core.<$> accelerator])

instance Core.FromJSON TargetPlatform where
        parseJSON
          = Core.withObject "TargetPlatform" Core.$
              \ x ->
                TargetPlatform' Core.<$>
                  (x Core..: "Os") Core.<*> x Core..: "Arch" Core.<*>
                    x Core..:? "Accelerator"
