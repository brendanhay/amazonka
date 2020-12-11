-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TargetPlatform
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TargetPlatform
  ( TargetPlatform (..),

    -- * Smart constructor
    mkTargetPlatform,

    -- * Lenses
    tpAccelerator,
    tpOS,
    tpArch,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.TargetPlatformAccelerator
import Network.AWS.SageMaker.Types.TargetPlatformArch
import Network.AWS.SageMaker.Types.TargetPlatformOS

-- | Contains information about a target platform that you want your model to run on, such as OS, architecture, and accelerators. It is an alternative of @TargetDevice@ .
--
-- /See:/ 'mkTargetPlatform' smart constructor.
data TargetPlatform = TargetPlatform'
  { accelerator ::
      Lude.Maybe TargetPlatformAccelerator,
    os :: TargetPlatformOS,
    arch :: TargetPlatformArch
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TargetPlatform' with the minimum fields required to make a request.
--
-- * 'accelerator' - Specifies a target platform accelerator (optional).
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
-- * 'arch' - Specifies a target platform architecture.
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
-- * 'os' - Specifies a target platform OS.
--
--
--     * @LINUX@ : Linux-based operating systems.
--
--
--     * @ANDROID@ : Android operating systems. Android API level can be specified using the @ANDROID_PLATFORM@ compiler option. For example, @"CompilerOptions": {'ANDROID_PLATFORM': 28}@
mkTargetPlatform ::
  -- | 'os'
  TargetPlatformOS ->
  -- | 'arch'
  TargetPlatformArch ->
  TargetPlatform
mkTargetPlatform pOS_ pArch_ =
  TargetPlatform'
    { accelerator = Lude.Nothing,
      os = pOS_,
      arch = pArch_
    }

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
tpAccelerator :: Lens.Lens' TargetPlatform (Lude.Maybe TargetPlatformAccelerator)
tpAccelerator = Lens.lens (accelerator :: TargetPlatform -> Lude.Maybe TargetPlatformAccelerator) (\s a -> s {accelerator = a} :: TargetPlatform)
{-# DEPRECATED tpAccelerator "Use generic-lens or generic-optics with 'accelerator' instead." #-}

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
tpOS :: Lens.Lens' TargetPlatform TargetPlatformOS
tpOS = Lens.lens (os :: TargetPlatform -> TargetPlatformOS) (\s a -> s {os = a} :: TargetPlatform)
{-# DEPRECATED tpOS "Use generic-lens or generic-optics with 'os' instead." #-}

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
tpArch :: Lens.Lens' TargetPlatform TargetPlatformArch
tpArch = Lens.lens (arch :: TargetPlatform -> TargetPlatformArch) (\s a -> s {arch = a} :: TargetPlatform)
{-# DEPRECATED tpArch "Use generic-lens or generic-optics with 'arch' instead." #-}

instance Lude.FromJSON TargetPlatform where
  parseJSON =
    Lude.withObject
      "TargetPlatform"
      ( \x ->
          TargetPlatform'
            Lude.<$> (x Lude..:? "Accelerator")
            Lude.<*> (x Lude..: "Os")
            Lude.<*> (x Lude..: "Arch")
      )

instance Lude.ToJSON TargetPlatform where
  toJSON TargetPlatform' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Accelerator" Lude..=) Lude.<$> accelerator,
            Lude.Just ("Os" Lude..= os),
            Lude.Just ("Arch" Lude..= arch)
          ]
      )
