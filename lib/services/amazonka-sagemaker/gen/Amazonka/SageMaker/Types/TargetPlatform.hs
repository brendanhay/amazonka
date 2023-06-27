{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMaker.Types.TargetPlatform
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TargetPlatform where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.TargetPlatformAccelerator
import Amazonka.SageMaker.Types.TargetPlatformArch
import Amazonka.SageMaker.Types.TargetPlatformOs

-- | Contains information about a target platform that you want your model to
-- run on, such as OS, architecture, and accelerators. It is an alternative
-- of @TargetDevice@.
--
-- /See:/ 'newTargetPlatform' smart constructor.
data TargetPlatform = TargetPlatform'
  { -- | Specifies a target platform accelerator (optional).
    --
    -- -   @NVIDIA@: Nvidia graphics processing unit. It also requires
    --     @gpu-code@, @trt-ver@, @cuda-ver@ compiler options
    --
    -- -   @MALI@: ARM Mali graphics processor
    --
    -- -   @INTEL_GRAPHICS@: Integrated Intel graphics
    accelerator :: Prelude.Maybe TargetPlatformAccelerator,
    -- | Specifies a target platform OS.
    --
    -- -   @LINUX@: Linux-based operating systems.
    --
    -- -   @ANDROID@: Android operating systems. Android API level can be
    --     specified using the @ANDROID_PLATFORM@ compiler option. For example,
    --     @\"CompilerOptions\": {\'ANDROID_PLATFORM\': 28}@
    os :: TargetPlatformOs,
    -- | Specifies a target platform architecture.
    --
    -- -   @X86_64@: 64-bit version of the x86 instruction set.
    --
    -- -   @X86@: 32-bit version of the x86 instruction set.
    --
    -- -   @ARM64@: ARMv8 64-bit CPU.
    --
    -- -   @ARM_EABIHF@: ARMv7 32-bit, Hard Float.
    --
    -- -   @ARM_EABI@: ARMv7 32-bit, Soft Float. Used by Android 32-bit ARM
    --     platform.
    arch :: TargetPlatformArch
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetPlatform' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accelerator', 'targetPlatform_accelerator' - Specifies a target platform accelerator (optional).
--
-- -   @NVIDIA@: Nvidia graphics processing unit. It also requires
--     @gpu-code@, @trt-ver@, @cuda-ver@ compiler options
--
-- -   @MALI@: ARM Mali graphics processor
--
-- -   @INTEL_GRAPHICS@: Integrated Intel graphics
--
-- 'os', 'targetPlatform_os' - Specifies a target platform OS.
--
-- -   @LINUX@: Linux-based operating systems.
--
-- -   @ANDROID@: Android operating systems. Android API level can be
--     specified using the @ANDROID_PLATFORM@ compiler option. For example,
--     @\"CompilerOptions\": {\'ANDROID_PLATFORM\': 28}@
--
-- 'arch', 'targetPlatform_arch' - Specifies a target platform architecture.
--
-- -   @X86_64@: 64-bit version of the x86 instruction set.
--
-- -   @X86@: 32-bit version of the x86 instruction set.
--
-- -   @ARM64@: ARMv8 64-bit CPU.
--
-- -   @ARM_EABIHF@: ARMv7 32-bit, Hard Float.
--
-- -   @ARM_EABI@: ARMv7 32-bit, Soft Float. Used by Android 32-bit ARM
--     platform.
newTargetPlatform ::
  -- | 'os'
  TargetPlatformOs ->
  -- | 'arch'
  TargetPlatformArch ->
  TargetPlatform
newTargetPlatform pOs_ pArch_ =
  TargetPlatform'
    { accelerator = Prelude.Nothing,
      os = pOs_,
      arch = pArch_
    }

-- | Specifies a target platform accelerator (optional).
--
-- -   @NVIDIA@: Nvidia graphics processing unit. It also requires
--     @gpu-code@, @trt-ver@, @cuda-ver@ compiler options
--
-- -   @MALI@: ARM Mali graphics processor
--
-- -   @INTEL_GRAPHICS@: Integrated Intel graphics
targetPlatform_accelerator :: Lens.Lens' TargetPlatform (Prelude.Maybe TargetPlatformAccelerator)
targetPlatform_accelerator = Lens.lens (\TargetPlatform' {accelerator} -> accelerator) (\s@TargetPlatform' {} a -> s {accelerator = a} :: TargetPlatform)

-- | Specifies a target platform OS.
--
-- -   @LINUX@: Linux-based operating systems.
--
-- -   @ANDROID@: Android operating systems. Android API level can be
--     specified using the @ANDROID_PLATFORM@ compiler option. For example,
--     @\"CompilerOptions\": {\'ANDROID_PLATFORM\': 28}@
targetPlatform_os :: Lens.Lens' TargetPlatform TargetPlatformOs
targetPlatform_os = Lens.lens (\TargetPlatform' {os} -> os) (\s@TargetPlatform' {} a -> s {os = a} :: TargetPlatform)

-- | Specifies a target platform architecture.
--
-- -   @X86_64@: 64-bit version of the x86 instruction set.
--
-- -   @X86@: 32-bit version of the x86 instruction set.
--
-- -   @ARM64@: ARMv8 64-bit CPU.
--
-- -   @ARM_EABIHF@: ARMv7 32-bit, Hard Float.
--
-- -   @ARM_EABI@: ARMv7 32-bit, Soft Float. Used by Android 32-bit ARM
--     platform.
targetPlatform_arch :: Lens.Lens' TargetPlatform TargetPlatformArch
targetPlatform_arch = Lens.lens (\TargetPlatform' {arch} -> arch) (\s@TargetPlatform' {} a -> s {arch = a} :: TargetPlatform)

instance Data.FromJSON TargetPlatform where
  parseJSON =
    Data.withObject
      "TargetPlatform"
      ( \x ->
          TargetPlatform'
            Prelude.<$> (x Data..:? "Accelerator")
            Prelude.<*> (x Data..: "Os")
            Prelude.<*> (x Data..: "Arch")
      )

instance Prelude.Hashable TargetPlatform where
  hashWithSalt _salt TargetPlatform' {..} =
    _salt
      `Prelude.hashWithSalt` accelerator
      `Prelude.hashWithSalt` os
      `Prelude.hashWithSalt` arch

instance Prelude.NFData TargetPlatform where
  rnf TargetPlatform' {..} =
    Prelude.rnf accelerator
      `Prelude.seq` Prelude.rnf os
      `Prelude.seq` Prelude.rnf arch

instance Data.ToJSON TargetPlatform where
  toJSON TargetPlatform' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Accelerator" Data..=) Prelude.<$> accelerator,
            Prelude.Just ("Os" Data..= os),
            Prelude.Just ("Arch" Data..= arch)
          ]
      )
