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
-- Module      : Amazonka.LookoutVision.Types.TargetPlatform
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutVision.Types.TargetPlatform where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutVision.Types.TargetPlatformAccelerator
import Amazonka.LookoutVision.Types.TargetPlatformArch
import Amazonka.LookoutVision.Types.TargetPlatformOs
import qualified Amazonka.Prelude as Prelude

-- | The platform on which a model runs on an AWS IoT Greengrass core device.
--
-- /See:/ 'newTargetPlatform' smart constructor.
data TargetPlatform = TargetPlatform'
  { -- | The target accelerator for the model. Currently, Amazon Lookout for
    -- Vision only supports NVIDIA (Nvidia graphics processing unit) and CPU
    -- accelerators. If you specify NVIDIA as an accelerator, you must also
    -- specify the @gpu-code@, @trt-ver@, and @cuda-ver@ compiler options. If
    -- you don\'t specify an accelerator, Lookout for Vision uses the CPU for
    -- compilation and we highly recommend that you use the
    -- GreengrassConfiguration$CompilerOptions field. For example, you can use
    -- the following compiler options for CPU:
    --
    -- -   @mcpu@: CPU micro-architecture. For example,
    --     @{\'mcpu\': \'skylake-avx512\'}@
    --
    -- -   @mattr@: CPU flags. For example,
    --     @{\'mattr\': [\'+neon\', \'+vfpv4\']}@
    accelerator :: Prelude.Maybe TargetPlatformAccelerator,
    -- | The target operating system for the model. Linux is the only operating
    -- system that is currently supported.
    os :: TargetPlatformOs,
    -- | The target architecture for the model. The currently supported
    -- architectures are X86_64 (64-bit version of the x86 instruction set) and
    -- ARM_64 (ARMv8 64-bit CPU).
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
-- 'accelerator', 'targetPlatform_accelerator' - The target accelerator for the model. Currently, Amazon Lookout for
-- Vision only supports NVIDIA (Nvidia graphics processing unit) and CPU
-- accelerators. If you specify NVIDIA as an accelerator, you must also
-- specify the @gpu-code@, @trt-ver@, and @cuda-ver@ compiler options. If
-- you don\'t specify an accelerator, Lookout for Vision uses the CPU for
-- compilation and we highly recommend that you use the
-- GreengrassConfiguration$CompilerOptions field. For example, you can use
-- the following compiler options for CPU:
--
-- -   @mcpu@: CPU micro-architecture. For example,
--     @{\'mcpu\': \'skylake-avx512\'}@
--
-- -   @mattr@: CPU flags. For example,
--     @{\'mattr\': [\'+neon\', \'+vfpv4\']}@
--
-- 'os', 'targetPlatform_os' - The target operating system for the model. Linux is the only operating
-- system that is currently supported.
--
-- 'arch', 'targetPlatform_arch' - The target architecture for the model. The currently supported
-- architectures are X86_64 (64-bit version of the x86 instruction set) and
-- ARM_64 (ARMv8 64-bit CPU).
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

-- | The target accelerator for the model. Currently, Amazon Lookout for
-- Vision only supports NVIDIA (Nvidia graphics processing unit) and CPU
-- accelerators. If you specify NVIDIA as an accelerator, you must also
-- specify the @gpu-code@, @trt-ver@, and @cuda-ver@ compiler options. If
-- you don\'t specify an accelerator, Lookout for Vision uses the CPU for
-- compilation and we highly recommend that you use the
-- GreengrassConfiguration$CompilerOptions field. For example, you can use
-- the following compiler options for CPU:
--
-- -   @mcpu@: CPU micro-architecture. For example,
--     @{\'mcpu\': \'skylake-avx512\'}@
--
-- -   @mattr@: CPU flags. For example,
--     @{\'mattr\': [\'+neon\', \'+vfpv4\']}@
targetPlatform_accelerator :: Lens.Lens' TargetPlatform (Prelude.Maybe TargetPlatformAccelerator)
targetPlatform_accelerator = Lens.lens (\TargetPlatform' {accelerator} -> accelerator) (\s@TargetPlatform' {} a -> s {accelerator = a} :: TargetPlatform)

-- | The target operating system for the model. Linux is the only operating
-- system that is currently supported.
targetPlatform_os :: Lens.Lens' TargetPlatform TargetPlatformOs
targetPlatform_os = Lens.lens (\TargetPlatform' {os} -> os) (\s@TargetPlatform' {} a -> s {os = a} :: TargetPlatform)

-- | The target architecture for the model. The currently supported
-- architectures are X86_64 (64-bit version of the x86 instruction set) and
-- ARM_64 (ARMv8 64-bit CPU).
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
    Prelude.rnf accelerator `Prelude.seq`
      Prelude.rnf os `Prelude.seq`
        Prelude.rnf arch

instance Data.ToJSON TargetPlatform where
  toJSON TargetPlatform' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Accelerator" Data..=) Prelude.<$> accelerator,
            Prelude.Just ("Os" Data..= os),
            Prelude.Just ("Arch" Data..= arch)
          ]
      )
