{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.OutputConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.OutputConfig
  ( OutputConfig (..),

    -- * Smart constructor
    mkOutputConfig,

    -- * Lenses
    ocS3OutputLocation,
    ocCompilerOptions,
    ocTargetDevice,
    ocTargetPlatform,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.CompilerOptions as Types
import qualified Network.AWS.SageMaker.Types.S3OutputLocation as Types
import qualified Network.AWS.SageMaker.Types.TargetDevice as Types
import qualified Network.AWS.SageMaker.Types.TargetPlatform as Types

-- | Contains information about the output location for the compiled model and the target device that the model runs on. @TargetDevice@ and @TargetPlatform@ are mutually exclusive, so you need to choose one between the two to specify your target device or platform. If you cannot find your device you want to use from the @TargetDevice@ list, use @TargetPlatform@ to describe the platform of your edge device and @CompilerOptions@ if there are specific settings that are required or recommended to use for particular TargetPlatform.
--
-- /See:/ 'mkOutputConfig' smart constructor.
data OutputConfig = OutputConfig'
  { -- | Identifies the S3 bucket where you want Amazon SageMaker to store the model artifacts. For example, @s3://bucket-name/key-name-prefix@ .
    s3OutputLocation :: Types.S3OutputLocation,
    -- | Specifies additional parameters for compiler options in JSON format. The compiler options are @TargetPlatform@ specific. It is required for NVIDIA accelerators and highly recommended for CPU compilations. For any other cases, it is optional to specify @CompilerOptions.@
    --
    --
    --     * @CPU@ : Compilation for CPU supports the following compiler options.
    --
    --     * @mcpu@ : CPU micro-architecture. For example, @{'mcpu': 'skylake-avx512'}@
    --
    --
    --     * @mattr@ : CPU flags. For example, @{'mattr': ['+neon', '+vfpv4']}@
    --
    --
    --
    --
    --     * @ARM@ : Details of ARM CPU compilations.
    --
    --     * @NEON@ : NEON is an implementation of the Advanced SIMD extension used in ARMv7 processors.
    -- For example, add @{'mattr': ['+neon']}@ to the compiler options if compiling for ARM 32-bit platform with the NEON support.
    --
    --
    --
    --
    --     * @NVIDIA@ : Compilation for NVIDIA GPU supports the following compiler options.
    --
    --     * @gpu_code@ : Specifies the targeted architecture.
    --
    --
    --     * @trt-ver@ : Specifies the TensorRT versions in x.y.z. format.
    --
    --
    --     * @cuda-ver@ : Specifies the CUDA version in x.y format.
    --
    --
    -- For example, @{'gpu-code': 'sm_72', 'trt-ver': '6.0.1', 'cuda-ver': '10.1'}@
    --
    --
    --     * @ANDROID@ : Compilation for the Android OS supports the following compiler options:
    --
    --     * @ANDROID_PLATFORM@ : Specifies the Android API levels. Available levels range from 21 to 29. For example, @{'ANDROID_PLATFORM': 28}@ .
    --
    --
    --     * @mattr@ : Add @{'mattr': ['+neon']}@ to compiler options if compiling for ARM 32-bit platform with NEON support.
    --
    --
    --
    --
    --     * @INFERENTIA@ : Compilation for target ml_inf1 uses compiler options passed in as a JSON string. For example, @"CompilerOptions": "\"--verbose 1 --num-neuroncores 2 -O2\""@ .
    -- For information about supported compiler options, see <https://github.com/aws/aws-neuron-sdk/blob/master/docs/neuron-cc/command-line-reference.md Neuron Compiler CLI> .
    --
    --
    --     * @CoreML@ : Compilation for the CoreML 'OutputConfig$TargetDevice' supports the following compiler options:
    --
    --     * @class_labels@ : Specifies the classification labels file name inside input tar.gz file. For example, @{"class_labels": "imagenet_labels_1000.txt"}@ . Labels inside the txt file should be separated by newlines.
    compilerOptions :: Core.Maybe Types.CompilerOptions,
    -- | Identifies the target device or the machine learning instance that you want to run your model on after the compilation has completed. Alternatively, you can specify OS, architecture, and accelerator using 'TargetPlatform' fields. It can be used instead of @TargetPlatform@ .
    targetDevice :: Core.Maybe Types.TargetDevice,
    -- | Contains information about a target platform that you want your model to run on, such as OS, architecture, and accelerators. It is an alternative of @TargetDevice@ .
    --
    -- The following examples show how to configure the @TargetPlatform@ and @CompilerOptions@ JSON strings for popular target platforms:
    --
    --     * Raspberry Pi 3 Model B+
    -- @"TargetPlatform": {"Os": "LINUX", "Arch": "ARM_EABIHF"},@
    -- @"CompilerOptions": {'mattr': ['+neon']}@
    --
    --
    --     * Jetson TX2
    -- @"TargetPlatform": {"Os": "LINUX", "Arch": "ARM64", "Accelerator": "NVIDIA"},@
    -- @"CompilerOptions": {'gpu-code': 'sm_62', 'trt-ver': '6.0.1', 'cuda-ver': '10.0'}@
    --
    --
    --     * EC2 m5.2xlarge instance OS
    -- @"TargetPlatform": {"Os": "LINUX", "Arch": "X86_64", "Accelerator": "NVIDIA"},@
    -- @"CompilerOptions": {'mcpu': 'skylake-avx512'}@
    --
    --
    --     * RK3399
    -- @"TargetPlatform": {"Os": "LINUX", "Arch": "ARM64", "Accelerator": "MALI"}@
    --
    --
    --     * ARMv7 phone (CPU)
    -- @"TargetPlatform": {"Os": "ANDROID", "Arch": "ARM_EABI"},@
    -- @"CompilerOptions": {'ANDROID_PLATFORM': 25, 'mattr': ['+neon']}@
    --
    --
    --     * ARMv8 phone (CPU)
    -- @"TargetPlatform": {"Os": "ANDROID", "Arch": "ARM64"},@
    -- @"CompilerOptions": {'ANDROID_PLATFORM': 29}@
    targetPlatform :: Core.Maybe Types.TargetPlatform
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OutputConfig' value with any optional fields omitted.
mkOutputConfig ::
  -- | 's3OutputLocation'
  Types.S3OutputLocation ->
  OutputConfig
mkOutputConfig s3OutputLocation =
  OutputConfig'
    { s3OutputLocation,
      compilerOptions = Core.Nothing,
      targetDevice = Core.Nothing,
      targetPlatform = Core.Nothing
    }

-- | Identifies the S3 bucket where you want Amazon SageMaker to store the model artifacts. For example, @s3://bucket-name/key-name-prefix@ .
--
-- /Note:/ Consider using 's3OutputLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocS3OutputLocation :: Lens.Lens' OutputConfig Types.S3OutputLocation
ocS3OutputLocation = Lens.field @"s3OutputLocation"
{-# DEPRECATED ocS3OutputLocation "Use generic-lens or generic-optics with 's3OutputLocation' instead." #-}

-- | Specifies additional parameters for compiler options in JSON format. The compiler options are @TargetPlatform@ specific. It is required for NVIDIA accelerators and highly recommended for CPU compilations. For any other cases, it is optional to specify @CompilerOptions.@
--
--
--     * @CPU@ : Compilation for CPU supports the following compiler options.
--
--     * @mcpu@ : CPU micro-architecture. For example, @{'mcpu': 'skylake-avx512'}@
--
--
--     * @mattr@ : CPU flags. For example, @{'mattr': ['+neon', '+vfpv4']}@
--
--
--
--
--     * @ARM@ : Details of ARM CPU compilations.
--
--     * @NEON@ : NEON is an implementation of the Advanced SIMD extension used in ARMv7 processors.
-- For example, add @{'mattr': ['+neon']}@ to the compiler options if compiling for ARM 32-bit platform with the NEON support.
--
--
--
--
--     * @NVIDIA@ : Compilation for NVIDIA GPU supports the following compiler options.
--
--     * @gpu_code@ : Specifies the targeted architecture.
--
--
--     * @trt-ver@ : Specifies the TensorRT versions in x.y.z. format.
--
--
--     * @cuda-ver@ : Specifies the CUDA version in x.y format.
--
--
-- For example, @{'gpu-code': 'sm_72', 'trt-ver': '6.0.1', 'cuda-ver': '10.1'}@
--
--
--     * @ANDROID@ : Compilation for the Android OS supports the following compiler options:
--
--     * @ANDROID_PLATFORM@ : Specifies the Android API levels. Available levels range from 21 to 29. For example, @{'ANDROID_PLATFORM': 28}@ .
--
--
--     * @mattr@ : Add @{'mattr': ['+neon']}@ to compiler options if compiling for ARM 32-bit platform with NEON support.
--
--
--
--
--     * @INFERENTIA@ : Compilation for target ml_inf1 uses compiler options passed in as a JSON string. For example, @"CompilerOptions": "\"--verbose 1 --num-neuroncores 2 -O2\""@ .
-- For information about supported compiler options, see <https://github.com/aws/aws-neuron-sdk/blob/master/docs/neuron-cc/command-line-reference.md Neuron Compiler CLI> .
--
--
--     * @CoreML@ : Compilation for the CoreML 'OutputConfig$TargetDevice' supports the following compiler options:
--
--     * @class_labels@ : Specifies the classification labels file name inside input tar.gz file. For example, @{"class_labels": "imagenet_labels_1000.txt"}@ . Labels inside the txt file should be separated by newlines.
--
--
--
--
--
-- /Note:/ Consider using 'compilerOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocCompilerOptions :: Lens.Lens' OutputConfig (Core.Maybe Types.CompilerOptions)
ocCompilerOptions = Lens.field @"compilerOptions"
{-# DEPRECATED ocCompilerOptions "Use generic-lens or generic-optics with 'compilerOptions' instead." #-}

-- | Identifies the target device or the machine learning instance that you want to run your model on after the compilation has completed. Alternatively, you can specify OS, architecture, and accelerator using 'TargetPlatform' fields. It can be used instead of @TargetPlatform@ .
--
-- /Note:/ Consider using 'targetDevice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocTargetDevice :: Lens.Lens' OutputConfig (Core.Maybe Types.TargetDevice)
ocTargetDevice = Lens.field @"targetDevice"
{-# DEPRECATED ocTargetDevice "Use generic-lens or generic-optics with 'targetDevice' instead." #-}

-- | Contains information about a target platform that you want your model to run on, such as OS, architecture, and accelerators. It is an alternative of @TargetDevice@ .
--
-- The following examples show how to configure the @TargetPlatform@ and @CompilerOptions@ JSON strings for popular target platforms:
--
--     * Raspberry Pi 3 Model B+
-- @"TargetPlatform": {"Os": "LINUX", "Arch": "ARM_EABIHF"},@
-- @"CompilerOptions": {'mattr': ['+neon']}@
--
--
--     * Jetson TX2
-- @"TargetPlatform": {"Os": "LINUX", "Arch": "ARM64", "Accelerator": "NVIDIA"},@
-- @"CompilerOptions": {'gpu-code': 'sm_62', 'trt-ver': '6.0.1', 'cuda-ver': '10.0'}@
--
--
--     * EC2 m5.2xlarge instance OS
-- @"TargetPlatform": {"Os": "LINUX", "Arch": "X86_64", "Accelerator": "NVIDIA"},@
-- @"CompilerOptions": {'mcpu': 'skylake-avx512'}@
--
--
--     * RK3399
-- @"TargetPlatform": {"Os": "LINUX", "Arch": "ARM64", "Accelerator": "MALI"}@
--
--
--     * ARMv7 phone (CPU)
-- @"TargetPlatform": {"Os": "ANDROID", "Arch": "ARM_EABI"},@
-- @"CompilerOptions": {'ANDROID_PLATFORM': 25, 'mattr': ['+neon']}@
--
--
--     * ARMv8 phone (CPU)
-- @"TargetPlatform": {"Os": "ANDROID", "Arch": "ARM64"},@
-- @"CompilerOptions": {'ANDROID_PLATFORM': 29}@
--
--
--
-- /Note:/ Consider using 'targetPlatform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocTargetPlatform :: Lens.Lens' OutputConfig (Core.Maybe Types.TargetPlatform)
ocTargetPlatform = Lens.field @"targetPlatform"
{-# DEPRECATED ocTargetPlatform "Use generic-lens or generic-optics with 'targetPlatform' instead." #-}

instance Core.FromJSON OutputConfig where
  toJSON OutputConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("S3OutputLocation" Core..= s3OutputLocation),
            ("CompilerOptions" Core..=) Core.<$> compilerOptions,
            ("TargetDevice" Core..=) Core.<$> targetDevice,
            ("TargetPlatform" Core..=) Core.<$> targetPlatform
          ]
      )

instance Core.FromJSON OutputConfig where
  parseJSON =
    Core.withObject "OutputConfig" Core.$
      \x ->
        OutputConfig'
          Core.<$> (x Core..: "S3OutputLocation")
          Core.<*> (x Core..:? "CompilerOptions")
          Core.<*> (x Core..:? "TargetDevice")
          Core.<*> (x Core..:? "TargetPlatform")
