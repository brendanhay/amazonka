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
    ocTargetPlatform,
    ocCompilerOptions,
    ocTargetDevice,
    ocS3OutputLocation,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.TargetDevice
import Network.AWS.SageMaker.Types.TargetPlatform

-- | Contains information about the output location for the compiled model and the target device that the model runs on. @TargetDevice@ and @TargetPlatform@ are mutually exclusive, so you need to choose one between the two to specify your target device or platform. If you cannot find your device you want to use from the @TargetDevice@ list, use @TargetPlatform@ to describe the platform of your edge device and @CompilerOptions@ if there are specific settings that are required or recommended to use for particular TargetPlatform.
--
-- /See:/ 'mkOutputConfig' smart constructor.
data OutputConfig = OutputConfig'
  { targetPlatform ::
      Lude.Maybe TargetPlatform,
    compilerOptions :: Lude.Maybe Lude.Text,
    targetDevice :: Lude.Maybe TargetDevice,
    s3OutputLocation :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OutputConfig' with the minimum fields required to make a request.
--
-- * 'compilerOptions' - Specifies additional parameters for compiler options in JSON format. The compiler options are @TargetPlatform@ specific. It is required for NVIDIA accelerators and highly recommended for CPU compilations. For any other cases, it is optional to specify @CompilerOptions.@
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
-- * 's3OutputLocation' - Identifies the S3 bucket where you want Amazon SageMaker to store the model artifacts. For example, @s3://bucket-name/key-name-prefix@ .
-- * 'targetDevice' - Identifies the target device or the machine learning instance that you want to run your model on after the compilation has completed. Alternatively, you can specify OS, architecture, and accelerator using 'TargetPlatform' fields. It can be used instead of @TargetPlatform@ .
-- * 'targetPlatform' - Contains information about a target platform that you want your model to run on, such as OS, architecture, and accelerators. It is an alternative of @TargetDevice@ .
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
mkOutputConfig ::
  -- | 's3OutputLocation'
  Lude.Text ->
  OutputConfig
mkOutputConfig pS3OutputLocation_ =
  OutputConfig'
    { targetPlatform = Lude.Nothing,
      compilerOptions = Lude.Nothing,
      targetDevice = Lude.Nothing,
      s3OutputLocation = pS3OutputLocation_
    }

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
ocTargetPlatform :: Lens.Lens' OutputConfig (Lude.Maybe TargetPlatform)
ocTargetPlatform = Lens.lens (targetPlatform :: OutputConfig -> Lude.Maybe TargetPlatform) (\s a -> s {targetPlatform = a} :: OutputConfig)
{-# DEPRECATED ocTargetPlatform "Use generic-lens or generic-optics with 'targetPlatform' instead." #-}

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
ocCompilerOptions :: Lens.Lens' OutputConfig (Lude.Maybe Lude.Text)
ocCompilerOptions = Lens.lens (compilerOptions :: OutputConfig -> Lude.Maybe Lude.Text) (\s a -> s {compilerOptions = a} :: OutputConfig)
{-# DEPRECATED ocCompilerOptions "Use generic-lens or generic-optics with 'compilerOptions' instead." #-}

-- | Identifies the target device or the machine learning instance that you want to run your model on after the compilation has completed. Alternatively, you can specify OS, architecture, and accelerator using 'TargetPlatform' fields. It can be used instead of @TargetPlatform@ .
--
-- /Note:/ Consider using 'targetDevice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocTargetDevice :: Lens.Lens' OutputConfig (Lude.Maybe TargetDevice)
ocTargetDevice = Lens.lens (targetDevice :: OutputConfig -> Lude.Maybe TargetDevice) (\s a -> s {targetDevice = a} :: OutputConfig)
{-# DEPRECATED ocTargetDevice "Use generic-lens or generic-optics with 'targetDevice' instead." #-}

-- | Identifies the S3 bucket where you want Amazon SageMaker to store the model artifacts. For example, @s3://bucket-name/key-name-prefix@ .
--
-- /Note:/ Consider using 's3OutputLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocS3OutputLocation :: Lens.Lens' OutputConfig Lude.Text
ocS3OutputLocation = Lens.lens (s3OutputLocation :: OutputConfig -> Lude.Text) (\s a -> s {s3OutputLocation = a} :: OutputConfig)
{-# DEPRECATED ocS3OutputLocation "Use generic-lens or generic-optics with 's3OutputLocation' instead." #-}

instance Lude.FromJSON OutputConfig where
  parseJSON =
    Lude.withObject
      "OutputConfig"
      ( \x ->
          OutputConfig'
            Lude.<$> (x Lude..:? "TargetPlatform")
            Lude.<*> (x Lude..:? "CompilerOptions")
            Lude.<*> (x Lude..:? "TargetDevice")
            Lude.<*> (x Lude..: "S3OutputLocation")
      )

instance Lude.ToJSON OutputConfig where
  toJSON OutputConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TargetPlatform" Lude..=) Lude.<$> targetPlatform,
            ("CompilerOptions" Lude..=) Lude.<$> compilerOptions,
            ("TargetDevice" Lude..=) Lude.<$> targetDevice,
            Lude.Just ("S3OutputLocation" Lude..= s3OutputLocation)
          ]
      )
