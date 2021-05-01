{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.Types.OutputConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.OutputConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.TargetDevice
import Network.AWS.SageMaker.Types.TargetPlatform

-- | Contains information about the output location for the compiled model
-- and the target device that the model runs on. @TargetDevice@ and
-- @TargetPlatform@ are mutually exclusive, so you need to choose one
-- between the two to specify your target device or platform. If you cannot
-- find your device you want to use from the @TargetDevice@ list, use
-- @TargetPlatform@ to describe the platform of your edge device and
-- @CompilerOptions@ if there are specific settings that are required or
-- recommended to use for particular TargetPlatform.
--
-- /See:/ 'newOutputConfig' smart constructor.
data OutputConfig = OutputConfig'
  { -- | Specifies additional parameters for compiler options in JSON format. The
    -- compiler options are @TargetPlatform@ specific. It is required for
    -- NVIDIA accelerators and highly recommended for CPU compilations. For any
    -- other cases, it is optional to specify @CompilerOptions.@
    --
    -- -   @CPU@: Compilation for CPU supports the following compiler options.
    --
    --     -   @mcpu@: CPU micro-architecture. For example,
    --         @{\'mcpu\': \'skylake-avx512\'}@
    --
    --     -   @mattr@: CPU flags. For example,
    --         @{\'mattr\': [\'+neon\', \'+vfpv4\']}@
    --
    -- -   @ARM@: Details of ARM CPU compilations.
    --
    --     -   @NEON@: NEON is an implementation of the Advanced SIMD extension
    --         used in ARMv7 processors.
    --
    --         For example, add @{\'mattr\': [\'+neon\']}@ to the compiler
    --         options if compiling for ARM 32-bit platform with the NEON
    --         support.
    --
    -- -   @NVIDIA@: Compilation for NVIDIA GPU supports the following compiler
    --     options.
    --
    --     -   @gpu_code@: Specifies the targeted architecture.
    --
    --     -   @trt-ver@: Specifies the TensorRT versions in x.y.z. format.
    --
    --     -   @cuda-ver@: Specifies the CUDA version in x.y format.
    --
    --     For example,
    --     @{\'gpu-code\': \'sm_72\', \'trt-ver\': \'6.0.1\', \'cuda-ver\': \'10.1\'}@
    --
    -- -   @ANDROID@: Compilation for the Android OS supports the following
    --     compiler options:
    --
    --     -   @ANDROID_PLATFORM@: Specifies the Android API levels. Available
    --         levels range from 21 to 29. For example,
    --         @{\'ANDROID_PLATFORM\': 28}@.
    --
    --     -   @mattr@: Add @{\'mattr\': [\'+neon\']}@ to compiler options if
    --         compiling for ARM 32-bit platform with NEON support.
    --
    -- -   @INFERENTIA@: Compilation for target ml_inf1 uses compiler options
    --     passed in as a JSON string. For example,
    --     @\"CompilerOptions\": \"\\\"--verbose 1 --num-neuroncores 2 -O2\\\"\"@.
    --
    --     For information about supported compiler options, see
    --     <https://github.com/aws/aws-neuron-sdk/blob/master/docs/neuron-cc/command-line-reference.md Neuron Compiler CLI>.
    --
    -- -   @CoreML@: Compilation for the CoreML OutputConfig$TargetDevice
    --     supports the following compiler options:
    --
    --     -   @class_labels@: Specifies the classification labels file name
    --         inside input tar.gz file. For example,
    --         @{\"class_labels\": \"imagenet_labels_1000.txt\"}@. Labels
    --         inside the txt file should be separated by newlines.
    compilerOptions :: Prelude.Maybe Prelude.Text,
    -- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses
    -- to encrypt data on the storage volume after compilation job. If you
    -- don\'t provide a KMS key ID, Amazon SageMaker uses the default KMS key
    -- for Amazon S3 for your role\'s account
    --
    -- The KmsKeyId can be any of the following formats:
    --
    -- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Key ARN:
    --     @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Alias name: @alias\/ExampleAlias@
    --
    -- -   Alias name ARN:
    --     @arn:aws:kms:us-west-2:111122223333:alias\/ExampleAlias@
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Identifies the target device or the machine learning instance that you
    -- want to run your model on after the compilation has completed.
    -- Alternatively, you can specify OS, architecture, and accelerator using
    -- TargetPlatform fields. It can be used instead of @TargetPlatform@.
    targetDevice :: Prelude.Maybe TargetDevice,
    -- | Contains information about a target platform that you want your model to
    -- run on, such as OS, architecture, and accelerators. It is an alternative
    -- of @TargetDevice@.
    --
    -- The following examples show how to configure the @TargetPlatform@ and
    -- @CompilerOptions@ JSON strings for popular target platforms:
    --
    -- -   Raspberry Pi 3 Model B+
    --
    --     @\"TargetPlatform\": {\"Os\": \"LINUX\", \"Arch\": \"ARM_EABIHF\"},@
    --
    --     @ \"CompilerOptions\": {\'mattr\': [\'+neon\']}@
    --
    -- -   Jetson TX2
    --
    --     @\"TargetPlatform\": {\"Os\": \"LINUX\", \"Arch\": \"ARM64\", \"Accelerator\": \"NVIDIA\"},@
    --
    --     @ \"CompilerOptions\": {\'gpu-code\': \'sm_62\', \'trt-ver\': \'6.0.1\', \'cuda-ver\': \'10.0\'}@
    --
    -- -   EC2 m5.2xlarge instance OS
    --
    --     @\"TargetPlatform\": {\"Os\": \"LINUX\", \"Arch\": \"X86_64\", \"Accelerator\": \"NVIDIA\"},@
    --
    --     @ \"CompilerOptions\": {\'mcpu\': \'skylake-avx512\'}@
    --
    -- -   RK3399
    --
    --     @\"TargetPlatform\": {\"Os\": \"LINUX\", \"Arch\": \"ARM64\", \"Accelerator\": \"MALI\"}@
    --
    -- -   ARMv7 phone (CPU)
    --
    --     @\"TargetPlatform\": {\"Os\": \"ANDROID\", \"Arch\": \"ARM_EABI\"},@
    --
    --     @ \"CompilerOptions\": {\'ANDROID_PLATFORM\': 25, \'mattr\': [\'+neon\']}@
    --
    -- -   ARMv8 phone (CPU)
    --
    --     @\"TargetPlatform\": {\"Os\": \"ANDROID\", \"Arch\": \"ARM64\"},@
    --
    --     @ \"CompilerOptions\": {\'ANDROID_PLATFORM\': 29}@
    targetPlatform :: Prelude.Maybe TargetPlatform,
    -- | Identifies the S3 bucket where you want Amazon SageMaker to store the
    -- model artifacts. For example, @s3:\/\/bucket-name\/key-name-prefix@.
    s3OutputLocation :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OutputConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compilerOptions', 'outputConfig_compilerOptions' - Specifies additional parameters for compiler options in JSON format. The
-- compiler options are @TargetPlatform@ specific. It is required for
-- NVIDIA accelerators and highly recommended for CPU compilations. For any
-- other cases, it is optional to specify @CompilerOptions.@
--
-- -   @CPU@: Compilation for CPU supports the following compiler options.
--
--     -   @mcpu@: CPU micro-architecture. For example,
--         @{\'mcpu\': \'skylake-avx512\'}@
--
--     -   @mattr@: CPU flags. For example,
--         @{\'mattr\': [\'+neon\', \'+vfpv4\']}@
--
-- -   @ARM@: Details of ARM CPU compilations.
--
--     -   @NEON@: NEON is an implementation of the Advanced SIMD extension
--         used in ARMv7 processors.
--
--         For example, add @{\'mattr\': [\'+neon\']}@ to the compiler
--         options if compiling for ARM 32-bit platform with the NEON
--         support.
--
-- -   @NVIDIA@: Compilation for NVIDIA GPU supports the following compiler
--     options.
--
--     -   @gpu_code@: Specifies the targeted architecture.
--
--     -   @trt-ver@: Specifies the TensorRT versions in x.y.z. format.
--
--     -   @cuda-ver@: Specifies the CUDA version in x.y format.
--
--     For example,
--     @{\'gpu-code\': \'sm_72\', \'trt-ver\': \'6.0.1\', \'cuda-ver\': \'10.1\'}@
--
-- -   @ANDROID@: Compilation for the Android OS supports the following
--     compiler options:
--
--     -   @ANDROID_PLATFORM@: Specifies the Android API levels. Available
--         levels range from 21 to 29. For example,
--         @{\'ANDROID_PLATFORM\': 28}@.
--
--     -   @mattr@: Add @{\'mattr\': [\'+neon\']}@ to compiler options if
--         compiling for ARM 32-bit platform with NEON support.
--
-- -   @INFERENTIA@: Compilation for target ml_inf1 uses compiler options
--     passed in as a JSON string. For example,
--     @\"CompilerOptions\": \"\\\"--verbose 1 --num-neuroncores 2 -O2\\\"\"@.
--
--     For information about supported compiler options, see
--     <https://github.com/aws/aws-neuron-sdk/blob/master/docs/neuron-cc/command-line-reference.md Neuron Compiler CLI>.
--
-- -   @CoreML@: Compilation for the CoreML OutputConfig$TargetDevice
--     supports the following compiler options:
--
--     -   @class_labels@: Specifies the classification labels file name
--         inside input tar.gz file. For example,
--         @{\"class_labels\": \"imagenet_labels_1000.txt\"}@. Labels
--         inside the txt file should be separated by newlines.
--
-- 'kmsKeyId', 'outputConfig_kmsKeyId' - The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses
-- to encrypt data on the storage volume after compilation job. If you
-- don\'t provide a KMS key ID, Amazon SageMaker uses the default KMS key
-- for Amazon S3 for your role\'s account
--
-- The KmsKeyId can be any of the following formats:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Alias name: @alias\/ExampleAlias@
--
-- -   Alias name ARN:
--     @arn:aws:kms:us-west-2:111122223333:alias\/ExampleAlias@
--
-- 'targetDevice', 'outputConfig_targetDevice' - Identifies the target device or the machine learning instance that you
-- want to run your model on after the compilation has completed.
-- Alternatively, you can specify OS, architecture, and accelerator using
-- TargetPlatform fields. It can be used instead of @TargetPlatform@.
--
-- 'targetPlatform', 'outputConfig_targetPlatform' - Contains information about a target platform that you want your model to
-- run on, such as OS, architecture, and accelerators. It is an alternative
-- of @TargetDevice@.
--
-- The following examples show how to configure the @TargetPlatform@ and
-- @CompilerOptions@ JSON strings for popular target platforms:
--
-- -   Raspberry Pi 3 Model B+
--
--     @\"TargetPlatform\": {\"Os\": \"LINUX\", \"Arch\": \"ARM_EABIHF\"},@
--
--     @ \"CompilerOptions\": {\'mattr\': [\'+neon\']}@
--
-- -   Jetson TX2
--
--     @\"TargetPlatform\": {\"Os\": \"LINUX\", \"Arch\": \"ARM64\", \"Accelerator\": \"NVIDIA\"},@
--
--     @ \"CompilerOptions\": {\'gpu-code\': \'sm_62\', \'trt-ver\': \'6.0.1\', \'cuda-ver\': \'10.0\'}@
--
-- -   EC2 m5.2xlarge instance OS
--
--     @\"TargetPlatform\": {\"Os\": \"LINUX\", \"Arch\": \"X86_64\", \"Accelerator\": \"NVIDIA\"},@
--
--     @ \"CompilerOptions\": {\'mcpu\': \'skylake-avx512\'}@
--
-- -   RK3399
--
--     @\"TargetPlatform\": {\"Os\": \"LINUX\", \"Arch\": \"ARM64\", \"Accelerator\": \"MALI\"}@
--
-- -   ARMv7 phone (CPU)
--
--     @\"TargetPlatform\": {\"Os\": \"ANDROID\", \"Arch\": \"ARM_EABI\"},@
--
--     @ \"CompilerOptions\": {\'ANDROID_PLATFORM\': 25, \'mattr\': [\'+neon\']}@
--
-- -   ARMv8 phone (CPU)
--
--     @\"TargetPlatform\": {\"Os\": \"ANDROID\", \"Arch\": \"ARM64\"},@
--
--     @ \"CompilerOptions\": {\'ANDROID_PLATFORM\': 29}@
--
-- 's3OutputLocation', 'outputConfig_s3OutputLocation' - Identifies the S3 bucket where you want Amazon SageMaker to store the
-- model artifacts. For example, @s3:\/\/bucket-name\/key-name-prefix@.
newOutputConfig ::
  -- | 's3OutputLocation'
  Prelude.Text ->
  OutputConfig
newOutputConfig pS3OutputLocation_ =
  OutputConfig'
    { compilerOptions = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      targetDevice = Prelude.Nothing,
      targetPlatform = Prelude.Nothing,
      s3OutputLocation = pS3OutputLocation_
    }

-- | Specifies additional parameters for compiler options in JSON format. The
-- compiler options are @TargetPlatform@ specific. It is required for
-- NVIDIA accelerators and highly recommended for CPU compilations. For any
-- other cases, it is optional to specify @CompilerOptions.@
--
-- -   @CPU@: Compilation for CPU supports the following compiler options.
--
--     -   @mcpu@: CPU micro-architecture. For example,
--         @{\'mcpu\': \'skylake-avx512\'}@
--
--     -   @mattr@: CPU flags. For example,
--         @{\'mattr\': [\'+neon\', \'+vfpv4\']}@
--
-- -   @ARM@: Details of ARM CPU compilations.
--
--     -   @NEON@: NEON is an implementation of the Advanced SIMD extension
--         used in ARMv7 processors.
--
--         For example, add @{\'mattr\': [\'+neon\']}@ to the compiler
--         options if compiling for ARM 32-bit platform with the NEON
--         support.
--
-- -   @NVIDIA@: Compilation for NVIDIA GPU supports the following compiler
--     options.
--
--     -   @gpu_code@: Specifies the targeted architecture.
--
--     -   @trt-ver@: Specifies the TensorRT versions in x.y.z. format.
--
--     -   @cuda-ver@: Specifies the CUDA version in x.y format.
--
--     For example,
--     @{\'gpu-code\': \'sm_72\', \'trt-ver\': \'6.0.1\', \'cuda-ver\': \'10.1\'}@
--
-- -   @ANDROID@: Compilation for the Android OS supports the following
--     compiler options:
--
--     -   @ANDROID_PLATFORM@: Specifies the Android API levels. Available
--         levels range from 21 to 29. For example,
--         @{\'ANDROID_PLATFORM\': 28}@.
--
--     -   @mattr@: Add @{\'mattr\': [\'+neon\']}@ to compiler options if
--         compiling for ARM 32-bit platform with NEON support.
--
-- -   @INFERENTIA@: Compilation for target ml_inf1 uses compiler options
--     passed in as a JSON string. For example,
--     @\"CompilerOptions\": \"\\\"--verbose 1 --num-neuroncores 2 -O2\\\"\"@.
--
--     For information about supported compiler options, see
--     <https://github.com/aws/aws-neuron-sdk/blob/master/docs/neuron-cc/command-line-reference.md Neuron Compiler CLI>.
--
-- -   @CoreML@: Compilation for the CoreML OutputConfig$TargetDevice
--     supports the following compiler options:
--
--     -   @class_labels@: Specifies the classification labels file name
--         inside input tar.gz file. For example,
--         @{\"class_labels\": \"imagenet_labels_1000.txt\"}@. Labels
--         inside the txt file should be separated by newlines.
outputConfig_compilerOptions :: Lens.Lens' OutputConfig (Prelude.Maybe Prelude.Text)
outputConfig_compilerOptions = Lens.lens (\OutputConfig' {compilerOptions} -> compilerOptions) (\s@OutputConfig' {} a -> s {compilerOptions = a} :: OutputConfig)

-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses
-- to encrypt data on the storage volume after compilation job. If you
-- don\'t provide a KMS key ID, Amazon SageMaker uses the default KMS key
-- for Amazon S3 for your role\'s account
--
-- The KmsKeyId can be any of the following formats:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Alias name: @alias\/ExampleAlias@
--
-- -   Alias name ARN:
--     @arn:aws:kms:us-west-2:111122223333:alias\/ExampleAlias@
outputConfig_kmsKeyId :: Lens.Lens' OutputConfig (Prelude.Maybe Prelude.Text)
outputConfig_kmsKeyId = Lens.lens (\OutputConfig' {kmsKeyId} -> kmsKeyId) (\s@OutputConfig' {} a -> s {kmsKeyId = a} :: OutputConfig)

-- | Identifies the target device or the machine learning instance that you
-- want to run your model on after the compilation has completed.
-- Alternatively, you can specify OS, architecture, and accelerator using
-- TargetPlatform fields. It can be used instead of @TargetPlatform@.
outputConfig_targetDevice :: Lens.Lens' OutputConfig (Prelude.Maybe TargetDevice)
outputConfig_targetDevice = Lens.lens (\OutputConfig' {targetDevice} -> targetDevice) (\s@OutputConfig' {} a -> s {targetDevice = a} :: OutputConfig)

-- | Contains information about a target platform that you want your model to
-- run on, such as OS, architecture, and accelerators. It is an alternative
-- of @TargetDevice@.
--
-- The following examples show how to configure the @TargetPlatform@ and
-- @CompilerOptions@ JSON strings for popular target platforms:
--
-- -   Raspberry Pi 3 Model B+
--
--     @\"TargetPlatform\": {\"Os\": \"LINUX\", \"Arch\": \"ARM_EABIHF\"},@
--
--     @ \"CompilerOptions\": {\'mattr\': [\'+neon\']}@
--
-- -   Jetson TX2
--
--     @\"TargetPlatform\": {\"Os\": \"LINUX\", \"Arch\": \"ARM64\", \"Accelerator\": \"NVIDIA\"},@
--
--     @ \"CompilerOptions\": {\'gpu-code\': \'sm_62\', \'trt-ver\': \'6.0.1\', \'cuda-ver\': \'10.0\'}@
--
-- -   EC2 m5.2xlarge instance OS
--
--     @\"TargetPlatform\": {\"Os\": \"LINUX\", \"Arch\": \"X86_64\", \"Accelerator\": \"NVIDIA\"},@
--
--     @ \"CompilerOptions\": {\'mcpu\': \'skylake-avx512\'}@
--
-- -   RK3399
--
--     @\"TargetPlatform\": {\"Os\": \"LINUX\", \"Arch\": \"ARM64\", \"Accelerator\": \"MALI\"}@
--
-- -   ARMv7 phone (CPU)
--
--     @\"TargetPlatform\": {\"Os\": \"ANDROID\", \"Arch\": \"ARM_EABI\"},@
--
--     @ \"CompilerOptions\": {\'ANDROID_PLATFORM\': 25, \'mattr\': [\'+neon\']}@
--
-- -   ARMv8 phone (CPU)
--
--     @\"TargetPlatform\": {\"Os\": \"ANDROID\", \"Arch\": \"ARM64\"},@
--
--     @ \"CompilerOptions\": {\'ANDROID_PLATFORM\': 29}@
outputConfig_targetPlatform :: Lens.Lens' OutputConfig (Prelude.Maybe TargetPlatform)
outputConfig_targetPlatform = Lens.lens (\OutputConfig' {targetPlatform} -> targetPlatform) (\s@OutputConfig' {} a -> s {targetPlatform = a} :: OutputConfig)

-- | Identifies the S3 bucket where you want Amazon SageMaker to store the
-- model artifacts. For example, @s3:\/\/bucket-name\/key-name-prefix@.
outputConfig_s3OutputLocation :: Lens.Lens' OutputConfig Prelude.Text
outputConfig_s3OutputLocation = Lens.lens (\OutputConfig' {s3OutputLocation} -> s3OutputLocation) (\s@OutputConfig' {} a -> s {s3OutputLocation = a} :: OutputConfig)

instance Prelude.FromJSON OutputConfig where
  parseJSON =
    Prelude.withObject
      "OutputConfig"
      ( \x ->
          OutputConfig'
            Prelude.<$> (x Prelude..:? "CompilerOptions")
            Prelude.<*> (x Prelude..:? "KmsKeyId")
            Prelude.<*> (x Prelude..:? "TargetDevice")
            Prelude.<*> (x Prelude..:? "TargetPlatform")
            Prelude.<*> (x Prelude..: "S3OutputLocation")
      )

instance Prelude.Hashable OutputConfig

instance Prelude.NFData OutputConfig

instance Prelude.ToJSON OutputConfig where
  toJSON OutputConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CompilerOptions" Prelude..=)
              Prelude.<$> compilerOptions,
            ("KmsKeyId" Prelude..=) Prelude.<$> kmsKeyId,
            ("TargetDevice" Prelude..=) Prelude.<$> targetDevice,
            ("TargetPlatform" Prelude..=)
              Prelude.<$> targetPlatform,
            Prelude.Just
              ("S3OutputLocation" Prelude..= s3OutputLocation)
          ]
      )
