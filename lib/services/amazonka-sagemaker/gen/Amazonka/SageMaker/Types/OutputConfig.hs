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
-- Module      : Amazonka.SageMaker.Types.OutputConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.OutputConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.TargetDevice
import Amazonka.SageMaker.Types.TargetPlatform

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
  { -- | Identifies the target device or the machine learning instance that you
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
    -- | The Amazon Web Services Key Management Service key (Amazon Web Services
    -- KMS) that Amazon SageMaker uses to encrypt your output models with
    -- Amazon S3 server-side encryption after compilation job. If you don\'t
    -- provide a KMS key ID, Amazon SageMaker uses the default KMS key for
    -- Amazon S3 for your role\'s account. For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/UsingKMSEncryption.html KMS-Managed Encryption Keys>
    -- in the /Amazon Simple Storage Service Developer Guide./
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
    -- | Specifies additional parameters for compiler options in JSON format. The
    -- compiler options are @TargetPlatform@ specific. It is required for
    -- NVIDIA accelerators and highly recommended for CPU compilations. For any
    -- other cases, it is optional to specify @CompilerOptions.@
    --
    -- -   @DTYPE@: Specifies the data type for the input. When compiling for
    --     @ml_*@ (except for @ml_inf@) instances using PyTorch framework,
    --     provide the data type (dtype) of the model\'s input. @\"float32\"@
    --     is used if @\"DTYPE\"@ is not specified. Options for data type are:
    --
    --     -   float32: Use either @\"float\"@ or @\"float32\"@.
    --
    --     -   int64: Use either @\"int64\"@ or @\"long\"@.
    --
    --     For example, @{\"dtype\" : \"float32\"}@.
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
    -- -   @EIA@: Compilation for the Elastic Inference Accelerator supports
    --     the following compiler options:
    --
    --     -   @precision_mode@: Specifies the precision of compiled artifacts.
    --         Supported values are @\"FP16\"@ and @\"FP32\"@. Default is
    --         @\"FP32\"@.
    --
    --     -   @signature_def_key@: Specifies the signature to use for models
    --         in SavedModel format. Defaults is TensorFlow\'s default
    --         signature def key.
    --
    --     -   @output_names@: Specifies a list of output tensor names for
    --         models in FrozenGraph format. Set at most one API field, either:
    --         @signature_def_key@ or @output_names@.
    --
    --     For example:
    --     @{\"precision_mode\": \"FP32\", \"output_names\": [\"output:0\"]}@
    compilerOptions :: Prelude.Maybe Prelude.Text,
    -- | Identifies the S3 bucket where you want Amazon SageMaker to store the
    -- model artifacts. For example, @s3:\/\/bucket-name\/key-name-prefix@.
    s3OutputLocation :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'kmsKeyId', 'outputConfig_kmsKeyId' - The Amazon Web Services Key Management Service key (Amazon Web Services
-- KMS) that Amazon SageMaker uses to encrypt your output models with
-- Amazon S3 server-side encryption after compilation job. If you don\'t
-- provide a KMS key ID, Amazon SageMaker uses the default KMS key for
-- Amazon S3 for your role\'s account. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/UsingKMSEncryption.html KMS-Managed Encryption Keys>
-- in the /Amazon Simple Storage Service Developer Guide./
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
-- 'compilerOptions', 'outputConfig_compilerOptions' - Specifies additional parameters for compiler options in JSON format. The
-- compiler options are @TargetPlatform@ specific. It is required for
-- NVIDIA accelerators and highly recommended for CPU compilations. For any
-- other cases, it is optional to specify @CompilerOptions.@
--
-- -   @DTYPE@: Specifies the data type for the input. When compiling for
--     @ml_*@ (except for @ml_inf@) instances using PyTorch framework,
--     provide the data type (dtype) of the model\'s input. @\"float32\"@
--     is used if @\"DTYPE\"@ is not specified. Options for data type are:
--
--     -   float32: Use either @\"float\"@ or @\"float32\"@.
--
--     -   int64: Use either @\"int64\"@ or @\"long\"@.
--
--     For example, @{\"dtype\" : \"float32\"}@.
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
-- -   @EIA@: Compilation for the Elastic Inference Accelerator supports
--     the following compiler options:
--
--     -   @precision_mode@: Specifies the precision of compiled artifacts.
--         Supported values are @\"FP16\"@ and @\"FP32\"@. Default is
--         @\"FP32\"@.
--
--     -   @signature_def_key@: Specifies the signature to use for models
--         in SavedModel format. Defaults is TensorFlow\'s default
--         signature def key.
--
--     -   @output_names@: Specifies a list of output tensor names for
--         models in FrozenGraph format. Set at most one API field, either:
--         @signature_def_key@ or @output_names@.
--
--     For example:
--     @{\"precision_mode\": \"FP32\", \"output_names\": [\"output:0\"]}@
--
-- 's3OutputLocation', 'outputConfig_s3OutputLocation' - Identifies the S3 bucket where you want Amazon SageMaker to store the
-- model artifacts. For example, @s3:\/\/bucket-name\/key-name-prefix@.
newOutputConfig ::
  -- | 's3OutputLocation'
  Prelude.Text ->
  OutputConfig
newOutputConfig pS3OutputLocation_ =
  OutputConfig'
    { targetDevice = Prelude.Nothing,
      targetPlatform = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      compilerOptions = Prelude.Nothing,
      s3OutputLocation = pS3OutputLocation_
    }

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

-- | The Amazon Web Services Key Management Service key (Amazon Web Services
-- KMS) that Amazon SageMaker uses to encrypt your output models with
-- Amazon S3 server-side encryption after compilation job. If you don\'t
-- provide a KMS key ID, Amazon SageMaker uses the default KMS key for
-- Amazon S3 for your role\'s account. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/UsingKMSEncryption.html KMS-Managed Encryption Keys>
-- in the /Amazon Simple Storage Service Developer Guide./
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

-- | Specifies additional parameters for compiler options in JSON format. The
-- compiler options are @TargetPlatform@ specific. It is required for
-- NVIDIA accelerators and highly recommended for CPU compilations. For any
-- other cases, it is optional to specify @CompilerOptions.@
--
-- -   @DTYPE@: Specifies the data type for the input. When compiling for
--     @ml_*@ (except for @ml_inf@) instances using PyTorch framework,
--     provide the data type (dtype) of the model\'s input. @\"float32\"@
--     is used if @\"DTYPE\"@ is not specified. Options for data type are:
--
--     -   float32: Use either @\"float\"@ or @\"float32\"@.
--
--     -   int64: Use either @\"int64\"@ or @\"long\"@.
--
--     For example, @{\"dtype\" : \"float32\"}@.
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
-- -   @EIA@: Compilation for the Elastic Inference Accelerator supports
--     the following compiler options:
--
--     -   @precision_mode@: Specifies the precision of compiled artifacts.
--         Supported values are @\"FP16\"@ and @\"FP32\"@. Default is
--         @\"FP32\"@.
--
--     -   @signature_def_key@: Specifies the signature to use for models
--         in SavedModel format. Defaults is TensorFlow\'s default
--         signature def key.
--
--     -   @output_names@: Specifies a list of output tensor names for
--         models in FrozenGraph format. Set at most one API field, either:
--         @signature_def_key@ or @output_names@.
--
--     For example:
--     @{\"precision_mode\": \"FP32\", \"output_names\": [\"output:0\"]}@
outputConfig_compilerOptions :: Lens.Lens' OutputConfig (Prelude.Maybe Prelude.Text)
outputConfig_compilerOptions = Lens.lens (\OutputConfig' {compilerOptions} -> compilerOptions) (\s@OutputConfig' {} a -> s {compilerOptions = a} :: OutputConfig)

-- | Identifies the S3 bucket where you want Amazon SageMaker to store the
-- model artifacts. For example, @s3:\/\/bucket-name\/key-name-prefix@.
outputConfig_s3OutputLocation :: Lens.Lens' OutputConfig Prelude.Text
outputConfig_s3OutputLocation = Lens.lens (\OutputConfig' {s3OutputLocation} -> s3OutputLocation) (\s@OutputConfig' {} a -> s {s3OutputLocation = a} :: OutputConfig)

instance Core.FromJSON OutputConfig where
  parseJSON =
    Core.withObject
      "OutputConfig"
      ( \x ->
          OutputConfig'
            Prelude.<$> (x Core..:? "TargetDevice")
            Prelude.<*> (x Core..:? "TargetPlatform")
            Prelude.<*> (x Core..:? "KmsKeyId")
            Prelude.<*> (x Core..:? "CompilerOptions")
            Prelude.<*> (x Core..: "S3OutputLocation")
      )

instance Prelude.Hashable OutputConfig where
  hashWithSalt _salt OutputConfig' {..} =
    _salt `Prelude.hashWithSalt` targetDevice
      `Prelude.hashWithSalt` targetPlatform
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` compilerOptions
      `Prelude.hashWithSalt` s3OutputLocation

instance Prelude.NFData OutputConfig where
  rnf OutputConfig' {..} =
    Prelude.rnf targetDevice
      `Prelude.seq` Prelude.rnf targetPlatform
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf compilerOptions
      `Prelude.seq` Prelude.rnf s3OutputLocation

instance Core.ToJSON OutputConfig where
  toJSON OutputConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TargetDevice" Core..=) Prelude.<$> targetDevice,
            ("TargetPlatform" Core..=)
              Prelude.<$> targetPlatform,
            ("KmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("CompilerOptions" Core..=)
              Prelude.<$> compilerOptions,
            Prelude.Just
              ("S3OutputLocation" Core..= s3OutputLocation)
          ]
      )
