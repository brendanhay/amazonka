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
-- Module      : Network.AWS.SageMaker.Types.InputConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.InputConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.Framework

-- | Contains information about the location of input model artifacts, the
-- name and shape of the expected data inputs, and the framework in which
-- the model was trained.
--
-- /See:/ 'newInputConfig' smart constructor.
data InputConfig = InputConfig'
  { -- | Specifies the framework version to use.
    --
    -- This API field is only supported for PyTorch framework versions @1.4@,
    -- @1.5@, and @1.6@ for cloud instance target devices: @ml_c4@, @ml_c5@,
    -- @ml_m4@, @ml_m5@, @ml_p2@, @ml_p3@, and @ml_g4dn@.
    frameworkVersion :: Prelude.Maybe Prelude.Text,
    -- | The S3 path where the model artifacts, which result from model training,
    -- are stored. This path must point to a single gzip compressed tar archive
    -- (.tar.gz suffix).
    s3Uri :: Prelude.Text,
    -- | Specifies the name and shape of the expected data inputs for your
    -- trained model with a JSON dictionary form. The data inputs are
    -- InputConfig$Framework specific.
    --
    -- -   @TensorFlow@: You must specify the name and shape (NHWC format) of
    --     the expected data inputs using a dictionary format for your trained
    --     model. The dictionary formats required for the console and CLI are
    --     different.
    --
    --     -   Examples for one input:
    --
    --         -   If using the console, @{\"input\":[1,1024,1024,3]}@
    --
    --         -   If using the CLI, @{\\\"input\\\":[1,1024,1024,3]}@
    --
    --     -   Examples for two inputs:
    --
    --         -   If using the console,
    --             @{\"data1\": [1,28,28,1], \"data2\":[1,28,28,1]}@
    --
    --         -   If using the CLI,
    --             @{\\\"data1\\\": [1,28,28,1], \\\"data2\\\":[1,28,28,1]}@
    --
    -- -   @KERAS@: You must specify the name and shape (NCHW format) of
    --     expected data inputs using a dictionary format for your trained
    --     model. Note that while Keras model artifacts should be uploaded in
    --     NHWC (channel-last) format, @DataInputConfig@ should be specified in
    --     NCHW (channel-first) format. The dictionary formats required for the
    --     console and CLI are different.
    --
    --     -   Examples for one input:
    --
    --         -   If using the console, @{\"input_1\":[1,3,224,224]}@
    --
    --         -   If using the CLI, @{\\\"input_1\\\":[1,3,224,224]}@
    --
    --     -   Examples for two inputs:
    --
    --         -   If using the console,
    --             @{\"input_1\": [1,3,224,224], \"input_2\":[1,3,224,224]} @
    --
    --         -   If using the CLI,
    --             @{\\\"input_1\\\": [1,3,224,224], \\\"input_2\\\":[1,3,224,224]}@
    --
    -- -   @MXNET\/ONNX\/DARKNET@: You must specify the name and shape (NCHW
    --     format) of the expected data inputs in order using a dictionary
    --     format for your trained model. The dictionary formats required for
    --     the console and CLI are different.
    --
    --     -   Examples for one input:
    --
    --         -   If using the console, @{\"data\":[1,3,1024,1024]}@
    --
    --         -   If using the CLI, @{\\\"data\\\":[1,3,1024,1024]}@
    --
    --     -   Examples for two inputs:
    --
    --         -   If using the console,
    --             @{\"var1\": [1,1,28,28], \"var2\":[1,1,28,28]} @
    --
    --         -   If using the CLI,
    --             @{\\\"var1\\\": [1,1,28,28], \\\"var2\\\":[1,1,28,28]}@
    --
    -- -   @PyTorch@: You can either specify the name and shape (NCHW format)
    --     of expected data inputs in order using a dictionary format for your
    --     trained model or you can specify the shape only using a list format.
    --     The dictionary formats required for the console and CLI are
    --     different. The list formats for the console and CLI are the same.
    --
    --     -   Examples for one input in dictionary format:
    --
    --         -   If using the console, @{\"input0\":[1,3,224,224]}@
    --
    --         -   If using the CLI, @{\\\"input0\\\":[1,3,224,224]}@
    --
    --     -   Example for one input in list format: @[[1,3,224,224]]@
    --
    --     -   Examples for two inputs in dictionary format:
    --
    --         -   If using the console,
    --             @{\"input0\":[1,3,224,224], \"input1\":[1,3,224,224]}@
    --
    --         -   If using the CLI,
    --             @{\\\"input0\\\":[1,3,224,224], \\\"input1\\\":[1,3,224,224]} @
    --
    --     -   Example for two inputs in list format:
    --         @[[1,3,224,224], [1,3,224,224]]@
    --
    -- -   @XGBOOST@: input data name and shape are not needed.
    --
    -- @DataInputConfig@ supports the following parameters for @CoreML@
    -- OutputConfig$TargetDevice (ML Model format):
    --
    -- -   @shape@: Input shape, for example
    --     @{\"input_1\": {\"shape\": [1,224,224,3]}}@. In addition to static
    --     input shapes, CoreML converter supports Flexible input shapes:
    --
    --     -   Range Dimension. You can use the Range Dimension feature if you
    --         know the input shape will be within some specific interval in
    --         that dimension, for example:
    --         @{\"input_1\": {\"shape\": [\"1..10\", 224, 224, 3]}}@
    --
    --     -   Enumerated shapes. Sometimes, the models are trained to work
    --         only on a select set of inputs. You can enumerate all supported
    --         input shapes, for example:
    --         @{\"input_1\": {\"shape\": [[1, 224, 224, 3], [1, 160, 160, 3]]}}@
    --
    -- -   @default_shape@: Default input shape. You can set a default shape
    --     during conversion for both Range Dimension and Enumerated Shapes.
    --     For example
    --     @{\"input_1\": {\"shape\": [\"1..10\", 224, 224, 3], \"default_shape\": [1, 224, 224, 3]}}@
    --
    -- -   @type@: Input type. Allowed values: @Image@ and @Tensor@. By
    --     default, the converter generates an ML Model with inputs of type
    --     Tensor (MultiArray). User can set input type to be Image. Image
    --     input type requires additional input parameters such as @bias@ and
    --     @scale@.
    --
    -- -   @bias@: If the input type is an Image, you need to provide the bias
    --     vector.
    --
    -- -   @scale@: If the input type is an Image, you need to provide a scale
    --     factor.
    --
    -- CoreML @ClassifierConfig@ parameters can be specified using
    -- OutputConfig$CompilerOptions. CoreML converter supports Tensorflow and
    -- PyTorch models. CoreML conversion examples:
    --
    -- -   Tensor type input:
    --
    --     -   @\"DataInputConfig\": {\"input_1\": {\"shape\": [[1,224,224,3], [1,160,160,3]], \"default_shape\": [1,224,224,3]}}@
    --
    -- -   Tensor type input without input name (PyTorch):
    --
    --     -   @\"DataInputConfig\": [{\"shape\": [[1,3,224,224], [1,3,160,160]], \"default_shape\": [1,3,224,224]}]@
    --
    -- -   Image type input:
    --
    --     -   @\"DataInputConfig\": {\"input_1\": {\"shape\": [[1,224,224,3], [1,160,160,3]], \"default_shape\": [1,224,224,3], \"type\": \"Image\", \"bias\": [-1,-1,-1], \"scale\": 0.007843137255}}@
    --
    --     -   @\"CompilerOptions\": {\"class_labels\": \"imagenet_labels_1000.txt\"}@
    --
    -- -   Image type input without input name (PyTorch):
    --
    --     -   @\"DataInputConfig\": [{\"shape\": [[1,3,224,224], [1,3,160,160]], \"default_shape\": [1,3,224,224], \"type\": \"Image\", \"bias\": [-1,-1,-1], \"scale\": 0.007843137255}]@
    --
    --     -   @\"CompilerOptions\": {\"class_labels\": \"imagenet_labels_1000.txt\"}@
    dataInputConfig :: Prelude.Text,
    -- | Identifies the framework in which the model was trained. For example:
    -- TENSORFLOW.
    framework :: Framework
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InputConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'frameworkVersion', 'inputConfig_frameworkVersion' - Specifies the framework version to use.
--
-- This API field is only supported for PyTorch framework versions @1.4@,
-- @1.5@, and @1.6@ for cloud instance target devices: @ml_c4@, @ml_c5@,
-- @ml_m4@, @ml_m5@, @ml_p2@, @ml_p3@, and @ml_g4dn@.
--
-- 's3Uri', 'inputConfig_s3Uri' - The S3 path where the model artifacts, which result from model training,
-- are stored. This path must point to a single gzip compressed tar archive
-- (.tar.gz suffix).
--
-- 'dataInputConfig', 'inputConfig_dataInputConfig' - Specifies the name and shape of the expected data inputs for your
-- trained model with a JSON dictionary form. The data inputs are
-- InputConfig$Framework specific.
--
-- -   @TensorFlow@: You must specify the name and shape (NHWC format) of
--     the expected data inputs using a dictionary format for your trained
--     model. The dictionary formats required for the console and CLI are
--     different.
--
--     -   Examples for one input:
--
--         -   If using the console, @{\"input\":[1,1024,1024,3]}@
--
--         -   If using the CLI, @{\\\"input\\\":[1,1024,1024,3]}@
--
--     -   Examples for two inputs:
--
--         -   If using the console,
--             @{\"data1\": [1,28,28,1], \"data2\":[1,28,28,1]}@
--
--         -   If using the CLI,
--             @{\\\"data1\\\": [1,28,28,1], \\\"data2\\\":[1,28,28,1]}@
--
-- -   @KERAS@: You must specify the name and shape (NCHW format) of
--     expected data inputs using a dictionary format for your trained
--     model. Note that while Keras model artifacts should be uploaded in
--     NHWC (channel-last) format, @DataInputConfig@ should be specified in
--     NCHW (channel-first) format. The dictionary formats required for the
--     console and CLI are different.
--
--     -   Examples for one input:
--
--         -   If using the console, @{\"input_1\":[1,3,224,224]}@
--
--         -   If using the CLI, @{\\\"input_1\\\":[1,3,224,224]}@
--
--     -   Examples for two inputs:
--
--         -   If using the console,
--             @{\"input_1\": [1,3,224,224], \"input_2\":[1,3,224,224]} @
--
--         -   If using the CLI,
--             @{\\\"input_1\\\": [1,3,224,224], \\\"input_2\\\":[1,3,224,224]}@
--
-- -   @MXNET\/ONNX\/DARKNET@: You must specify the name and shape (NCHW
--     format) of the expected data inputs in order using a dictionary
--     format for your trained model. The dictionary formats required for
--     the console and CLI are different.
--
--     -   Examples for one input:
--
--         -   If using the console, @{\"data\":[1,3,1024,1024]}@
--
--         -   If using the CLI, @{\\\"data\\\":[1,3,1024,1024]}@
--
--     -   Examples for two inputs:
--
--         -   If using the console,
--             @{\"var1\": [1,1,28,28], \"var2\":[1,1,28,28]} @
--
--         -   If using the CLI,
--             @{\\\"var1\\\": [1,1,28,28], \\\"var2\\\":[1,1,28,28]}@
--
-- -   @PyTorch@: You can either specify the name and shape (NCHW format)
--     of expected data inputs in order using a dictionary format for your
--     trained model or you can specify the shape only using a list format.
--     The dictionary formats required for the console and CLI are
--     different. The list formats for the console and CLI are the same.
--
--     -   Examples for one input in dictionary format:
--
--         -   If using the console, @{\"input0\":[1,3,224,224]}@
--
--         -   If using the CLI, @{\\\"input0\\\":[1,3,224,224]}@
--
--     -   Example for one input in list format: @[[1,3,224,224]]@
--
--     -   Examples for two inputs in dictionary format:
--
--         -   If using the console,
--             @{\"input0\":[1,3,224,224], \"input1\":[1,3,224,224]}@
--
--         -   If using the CLI,
--             @{\\\"input0\\\":[1,3,224,224], \\\"input1\\\":[1,3,224,224]} @
--
--     -   Example for two inputs in list format:
--         @[[1,3,224,224], [1,3,224,224]]@
--
-- -   @XGBOOST@: input data name and shape are not needed.
--
-- @DataInputConfig@ supports the following parameters for @CoreML@
-- OutputConfig$TargetDevice (ML Model format):
--
-- -   @shape@: Input shape, for example
--     @{\"input_1\": {\"shape\": [1,224,224,3]}}@. In addition to static
--     input shapes, CoreML converter supports Flexible input shapes:
--
--     -   Range Dimension. You can use the Range Dimension feature if you
--         know the input shape will be within some specific interval in
--         that dimension, for example:
--         @{\"input_1\": {\"shape\": [\"1..10\", 224, 224, 3]}}@
--
--     -   Enumerated shapes. Sometimes, the models are trained to work
--         only on a select set of inputs. You can enumerate all supported
--         input shapes, for example:
--         @{\"input_1\": {\"shape\": [[1, 224, 224, 3], [1, 160, 160, 3]]}}@
--
-- -   @default_shape@: Default input shape. You can set a default shape
--     during conversion for both Range Dimension and Enumerated Shapes.
--     For example
--     @{\"input_1\": {\"shape\": [\"1..10\", 224, 224, 3], \"default_shape\": [1, 224, 224, 3]}}@
--
-- -   @type@: Input type. Allowed values: @Image@ and @Tensor@. By
--     default, the converter generates an ML Model with inputs of type
--     Tensor (MultiArray). User can set input type to be Image. Image
--     input type requires additional input parameters such as @bias@ and
--     @scale@.
--
-- -   @bias@: If the input type is an Image, you need to provide the bias
--     vector.
--
-- -   @scale@: If the input type is an Image, you need to provide a scale
--     factor.
--
-- CoreML @ClassifierConfig@ parameters can be specified using
-- OutputConfig$CompilerOptions. CoreML converter supports Tensorflow and
-- PyTorch models. CoreML conversion examples:
--
-- -   Tensor type input:
--
--     -   @\"DataInputConfig\": {\"input_1\": {\"shape\": [[1,224,224,3], [1,160,160,3]], \"default_shape\": [1,224,224,3]}}@
--
-- -   Tensor type input without input name (PyTorch):
--
--     -   @\"DataInputConfig\": [{\"shape\": [[1,3,224,224], [1,3,160,160]], \"default_shape\": [1,3,224,224]}]@
--
-- -   Image type input:
--
--     -   @\"DataInputConfig\": {\"input_1\": {\"shape\": [[1,224,224,3], [1,160,160,3]], \"default_shape\": [1,224,224,3], \"type\": \"Image\", \"bias\": [-1,-1,-1], \"scale\": 0.007843137255}}@
--
--     -   @\"CompilerOptions\": {\"class_labels\": \"imagenet_labels_1000.txt\"}@
--
-- -   Image type input without input name (PyTorch):
--
--     -   @\"DataInputConfig\": [{\"shape\": [[1,3,224,224], [1,3,160,160]], \"default_shape\": [1,3,224,224], \"type\": \"Image\", \"bias\": [-1,-1,-1], \"scale\": 0.007843137255}]@
--
--     -   @\"CompilerOptions\": {\"class_labels\": \"imagenet_labels_1000.txt\"}@
--
-- 'framework', 'inputConfig_framework' - Identifies the framework in which the model was trained. For example:
-- TENSORFLOW.
newInputConfig ::
  -- | 's3Uri'
  Prelude.Text ->
  -- | 'dataInputConfig'
  Prelude.Text ->
  -- | 'framework'
  Framework ->
  InputConfig
newInputConfig pS3Uri_ pDataInputConfig_ pFramework_ =
  InputConfig'
    { frameworkVersion = Prelude.Nothing,
      s3Uri = pS3Uri_,
      dataInputConfig = pDataInputConfig_,
      framework = pFramework_
    }

-- | Specifies the framework version to use.
--
-- This API field is only supported for PyTorch framework versions @1.4@,
-- @1.5@, and @1.6@ for cloud instance target devices: @ml_c4@, @ml_c5@,
-- @ml_m4@, @ml_m5@, @ml_p2@, @ml_p3@, and @ml_g4dn@.
inputConfig_frameworkVersion :: Lens.Lens' InputConfig (Prelude.Maybe Prelude.Text)
inputConfig_frameworkVersion = Lens.lens (\InputConfig' {frameworkVersion} -> frameworkVersion) (\s@InputConfig' {} a -> s {frameworkVersion = a} :: InputConfig)

-- | The S3 path where the model artifacts, which result from model training,
-- are stored. This path must point to a single gzip compressed tar archive
-- (.tar.gz suffix).
inputConfig_s3Uri :: Lens.Lens' InputConfig Prelude.Text
inputConfig_s3Uri = Lens.lens (\InputConfig' {s3Uri} -> s3Uri) (\s@InputConfig' {} a -> s {s3Uri = a} :: InputConfig)

-- | Specifies the name and shape of the expected data inputs for your
-- trained model with a JSON dictionary form. The data inputs are
-- InputConfig$Framework specific.
--
-- -   @TensorFlow@: You must specify the name and shape (NHWC format) of
--     the expected data inputs using a dictionary format for your trained
--     model. The dictionary formats required for the console and CLI are
--     different.
--
--     -   Examples for one input:
--
--         -   If using the console, @{\"input\":[1,1024,1024,3]}@
--
--         -   If using the CLI, @{\\\"input\\\":[1,1024,1024,3]}@
--
--     -   Examples for two inputs:
--
--         -   If using the console,
--             @{\"data1\": [1,28,28,1], \"data2\":[1,28,28,1]}@
--
--         -   If using the CLI,
--             @{\\\"data1\\\": [1,28,28,1], \\\"data2\\\":[1,28,28,1]}@
--
-- -   @KERAS@: You must specify the name and shape (NCHW format) of
--     expected data inputs using a dictionary format for your trained
--     model. Note that while Keras model artifacts should be uploaded in
--     NHWC (channel-last) format, @DataInputConfig@ should be specified in
--     NCHW (channel-first) format. The dictionary formats required for the
--     console and CLI are different.
--
--     -   Examples for one input:
--
--         -   If using the console, @{\"input_1\":[1,3,224,224]}@
--
--         -   If using the CLI, @{\\\"input_1\\\":[1,3,224,224]}@
--
--     -   Examples for two inputs:
--
--         -   If using the console,
--             @{\"input_1\": [1,3,224,224], \"input_2\":[1,3,224,224]} @
--
--         -   If using the CLI,
--             @{\\\"input_1\\\": [1,3,224,224], \\\"input_2\\\":[1,3,224,224]}@
--
-- -   @MXNET\/ONNX\/DARKNET@: You must specify the name and shape (NCHW
--     format) of the expected data inputs in order using a dictionary
--     format for your trained model. The dictionary formats required for
--     the console and CLI are different.
--
--     -   Examples for one input:
--
--         -   If using the console, @{\"data\":[1,3,1024,1024]}@
--
--         -   If using the CLI, @{\\\"data\\\":[1,3,1024,1024]}@
--
--     -   Examples for two inputs:
--
--         -   If using the console,
--             @{\"var1\": [1,1,28,28], \"var2\":[1,1,28,28]} @
--
--         -   If using the CLI,
--             @{\\\"var1\\\": [1,1,28,28], \\\"var2\\\":[1,1,28,28]}@
--
-- -   @PyTorch@: You can either specify the name and shape (NCHW format)
--     of expected data inputs in order using a dictionary format for your
--     trained model or you can specify the shape only using a list format.
--     The dictionary formats required for the console and CLI are
--     different. The list formats for the console and CLI are the same.
--
--     -   Examples for one input in dictionary format:
--
--         -   If using the console, @{\"input0\":[1,3,224,224]}@
--
--         -   If using the CLI, @{\\\"input0\\\":[1,3,224,224]}@
--
--     -   Example for one input in list format: @[[1,3,224,224]]@
--
--     -   Examples for two inputs in dictionary format:
--
--         -   If using the console,
--             @{\"input0\":[1,3,224,224], \"input1\":[1,3,224,224]}@
--
--         -   If using the CLI,
--             @{\\\"input0\\\":[1,3,224,224], \\\"input1\\\":[1,3,224,224]} @
--
--     -   Example for two inputs in list format:
--         @[[1,3,224,224], [1,3,224,224]]@
--
-- -   @XGBOOST@: input data name and shape are not needed.
--
-- @DataInputConfig@ supports the following parameters for @CoreML@
-- OutputConfig$TargetDevice (ML Model format):
--
-- -   @shape@: Input shape, for example
--     @{\"input_1\": {\"shape\": [1,224,224,3]}}@. In addition to static
--     input shapes, CoreML converter supports Flexible input shapes:
--
--     -   Range Dimension. You can use the Range Dimension feature if you
--         know the input shape will be within some specific interval in
--         that dimension, for example:
--         @{\"input_1\": {\"shape\": [\"1..10\", 224, 224, 3]}}@
--
--     -   Enumerated shapes. Sometimes, the models are trained to work
--         only on a select set of inputs. You can enumerate all supported
--         input shapes, for example:
--         @{\"input_1\": {\"shape\": [[1, 224, 224, 3], [1, 160, 160, 3]]}}@
--
-- -   @default_shape@: Default input shape. You can set a default shape
--     during conversion for both Range Dimension and Enumerated Shapes.
--     For example
--     @{\"input_1\": {\"shape\": [\"1..10\", 224, 224, 3], \"default_shape\": [1, 224, 224, 3]}}@
--
-- -   @type@: Input type. Allowed values: @Image@ and @Tensor@. By
--     default, the converter generates an ML Model with inputs of type
--     Tensor (MultiArray). User can set input type to be Image. Image
--     input type requires additional input parameters such as @bias@ and
--     @scale@.
--
-- -   @bias@: If the input type is an Image, you need to provide the bias
--     vector.
--
-- -   @scale@: If the input type is an Image, you need to provide a scale
--     factor.
--
-- CoreML @ClassifierConfig@ parameters can be specified using
-- OutputConfig$CompilerOptions. CoreML converter supports Tensorflow and
-- PyTorch models. CoreML conversion examples:
--
-- -   Tensor type input:
--
--     -   @\"DataInputConfig\": {\"input_1\": {\"shape\": [[1,224,224,3], [1,160,160,3]], \"default_shape\": [1,224,224,3]}}@
--
-- -   Tensor type input without input name (PyTorch):
--
--     -   @\"DataInputConfig\": [{\"shape\": [[1,3,224,224], [1,3,160,160]], \"default_shape\": [1,3,224,224]}]@
--
-- -   Image type input:
--
--     -   @\"DataInputConfig\": {\"input_1\": {\"shape\": [[1,224,224,3], [1,160,160,3]], \"default_shape\": [1,224,224,3], \"type\": \"Image\", \"bias\": [-1,-1,-1], \"scale\": 0.007843137255}}@
--
--     -   @\"CompilerOptions\": {\"class_labels\": \"imagenet_labels_1000.txt\"}@
--
-- -   Image type input without input name (PyTorch):
--
--     -   @\"DataInputConfig\": [{\"shape\": [[1,3,224,224], [1,3,160,160]], \"default_shape\": [1,3,224,224], \"type\": \"Image\", \"bias\": [-1,-1,-1], \"scale\": 0.007843137255}]@
--
--     -   @\"CompilerOptions\": {\"class_labels\": \"imagenet_labels_1000.txt\"}@
inputConfig_dataInputConfig :: Lens.Lens' InputConfig Prelude.Text
inputConfig_dataInputConfig = Lens.lens (\InputConfig' {dataInputConfig} -> dataInputConfig) (\s@InputConfig' {} a -> s {dataInputConfig = a} :: InputConfig)

-- | Identifies the framework in which the model was trained. For example:
-- TENSORFLOW.
inputConfig_framework :: Lens.Lens' InputConfig Framework
inputConfig_framework = Lens.lens (\InputConfig' {framework} -> framework) (\s@InputConfig' {} a -> s {framework = a} :: InputConfig)

instance Prelude.FromJSON InputConfig where
  parseJSON =
    Prelude.withObject
      "InputConfig"
      ( \x ->
          InputConfig'
            Prelude.<$> (x Prelude..:? "FrameworkVersion")
            Prelude.<*> (x Prelude..: "S3Uri")
            Prelude.<*> (x Prelude..: "DataInputConfig")
            Prelude.<*> (x Prelude..: "Framework")
      )

instance Prelude.Hashable InputConfig

instance Prelude.NFData InputConfig

instance Prelude.ToJSON InputConfig where
  toJSON InputConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("FrameworkVersion" Prelude..=)
              Prelude.<$> frameworkVersion,
            Prelude.Just ("S3Uri" Prelude..= s3Uri),
            Prelude.Just
              ("DataInputConfig" Prelude..= dataInputConfig),
            Prelude.Just ("Framework" Prelude..= framework)
          ]
      )
